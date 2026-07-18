// ADR 0002 spike: rewrite lib/cljs.core.js into an esbuild-shakeable form.
// Phase A: singly-assigned $APP.p -> plain var p (defs become `var p = ...`,
//          every other reference becomes `p`).
// Phase B: re-parse; fold `var p = function ...` plus its contiguous
//          `p.x = ...` installs into var p = /* @__PURE__ */ (IIFE);
//          annotate `var p = new X(...)` initializers as PURE.
// Multi-assigned props (prototype cursors like $APP.g) stay on $APP.
import fs from 'fs';
import * as acorn from 'acorn';

const src = fs.readFileSync('../lib/cljs.core.js', 'utf8');

function parse(s) { return acorn.parse(s, { ecmaVersion: 2022, sourceType: 'module' }); }
function walk(root, cb) {
  const stack = [root];
  while (stack.length) {
    const n = stack.pop();
    cb(n);
    for (const k in n) {
      if (k === 'type' || k === 'start' || k === 'end') continue;
      const v = n[k];
      if (v && typeof v === 'object') {
        if (Array.isArray(v)) { for (const x of v) if (x && x.type) stack.push(x); }
        else if (v.type) stack.push(v);
      }
    }
  }
}

// ---- phase A -------------------------------------------------------------
const ast = parse(src);
const assignCount = {};
const topLevelDefLhs = new Set(); // start offsets of def-site lhs members
for (const st of ast.body) {
  if (st.type !== 'ExpressionStatement') continue;
  const e = st.expression;
  if (e.type !== 'AssignmentExpression') continue;
  const l = e.left;
  if (l.type === 'MemberExpression' && !l.computed &&
      l.object.type === 'Identifier' && l.object.name === '$APP') {
    assignCount[l.property.name] = (assignCount[l.property.name] || 0) + 1;
  }
}
const single = new Set(Object.keys(assignCount).filter((p) => assignCount[p] === 1));
// prototype-stamp cursors: multi-assigned props whose every assignment rhs is
// <X>.prototype ('g' in short-name builds, '$JSCompiler_prototypeAlias$$' with
// pseudo-names)
const protoRhs = {};
for (const st of ast.body) {
  if (st.type !== 'ExpressionStatement') continue;
  const e = st.expression;
  if (e.type !== 'AssignmentExpression') continue;
  const l = e.left;
  if (l.type === 'MemberExpression' && !l.computed &&
      l.object.type === 'Identifier' && l.object.name === '$APP') {
    const isProto = e.right.type === 'MemberExpression' && !e.right.computed &&
      e.right.property.name === 'prototype';
    protoRhs[l.property.name] = (protoRhs[l.property.name] || 0) + (isProto ? 1 : 0);
  }
}
const cursorProps = new Set(Object.keys(assignCount).filter(
  (p) => assignCount[p] > 1 && protoRhs[p] === assignCount[p]));
console.log('cursor props:', [...cursorProps]);
const isCursor = (name) => cursorProps.has(name);
const cursorRe = () => new RegExp('\\$APP\\.(?:' + [...cursorProps].map((c) => c.replace(/\$/g, '\\$')).join('|') + ')(?![A-Za-z0-9_$])', 'g');

console.log('singly-assigned props:', single.size, '/', Object.keys(assignCount).length);

for (const st of ast.body) {
  if (st.type !== 'ExpressionStatement') continue;
  const e = st.expression;
  if (e.type !== 'AssignmentExpression') continue;
  const l = e.left;
  if (l.type === 'MemberExpression' && !l.computed &&
      l.object.type === 'Identifier' && l.object.name === '$APP' &&
      single.has(l.property.name)) {
    topLevelDefLhs.add(l.start);
  }
}

const editsA = [];
walk(ast, (n) => {
  if (n.type === 'MemberExpression' && !n.computed &&
      n.object.type === 'Identifier' && n.object.name === '$APP' &&
      single.has(n.property.name)) {
    const text = topLevelDefLhs.has(n.start) ? `var ${n.property.name}` : n.property.name;
    editsA.push({ start: n.start, end: n.end, text });
  }
});
// nested member chains produce overlapping refs ($APP.p and $APP.p.x's object
// are the same node, fine) - dedupe by start
const seen = new Set();
const uniq = editsA.filter((e) => !seen.has(e.start) && seen.add(e.start));
uniq.sort((a, b) => b.start - a.start);
let mid = src;
for (const e of uniq) mid = mid.slice(0, e.start) + e.text + mid.slice(e.end);
console.log('phase A rewrites:', uniq.length);

// ---- phase B -------------------------------------------------------------
const ast2 = parse(mid);
const stmts = ast2.body;
const editsB = [];
let folded = 0, pureNew = 0;
for (let i = 0; i < stmts.length; i++) {
  const st = stmts[i];
  if (st.type !== 'VariableDeclaration' || st.declarations.length !== 1) continue;
  const d = st.declarations[0];
  if (!d.init) continue;
  const name = d.id.name;
  if (!single.has(name)) continue; // only ones we created
  if (d.init.type === 'NewExpression') {
    editsB.push({ start: d.init.start, end: d.init.start, text: '/* @__PURE__ */ ' });
    pureNew++;
    continue;
  }
  if (d.init.type !== 'FunctionExpression') continue;
  // contiguous installs name.x = ...
  const installs = [];
  for (let j = i + 1; j < stmts.length; j++) {
    const nx = stmts[j];
    if (nx.type !== 'ExpressionStatement' || nx.expression.type !== 'AssignmentExpression') break;
    const nl = nx.expression.left;
    if (nl.type === 'MemberExpression' && !nl.computed &&
        nl.object.type === 'Identifier' && nl.object.name === name) {
      installs.push(j);
    } else break;
  }
  if (!installs.length) continue;
  const rhsSrc = mid.slice(d.init.start, d.init.end);
  // installs may read the var eagerly (arity aliasing like p.A = p.o), which
  // inside the IIFE is undefined - route self-references through the local f
  const selfRef = new RegExp(`(?<![A-Za-z0-9_$.])${name.replace(/\$/g, '\\$')}(?![A-Za-z0-9_$])`, 'g');
  const bodyParts = installs.map((j) => {
    const nx = stmts[j];
    const nl = nx.expression.left;
    const val = mid.slice(nx.expression.right.start, nx.expression.right.end).replace(selfRef, '$self');
    return `$self.${nl.property.name}=${val};`;
  });
  editsB.push({
    start: st.start, end: stmts[installs[installs.length - 1]].end,
    text: `var ${name}=/* @__PURE__ */(function(){var $self=${rhsSrc};${bodyParts.join('')}return $self})();`,
  });
  folded++;
}
console.log('folded fn+installs:', folded, ' pure-new:', pureNew);

editsB.sort((a, b) => b.start - a.start);
let out = mid;
for (const e of editsB) out = out.slice(0, e.start) + e.text + out.slice(e.end);

fs.writeFileSync('cljs.core.shaken.js', out);
console.log('wrote cljs.core.shaken.js', out.length, 'bytes');
parse(out);
console.log('reparse OK');

// ---- phase C: fold type constructors + their prototype-stamp segment ------
// $APP.g = T.prototype; ($APP.g.x = ...)*  becomes part of
// var T = /* @__PURE__ */ (function(){var f=<ctor>; var g=f.prototype; ...; return f})();
// placed AT THE SEGMENT position (the ctor def moves down). Safe only when:
//  - T is one of our singly-assigned vars whose init is a FunctionExpression
//  - no eager (top-level, outside function bodies) reference to T exists
//    between the original def and the segment
//  - the segment target is not a native (Date etc.)
const ast3 = parse(out);
const stmts3 = ast3.body;
// index defs: var T = function
const defIdx = {};
for (let i = 0; i < stmts3.length; i++) {
  const st = stmts3[i];
  if (st.type === 'VariableDeclaration' && st.declarations.length === 1) {
    const d = st.declarations[0];
    if (d.init && d.init.type === 'FunctionExpression' && single.has(d.id.name)) {
      defIdx[d.id.name] = i;
    }
  }
}
function eagerRefs(node, name, acc) {
  const stack = [node];
  while (stack.length) {
    const n = stack.pop();
    if (n.type === 'FunctionExpression' || n.type === 'FunctionDeclaration' ||
        n.type === 'ArrowFunctionExpression') continue; // deferred
    if (n.type === 'Identifier' && n.name === name) { acc.push(n); continue; }
    for (const k in n) {
      if (k === 'type' || k === 'start' || k === 'end') continue;
      const v = n[k];
      if (v && typeof v === 'object') {
        if (Array.isArray(v)) { for (const x of v) if (x && x.type) stack.push(x); }
        else if (v.type) stack.push(v);
      }
    }
  }
}
const editsC = [];
let foldedTypes = 0, skippedTypes = [];
for (let i = 0; i < stmts3.length; i++) {
  const st = stmts3[i];
  if (st.type !== 'ExpressionStatement' || st.expression.type !== 'AssignmentExpression') continue;
  const e = st.expression;
  const l = e.left;
  // $APP.g = T.prototype
  if (!(l.type === 'MemberExpression' && !l.computed &&
        l.object.type === 'Identifier' && l.object.name === '$APP' &&
        isCursor(l.property.name))) continue;
  if (!(e.right.type === 'MemberExpression' && e.right.property.name === 'prototype' &&
        e.right.object.type === 'Identifier')) continue;
  const T = e.right.object.name;
  // collect the contiguous stamp run $APP.g.x = ...
  const run = [];
  for (let j = i + 1; j < stmts3.length; j++) {
    const nx = stmts3[j];
    if (nx.type !== 'ExpressionStatement' || nx.expression.type !== 'AssignmentExpression') break;
    const nl = nx.expression.left;
    if (nl.type === 'MemberExpression' && !nl.computed &&
        nl.object.type === 'MemberExpression' && !nl.object.computed &&
        nl.object.object.type === 'Identifier' && nl.object.object.name === '$APP' &&
        isCursor(nl.object.property.name)) { run.push(j); } else break;
  }
  if (!(T in defIdx)) { skippedTypes.push(T); continue; }
  const di = defIdx[T];
  if (di > i) { skippedTypes.push(T + '(def-after-stamp)'); continue; }
  // eager refs to T strictly between def and segment?
  let eager = [];
  for (let k = di + 1; k < i; k++) eagerRefs(stmts3[k], T, eager);
  if (eager.length) { skippedTypes.push(T + '(eager:' + eager.length + ')'); continue; }
  const defSt = stmts3[di];
  const ctorSrc = out.slice(defSt.declarations[0].init.start, defSt.declarations[0].init.end);
  const selfRef = new RegExp(`(?<![A-Za-z0-9_$.])${T.replace(/\$/g, '\\$')}(?![A-Za-z0-9_$])`, 'g');
  const stampParts = run.map((j) => {
    const nx = stmts3[j];
    const key = nx.expression.left.property.name;
    const val = out.slice(nx.expression.right.start, nx.expression.right.end).replace(selfRef, '$self');
    return `g$p.${key}=${val};`;
  });
  // remove original def, replace segment start..runEnd with folded init
  editsC.push({ start: defSt.start, end: defSt.end, text: '' });
  const segEnd = run.length ? stmts3[run[run.length - 1]].end : st.end;
  editsC.push({
    start: st.start, end: segEnd,
    text: `var ${T}=/* @__PURE__ */(function(){var $self=${ctorSrc.replace(selfRef, '$self')};var g$p=$self.prototype;${stampParts.join('')}return $self})();`,
  });
  foldedTypes++;
}
console.log('phase C folded types:', foldedTypes, 'skipped:', skippedTypes.length, skippedTypes.slice(0, 12));
editsC.sort((a, b) => b.start - a.start);
let out2 = out;
for (const e of editsC) out2 = out2.slice(0, e.start) + e.text + out2.slice(e.end);
fs.writeFileSync('cljs.core.shaken.js', out2);
parse(out2);
console.log('phase C reparse OK,', out2.length, 'bytes');

// ---- phase D: hoisted vars -> declaration-at-assignment -------------------
// Closure emits `var ha,oa,...;` then `ha = <rhs>;` later. The assignment is a
// side effect esbuild must keep. For names assigned exactly once at top level,
// drop the name from the hoist list and turn the assignment into `var ha = <rhs>;`
const ast4 = parse(out2);
const stmts4 = ast4.body;
const hoisted = new Set();
for (const st of stmts4) {
  if (st.type === 'VariableDeclaration') {
    for (const d of st.declarations) if (!d.init) hoisted.add(d.id.name);
  }
}
// count top-level assignments and detect any nested (non-top-level) writes
const topAssign = {};
const nestedWrite = new Set();
{
  // nested writes: walk everything below top level
  for (const st of stmts4) {
    if (st.type === 'ExpressionStatement' && st.expression.type === 'AssignmentExpression' &&
        st.expression.left.type === 'Identifier' && hoisted.has(st.expression.left.name)) {
      topAssign[st.expression.left.name] = (topAssign[st.expression.left.name] || 0) + 1;
      // still walk the RHS for nested writes
      walk(st.expression.right, (n) => {
        if (n.type === 'AssignmentExpression' && n.left.type === 'Identifier' && hoisted.has(n.left.name)) {
          nestedWrite.add(n.left.name);
        }
      });
    } else {
      walk(st, (n) => {
        if (n.type === 'AssignmentExpression' && n.left.type === 'Identifier' && hoisted.has(n.left.name)) {
          nestedWrite.add(n.left.name);
        }
      });
    }
  }
}
const promotable = new Set([...hoisted].filter((n) => topAssign[n] === 1 && !nestedWrite.has(n)));
console.log('hoisted:', hoisted.size, 'promotable:', promotable.size);

const editsD = [];
for (const st of stmts4) {
  if (st.type === 'VariableDeclaration') {
    const keep = st.declarations.filter((d) => d.init || !promotable.has(d.id.name));
    if (keep.length !== st.declarations.length) {
      const text = keep.length
        ? 'var ' + keep.map((d) => out2.slice(d.start, d.end)).join(',') + ';'
        : '';
      editsD.push({ start: st.start, end: st.end, text });
    }
  } else if (st.type === 'ExpressionStatement' && st.expression.type === 'AssignmentExpression' &&
             st.expression.left.type === 'Identifier' && promotable.has(st.expression.left.name)) {
    editsD.push({ start: st.expression.left.start, end: st.expression.left.start, text: 'var ' });
  }
}
editsD.sort((a, b) => b.start - a.start);
let out3 = out2;
for (const e of editsD) out3 = out3.slice(0, e.start) + e.text + out3.slice(e.end);
fs.writeFileSync('cljs.core.shaken.js', out3);
parse(out3);
console.log('phase D reparse OK,', out3.length, 'bytes');

// ---- phase E: for remaining cursor segments, move stamps UP into the def --
// var T = /* @__PURE__ */(function(){var f=<ctor>; <T-anchored stmts, T->f>;
//   var g=f.prototype; <stamps>; return f})();  at the DEF position.
// Moving stamps earlier is safe when every eager identifier in the moved
// values is declared before the def (function bodies are deferred).
const NATIVES = new Set(['Date','String','Number','Boolean','Array','Object','Function','Symbol','Math','JSON','RegExp','Error','TypeError','Map','Set','Infinity','NaN','undefined','isNaN','parseFloat','parseInt','decodeURIComponent','encodeURIComponent','globalThis','console']);
let src5 = fs.readFileSync('cljs.core.shaken.js', 'utf8');
const ast5 = parse(src5);
const stmts5 = ast5.body;
const declPos = {};
for (let i = 0; i < stmts5.length; i++) {
  const st = stmts5[i];
  if (st.type === 'VariableDeclaration') for (const d of st.declarations) declPos[d.id.name] = i;
}
function eagerIds(node) {
  const acc = [];
  const stack = [node];
  while (stack.length) {
    const n = stack.pop();
    if (n.type === 'FunctionExpression' || n.type === 'ArrowFunctionExpression') continue;
    if (n.type === 'Identifier') { acc.push(n.name); continue; }
    if (n.type === 'MemberExpression' && !n.computed) { stack.push(n.object); continue; }
    for (const k in n) {
      if (k === 'type' || k === 'start' || k === 'end') continue;
      const v = n[k];
      if (v && typeof v === 'object') {
        if (Array.isArray(v)) { for (const x of v) if (x && x.type) stack.push(x); }
        else if (v.type) stack.push(v);
      }
    }
  }
  return acc;
}
function lhsRoot(l) {
  let n = l;
  while (n.type === 'MemberExpression') n = n.object;
  return n.type === 'Identifier' ? n.name : null;
}
const editsE = [];
let foldedE = 0, skippedE = [];
for (let i = 0; i < stmts5.length; i++) {
  const st = stmts5[i];
  if (st.type !== 'ExpressionStatement' || st.expression.type !== 'AssignmentExpression') continue;
  const e = st.expression;
  const l = e.left;
  if (!(l.type === 'MemberExpression' && !l.computed &&
        l.object.type === 'Identifier' && l.object.name === '$APP' && isCursor(l.property.name))) continue;
  if (!(e.right.type === 'MemberExpression' && e.right.property.name === 'prototype' &&
        e.right.object.type === 'Identifier')) continue;
  const T = e.right.object.name;
  if (!(T in declPos) || NATIVES.has(T)) { skippedE.push(T + '(no-var)'); continue; }
  const di = declPos[T];
  const defSt = stmts5[di];
  const defD = defSt.declarations && defSt.declarations[0];
  if (!defD || !defD.init || defD.init.type !== 'FunctionExpression' || di > i) { skippedE.push(T + '(def-shape)'); continue; }
  // stamps run
  const run = [];
  for (let j = i + 1; j < stmts5.length; j++) {
    const nx = stmts5[j];
    if (nx.type !== 'ExpressionStatement' || nx.expression.type !== 'AssignmentExpression') break;
    const nl = nx.expression.left;
    if (nl.type === 'MemberExpression' && !nl.computed &&
        nl.object.type === 'MemberExpression' && !nl.object.computed &&
        nl.object.object.type === 'Identifier' && nl.object.object.name === '$APP' &&
        isCursor(nl.object.property.name)) { run.push(j); } else break;
  }
  // T-anchored statements between def and segment (T.x=, T.prototype=...)
  const anchored = [];
  let blocked = null;
  for (let k = di + 1; k < i; k++) {
    const sk = stmts5[k];
    if (sk.type === 'ExpressionStatement' && sk.expression.type === 'AssignmentExpression' &&
        lhsRoot(sk.expression.left) === T) anchored.push(k);
  }
  // eager-id safety over everything we move (anchored stmt RHS + stamp values)
  const movedValueNodes = [
    ...anchored.map((k) => stmts5[k].expression.right),
    ...run.map((j) => stmts5[j].expression.right),
  ];
  for (const vn of movedValueNodes) {
    for (const id of eagerIds(vn)) {
      if (id === T || id === '$APP' || NATIVES.has(id)) continue;
      if (!(id in declPos) || declPos[id] > di) { blocked = T + '(eager:' + id + ')'; break; }
    }
    if (blocked) break;
  }
  if (blocked) { skippedE.push(blocked); continue; }
  const selfRef = new RegExp(`(?<![A-Za-z0-9_$.])${T.replace(/\$/g, '\\$')}(?![A-Za-z0-9_$])`, 'g');
  const ctorSrc = src5.slice(defD.init.start, defD.init.end).replace(selfRef, '$self');
  const anchoredSrc = anchored.map((k) => {
    const sk = stmts5[k];
    return src5.slice(sk.start, sk.end).replace(selfRef, '$self');
  }).join('');
  const stampSrc = run.map((j) => {
    const nx = stmts5[j];
    const key = nx.expression.left.property.name;
    const val = src5.slice(nx.expression.right.start, nx.expression.right.end).replace(selfRef, '$self');
    return `g$p.${key}=${val};`;
  }).join('');
  editsE.push({ start: defSt.start, end: defSt.end,
    text: `var ${T}=/* @__PURE__ */(function(){var $self=${ctorSrc};${anchoredSrc}var g$p=$self.prototype;${stampSrc}return $self})();` });
  for (const k of anchored) editsE.push({ start: stmts5[k].start, end: stmts5[k].end, text: '' });
  editsE.push({ start: st.start, end: run.length ? stmts5[run[run.length - 1]].end : st.end, text: '' });
  foldedE++;
}
console.log('phase E folded:', foldedE, 'skipped:', skippedE.length, skippedE.slice(0, 12));
editsE.sort((a, b) => b.start - a.start);
for (const e of editsE) src5 = src5.slice(0, e.start) + e.text + src5.slice(e.end);
fs.writeFileSync('cljs.core.shaken.js', src5);
parse(src5);
console.log('phase E reparse OK,', src5.length, 'bytes');

// ---- phase F: relocate the sentinel, then fixpoint-fold all remaining -----
// writes into earlier-declared vars (T.x=, T.prototype=..., cursor segments)
// into T's initializer as a PURE IIFE. Eager-gated like phase E.
{
  let text = fs.readFileSync('cljs.core.shaken.js', 'utf8');
  // move the sentinel decl (blocker of most phase-E folds) to the top: its
  // only eager deps are globals
  {
    const a = parse(text);
    for (const st of a.body) {
      if (st.type === 'VariableDeclaration' && st.declarations.length === 1 && st.declarations[0].init) {
        const initSrc = text.slice(st.declarations[0].init.start, st.declarations[0].init.end);
        // the sentinel init depends only on globals; type inits merely
        // reference the sentinel var (whose name may contain the marker too)
        if (initSrc.includes('.PROTOCOL_SENTINEL') &&
            eagerIds(st.declarations[0].init).every((id) => NATIVES.has(id))) {
          const stSrc = text.slice(st.start, st.end);
          text = text.slice(0, st.start) + text.slice(st.end);
          const anchor = text.indexOf('\n', text.indexOf('export const shadow$provide'));
          text = text.slice(0, anchor + 1) + stSrc + '\n' + text.slice(anchor + 1);
          console.log('phase F: sentinel relocated to top');
          break;
        }
      }
    }
  }
  // split multi-declarator var statements so every def is foldable
  {
    const a = parse(text);
    const edits = [];
    for (const st of a.body) {
      if (st.type !== 'VariableDeclaration' || st.declarations.length < 2) continue;
      if (!st.declarations.every((d) => d.init)) continue; // hoist lists stay
      const parts = st.declarations.map((d) => `var ${text.slice(d.start, d.end)};`).join('');
      edits.push({ start: st.start, end: st.end, text: parts });
    }
    console.log('phase F: split multi-declarators:', edits.length);
    edits.sort((x, y) => y.start - x.start);
    for (const e of edits) text = text.slice(0, e.start) + e.text + text.slice(e.end);
  }
  const lhsRootF = (l) => { let n = l; while (n.type === 'MemberExpression') n = n.object; return n.type === 'Identifier' ? n.name : null; };
  for (let round = 1; round <= 6; round++) {
    const a = parse(text);
    const B = a.body;
    const decl = {};
    for (let i = 0; i < B.length; i++) {
      const st = B[i];
      if (st.type === 'VariableDeclaration') for (const d of st.declarations) decl[d.id.name] = i;
    }
    // group consecutive side-effect assignments by their root var
    const groups = {}; // T -> [stmt idx]
    for (let i = 0; i < B.length; i++) {
      const st = B[i];
      if (st.type !== 'ExpressionStatement' || st.expression.type !== 'AssignmentExpression') continue;
      let l = st.expression.left, T = null, cursor = false;
      // cursor segment start: $APP.g = T.prototype
      const isCursorStart = l.type === 'MemberExpression' && !l.computed && l.object.type === 'Identifier' &&
        l.object.name === '$APP' && isCursor(l.property.name);
      if (isCursorStart && st.expression.right.type === 'MemberExpression' &&
          st.expression.right.property.name === 'prototype' && st.expression.right.object.type === 'Identifier') {
        T = st.expression.right.object.name; cursor = true;
      } else {
        T = lhsRootF(l);
        if (T === '$APP') continue;
      }
      if (!T || !(T in decl) || NATIVES.has(T)) continue;
      (groups[T] = groups[T] || []).push(i);
      if (cursor) {
        for (let j = i + 1; j < B.length; j++) {
          const nx = B[j];
          if (nx.type === 'ExpressionStatement' && nx.expression.type === 'AssignmentExpression' &&
              nx.expression.left.type === 'MemberExpression' &&
              nx.expression.left.object.type === 'MemberExpression' &&
              nx.expression.left.object.object.type === 'Identifier' &&
              nx.expression.left.object.object.name === '$APP' &&
              isCursor(nx.expression.left.object.property.name)) { groups[T].push(j); i = j; } else break;
        }
      }
    }
    const edits = [];
    let folds = 0;
    const blockers = {};
    for (const [T, idxs] of Object.entries(groups)) {
      const di = decl[T];
      const defSt = B[di];
      if (defSt.declarations.length !== 1) continue;
      const d = defSt.declarations[0];
      if (!d.init) continue;
      if (idxs.some((i) => i < di)) continue;
      // eager safety for all moved values
      let blocked = null;
      for (const i of idxs) {
        for (const id of eagerIds(B[i].expression.right)) {
          if (id === T || id === '$APP' || NATIVES.has(id)) continue;
          if (!(id in decl) || decl[id] > di) { blocked = id; break; }
        }
        if (blocked) break;
      }
      if (blocked) { blockers[blocked] = (blockers[blocked] || 0) + 1; continue; }
      const selfRef = new RegExp(`(?<![A-Za-z0-9_$.])${T.replace(/\$/g, '\\$')}(?![A-Za-z0-9_$])`, 'g');
      // if init is already our PURE IIFE, splice new statements before `return f`
      const initSrc = text.slice(d.init.start, d.init.end);
      const moved = idxs.map((i) => {
        const st = B[i];
        let s = text.slice(st.start, st.end);
        if (!s.endsWith(';')) s += ';';
        // cursor statements: $APP.g=f.prototype / $APP.g.x=v -> local g
        s = s.replace(cursorRe(), 'g$p');
        return s.replace(selfRef, '$self');
      }).join('');
      let newInit;
      if (initSrc.startsWith('/* @__PURE__ */(function(){') && initSrc.endsWith('return $self})()')) {
        newInit = initSrc.slice(0, initSrc.length - 'return $self})()'.length) + 'var g$p;' + moved + 'return $self})()';
      } else {
        newInit = `/* @__PURE__ */(function(){var $self=${initSrc};var g$p;${moved}return $self})()`;
      }
      edits.push({ start: d.init.start, end: d.init.end, text: newInit });
      for (const i of idxs) edits.push({ start: B[i].start, end: B[i].end, text: '' });
      folds++;
    }
    console.log(`phase F round ${round}: folds ${folds}; top blockers`,
      Object.entries(blockers).sort((x, y) => y[1] - x[1]).slice(0, 5));
    if (!folds) break;
    edits.sort((x, y) => y.start - x.start);
    for (const e of edits) text = text.slice(0, e.start) + e.text + text.slice(e.end);
    parse(text);
  }
  fs.writeFileSync('cljs.core.shaken.js', text);
  console.log('phase F done,', text.length, 'bytes');
}

// ---- phase G: PURE-annotate every remaining top-level call/new initializer -
// (constant builders: dispatch-fn IIFEs, keyword/symbol/type singletons, the
// sentinel IIFE - droppable when no core survives, per ADR 0001 coexistence
// only matters when protocols exist in the bundle)
{
  let text = fs.readFileSync('cljs.core.shaken.js', 'utf8');
  const a = parse(text);
  const edits = [];
  for (const st of a.body) {
    if (st.type !== 'VariableDeclaration') continue;
    for (const d of st.declarations) {
      if (!d.init) continue;
      if (d.init.type !== 'CallExpression' && d.init.type !== 'NewExpression') continue;
      const before = text.slice(Math.max(0, d.init.start - 20), d.init.start);
      if (before.includes('@__PURE__')) continue;
      edits.push({ start: d.init.start, end: d.init.start, text: '/* @__PURE__ */ ' });
    }
  }
  console.log('phase G annotated:', edits.length);
  edits.sort((x, y) => y.start - x.start);
  for (const e of edits) text = text.slice(0, e.start) + e.text + text.slice(e.end);
  fs.writeFileSync('cljs.core.shaken.js', text);
  parse(text);
  console.log('phase G done,', text.length, 'bytes');
}
