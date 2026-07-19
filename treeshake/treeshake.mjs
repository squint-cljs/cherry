#!/usr/bin/env node
// Production-grade successor to the ADR 0002 spike (doc/ai/adr/0002-*).
//
// Rewrites the advanced-compiled cherry core and lib modules into a form
// esbuild can tree-shake. AST-only: parsing, scope analysis, and node
// construction via Babel; no textual substitution. Every assumption the
// spike measured once is enforced here as a hard assertion that fails the
// build instead of shipping a half-rewritten bundle.
//
// Output layout (out/):
//   internal/cljs.core.js   all definitions; exports internals + public names
//   cljs.core.js            facade: re-exports the public API only
//   clojure.string.js       rewritten: named imports of core internals
//   clojure.walk.js         rewritten: named imports of core internals
//
// Usage: node treeshake.mjs --core <cljs.core.js> \
//          --lib clojure.string=<path> --lib clojure.walk=<path> --out <dir>

import fs from 'node:fs';
import path from 'node:path';
import * as parser from '@babel/parser';
import _traverse from '@babel/traverse';
import _generate from '@babel/generator';
import * as t from '@babel/types';

const traverse = _traverse.default || _traverse;
// Babel caches scope info per node; we mutate the AST between traversals,
// so every scope-dependent traverse must start from a cold cache.
function freshTraverse(ast, visitors) {
  traverse.cache.clear();
  traverse(ast, visitors);
}
const generate = _generate.default || _generate;

// ---------------------------------------------------------------- helpers --

class BuildError extends Error {}
function fail(msg) { throw new BuildError(msg); }
function assert(cond, msg) { if (!cond) fail(msg); }

function parse(code, label) {
  try {
    return parser.parse(code, { sourceType: 'module' });
  } catch (e) {
    fail(`${label}: does not parse: ${e.message}`);
  }
}

function gen(ast) {
  return generate(ast, { compact: true, comments: true }).code;
}

// replace a node's contents in place (keeps the parent's reference valid)
function morph(node, replacement) {
  for (const k of Object.keys(node)) delete node[k];
  Object.assign(node, replacement);
}

function pure(callOrNew) {
  callOrNew.leadingComments = [{ type: 'CommentBlock', value: ' @__PURE__ ' }];
  return callOrNew;
}

const NATIVES = new Set([
  'globalThis', 'Date', 'String', 'Number', 'Boolean', 'Array', 'Object',
  'Function', 'Symbol', 'Math', 'JSON', 'RegExp', 'Error', 'TypeError',
  'RangeError', 'Map', 'Set', 'WeakMap', 'WeakSet', 'Promise', 'Proxy',
  'Reflect', 'Infinity', 'NaN', 'undefined', 'arguments', 'isNaN', 'isFinite',
  'parseFloat', 'parseInt', 'decodeURIComponent', 'encodeURIComponent',
  'console', 'Uint8Array', 'Int32Array', 'Float64Array', 'ArrayBuffer',
  'DataView', 'BigInt', 'eval',
]);

// ------------------------------------------------------------- normalize ---

// Split top-level `a = 1, b = 2;` sequences into separate statements so the
// census sees every assignment as its own statement. Removes a whole class
// of hidden-write hazards.
function splitSequences(ast) {
  const body = ast.program.body;
  for (let i = 0; i < body.length; i++) {
    const st = body[i];
    if (st.type === 'ExpressionStatement' && st.expression.type === 'SequenceExpression') {
      const parts = st.expression.expressions.map((e) => t.expressionStatement(e));
      body.splice(i, 1, ...parts);
      i += parts.length - 1;
    }
  }
}

// Split `var a = .., b = ..;` (all initialized) so each definition can fold
// independently. Hoist lists (`var a, b;`) stay.
function splitDeclarators(ast) {
  const body = ast.program.body;
  for (let i = 0; i < body.length; i++) {
    const st = body[i];
    if (st.type === 'VariableDeclaration' && st.declarations.length > 1 &&
        st.declarations.every((d) => d.init)) {
      const parts = st.declarations.map((d) => t.variableDeclaration(st.kind, [d]));
      body.splice(i, 1, ...parts);
      i += parts.length - 1;
    }
  }
}

// ------------------------------------------------------ census over $APP ---

// Every $APP touch in the module, wherever it hides. Fails on the shapes the
// transform cannot handle rather than guessing.
function appCensus(ast, label) {
  const props = new Map(); // name -> {topDefs: [stmt], nested: n, members: [node], protoRhs: n}
  let exportDecl = null;
  const body = ast.program.body;
  const topIndexByNode = new Map();
  body.forEach((st, i) => topIndexByNode.set(st, i));

  freshTraverse(ast, {
    Identifier(p) {
      if (p.node.name !== '$APP') return;
      // binding occurrences: only the export const $APP = {} declarator
      if (p.isBindingIdentifier()) {
        const decl = p.parentPath;
        assert(decl.isVariableDeclarator() &&
               decl.parentPath.parentPath.isExportNamedDeclaration(),
               `${label}: unexpected $APP binding at ${p.node.start}`);
        exportDecl = decl.node;
        return;
      }
      if (p.isReferencedIdentifier()) {
        const parent = p.parentPath;
        if (parent.isMemberExpression() && parent.node.object === p.node) {
          assert(!parent.node.computed,
                 `${label}: computed $APP[..] access at ${p.node.start} - unsupported shape`);
          return; // handled below via MemberExpression
        }
        if (parent.isImportSpecifier() || parent.isExportSpecifier()) return;
        fail(`${label}: bare $APP reference outside member access at ${p.node.start}`);
      }
    },
    MemberExpression(p) {
      const n = p.node;
      if (n.object.type !== 'Identifier' || n.object.name !== '$APP') return;
      if (n.computed) return; // asserted above
      const prop = n.property.name;
      let rec = props.get(prop);
      if (!rec) { rec = { topDefs: [], nested: 0, members: [], protoRhs: 0 }; props.set(prop, rec); }
      rec.members.push(n);
      // is this member the LHS of a whole top-level assignment statement?
      const parent = p.parentPath;
      if (parent.isAssignmentExpression() && parent.node.left === n && parent.node.operator === '=') {
        const stmt = parent.parentPath;
        if (stmt.isExpressionStatement() && stmt.parentPath.isProgram()) {
          rec.topDefs.push(stmt.node);
          const r = parent.node.right;
          if (r.type === 'MemberExpression' && !r.computed && r.property.name === 'prototype') {
            rec.protoRhs++;
          }
        } else {
          rec.nested++;
        }
      }
    },
  });
  return { props, exportDecl, topIndexByNode };
}

// ------------------------------------------- pass 1: $APP props -> vars ----

function propsToVars(ast, label) {
  const { props } = appCensus(ast, label);

  const cursors = new Set();
  const convertible = new Set();  // one top-level def; deferred writes become var assigns
  const declareOnly = new Set();  // no top-level def: hoist `var p;`, writes are deferred
  const retained = new Set();
  for (const [name, rec] of props) {
    if (rec.topDefs.length > 1 && rec.protoRhs === rec.topDefs.length && rec.nested === 0) {
      cursors.add(name);
    } else if (rec.topDefs.length === 1) {
      convertible.add(name);
    } else if (rec.topDefs.length === 0) {
      declareOnly.add(name);
    } else {
      retained.add(name);
    }
  }
  assert(retained.size === 0,
         `${label}: props with unsupported write patterns: ` +
         [...retained].slice(0, 5).join(', '));

  // name collisions with existing top-level bindings would silently merge
  const topNames = new Set();
  for (const st of ast.program.body) {
    if (st.type === 'VariableDeclaration') {
      for (const d of st.declarations) topNames.add(d.id.name);
    }
  }
  const collisions = [...convertible, ...declareOnly].filter((n) => topNames.has(n));
  assert(collisions.length === 0,
         `${label}: $APP prop names collide with top-level vars: ${collisions.slice(0, 5).join(', ')}`);

  // rewrite: defs become var declarations; every other occurrence (reads and
  // deferred writes alike) becomes the identifier
  for (const [name, rec] of props) {
    if (convertible.has(name)) {
      const defStmt = rec.topDefs[0];
      const rhs = defStmt.expression.right;
      for (const m of rec.members) {
        if (m === defStmt.expression.left) continue; // replaced with the stmt
        morph(m, t.identifier(name));
      }
      morph(defStmt, t.variableDeclaration('var', [t.variableDeclarator(t.identifier(name), rhs)]));
    } else if (declareOnly.has(name)) {
      for (const m of rec.members) morph(m, t.identifier(name));
    }
  }
  if (declareOnly.size) {
    const body = ast.program.body;
    let at = 0;
    while (at < body.length && body[at].type === 'ExportNamedDeclaration') at++;
    body.splice(at, 0, t.variableDeclaration('var',
      [...declareOnly].map((n) => t.variableDeclarator(t.identifier(n)))));
  }
  const allConverted = new Set([...convertible, ...declareOnly]);
  return { cursors, convertible: allConverted };
}

// ----------------------- hoisted `var a,b;` -> declaration-at-assignment ---

// Closure hoists names up front and assigns later; the assignment is a side
// effect esbuild keeps. Names with exactly one top-level assignment get the
// declaration moved onto it. Deferred writes stay ordinary assignments to
// the (hoisted-either-way) var.
function promoteHoisted(ast) {
  const body = ast.program.body;
  const hoisted = new Set();
  for (const st of body) {
    if (st.type === 'VariableDeclaration') {
      for (const d of st.declarations) if (!d.init) hoisted.add(d.id.name);
    }
  }
  const topAssigns = new Map(); // name -> [stmt]
  for (const st of body) {
    if (st.type === 'ExpressionStatement' && st.expression.type === 'AssignmentExpression' &&
        st.expression.operator === '=' && st.expression.left.type === 'Identifier' &&
        hoisted.has(st.expression.left.name)) {
      const n = st.expression.left.name;
      if (!topAssigns.has(n)) topAssigns.set(n, []);
      topAssigns.get(n).push(st);
    }
  }
  const promote = new Set([...topAssigns].filter(([, v]) => v.length === 1).map(([k]) => k));
  let n = 0;
  for (let i = 0; i < body.length; i++) {
    const st = body[i];
    if (st.type === 'VariableDeclaration' && st.declarations.some((d) => !d.init)) {
      st.declarations = st.declarations.filter((d) => d.init || !promote.has(d.id.name));
      if (st.declarations.length === 0) { body.splice(i, 1); i--; }
    } else if (st.type === 'ExpressionStatement' && st.expression.type === 'AssignmentExpression' &&
               st.expression.left.type === 'Identifier' && promote.has(st.expression.left.name)) {
      const name = st.expression.left.name;
      const init = st.expression.right;
      morph(st, t.variableDeclaration('var', [
        t.variableDeclarator(t.identifier(name), init)]));
      n++;
    }
  }
  return { promote, promoted: n };
}

// ------------------------------------- reference index for a fold round ----

// One traverse: every program-scope-bound identifier reference, with its
// top-level statement index and whether it is eager (outside any function).
function indexReferences(ast) {
  const body = ast.program.body;
  const stmtIdx = new Map(body.map((s, i) => [s, i]));
  const refs = new Map(); // name -> [{node, top, eager}]
  let programScope = null;
  freshTraverse(ast, {
    Program(p) { programScope = p.scope; },
    Identifier(p) {
      if (!p.isReferencedIdentifier()) return;
      const name = p.node.name;
      const binding = p.scope.getBinding(name);
      if (!binding || binding.scope !== programScope) return;
      // identity-based top-level statement lookup: morphing strips node
      // positions, so offsets are not trustworthy
      let a = p;
      while (a.parentPath && !a.parentPath.isProgram()) a = a.parentPath;
      const top = stmtIdx.get(a.node);
      if (top === undefined) return;
      let arr = refs.get(name);
      if (!arr) { arr = []; refs.set(name, arr); }
      arr.push({ node: p.node, top, eager: p.getFunctionParent() === null });
    },
  });
  return { refs, programScope };
}

function declIndexOf(ast) {
  const map = new Map(); // name -> {idx, stmt, declarator|null}
  ast.program.body.forEach((st, i) => {
    if (st.type === 'VariableDeclaration') {
      for (const d of st.declarations) {
        map.set(d.id.name, { idx: i, stmt: st, declarator: st.declarations.length === 1 ? d : null });
      }
    } else if (st.type === 'ImportDeclaration') {
      for (const sp of st.specifiers) map.set(sp.local.name, { idx: i, stmt: st, declarator: null });
    } else if (st.type === 'FunctionDeclaration' && st.id) {
      map.set(st.id.name, { idx: i, stmt: st, declarator: null });
    }
  });
  return map;
}

function lhsRoot(node) {
  let n = node;
  while (n.type === 'MemberExpression') n = n.object;
  return n.type === 'Identifier' ? n.name : null;
}

// eager identifier names in a subtree (function bodies are deferred)
function eagerIds(root) {
  const out = [];
  const stack = [root];
  while (stack.length) {
    const n = stack.pop();
    if (!n || typeof n.type !== 'string') continue;
    if (n.type === 'FunctionExpression' || n.type === 'FunctionDeclaration' ||
        n.type === 'ArrowFunctionExpression' || n.type === 'ClassBody') continue;
    if (n.type === 'Identifier') { out.push(n.name); continue; }
    if (n.type === 'MemberExpression' && !n.computed) { stack.push(n.object); continue; }
    for (const k of Object.keys(n)) {
      if (k === 'loc' || k === 'leadingComments' || k === 'trailingComments') continue;
      const v = n[k];
      if (Array.isArray(v)) { for (const x of v) stack.push(x); }
      else if (v && typeof v === 'object') stack.push(v);
    }
  }
  return out;
}

// detect an initializer we folded earlier: PURE (function(){var U=..; ..; return U})()
function foldedUidOf(init) {
  if (init.type !== 'CallExpression' || init.callee.type !== 'FunctionExpression') return null;
  const b = init.callee.body.body;
  if (b.length < 2) return null;
  const first = b[0], last = b[b.length - 1];
  if (first.type !== 'VariableDeclaration' || first.declarations.length !== 1) return null;
  if (last.type !== 'ReturnStatement' || last.argument?.type !== 'Identifier') return null;
  if (first.declarations[0].id.name !== last.argument.name) return null;
  return last.argument.name;
}

// ---------------------------------- pass 2: relocate sentinel, fold all ----

function relocateSentinel(ast, label) {
  const body = ast.program.body;
  const candidates = [];
  for (let i = 0; i < body.length; i++) {
    const st = body[i];
    if (st.type !== 'VariableDeclaration' || st.declarations.length !== 1) continue;
    const init = st.declarations[0].init;
    if (!init) continue;
    // the sentinel init reads o.PROTOCOL_SENTINEL; find that member access
    let found = false;
    const stack = [init];
    while (stack.length && !found) {
      const n = stack.pop();
      if (!n || typeof n.type !== 'string') continue;
      if (n.type === 'MemberExpression' && !n.computed &&
          n.property.name === 'PROTOCOL_SENTINEL') { found = true; break; }
      for (const k of Object.keys(n)) {
        const v = n[k];
        if (Array.isArray(v)) stack.push(...v);
        else if (v && typeof v === 'object' && v.type) stack.push(v);
      }
    }
    if (!found) continue;
    // its eager deps must all be globals (arrow bodies count as deferred only
    // if wrapped; the sentinel IIFE is an arrow call, so walk it eagerly)
    const ids = eagerIds(init).filter((n) => !NATIVES.has(n));
    const decls = declIndexOf(ast);
    if (ids.every((n) => !decls.has(n))) candidates.push(i);
  }
  assert(candidates.length === 1,
         `${label}: expected exactly one sentinel init, found ${candidates.length}`);
  const [stmt] = body.splice(candidates[0], 1);
  // after the leading export-const prelude
  let insertAt = 0;
  while (insertAt < body.length && body[insertAt].type === 'ExportNamedDeclaration') insertAt++;
  body.splice(insertAt, 0, stmt);
}

// Known top-level side-effect shapes that are allowed to remain. Anything
// else fails the build (shape drift protection).
function classifyRetained(st, cursors, label) {
  if (st.type === 'ExpressionStatement') {
    const e = st.expression;
    if (e.type === 'CallExpression') return 'bare-call';
    if (e.type === 'LogicalExpression') return 'guarded-call';
    if (e.type === 'AssignmentExpression') {
      const root = lhsRoot(e.left);
      if (root === '$APP') return 'app-retained';
      return 'member-assign';
    }
  }
  fail(`${label}: unrecognized top-level statement shape (${st.type}) at ${st.start}`);
}

function foldFixpoint(ast, cursors, label, uidState) {
  const genUid = (base) => `${base}$${uidState.n++}$ts`;
  for (let round = 1; round <= 12; round++) {
    const body = ast.program.body;
    const decls = declIndexOf(ast);
    const { refs } = indexReferences(ast);

    // group foldable side-effect statements by their root binding
    const groups = new Map(); // T -> [{idx, kind: 'assign'|'cursor-head'|'cursor-stamp', node}]
    const consumed = new Set();
    for (let i = 0; i < body.length; i++) {
      const st = body[i];
      if (consumed.has(i)) continue;
      if (st.type !== 'ExpressionStatement') continue;
      const e = st.expression;
      if (e.type !== 'AssignmentExpression') continue;
      const l = e.left;
      // cursor segment head: $APP.<cursor> = T.prototype
      if (l.type === 'MemberExpression' && !l.computed &&
          l.object.type === 'Identifier' && l.object.name === '$APP' &&
          cursors.has(l.property.name)) {
        const r = e.right;
        assert(r.type === 'MemberExpression' && !r.computed &&
               r.property.name === 'prototype' && r.object.type === 'Identifier',
               `${label}: cursor assignment with unexpected rhs at ${st.start}`);
        const T = r.object.name;
        const seg = [{ idx: i, kind: 'cursor-head', node: st }];
        for (let j = i + 1; j < body.length; j++) {
          const nx = body[j];
          if (nx.type === 'ExpressionStatement' &&
              nx.expression.type === 'AssignmentExpression' &&
              nx.expression.left.type === 'MemberExpression' &&
              nx.expression.left.object.type === 'MemberExpression' &&
              !nx.expression.left.object.computed &&
              nx.expression.left.object.object.type === 'Identifier' &&
              nx.expression.left.object.object.name === '$APP' &&
              cursors.has(nx.expression.left.object.property.name)) {
            seg.push({ idx: j, kind: 'cursor-stamp', node: nx });
          } else break;
        }
        seg.forEach((s) => consumed.add(s.idx));
        if (!groups.has(T)) groups.set(T, []);
        groups.get(T).push(...seg);
        continue;
      }
      const root = lhsRoot(l);
      if (!root || root === '$APP') continue;
      if (!decls.has(root)) continue;
      if (!groups.has(root)) groups.set(root, []);
      groups.get(root).push({ idx: i, kind: 'assign', node: st });
    }

    // attempt folds
    let folds = 0;
    const toRemove = new Set();
    for (const [T, items] of groups) {
      const d = decls.get(T);
      if (!d || !d.declarator || !d.declarator.init) continue;
      if (NATIVES.has(T)) continue;
      if (items.some((it) => it.idx <= d.idx)) continue;
      // gate 1: every moved value's eager deps must be declared before T
      let blocked = false;
      for (const it of items) {
        const val = it.node.expression.right;
        for (const id of eagerIds(val)) {
          if (id === T || NATIVES.has(id)) continue;
          const dd = decls.get(id);
          if (!dd || dd.idx > d.idx) { blocked = true; break; }
        }
        if (blocked) break;
      }
      if (blocked) continue;
      // gate 2: moving stamps earlier must not change what intermediate eager
      // code observes. A whole-prototype replacement in the moved set with any
      // intermediate eager T reference blocks; so does an intermediate eager
      // read of a property the moved set writes.
      const movedIdx = new Set(items.map((it) => it.idx));
      const wholeProtoWrite = items.some((it) => {
        const l = it.node.expression.left;
        return it.kind === 'assign' && l.type === 'MemberExpression' &&
               !l.computed && l.property.name === 'prototype' &&
               l.object.type === 'Identifier';
      });
      const writtenProps = new Set(items
        .filter((it) => it.kind === 'assign')
        .map((it) => it.node.expression.left)
        .filter((l) => l.type === 'MemberExpression' && !l.computed &&
                       l.object.type === 'Identifier')
        .map((l) => l.property.name));
      const between = (refs.get(T) || []).filter(
        (r) => r.eager && r.top > d.idx && r.top < items[items.length - 1].idx &&
               !movedIdx.has(r.top));
      if (wholeProtoWrite && between.length) continue;
      let readConflict = false;
      for (const r of between) {
        // is this reference the object of a member read of a written prop?
        // conservative: any eager T.<writtenProp> access in between blocks
        // (we cannot cheaply distinguish read from write here, so block both)
        if (writtenProps.size) { readConflict = true; break; }
      }
      if (readConflict && between.length) continue;

      // build or extend the PURE initializer
      const init = d.declarator.init;
      let uid = foldedUidOf(init);
      let iifeBody, insertBefore;
      if (uid) {
        iifeBody = init.callee.body.body;
        insertBefore = iifeBody.length - 1; // before return
      } else {
        uid = genUid('self');
        const inner = t.variableDeclaration('var', [
          t.variableDeclarator(t.identifier(uid), init)]);
        const ret = t.returnStatement(t.identifier(uid));
        iifeBody = [inner, ret];
        insertBefore = 1;
        d.declarator.init = pure(t.callExpression(
          t.functionExpression(null, [], t.blockStatement(iifeBody)), []));
      }
      // rename references to T inside moved nodes (scope-checked via refs
      // index: those recorded refs ARE program-scope-bound)
      const tRefs = (refs.get(T) || []).filter((r) => movedIdx.has(r.top));
      for (const r of tRefs) r.node.name = uid;
      // rewrite and insert
      let gUid = null;
      const newStmts = [];
      for (const it of items) {
        const e = it.node.expression;
        if (it.kind === 'cursor-head') {
          gUid = genUid('g');
          // rhs was T.prototype; T already renamed to uid where applicable
          newStmts.push(t.variableDeclaration('var', [
            t.variableDeclarator(t.identifier(gUid), e.right)]));
        } else if (it.kind === 'cursor-stamp') {
          assert(gUid, `${label}: cursor stamp before head`);
          newStmts.push(t.expressionStatement(t.assignmentExpression('=',
            t.memberExpression(t.identifier(gUid), e.left.property), e.right)));
        } else {
          newStmts.push(it.node);
        }
        toRemove.add(it.node);
      }
      iifeBody.splice(insertBefore, 0, ...newStmts);
      folds++;
    }
    if (!folds) break;
    ast.program.body = body.filter((st) => !toRemove.has(st));
    assert(round < 12, `${label}: fold did not converge in 12 rounds`);
  }
}

// retained cursor statements (native prototype stamps): move off $APP onto a
// module-local cursor variable so no code depends on $APP being populated
function localizeRetainedCursors(ast, cursors, uidState) {
  if (!cursors.size) return;
  const body = ast.program.body;
  let cursorVar = null;
  for (const st of body) {
    if (st.type !== 'ExpressionStatement') continue;
    const e = st.expression;
    if (e.type !== 'AssignmentExpression') continue;
    const stack = [e];
    while (stack.length) {
      const n = stack.pop();
      if (!n || typeof n.type !== 'string') continue;
      if (n.type === 'MemberExpression' && !n.computed &&
          n.object.type === 'Identifier' && n.object.name === '$APP' &&
          cursors.has(n.property.name)) {
        if (!cursorVar) cursorVar = `g$${uidState.n++}$ts`;
        morph(n, t.identifier(cursorVar));
        continue;
      }
      for (const k of Object.keys(n)) {
        const v = n[k];
        if (Array.isArray(v)) stack.push(...v);
        else if (v && typeof v === 'object' && v.type) stack.push(v);
      }
    }
  }
  if (cursorVar) {
    let at = 0;
    while (at < body.length && body[at].type === 'ExportNamedDeclaration') at++;
    body.splice(at, 0, t.variableDeclaration('var', [
      t.variableDeclarator(t.identifier(cursorVar))]));
  }
}

// -------------------------------------- pass 3: PURE the constant makers ---

function annotateInitializers(ast) {
  let n = 0;
  for (const st of ast.program.body) {
    if (st.type !== 'VariableDeclaration') continue;
    for (const d of st.declarations) {
      const init = d.init;
      if (!init) continue;
      if (init.type !== 'CallExpression' && init.type !== 'NewExpression') continue;
      if (init.leadingComments?.some((c) => c.value.includes('@__PURE__'))) continue;
      pure(init);
      n++;
    }
  }
  return n;
}

// -------------------------------------------- post-transform assertions ----

function assertNoApp(ast, convertible, label) {
  const bad = [];
  freshTraverse(ast, {
    MemberExpression(p) {
      const n = p.node;
      if (n.object.type === 'Identifier' && n.object.name === '$APP' &&
          !n.computed && convertible.has(n.property.name)) bad.push(n.property.name);
    },
  });
  assert(bad.length === 0,
         `${label}: converted props still referenced via $APP: ${bad.slice(0, 5).join(', ')}`);
}

function retainedReport(ast, cursors, label) {
  const counts = {};
  for (const st of ast.program.body) {
    if (st.type === 'VariableDeclaration' || st.type === 'ExportNamedDeclaration' ||
        st.type === 'ImportDeclaration' || st.type === 'EmptyStatement') continue;
    const c = classifyRetained(st, cursors, label);
    counts[c] = (counts[c] || 0) + 1;
  }
  return counts;
}

// ------------------------------------------------------------- core flow ---

function transformCore(code) {
  const label = 'cljs.core';
  const ast = parse(code, label);
  splitSequences(ast);
  splitDeclarators(ast);

  const uidState = { n: 0 };
  const { cursors, convertible } = propsToVars(ast, label);
  const { promoted } = promoteHoisted(ast);
  console.log(`${label}: converted ${convertible.size} props, promoted ${promoted} hoisted vars, cursors: [${[...cursors]}]`);

  relocateSentinel(ast, label);
  foldFixpoint(ast, cursors, label, uidState);
  localizeRetainedCursors(ast, cursors, uidState);
  const annotated = annotateInitializers(ast);
  assertNoApp(ast, convertible, label);
  const retained = retainedReport(ast, cursors, label);
  console.log(`${label}: PURE-annotated ${annotated} initializers; retained side effects:`, retained);

  // public export surface (must be preserved exactly)
  const publicNames = [];
  for (const st of ast.program.body) {
    if (st.type === 'ExportNamedDeclaration') {
      if (st.declaration?.declarations) {
        for (const d of st.declaration.declarations) publicNames.push(d.id.name);
      }
      for (const sp of st.specifiers || []) publicNames.push(sp.exported.name);
    }
  }
  // internal exports: every top-level binding not already exported
  const internals = [];
  const pub = new Set(publicNames);
  for (const st of ast.program.body) {
    if (st.type === 'VariableDeclaration') {
      for (const d of st.declarations) if (!pub.has(d.id.name)) internals.push(d.id.name);
    }
  }
  ast.program.body.push(t.exportNamedDeclaration(null,
    internals.map((n) => t.exportSpecifier(t.identifier(n), t.identifier(n)))));

  const internalCode = gen(ast);
  parse(internalCode, `${label} (generated)`); // must reparse
  const facadeCode = gen(t.file(t.program([
    t.exportNamedDeclaration(null,
      publicNames.map((n) => t.exportSpecifier(t.identifier(n), t.identifier(n))),
      t.stringLiteral('./internal/cljs.core.js')),
  ])));
  const coreVars = new Set([...internals, ...publicNames]);
  return { internalCode, facadeCode, coreVars, publicNames };
}

// -------------------------------------------------------------- lib flow ---

function transformLib(code, name, coreVars) {
  const label = name;
  const ast = parse(code, label);
  splitSequences(ast);
  splitDeclarators(ast);

  // drop the `import { $APP, shadow$provide } from "./cljs.core.js"`;
  // named imports are added below
  const body = ast.program.body;
  const importIdx = body.findIndex((st) => st.type === 'ImportDeclaration' &&
    st.specifiers.some((sp) => sp.local?.name === '$APP'));
  assert(importIdx >= 0, `${label}: expected the $APP import`);
  body.splice(importIdx, 1);

  const { props } = appCensus(ast, label);
  const cursors = new Set();
  const own = new Set();
  const external = new Set();
  for (const [p, rec] of props) {
    if (rec.topDefs.length > 1 && rec.protoRhs === rec.topDefs.length && rec.nested === 0) {
      cursors.add(p);
    } else if (rec.topDefs.length === 1) {
      own.add(p);
    } else if (rec.topDefs.length === 0 && rec.nested === 0) {
      assert(coreVars.has(p), `${label}: $APP.${p} resolves to neither this module nor core`);
      external.add(p);
    } else {
      // a deferred write to a core prop would be an assignment to an import
      fail(`${label}: $APP.${p} has an unsupported write pattern (nested write to non-own prop?)`);
    }
  }

  // externals -> identifiers (imported); own -> vars; cursors handled after
  for (const [p, rec] of props) {
    if (external.has(p)) {
      for (const m of rec.members) morph(m, t.identifier(p));
    } else if (own.has(p)) {
      const defStmt = rec.topDefs[0];
      const rhs = defStmt.expression.right;
      for (const m of rec.members) {
        if (m === defStmt.expression.left) continue;
        morph(m, t.identifier(p));
      }
      morph(defStmt, t.variableDeclaration('var', [t.variableDeclarator(t.identifier(p), rhs)]));
    }
  }
  body.unshift(t.importDeclaration(
    [...external].map((p) => t.importSpecifier(t.identifier(p), t.identifier(p))),
    t.stringLiteral('./internal/cljs.core.js')));

  promoteHoisted(ast);
  const uidState = { n: 0 };
  foldFixpoint(ast, cursors, label, uidState);
  localizeRetainedCursors(ast, cursors, uidState);
  annotateInitializers(ast);
  assertNoApp(ast, new Set([...own, ...external]), label);
  // nothing at all may reference $APP now
  let appLeft = 0;
  freshTraverse(ast, { Identifier(p) { if (p.node.name === '$APP') appLeft++; } });
  assert(appLeft === 0, `${label}: ${appLeft} $APP references survived`);

  const out = gen(ast);
  parse(out, `${label} (generated)`);
  console.log(`${label}: ${own.size} own defs, ${external.size} core imports`);
  return out;
}

// ------------------------------------------------------------------ main ---

function main() {
  const args = process.argv.slice(2);
  const opt = { libs: [] };
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--core') opt.core = args[++i];
    else if (args[i] === '--out') opt.out = args[++i];
    else if (args[i] === '--lib') { const [n, p] = args[++i].split('='); opt.libs.push([n, p]); }
    else fail(`unknown argument ${args[i]}`);
  }
  assert(opt.core && opt.out, 'usage: --core <file> [--lib name=file ...] --out <dir>');

  const { internalCode, facadeCode, coreVars } = transformCore(fs.readFileSync(opt.core, 'utf8'));
  fs.mkdirSync(path.join(opt.out, 'internal'), { recursive: true });
  fs.writeFileSync(path.join(opt.out, 'internal', 'cljs.core.js'), internalCode);
  fs.writeFileSync(path.join(opt.out, 'cljs.core.js'), facadeCode);

  for (const [name, file] of opt.libs) {
    const out = transformLib(fs.readFileSync(file, 'utf8'), name, coreVars);
    fs.writeFileSync(path.join(opt.out, `${name}.js`), out);
  }
  console.log('treeshake: OK ->', opt.out);
}

try {
  main();
} catch (e) {
  if (e instanceof BuildError) {
    console.error('treeshake FAILED:', e.message);
    process.exit(1);
  }
  throw e;
}
