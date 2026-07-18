// ADR 0002 spike, cross-module: make lib modules (clojure.string.js,
// clojure.walk.js) work with the shaken core.
// - the shaken core additionally exports all its top-level internal names
// - lib modules: own `$APP.q = ..` defs become vars, reads of core props
//   become named imports, the `$APP.g` cursor becomes a module-local var.
// Hard assertion: no `$APP` reference survives in a rewritten lib module.
import fs from 'fs';
import * as acorn from 'acorn';

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

// ---- 1. core: export internals -------------------------------------------
let core = fs.readFileSync('cljs.core.shaken.js', 'utf8');
{
  const ast = parse(core);
  const vars = new Set();
  const exported = new Set();
  for (const st of ast.body) {
    if (st.type === 'VariableDeclaration') {
      for (const d of st.declarations) vars.add(d.id.name);
    }
    if (st.type === 'ExportNamedDeclaration') {
      if (st.declaration && st.declaration.declarations) {
        for (const d of st.declaration.declarations) exported.add(d.id.name);
      }
      for (const sp of st.specifiers || []) exported.add(sp.exported.name);
    }
  }
  const internal = [...vars].filter((n) => !exported.has(n));
  core += `\nexport { ${internal.join(',')} };\n`;
  fs.writeFileSync('cljs.core.shaken2.js', core);
  parse(core);
  console.log('core: exported', internal.size ?? internal.length, 'internals');
}

// ---- 2. lib modules -------------------------------------------------------
const coreVars = new Set();
{
  const ast = parse(core);
  for (const st of ast.body) {
    if (st.type === 'VariableDeclaration') for (const d of st.declarations) coreVars.add(d.id.name);
  }
}

for (const mod of ['clojure.string', 'clojure.walk']) {
  let s = fs.readFileSync(`../lib/${mod}.js.orig`, 'utf8');
  const ast = parse(s);
  // own prop assignment counts
  const own = {};
  for (const st of ast.body) {
    if (st.type === 'ExpressionStatement' && st.expression.type === 'AssignmentExpression') {
      const l = st.expression.left;
      if (l.type === 'MemberExpression' && !l.computed && l.object.type === 'Identifier' &&
          l.object.name === '$APP' && l.property.name !== 'g') {
        own[l.property.name] = (own[l.property.name] || 0) + 1;
      }
    }
  }
  const ownSingle = new Set(Object.keys(own).filter((p) => own[p] === 1));
  const defLhs = new Set();
  for (const st of ast.body) {
    if (st.type === 'ExpressionStatement' && st.expression.type === 'AssignmentExpression') {
      const l = st.expression.left;
      if (l.type === 'MemberExpression' && !l.computed && l.object.type === 'Identifier' &&
          l.object.name === '$APP' && ownSingle.has(l.property.name)) defLhs.add(l.start);
    }
  }
  const needImport = new Set();
  const edits = [];
  const seen = new Set();
  walk(ast, (n) => {
    if (n.type === 'MemberExpression' && !n.computed &&
        n.object.type === 'Identifier' && n.object.name === '$APP') {
      if (seen.has(n.start)) return;
      seen.add(n.start);
      const p = n.property.name;
      if (p === 'g') { edits.push({ start: n.start, end: n.end, text: 'g$p' }); return; }
      if (ownSingle.has(p)) {
        edits.push({ start: n.start, end: n.end, text: defLhs.has(n.start) ? `var ${p}` : p });
        return;
      }
      if (coreVars.has(p)) { needImport.add(p); edits.push({ start: n.start, end: n.end, text: p }); return; }
      throw new Error(`${mod}: unresolvable $APP.${p}`);
    }
  });
  edits.sort((a, b) => b.start - a.start);
  for (const e of edits) s = s.slice(0, e.start) + e.text + s.slice(e.end);
  // replace the $APP import with named imports + cursor decl
  s = s.replace(/import \{ \$APP, shadow\$provide \} from "\.\/cljs\.core\.js";/,
    `import { ${[...needImport].join(',')} } from "./cljs.core.js";var g$p;`);
  if (/\$APP/.test(s)) throw new Error(`${mod}: $APP survived`);
  parse(s);
  fs.writeFileSync(`${mod}.shaken.js`, s);
  console.log(mod, ': imports', needImport.size, 'core internals; own props', ownSingle.size);
}
