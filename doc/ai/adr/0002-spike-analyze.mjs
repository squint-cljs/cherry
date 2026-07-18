import fs from 'fs';
import * as acorn from 'acorn';

const s = fs.readFileSync('../lib/cljs.core.js', 'utf8');
const ast = acorn.parse(s, { ecmaVersion: 2022, sourceType: 'module' });

const props = new Set(), vars = new Set();
let computed = 0, appBare = 0;

// iterative walk (minified nesting overflows recursion)
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

for (const st of ast.body) {
  if (st.type === 'VariableDeclaration') for (const d of st.declarations) vars.add(d.id.name);
}

walk(ast, (n) => {
  if (n.type === 'MemberExpression' && n.object.type === 'Identifier' && n.object.name === '$APP') {
    if (n.computed) computed++; else props.add(n.property.name);
  }
});
// bare $APP identifiers not as member-object: count via regex minus member count
const memberCount = (s.match(/\$APP\./g) || []).length;
const allCount = (s.match(/\$APP/g) || []).length;
appBare = allCount - memberCount - (s.match(/\$APP\[/g) || []).length;

const collisions = [...props].filter((p) => vars.has(p));
console.log('props referenced on $APP:', props.size);
console.log('plain top-level vars:', vars.size);
console.log('collisions:', collisions.length, collisions.slice(0, 10));
console.log('computed $APP[..]:', computed);
console.log('bare $APP identifiers (non-member):', appBare);
console.log('top-level statements:', ast.body.length);

// classify top-level statements
const classes = {};
function cls(st) {
  if (st.type === 'VariableDeclaration') return 'var-decl';
  if (st.type === 'ExportNamedDeclaration') return 'export';
  if (st.type === 'ExpressionStatement') {
    const e = st.expression;
    if (e.type === 'AssignmentExpression') {
      const l = e.left;
      if (l.type === 'MemberExpression' && l.object.type === 'Identifier' && l.object.name === '$APP') {
        const r = e.right.type;
        return '$APP.x=' + (r === 'FunctionExpression' ? 'fn' : r === 'CallExpression' ? 'call' : r);
      }
      if (l.type === 'MemberExpression') {
        const src = s.slice(l.start, Math.min(l.end, l.start + 60));
        if (src.includes('.prototype')) return 'proto-assign';
        return 'member-assign:' + src.split('.')[0].slice(0, 20);
      }
      if (l.type === 'Identifier') {
        const r = e.right.type;
        return 'var=' + (r === 'FunctionExpression' ? 'fn' : r === 'CallExpression' ? 'call' : r);
      }
    }
    if (e.type === 'SequenceExpression') return 'sequence(' + e.expressions.length + ')';
    if (e.type === 'CallExpression') return 'bare-call';
  }
  return st.type;
}
for (const st of ast.body) {
  const c = cls(st);
  classes[c] = (classes[c] || 0) + 1;
}
console.log('\nstatement classes:');
for (const [k, v] of Object.entries(classes).sort((a, b) => b[1] - a[1])) console.log(' ', k, v);
