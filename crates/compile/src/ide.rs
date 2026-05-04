//! Hover types and go-to-definition (used by `ds ide` and the language server).

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use ast::expr::{ForExpression, PathExpr};
use ast::{Expr, Pat, Root, Stmt};
use diagnostics::{
  byte_offset_to_line_character_utf16, line_character_utf16_to_byte_offset, TextRange,
};
use hir::lower;
use parser::parse;
use syntax::{SyntaxKind, SyntaxNode, SyntaxToken, TextSize};
use tycheck::typed_hir::Ty;
use tycheck::TyCheck;

use crate::manifest::{self, Backend as ManifestBackend};
use crate::modules::{self, load_module_tree, resolve_module_path};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LspPositionJson {
  pub line: u32,
  pub character: u32,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LspRangeJson {
  pub start: LspPositionJson,
  pub end: LspPositionJson,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct IdeLocationJson {
  pub uri: String,
  pub range: LspRangeJson,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct IdeQueryResult {
  pub hover: Option<String>,
  pub definition: Option<IdeLocationJson>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub error: Option<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct IdeSymbol {
  pub module: String,
  pub name: String,
  pub kind: String,
  pub ty: String,
  pub file: String,
  pub range: LspRangeJson,
}

fn text_range_to_lsp_range(source: &str, range: TextRange) -> LspRangeJson {
  let s = u32::from(range.start()) as usize;
  let e = u32::from(range.end()) as usize;
  let (sl, sc) = byte_offset_to_line_character_utf16(source, s);
  let (el, ec) = byte_offset_to_line_character_utf16(source, e);
  LspRangeJson {
    start: LspPositionJson {
      line: sl,
      character: sc,
    },
    end: LspPositionJson {
      line: el,
      character: ec,
    },
  }
}

fn path_to_file_uri(path: &Path) -> String {
  let abs = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
  format!("file://{}", abs.display())
}

fn byte_in_text_range(offset: usize, r: TextRange) -> bool {
  let start = u32::from(r.start()) as usize;
  let end = u32::from(r.end()) as usize;
  offset >= start && offset < end
}

fn path_last_ident_range(p: &PathExpr) -> Option<TextRange> {
  p.last_ident_token().map(|t| t.text_range())
}

#[derive(Clone)]
struct BindingSite {
  file: PathBuf,
  range: TextRange,
}

struct GotoCtx<'a> {
  offset: usize,
  current_file: &'a Path,
  project_root: Option<&'a Path>,
  layers: Vec<Vec<(String, BindingSite)>>,
}

impl<'a> GotoCtx<'a> {
  fn new(offset: usize, current_file: &'a Path, project_root: Option<&'a Path>) -> Self {
    Self {
      offset,
      current_file,
      project_root,
      layers: vec![Vec::new()],
    }
  }

  fn lookup(&self, name: &str) -> Option<BindingSite> {
    for layer in self.layers.iter().rev() {
      for (n, site) in layer.iter().rev() {
        if n == name {
          return Some(site.clone());
        }
      }
    }
    None
  }

  fn push_layer(&mut self) {
    self.layers.push(Vec::new());
  }

  fn pop_layer(&mut self) {
    self.layers.pop();
  }

  fn define(&mut self, name: String, site: BindingSite) {
    if let Some(top) = self.layers.last_mut() {
      top.push((name, site));
    }
  }
}

fn source_for_location(site: &BindingSite, entry_source: &str, entry_path: &Path) -> String {
  if site.file.as_path() == entry_path {
    return entry_source.to_string();
  }
  std::fs::read_to_string(&site.file).unwrap_or_default()
}

fn site_to_location(site: &BindingSite, entry_source: &str, entry_path: &Path) -> IdeLocationJson {
  let src = source_for_location(site, entry_source, entry_path);
  IdeLocationJson {
    uri: path_to_file_uri(&site.file),
    range: text_range_to_lsp_range(&src, site.range),
  }
}

fn resolve_path_goto(
  ctx: &GotoCtx<'_>,
  segments: &[String],
  entry_source: &str,
  entry_path: &Path,
) -> Option<IdeLocationJson> {
  if segments.is_empty() {
    return None;
  }
  if segments.len() == 1 {
    let site = ctx.lookup(segments.first()?)?;
    return Some(site_to_location(&site, entry_source, entry_path));
  }
  let mod_key = segments[0..segments.len() - 1].join("::");
  let item = segments.last()?;
  if duckstruct_std::is_stdlib_module(&mod_key) {
    return None;
  }
  let target_path = resolve_module_path(ctx.current_file, &mod_key, ctx.project_root).ok()?;
  let dep_src = std::fs::read_to_string(&target_path).ok()?;
  let dep_parse = parse(&dep_src);
  if !dep_parse.errors.is_empty() {
    return None;
  }
  let dep_root = Root::cast(dep_parse.syntax())?;
  let site = pub_def_binding_site(&dep_root, item, &target_path)?;
  Some(site_to_location(&site, &dep_src, &target_path))
}

fn pub_def_binding_site(root: &Root, name: &str, file: &Path) -> Option<BindingSite> {
  for stmt in root.stmts() {
    match stmt {
      Stmt::VariableDef(def) if def.is_pub() => {
        if def.name()?.text() == name {
          return Some(BindingSite {
            file: file.to_path_buf(),
            range: def.name()?.text_range(),
          });
        }
      }
      Stmt::FunctionDef(def) if def.is_pub() => {
        if def.name()?.text() == name {
          return Some(BindingSite {
            file: file.to_path_buf(),
            range: def.name()?.text_range(),
          });
        }
      }
      Stmt::StructDef(def) if def.is_pub() => {
        if def.name()?.text() == name {
          return Some(BindingSite {
            file: file.to_path_buf(),
            range: def.name()?.text_range(),
          });
        }
      }
      _ => {}
    }
  }
  None
}

fn walk_stmts(
  stmts: impl Iterator<Item = Stmt>,
  ctx: &mut GotoCtx<'_>,
  entry_source: &str,
  entry_path: &Path,
) -> Option<IdeLocationJson> {
  for stmt in stmts {
    if let Some(loc) = walk_stmt(stmt, ctx, entry_source, entry_path) {
      return Some(loc);
    }
  }
  None
}

fn walk_stmt(
  stmt: Stmt,
  ctx: &mut GotoCtx<'_>,
  entry_source: &str,
  entry_path: &Path,
) -> Option<IdeLocationJson> {
  match stmt {
    Stmt::VariableDef(def) => {
      if let Some(val) = def.value() {
        if let Some(loc) = walk_expr(&val, ctx, entry_source, entry_path) {
          return Some(loc);
        }
      }
      if let Some(name_tok) = def.name() {
        let site = BindingSite {
          file: ctx.current_file.to_path_buf(),
          range: name_tok.text_range(),
        };
        ctx.define(name_tok.text().to_string(), site);
      }
      None
    }
    Stmt::FunctionDef(def) => {
      let name_tok = def.name()?;
      let fn_site = BindingSite {
        file: ctx.current_file.to_path_buf(),
        range: name_tok.text_range(),
      };
      ctx.define(name_tok.text().to_string(), fn_site);

      let func_expr = def.func()?;
      let Expr::Function(func) = func_expr else {
        return None;
      };
      ctx.push_layer();
      for p in func.params() {
        let site = BindingSite {
          file: ctx.current_file.to_path_buf(),
          range: p.text_range(),
        };
        ctx.define(p.text().to_string(), site);
      }
      let out = func
        .body()
        .and_then(|b| walk_expr(&b, ctx, entry_source, entry_path));
      ctx.pop_layer();
      out
    }
    Stmt::StructDef(def) => {
      let name_tok = def.name()?;
      let site = BindingSite {
        file: ctx.current_file.to_path_buf(),
        range: name_tok.text_range(),
      };
      ctx.define(name_tok.text().to_string(), site);
      None
    }
    Stmt::Use(u) => {
      let segments = u.path().map(|p| p.segments()).unwrap_or_default();
      if segments.is_empty() {
        return None;
      }
      let mod_key = if segments.len() == 1 {
        segments[0].clone()
      } else {
        segments.join("::")
      };
      if duckstruct_std::is_stdlib_module(&mod_key) {
        return None;
      }
      let target_path = resolve_module_path(ctx.current_file, &mod_key, ctx.project_root).ok()?;
      let dep_src = std::fs::read_to_string(&target_path).ok()?;
      let dep_parse = parse(&dep_src);
      if !dep_parse.errors.is_empty() {
        return None;
      }
      let dep_root = Root::cast(dep_parse.syntax())?;
      for item in u.items() {
        if let Some(site) = pub_def_binding_site(&dep_root, &item, &target_path) {
          ctx.define(item, site);
        }
      }
      None
    }
    Stmt::Expr(e) => walk_expr(&e, ctx, entry_source, entry_path),
  }
}

fn walk_expr(
  expr: &Expr,
  ctx: &mut GotoCtx<'_>,
  entry_source: &str,
  entry_path: &Path,
) -> Option<IdeLocationJson> {
  match expr {
    Expr::VariableRef(v) => {
      let tok = v.name_token();
      let r = tok.text_range();
      if !byte_in_text_range(ctx.offset, r) {
        return None;
      }
      let name = tok.text().to_string();
      let site = ctx.lookup(&name)?;
      Some(site_to_location(&site, entry_source, entry_path))
    }
    Expr::PathExpr(p) => {
      let last_r = path_last_ident_range(p)?;
      if !byte_in_text_range(ctx.offset, last_r) {
        return None;
      }
      let segs = p.segments();
      resolve_path_goto(ctx, &segs, entry_source, entry_path)
    }
    Expr::ParenExpr(p) => walk_expr(&p.expr()?, ctx, entry_source, entry_path),
    Expr::UnaryExpr(u) => walk_expr(&u.expr()?, ctx, entry_source, entry_path),
    Expr::BinaryExpr(b) => walk_expr(&b.lhs()?, ctx, entry_source, entry_path)
      .or_else(|| walk_expr(&b.rhs()?, ctx, entry_source, entry_path)),
    Expr::FunctionCall(c) => {
      if let Some(f) = c.func() {
        if let Some(loc) = walk_expr(&f, ctx, entry_source, entry_path) {
          return Some(loc);
        }
      }
      for a in c.args() {
        if let Some(loc) = walk_expr(&a, ctx, entry_source, entry_path) {
          return Some(loc);
        }
      }
      None
    }
    Expr::Block(b) => {
      ctx.push_layer();
      let out = walk_stmts(b.stmts(), ctx, entry_source, entry_path);
      ctx.pop_layer();
      out
    }
    Expr::Array(a) => {
      for el in a.elements() {
        if let Some(loc) = walk_expr(&el, ctx, entry_source, entry_path) {
          return Some(loc);
        }
      }
      None
    }
    Expr::Conditional(c) => walk_expr(&c.predicate()?, ctx, entry_source, entry_path)
      .or_else(|| walk_expr(&c.then_branch()?, ctx, entry_source, entry_path))
      .or_else(|| walk_expr(&c.else_branch()?, ctx, entry_source, entry_path)),
    Expr::Object(o) => {
      for (_, v) in o.fields() {
        if let Some(ref ve) = v {
          if let Some(loc) = walk_expr(ve, ctx, entry_source, entry_path) {
            return Some(loc);
          }
        }
      }
      None
    }
    Expr::ObjectFieldAccess(o) => walk_expr(&o.object()?, ctx, entry_source, entry_path),
    Expr::StructLiteral(s) => {
      if let Some(te) = s.type_expr() {
        if let Some(loc) = walk_expr(&te, ctx, entry_source, entry_path) {
          return Some(loc);
        }
      }
      for (_, v) in s.fields() {
        if let Some(ref ve) = v {
          if let Some(loc) = walk_expr(ve, ctx, entry_source, entry_path) {
            return Some(loc);
          }
        }
      }
      None
    }
    Expr::Function(f) => {
      ctx.push_layer();
      for p in f.params() {
        let site = BindingSite {
          file: ctx.current_file.to_path_buf(),
          range: p.text_range(),
        };
        ctx.define(p.text().to_string(), site);
      }
      let out = f
        .body()
        .and_then(|b| walk_expr(&b, ctx, entry_source, entry_path));
      ctx.pop_layer();
      out
    }
    Expr::ForExpression(for_e) => {
      if let Some(it) = for_e.iterable() {
        if let Some(loc) = walk_expr(&it, ctx, entry_source, entry_path) {
          return Some(loc);
        }
      }
      ctx.push_layer();
      bind_pat_from_for(for_e, ctx);
      if let Some(w) = for_e.where_clause() {
        if let Some(loc) = walk_expr(&w, ctx, entry_source, entry_path) {
          ctx.pop_layer();
          return Some(loc);
        }
      }
      if let Some(acc) = for_e.acc_init() {
        if let Some(loc) = walk_expr(&acc, ctx, entry_source, entry_path) {
          ctx.pop_layer();
          return Some(loc);
        }
      }
      if let Some((acc_n, idx_n)) = for_e.fold_params() {
        if let Some(acc_tok) = for_e.fold_param_ident_token(&acc_n) {
          ctx.define(
            acc_n,
            BindingSite {
              file: ctx.current_file.to_path_buf(),
              range: acc_tok.text_range(),
            },
          );
        }
        if let Some(idx_tok) = for_e.fold_param_ident_token(&idx_n) {
          ctx.define(
            idx_n,
            BindingSite {
              file: ctx.current_file.to_path_buf(),
              range: idx_tok.text_range(),
            },
          );
        }
      }
      let out = for_e
        .body()
        .and_then(|b| walk_expr(&b, ctx, entry_source, entry_path));
      ctx.pop_layer();
      out
    }
    _ => None,
  }
}

fn bind_pat_from_for(for_e: &ForExpression, ctx: &mut GotoCtx<'_>) {
  if let Some(pat) = for_e.binding_pattern() {
    match pat {
      Pat::Ident(id) => {
        if let Some(tok) = for_e.binding_ident_token() {
          ctx.define(
            id.name.clone(),
            BindingSite {
              file: ctx.current_file.to_path_buf(),
              range: tok.text_range(),
            },
          );
        }
      }
      Pat::Array(a) => {
        for p in &a.items {
          bind_pat_nested(p, ctx);
        }
      }
      Pat::Object(_) | Pat::Rest(_) => {}
    }
  }
}

fn bind_pat_nested(pat: &Pat, ctx: &mut GotoCtx<'_>) {
  match pat {
    Pat::Ident(_) => {}
    Pat::Array(a) => {
      for p in &a.items {
        bind_pat_nested(p, ctx);
      }
    }
    Pat::Object(_) | Pat::Rest(_) => {}
  }
}

/// `entry_source` is the buffer used to typecheck the entry module (may differ from disk).
pub fn ide_goto_definition(
  root: &Root,
  entry_source: &str,
  entry_path: &Path,
  project_root: Option<&Path>,
  byte_offset: usize,
) -> Option<IdeLocationJson> {
  let mut ctx = GotoCtx::new(byte_offset, entry_path, project_root);
  walk_stmts(root.stmts(), &mut ctx, entry_source, entry_path)
}

fn std_backend_from_manifest_dir(project_root: Option<&Path>) -> ManifestBackend {
  let Some(root) = project_root else {
    return ManifestBackend::Js;
  };
  manifest::load_manifest(root)
    .map(|m| m.backend())
    .unwrap_or(ManifestBackend::Js)
}

fn prelude_and_externals(
  backend: ManifestBackend,
) -> (
  Vec<(String, tycheck::typed_hir::TypedExpr)>,
  Vec<(String, tycheck::typed_hir::Ty)>,
) {
  let std_backend = match backend {
    ManifestBackend::Js => duckstruct_std::Backend::Js,
    ManifestBackend::Llvm => duckstruct_std::Backend::Llvm,
  };
  let prelude = duckstruct_std::globals_for_backend(std_backend);
  let ext = duckstruct_std::external_signatures_for_backend(std_backend);
  (prelude, ext)
}

/// Hover text: use full [`Ty`] formatting so structural types (e.g. objects with fields) stay visible.
fn format_ty_for_hover(ty: &Ty) -> String {
  format!("{}", ty)
}

/// First line: module path; second: type in a `duckstruct` fence (editor highlighting when supported).
/// If `documentation` is non-empty, a horizontal rule and the doc follow.
fn format_hover_markdown(module: &str, ty: &str, documentation: Option<&str>) -> String {
  let mut s = String::new();
  s.push_str("**");
  s.push_str(module);
  s.push_str("**\n\n");
  s.push_str("```duckstruct\n");
  s.push_str(ty);
  s.push_str("\n```");
  if let Some(doc) = documentation {
    let doc = doc.trim();
    if !doc.is_empty() {
      s.push_str("\n\n---\n\n");
      s.push_str(doc);
    }
  }
  s
}

/// Project-relative path as `a::b::file` (file stem), or the file stem when `project_root` is missing.
fn duckstruct_module_display(file_path: &Path, project_root: Option<&Path>) -> String {
  if let Some(root) = project_root {
    if let Ok(rel) = file_path.strip_prefix(root) {
      let path = rel.with_extension("");
      let parts: Vec<String> = path
        .components()
        .filter_map(|c| match c {
          std::path::Component::Normal(os) => os.to_str().map(str::to_string),
          _ => None,
        })
        .filter(|s| !s.is_empty())
        .collect();
      if !parts.is_empty() {
        return parts.join("::");
      }
    }
  }
  file_path
    .file_stem()
    .and_then(|s| s.to_str())
    .unwrap_or("main")
    .to_string()
}

fn hover_token_at_offset(root: &Root, byte_offset: usize) -> Option<SyntaxToken> {
  let root_node = root.syntax();
  let pos = TextSize::try_from(byte_offset).ok()?;
  root_node.token_at_offset(pos).right_biased()
}

/// Statement (or import) node whose leading trivia is the doc comment for this binding/decl.
fn doc_comment_anchor_ancestor(token: &SyntaxToken) -> Option<SyntaxNode> {
  let mut n = token.parent()?;
  loop {
    match n.kind() {
      SyntaxKind::LetStatement
      | SyntaxKind::NamedFunction
      | SyntaxKind::StructStatement
      | SyntaxKind::UseStatement => return Some(n),
      SyntaxKind::Root => return None,
      _ => n = n.parent()?,
    }
  }
}

/// Leading `//` / `/* */` block attached to the syntax anchor containing `byte_offset` in `root`.
fn leading_docs_for_token_in_root(root: &Root, byte_offset: usize) -> Option<String> {
  let token = hover_token_at_offset(root, byte_offset)?;
  let anchor = doc_comment_anchor_ancestor(&token)?;
  let first = anchor.first_token()?;
  preceding_trivia_docs_before_token(first)
}

fn path_from_file_uri(uri: &str) -> Option<PathBuf> {
  let rest = uri.strip_prefix("file://")?;
  // `file:///abs/path` on Unix; Windows may use `file:///C:/...`
  let p = if cfg!(windows) {
    PathBuf::from(rest.trim_start_matches('/'))
  } else {
    PathBuf::from(rest)
  };
  Some(p)
}

fn paths_point_to_same_file(a: &Path, b: &Path) -> bool {
  let ca = a.canonicalize().unwrap_or_else(|_| a.to_path_buf());
  let cb = b.canonicalize().unwrap_or_else(|_| b.to_path_buf());
  ca == cb
}

fn lsp_start_to_byte_offset(source: &str, start: &LspPositionJson) -> Option<usize> {
  line_character_utf16_to_byte_offset(source, start.line, start.character)
}

/// Docs from the comment block before the **definition** at `def` (same or other file on disk).
fn leading_docs_for_definition_location(
  def: &IdeLocationJson,
  entry_source: &str,
  entry_path: &Path,
  entry_root: &Root,
) -> Option<String> {
  let def_path = path_from_file_uri(&def.uri)?;
  if paths_point_to_same_file(&def_path, entry_path) {
    let off = lsp_start_to_byte_offset(entry_source, &def.range.start)?;
    return leading_docs_for_token_in_root(entry_root, off);
  }
  let dep_src = std::fs::read_to_string(&def_path).ok()?;
  let dep_parse = parse(&dep_src);
  if !dep_parse.errors.is_empty() {
    return None;
  }
  let dep_root = Root::cast(dep_parse.syntax())?;
  let off = lsp_start_to_byte_offset(&dep_src, &def.range.start)?;
  leading_docs_for_token_in_root(&dep_root, off)
}

/// Trivia for hover: use leading comments at the **definition** when go-to-def resolves
/// (so references show the same doc as the decl), else comments at the cursor.
fn hover_documentation(
  root: &Root,
  source: &str,
  file_path: &Path,
  byte_offset: usize,
  definition: Option<&IdeLocationJson>,
) -> Option<String> {
  if let Some(def) = definition {
    if let Some(doc) = leading_docs_for_definition_location(def, source, file_path, root) {
      if !doc.trim().is_empty() {
        return Some(doc);
      }
    }
  }
  leading_docs_for_token_in_root(root, byte_offset)
}

fn preceding_trivia_docs_before_token(first: SyntaxToken) -> Option<String> {
  let mut lines: Vec<String> = Vec::new();
  let mut t = first.prev_token();
  while let Some(tok) = t {
    match tok.kind() {
      SyntaxKind::Whitespace => {
        if tok.text().contains("\n\n") {
          break;
        }
        t = tok.prev_token();
      }
      SyntaxKind::Comment => {
        let text = tok.text().trim();
        if let Some(rest) = text.strip_prefix("//") {
          lines.push(rest.trim().to_string());
        } else if text.starts_with("/*") {
          let inner = text
            .trim_start_matches("/*")
            .trim_end_matches("*/")
            .trim();
          if !inner.is_empty() {
            lines.push(inner.to_string());
          }
        }
        t = tok.prev_token();
      }
      _ => break,
    }
  }
  lines.reverse();
  if lines.is_empty() {
    None
  } else {
    Some(lines.join("\n"))
  }
}

/// Hover on a `let` binding identifier (top-level only: matches [`TypedDatabase::definition`]).
fn hover_top_level_let_binding_name(
  root: &Root,
  byte_offset: usize,
  tycheck: &TyCheck,
) -> Option<String> {
  let (name, is_top_level) = find_let_binding_name_at_offset(root, byte_offset)?;
  if !is_top_level {
    return None;
  }
  let def = tycheck.ty_db.definition(&name)?;
  let ty = tycheck.ty_db.expr(def.value()).ty();
  Some(format_ty_for_hover(&ty))
}

/// Returns `(name, is_top_level_let_for_tycheck_db)` when `byte_offset` lies on a `let` binding name.
fn find_let_binding_name_at_offset(root: &Root, byte_offset: usize) -> Option<(String, bool)> {
  for stmt in root.stmts() {
    if let Some(r) = find_let_in_stmt(stmt, byte_offset, true) {
      return Some(r);
    }
  }
  None
}

fn find_let_in_stmt(stmt: Stmt, byte_offset: usize, stmt_from_root: bool) -> Option<(String, bool)> {
  match stmt {
    Stmt::VariableDef(def) => {
      if let Some(nt) = def.name() {
        if byte_in_text_range(byte_offset, nt.text_range()) {
          let top_level = stmt_from_root && def.is_top_level_in_source_file();
          return Some((nt.text().to_string(), top_level));
        }
      }
      def.value().and_then(|e| find_let_in_expr(&e, byte_offset))
    }
    Stmt::FunctionDef(def) => {
      let func_expr = def.func()?;
      let Expr::Function(f) = func_expr else {
        return None;
      };
      for p in f.params() {
        if byte_in_text_range(byte_offset, p.text_range()) {
          return None;
        }
      }
      f.body().and_then(|b| find_let_in_expr(&b, byte_offset))
    }
    Stmt::StructDef(_) | Stmt::Use(_) => None,
    Stmt::Expr(e) => find_let_in_expr(&e, byte_offset),
  }
}

fn find_let_in_expr(expr: &Expr, byte_offset: usize) -> Option<(String, bool)> {
  match expr {
    Expr::ParenExpr(p) => find_let_in_expr(&p.expr()?, byte_offset),
    Expr::UnaryExpr(u) => find_let_in_expr(&u.expr()?, byte_offset),
    Expr::BinaryExpr(b) => find_let_in_expr(&b.lhs()?, byte_offset)
      .or_else(|| find_let_in_expr(&b.rhs()?, byte_offset)),
    Expr::FunctionCall(c) => {
      if let Some(f) = c.func() {
        if let Some(r) = find_let_in_expr(&f, byte_offset) {
          return Some(r);
        }
      }
      for a in c.args() {
        if let Some(r) = find_let_in_expr(&a, byte_offset) {
          return Some(r);
        }
      }
      None
    }
    Expr::Block(b) => {
      for s in b.stmts() {
        if let Some(r) = find_let_in_stmt(s, byte_offset, false) {
          return Some(r);
        }
      }
      None
    }
    Expr::Array(a) => {
      for el in a.elements() {
        if let Some(r) = find_let_in_expr(&el, byte_offset) {
          return Some(r);
        }
      }
      None
    }
    Expr::Conditional(c) => find_let_in_expr(&c.predicate()?, byte_offset)
      .or_else(|| find_let_in_expr(&c.then_branch()?, byte_offset))
      .or_else(|| find_let_in_expr(&c.else_branch()?, byte_offset)),
    Expr::Object(o) => {
      for (_, v) in o.fields() {
        if let Some(ref ve) = v {
          if let Some(r) = find_let_in_expr(ve, byte_offset) {
            return Some(r);
          }
        }
      }
      None
    }
    Expr::ObjectFieldAccess(o) => find_let_in_expr(&o.object()?, byte_offset),
    Expr::StructLiteral(s) => {
      if let Some(te) = s.type_expr() {
        if let Some(r) = find_let_in_expr(&te, byte_offset) {
          return Some(r);
        }
      }
      for (_, v) in s.fields() {
        if let Some(ref ve) = v {
          if let Some(r) = find_let_in_expr(ve, byte_offset) {
            return Some(r);
          }
        }
      }
      None
    }
    Expr::Function(f) => {
      for p in f.params() {
        if byte_in_text_range(byte_offset, p.text_range()) {
          return None;
        }
      }
      f.body().and_then(|b| find_let_in_expr(&b, byte_offset))
    }
    Expr::ForExpression(for_e) => {
      if let Some(tok) = for_e.binding_ident_token() {
        if byte_in_text_range(byte_offset, tok.text_range()) {
          return None;
        }
      }
      if let Some(it) = for_e.iterable() {
        if let Some(r) = find_let_in_expr(&it, byte_offset) {
          return Some(r);
        }
      }
      if let Some(w) = for_e.where_clause() {
        if let Some(r) = find_let_in_expr(&w, byte_offset) {
          return Some(r);
        }
      }
      if let Some(acc) = for_e.acc_init() {
        if let Some(r) = find_let_in_expr(&acc, byte_offset) {
          return Some(r);
        }
      }
      for_e.body().and_then(|b| find_let_in_expr(&b, byte_offset))
    }
    Expr::VariableRef(_) | Expr::PathExpr(_) | Expr::NumberLit(_)
    | Expr::StringLit(_) | Expr::BooleanLit(_) => None,
  }
}

fn typecheck_from_root(
  root: Root,
  entry_source: &str,
  file_path: &Path,
  project_root: Option<&Path>,
) -> Result<TyCheck, String> {
  let backend = std_backend_from_manifest_dir(project_root);
  let (prelude, ext_fns) = prelude_and_externals(backend);
  let prelude_ref = (!prelude.is_empty()).then_some(prelude.as_slice());
  let ext_ref = (!ext_fns.is_empty()).then_some(ext_fns.as_slice());

  if modules::collect_use_deps(&root).is_empty() {
    let hir = lower(root);
    let mut tycheck = TyCheck::new(hir);
    tycheck.infer_with_modules(
      None,
      prelude_ref,
      ext_ref,
      Some(duckstruct_std::PRIMITIVE_METHODS),
    );
    return Ok(tycheck);
  }

  let (entry_hir, deps) =
    load_module_tree(file_path, project_root, Some(entry_source)).map_err(|e| e.to_string())?;
  let mut module_map: HashMap<String, &TyCheck> = HashMap::new();
  for dep in &deps {
    module_map.insert(dep.name.clone(), &dep.tycheck);
  }
  let mut entry_tycheck = TyCheck::new(entry_hir);
  entry_tycheck.infer_with_modules(
    Some(&module_map),
    prelude_ref,
    ext_ref,
    Some(duckstruct_std::PRIMITIVE_METHODS),
  );
  Ok(entry_tycheck)
}

/// Hover + definition at LSP `(line, character)` in `source`.
///
/// `file_path` is the on-disk path of the document (module resolution). `source` is the text
/// to parse and typecheck (may be unsaved editor content).
pub fn ide_query_at_position(
  source: &str,
  file_path: &Path,
  project_root: Option<&Path>,
  line: u32,
  character: u32,
) -> IdeQueryResult {
  let Some(byte_offset) = line_character_utf16_to_byte_offset(source, line, character) else {
    return IdeQueryResult {
      hover: None,
      definition: None,
      error: Some("invalid position (line out of range)".into()),
    };
  };

  let parse = parse(source);
  if !parse.errors.is_empty() {
    return IdeQueryResult {
      hover: None,
      definition: None,
      error: Some("parse errors".into()),
    };
  }
  let Some(root) = Root::cast(parse.syntax()) else {
    return IdeQueryResult {
      hover: None,
      definition: None,
      error: Some("failed to build AST".into()),
    };
  };

  let tycheck = match typecheck_from_root(root.clone(), source, file_path, project_root) {
    Ok(t) => t,
    Err(e) => {
      return IdeQueryResult {
        hover: None,
        definition: None,
        error: Some(e),
      };
    }
  };

  let module = duckstruct_module_display(file_path, project_root);
  let definition = ide_goto_definition(
    &root,
    source,
    file_path,
    project_root,
    byte_offset,
  );
  let doc = hover_documentation(
    &root,
    source,
    file_path,
    byte_offset,
    definition.as_ref(),
  );
  let hover = tycheck
    .hir_db
    .as_ref()
    .ref_hir_idx_at_byte_offset(byte_offset)
    .and_then(|idx| tycheck.ty_at_hir_expr(&idx))
    .map(|ty| format_ty_for_hover(&ty))
    .or_else(|| hover_top_level_let_binding_name(&root, byte_offset, &tycheck))
    .map(|ty| format_hover_markdown(&module, &ty, doc.as_deref()));

  IdeQueryResult {
    hover,
    definition,
    error: None,
  }
}

fn module_name_for_path(path: &Path) -> String {
  path
    .file_stem()
    .and_then(|s| s.to_str())
    .unwrap_or("")
    .to_string()
}

fn ty_display_for_def(tycheck: &TyCheck, name: &str) -> String {
  tycheck
    .ty_db
    .definition_expr(name)
    .map(|e| format_ty_for_hover(&e.ty()))
    .unwrap_or_default()
}

fn collect_top_level_symbols_ast(
  root: &Root,
  file: &Path,
  source: &str,
  module: &str,
  tycheck: &TyCheck,
) -> Vec<IdeSymbol> {
  let mut out = Vec::new();
  for stmt in root.stmts() {
    match stmt {
      Stmt::VariableDef(def) => {
        if let Some(nt) = def.name() {
          let n = nt.text().to_string();
          out.push(IdeSymbol {
            module: module.to_string(),
            name: n.clone(),
            kind: "variable".into(),
            ty: ty_display_for_def(tycheck, &n),
            file: file.display().to_string(),
            range: text_range_to_lsp_range(source, nt.text_range()),
          });
        }
      }
      Stmt::FunctionDef(def) => {
        if let Some(nt) = def.name() {
          let n = nt.text().to_string();
          out.push(IdeSymbol {
            module: module.to_string(),
            name: n.clone(),
            kind: "function".into(),
            ty: ty_display_for_def(tycheck, &n),
            file: file.display().to_string(),
            range: text_range_to_lsp_range(source, nt.text_range()),
          });
        }
      }
      Stmt::StructDef(def) => {
        if let Some(nt) = def.name() {
          let n = nt.text().to_string();
          out.push(IdeSymbol {
            module: module.to_string(),
            name: n.clone(),
            kind: "struct".into(),
            ty: ty_display_for_def(tycheck, &n),
            file: file.display().to_string(),
            range: text_range_to_lsp_range(source, nt.text_range()),
          });
        }
      }
      _ => {}
    }
  }
  out
}

/// Top-level symbols for the project entrypoint and all `use`-loaded modules (from disk).
pub fn ide_symbols_project(project_root: &Path) -> Result<Vec<IdeSymbol>, String> {
  let m = manifest::load_manifest(project_root)?;
  let entry_path = project_root.join(&m.entrypoint);
  let entry_src = std::fs::read_to_string(&entry_path)
    .map_err(|e| format!("Failed to read {}: {}", entry_path.display(), e))?;
  let (entry_hir, deps) =
    load_module_tree(&entry_path, Some(project_root), None).map_err(|e| e.to_string())?;

  let backend = m.backend();
  let (prelude, ext_fns) = prelude_and_externals(backend);
  let prelude_ref = (!prelude.is_empty()).then_some(prelude.as_slice());
  let ext_ref = (!ext_fns.is_empty()).then_some(ext_fns.as_slice());

  let mut module_map: HashMap<String, &TyCheck> = HashMap::new();
  for dep in &deps {
    module_map.insert(dep.name.clone(), &dep.tycheck);
  }
  let mut entry_tycheck = TyCheck::new(entry_hir);
  entry_tycheck.infer_with_modules(
    Some(&module_map),
    prelude_ref,
    ext_ref,
    Some(duckstruct_std::PRIMITIVE_METHODS),
  );

  let entry_parse = parse(&entry_src);
  let entry_root = Root::cast(entry_parse.syntax()).ok_or_else(|| "entry AST".to_string())?;

  let mut symbols = collect_top_level_symbols_ast(
    &entry_root,
    &entry_path,
    &entry_src,
    &module_name_for_path(&entry_path),
    &entry_tycheck,
  );

  for dep in &deps {
    if dep.name.contains("::") {
      // synthetic std path segment
    }
    let dep_path = if duckstruct_std::is_stdlib_module(&dep.name) {
      continue;
    } else {
      resolve_module_path(&entry_path, &dep.name, Some(project_root))
        .map_err(|e| e.to_string())?
    };
    let dep_src = std::fs::read_to_string(&dep_path)
      .map_err(|e| format!("Failed to read {}: {}", dep_path.display(), e))?;
    let dep_parse = parse(&dep_src);
    if dep_parse.errors.is_empty() {
      if let Some(dep_root) = Root::cast(dep_parse.syntax()) {
        symbols.extend(collect_top_level_symbols_ast(
          &dep_root,
          &dep_path,
          &dep_src,
          &dep.name,
          &dep.tycheck,
        ));
      }
    }
  }

  Ok(symbols)
}

#[cfg(test)]
mod tests {
  use std::fs;

  use diagnostics::byte_offset_to_line_character_utf16;
  use tempfile::tempdir;

  use super::*;

  #[test]
  fn ide_hover_and_goto_local_let() {
    let dir = tempdir().unwrap();
    let main_ds = dir.path().join("main.ds");
    let src = "let x = 1\nx\n";
    fs::write(&main_ds, src).unwrap();
    let r = ide_query_at_position(src, &main_ds, None, 1, 0);
    assert!(r.error.is_none(), "{:?}", r.error);
    assert_eq!(
      r.hover.as_deref(),
      Some("**main**\n\n```duckstruct\n1\n```")
    );
    let def = r.definition.expect("goto x");
    assert!(def.uri.contains("main.ds"));
    assert_eq!(def.range.start.line, 0);
  }

  #[test]
  fn ide_goto_cross_file_pub_use() {
    let dir = tempdir().unwrap();
    let main_ds = dir.path().join("main.ds");
    let helper_ds = dir.path().join("helper.ds");
    fs::write(
      &helper_ds,
      r#"pub let ONE = 1;
"#,
    )
    .unwrap();
    let main_src = "use helper::{ONE}\nONE\n";
    fs::write(&main_ds, main_src).unwrap();
    // Second line: `ONE` path ref at column 0
    let r = ide_query_at_position(main_src, &main_ds, None, 1, 0);
    assert!(r.error.is_none(), "{:?}", r.error);
    assert_eq!(
      r.hover.as_deref(),
      Some("**main**\n\n```duckstruct\n1\n```")
    );
    let def = r.definition.expect("goto ONE");
    assert!(def.uri.contains("helper.ds"));
  }

  #[test]
  fn ide_hover_on_reference_shows_declaration_doc() {
    let dir = tempdir().unwrap();
    let main_ds = dir.path().join("main.ds");
    let src = "// About x\nlet x = 1\nx\n";
    fs::write(&main_ds, src).unwrap();
    // Line 2: reference to `x`
    let r = ide_query_at_position(src, &main_ds, None, 2, 0);
    assert!(r.error.is_none(), "{:?}", r.error);
    let h = r.hover.expect("hover on ref");
    assert!(
      h.contains("About x"),
      "expected declaration doc on reference hover, got: {h}"
    );
  }

  #[test]
  fn ide_hover_includes_preceding_line_doc() {
    let dir = tempdir().unwrap();
    let main_ds = dir.path().join("main.ds");
    let src = "// doc line\nlet x = 1\n";
    fs::write(&main_ds, src).unwrap();
    let pos = src.find("let x").expect("let x") + "let ".len();
    let (line, col) = byte_offset_to_line_character_utf16(src, pos);
    let r = ide_query_at_position(src, &main_ds, None, line, col);
    assert!(r.error.is_none(), "{:?}", r.error);
    let h = r.hover.expect("hover");
    assert!(h.contains("---\n\n"), "expected ruler + doc, got: {h}");
    assert!(h.contains("doc line"), "got: {h}");
  }

  #[test]
  fn ide_hover_on_let_p_binding_shows_structured_object() {
    let dir = tempdir().unwrap();
    let main_ds = dir.path().join("main.ds");
    let src = "let Person = f(name) = new {\n  name: name,\n};\n\nlet p = Person(\"duck\");\n";
    fs::write(&main_ds, src).unwrap();
    let p_name_offset = src.find("let p =").expect("let p") + 4;
    let (line, col) = byte_offset_to_line_character_utf16(src, p_name_offset);
    let r = ide_query_at_position(src, &main_ds, None, line, col);
    assert!(r.error.is_none(), "{:?}", r.error);
    let h = r.hover.expect("hover on p");
    assert!(
      h.contains("name") && (h.contains("'duck'") || h.contains("duck")),
      "expected structured object hover, got: {h}"
    );
  }
}
