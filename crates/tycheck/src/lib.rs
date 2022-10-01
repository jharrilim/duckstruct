use hir::{expr::Expr, stmt::Stmt, DatabaseIdx};
use rustc_hash::FxHashMap;

#[derive(Debug)]
pub enum Either<A, B> {
  Left(A),
  Right(B),
}

#[derive(Debug, Clone)]
pub enum Ty {
  Number(Option<f64>),
  String(Option<String>),
  Boolean(Option<bool>),
  Array(Option<Vec<Ty>>),
  Function(Func),
  Generic,
  Error,
}

#[derive(Debug, Clone)]
pub struct Func {
  pub params: FxHashMap<String, Ty>,
  pub locals: FxHashMap<String, Ty>,
  pub body_stmts: Vec<Stmt>,
}
impl Func {
  // pub fn evaluate(&self, args: Vec<Ty>) -> Ty {
  //   if args.len() != self.params.len() {
  //     return Ty::Error;
  //   }
  //   for stmt in self.body_stmts.iter() {
  //     match stmt {
  //       Stmt::VariableDef { name, value } => {
  //         self.locals.insert(name.clone(), value.evaluate());
  //       },
  //       Stmt::FunctionDef { name, params, body } => {
  //         self.locals.insert(name.clone(), Ty::Function(Func {
  //           params: params.clone(),
  //           locals: FxHashMap::default(),
  //           body_stmts: body.clone(),
  //         }));
  //       },
  //       Stmt::Expr(_) => {},
  //     }
  //   };
  // }
}

type TyContext = FxHashMap<String, Ty>;

pub struct TyCheck {
  hir_db: hir::Database, // in
  ty_db: FxHashMap<String, Ty>, // out
}

impl TyCheck {
  pub fn new(hir_db: hir::Database) -> Self {
    Self {
      hir_db,
      ty_db: FxHashMap::default(),
    }
  }

  pub fn infer(&mut self) -> &FxHashMap<String, Ty> {
    println!("{:?}", self.hir_db);
    for (stmt_ident, statement) in self.hir_db.defs_iter() {
      let ctx = FxHashMap::default();
      let ty = self.infer_stmt(Either::Left(statement), &ctx);
      self.ty_db.insert(stmt_ident.clone(), ty);
    }
    &self.ty_db
  }

  pub fn infer_stmt(&self, stmt: Either<&Stmt, &DatabaseIdx>, ctx: &TyContext) -> Ty {

    match stmt {
      Either::Left(stmt) => match stmt {
        Stmt::VariableDef { name: _, value } => self.infer_expr(value, ctx),
        Stmt::FunctionDef {
          name: _,
          params,
          body: Expr::Block { stmts },
        } => {
          let params: FxHashMap<String, Ty> = params
            .iter()
            .map(|name| (name.clone(), Ty::Generic))
            .collect();
          self.infer_function(
            Func {
            params: params,
            locals: FxHashMap::default(),
            body_stmts: stmts.clone(),
          }, ctx)
        },
        Stmt::FunctionDef { name: _, params: _, body } => self.infer_expr(body, ctx),
        Stmt::Expr(expr) => self.infer_expr(expr, ctx),
      },
      Either::Right(idx) => self.infer_expr(self.hir_db.get_expr(idx), ctx),
    }
  }

  pub fn infer_expr(&self, expr: &Expr, ctx: &TyContext) -> Ty {
    match expr {
      Expr::VariableRef { var } => {
        let ty = self.ty_db.get(var).unwrap_or(&Ty::Error);
        ty.clone()
      }
      Expr::Block { stmts } => {
        // TODO: Solve block types properly
        let mut ty = Ty::Generic;
        for stmt in stmts {
          ty = self.infer_stmt(Either::Left(stmt), ctx);
        }
        ty
      }
      Expr::Number { n } => Ty::Number(Some(*n)),
      Expr::String { s } => Ty::String(Some(s.clone())),
      Expr::Binary { op, lhs, rhs } => {
        let lhs = self.infer_expr(self.hir_db.get_expr(lhs), ctx);
        let rhs = self.infer_expr(self.hir_db.get_expr(rhs), ctx);

        match op {
          hir::expr::BinaryOp::Add => match (lhs, rhs) {
            (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs + rhs)),
            (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),

            (Ty::String(Some(lhs)), Ty::String(Some(rhs))) => Ty::String(Some(lhs + &rhs)),
            (Ty::String(Some(_)), Ty::String(None) | Ty::Generic) => Ty::String(None),
            (Ty::String(_), Ty::String(_)) => Ty::String(None),

            (Ty::Array(Some(lhs)), Ty::Array(Some(rhs))) => Ty::Array(Some([lhs, rhs].concat())),
            (Ty::Array(_), Ty::Array(_)) => Ty::Array(None),
            _ => Ty::Generic,
          },
          hir::expr::BinaryOp::Sub => match (lhs, rhs) {
            (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs - rhs)),
            (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
            _ => Ty::Generic,
          },
          hir::expr::BinaryOp::Mul => match (lhs, rhs) {
            (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs * rhs)),
            (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
            _ => Ty::Generic,
          },
          hir::expr::BinaryOp::Div => match (lhs, rhs) {
            (Ty::Number(Some(lhs)), Ty::Number(Some(rhs))) => Ty::Number(Some(lhs / rhs)),
            (Ty::Number(Some(_)), Ty::Number(None) | Ty::Generic) => Ty::Number(None),
            (Ty::Number(_), Ty::Number(_)) => Ty::Number(None),
            _ => Ty::Generic,
          },
        }
      }
      Expr::Missing => Ty::Error,
      _ => Ty::Generic,
    }
  }

  fn infer_function(&self, func: Func, ctx: &TyContext) -> Ty {
    let mut locals = ctx;
    for stmt in &func.body_stmts {
      self.infer_stmt(Either::Left(stmt), locals);
    }
    Ty::Function(func)
  }
}
