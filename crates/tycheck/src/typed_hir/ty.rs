use std::fmt::Display;

use data_structures::FxIndexMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
  Number(Option<f64>),
  String(Option<String>),
  Boolean(Option<bool>),
  Array(Option<Vec<Ty>>),
  Object(Option<FxIndexMap<String, Ty>>),
  Function {
    params: Vec<Ty>,
    ret: Option<Box<Ty>>,
  },
  /// Nominal instance of a struct (displayed as the struct name).
  Instance(String),
  Generic,
  Error,
}

impl Ty {
  pub fn has_value(&self) -> bool {
    matches!(self, |Ty::Number(Some(_))| Ty::String(Some(_))
      | Ty::Boolean(Some(_))
      | Ty::Array(Some(_))
      | Ty::Function {
        params: _,
        ret: Some(_)
      }
      | Ty::Object(Some(_)))
  }

  pub fn type_eq(&self, other: &Ty) -> bool {
    match (self, other) {
      (Ty::Number(_), Ty::Number(_)) => true,
      (Ty::String(_), Ty::String(_)) => true,
      (Ty::Boolean(_), Ty::Boolean(_)) => true,
      (Ty::Array(_), Ty::Array(_)) => true,
      (Ty::Object(_), Ty::Object(_)) => true,
      (Ty::Instance(a), Ty::Instance(b)) => a == b,
      (
        Ty::Function { params, ret },
        Ty::Function {
          params: other_params,
          ret: other_ret,
        },
      ) => {
        if params.len() != other_params.len() {
          return false;
        }

        for (param, other_param) in params.iter().zip(other_params.iter()) {
          if !param.type_eq(other_param) {
            return false;
          }
        }

        match (ret, other_ret) {
          (Some(ret), Some(other_ret)) => ret.type_eq(other_ret),
          (None, None) => true,
          _ => false,
        }
      }
      _ => false,
    }
  }

  pub fn deconst(&self) -> Ty {
    match self {
      Ty::Number(_) => Ty::Number(None),
      Ty::String(_) => Ty::String(None),
      Ty::Boolean(_) => Ty::Boolean(None),
      Ty::Array(_) => Ty::Array(None),
      Ty::Object(_) => Ty::Object(None),
      Ty::Function { params, ret } => Ty::Function {
        params: params.iter().map(|p| p.deconst()).collect(),
        ret: ret.as_ref().map(|r| Box::new(r.deconst())),
      },
      Ty::Instance(name) => Ty::Instance(name.clone()),
      Ty::Generic => Ty::Generic,
      Ty::Error => Ty::Error,
    }
  }

  pub fn truthy(&self) -> Truthiness {
    match self {
      Ty::Number(Some(val)) => Truthiness::Known(*val != 0.0),
      Ty::String(Some(val)) => Truthiness::Known(!val.is_empty()),
      Ty::Boolean(Some(val)) => Truthiness::Known(*val),
      Ty::Array(Some(val)) => Truthiness::Known(!val.is_empty()),
      Ty::Object(Some(val)) => Truthiness::Known(!val.is_empty()),
      Ty::Function { .. } => Truthiness::Known(true),
      Ty::Instance(_) => Truthiness::Unknown,
      Ty::Generic => Truthiness::Unknown,
      Ty::Error => Truthiness::Unknown,
      _ => Truthiness::Unknown,
    }
  }

  /// Whether `actual` can satisfy a use site that expects `required` (structural / use-site rules).
  pub fn assignable_from(actual: &Ty, required: &Ty) -> bool {
    match (required, actual) {
      (Ty::Generic, _) | (_, Ty::Generic) => true,
      (Ty::Error, _) | (_, Ty::Error) => true,
      (Ty::Number(req), Ty::Number(act)) => match (req, act) {
        (Some(a), Some(b)) => a == b,
        _ => true,
      },
      (Ty::String(req), Ty::String(act)) => match (req, act) {
        (Some(a), Some(b)) => a == b,
        _ => true,
      },
      (Ty::Boolean(req), Ty::Boolean(act)) => match (req, act) {
        (Some(a), Some(b)) => a == b,
        _ => true,
      },
      (Ty::Array(req), Ty::Array(act)) => match (req, act) {
        (None, _) | (_, None) => true,
        (Some(rv), Some(av)) if rv.len() == av.len() => rv
          .iter()
          .zip(av.iter())
          .all(|(r, a)| Self::assignable_from(a, r)),
        _ => false,
      },
      (Ty::Object(req), Ty::Object(act)) => match (req, act) {
        (None, _) | (_, None) => true,
        (Some(rf), Some(af)) => rf.iter().all(|(name, r_ty)| {
          if matches!(r_ty, Ty::Generic) {
            return true;
          }
          match af.get(name) {
            Some(a_ty) => Self::assignable_from(a_ty, r_ty),
            None => false,
          }
        }),
      },
      (
        Ty::Function {
          params: rp,
          ret: rr,
        },
        Ty::Function {
          params: ap,
          ret: ar,
        },
      ) => {
        if rp.len() != ap.len() {
          return false;
        }
        let params_ok = rp
          .iter()
          .zip(ap.iter())
          .all(|(r_ty, a_ty)| Self::assignable_from(r_ty, a_ty));
        let ret_ok = match (rr, ar) {
          (None, None) => true,
          (Some(r), Some(a)) => Self::assignable_from(a.as_ref(), r.as_ref()),
          _ => true,
        };
        params_ok && ret_ok
      }
      (Ty::Instance(r), Ty::Instance(a)) => r == a,
      _ => false,
    }
  }

  /// Human-oriented explanation when a call argument does not meet a formal parameter's inferred constraint.
  ///
  /// `object_label` names the argument value in diagnostics (e.g. a variable name, or `"anonymous"`).
  pub fn parameter_argument_constraint_message(
    actual: &Ty,
    required: &Ty,
    object_label: &str,
  ) -> Option<String> {
    if Self::assignable_from(actual, required) {
      return None;
    }

    if let Ty::Object(Some(req_fields)) = required {
      let missing_field_msg = |name: &str, r_ty: &Ty| {
        if matches!(r_ty, Ty::Function { .. }) {
          format!("object {object_label} must provide `{name}()`")
        } else {
          format!("object {object_label} must provide `{name}`")
        }
      };

      match actual {
        Ty::Object(Some(act_fields)) => {
          for (name, r_ty) in req_fields.iter() {
            if matches!(r_ty, Ty::Generic) {
              continue;
            }
            match act_fields.get(name) {
              None => return Some(missing_field_msg(name, r_ty)),
              Some(a_ty) if !Self::assignable_from(a_ty, r_ty) => {
                return Some(format!(
                  "object {object_label}: field `{name}` has incompatible type (expected {r_ty}, found {a_ty})"
                ));
              }
              _ => {}
            }
          }
        }
        // Nominal struct instance: this representation has no open field map, so treat
        // every required (non-generic) field as subject to the same missing-field messages.
        Ty::Instance(_) => {
          for (name, r_ty) in req_fields.iter() {
            if matches!(r_ty, Ty::Generic) {
              continue;
            }
            return Some(missing_field_msg(name, r_ty));
          }
        }
        _ => {
          if !matches!(actual, Ty::Generic) {
            return Some(format!(
              "object {object_label} does not match the parameter constraint (expected an object with required fields, found `{}`)",
              actual
            ));
          }
        }
      }
    }

    Some(format!(
      "object {object_label}: type `{}` is not assignable to parameter constraint `{}`",
      actual, required
    ))
  }
}

pub enum Truthiness {
  Known(bool),
  Unknown,
}

impl Display for Ty {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Ty::Number(Some(n)) => write!(f, "{}", n),
      Ty::String(Some(s)) => write!(f, "'{}'", s),
      Ty::Boolean(Some(b)) => write!(f, "{}", b),
      Ty::Array(Some(a)) => {
        write!(f, "[")?;
        for (i, ty) in a.iter().enumerate() {
          write!(f, "{}", ty)?;
          if i < a.len() - 1 {
            write!(f, ", ")?;
          }
        }
        write!(f, "]")
      }
      Ty::Object(Some(o)) => {
        write!(f, "new {{ ")?;
        for (i, (key, ty)) in o.iter().enumerate() {
          write!(f, "{}: {}", key, ty)?;
          if i < o.len() - 1 {
            write!(f, ", ")?;
          }
        }
        write!(f, " }}")
      }
      Ty::Function { params, ret } => {
        write!(f, "(")?;
        for (i, ty) in params.iter().enumerate() {
          write!(f, "{}", ty)?;
          if i < params.len() - 1 {
            write!(f, ", ")?;
          }
        }
        write!(f, ") => ")?;
        if let Some(ret) = ret {
          write!(f, "{}", ret)
        } else {
          write!(f, "unknown")
        }
      }
      Ty::Number(None) => write!(f, "number"),
      Ty::String(None) => write!(f, "string"),
      Ty::Boolean(None) => write!(f, "boolean"),
      Ty::Array(None) => write!(f, "array"),
      Ty::Object(None) => write!(f, "object"),
      Ty::Instance(name) => write!(f, "{}", name),
      Ty::Generic => write!(f, "generic"),
      Ty::Error => write!(f, "error"),
    }
  }
}
