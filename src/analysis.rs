mod class_resolver;
pub mod error;
mod typeck;
mod typectx;
mod validate;

pub use error::TypeckError;
pub use typeck::{TypeChecker, TypeckResult};
pub use typectx::TypeCtx;
pub use validate::{check_has_main_class, validate_classes};