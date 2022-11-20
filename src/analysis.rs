mod class_resolver;
pub mod error;
mod typeck;
mod typectx;

pub use error::TypeckError;
pub use typeck::{TypeChecker, TypeckResult};
pub use typectx::TypeCtx;
