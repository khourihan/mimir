use crate::context::{Context, Symbol};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
    pub id: Symbol,
}

impl Var {
    #[inline]
    pub fn new(id: Symbol) -> Var {
        Var { id }
    }

    #[inline]
    pub fn from_name(name: String) -> Var {
        let s = Context::get_context_mut().get_symbol(name);
        Var::new(s)
    }
}
