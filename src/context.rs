use std::{
    collections::{HashMap, hash_map::Entry},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use once_cell::sync::Lazy;

static CONTEXT: Lazy<RwLock<Context>> = Lazy::new(|| RwLock::new(Context::new()));
#[allow(clippy::type_complexity)]
static ID_TO_STR: Lazy<Arc<RwLock<Vec<(Symbol, String)>>>> = Lazy::new(|| Arc::new(RwLock::new(Vec::new())));

#[derive(Debug, Clone, Default)]
pub struct Context {
    symbols: HashMap<String, Symbol>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
    id: u32,
    wildcard_level: u8,
    flags: SymbolFlags,
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct SymbolFlags: u8 {
        const NONE = 0;
        /// The function is linear.
        const IS_LINEAR = 1 << 0;
        /// The function is symmetric; that is, its value is unchanged by the order of its
        /// arguments.
        const IS_SYMMETRIC = 1 << 1;
    }
}

impl Context {
    pub const BUILTIN_SYMBOL_NAMES: [&'static str; 6] = ["ln", "sin", "cos", "sqrt", "e", "pi"];

    pub fn new() -> Context {
        let mut ctx = Context::default();

        for x in Self::BUILTIN_SYMBOL_NAMES {
            ctx.get_symbol(x.to_string());
        }

        ctx
    }

    pub fn reset() {
        let mut ctx = CONTEXT.write().unwrap();

        ctx.symbols.clear();
        ID_TO_STR.write().unwrap().clear();

        for x in Self::BUILTIN_SYMBOL_NAMES {
            ctx.get_symbol(x.to_string());
        }
    }

    #[inline]
    pub fn get_context() -> RwLockReadGuard<'static, Context> {
        CONTEXT.read().unwrap()
    }

    #[inline]
    pub fn get_context_mut() -> RwLockWriteGuard<'static, Context> {
        CONTEXT.write().unwrap()
    }

    pub fn get_symbol(&mut self, name: String) -> Symbol {
        match self.symbols.entry(name) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let mut wildcard_level = 0;
                for x in v.key().chars().rev() {
                    if x != '_' {
                        break;
                    }
                    wildcard_level += 1;
                }

                let id = ID_TO_STR.read().unwrap().len();
                let symbol = Symbol::raw_var(id as u32, wildcard_level);
                ID_TO_STR.write().unwrap().push((symbol, v.key().clone()));

                v.insert(symbol);
                symbol
            },
        }
    }

    #[inline]
    pub fn get_name(id: Symbol) -> String {
        let id_to_str = ID_TO_STR.read().unwrap();
        if id_to_str.is_empty() {
            let _ = *CONTEXT;
        }

        id_to_str[id.id as usize].1.clone()
    }

    pub fn read_symbols() -> RwLockReadGuard<'static, Vec<(Symbol, String)>> {
        let id_to_str = ID_TO_STR.read().unwrap();
        if id_to_str.is_empty() {
            let _ = *CONTEXT;
        }

        id_to_str
    }

    pub fn is_builtin(id: Symbol) -> bool {
        id.id < Self::BUILTIN_SYMBOL_NAMES.len() as u32
    }
}

impl Symbol {
    pub const LN: Symbol = Symbol::raw_fun(1, 0, SymbolFlags::NONE);
    pub const SIN: Symbol = Symbol::raw_fun(2, 0, SymbolFlags::NONE);
    pub const COS: Symbol = Symbol::raw_fun(3, 0, SymbolFlags::NONE);
    pub const SQRT: Symbol = Symbol::raw_fun(4, 0, SymbolFlags::NONE);
    pub const E: Symbol = Symbol::raw_var(5, 0);
    pub const PI: Symbol = Symbol::raw_var(6, 0);

    #[inline(always)]
    pub const fn raw_var(id: u32, wildcard_level: u8) -> Symbol {
        Symbol {
            id,
            wildcard_level,
            flags: SymbolFlags::NONE,
        }
    }

    #[inline(always)]
    pub const fn raw_fun(id: u32, wildcard_level: u8, flags: SymbolFlags) -> Symbol {
        Symbol {
            id,
            wildcard_level,
            flags,
        }
    }

    #[inline]
    pub fn is_linear(&self) -> bool {
        self.flags.contains(SymbolFlags::IS_LINEAR)
    }

    #[inline]
    pub fn is_symmetric(&self) -> bool {
        self.flags.contains(SymbolFlags::IS_SYMMETRIC)
    }
}
