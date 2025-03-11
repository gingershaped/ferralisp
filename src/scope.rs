//! HashMap wrappers for storing a machine's global and local scopes.

use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
};

use intmap::IntMap;
use tracing::trace;

use crate::{
    builtins::BUILTINS,
    machine::{Error, Interner, ValueResult},
    value::{HashlessMicroSpur, Value},
};

type Scope = IntMap<HashlessMicroSpur, Value>;
type Locals = Rc<RefCell<Vec<Scope>>>;

pub struct LocalScope {
    locals: Locals,
}

impl LocalScope {
    fn scope<R, F: FnOnce(&mut Scope) -> R>(&self, f: F) -> R {
        let mut scopes = self.locals.borrow_mut();
        f(scopes.last_mut().expect("local scope stack underflow"))
    }

    pub fn insert(&mut self, key: HashlessMicroSpur, value: Value) {
        self.scope(|scope| scope.insert(key, value));
    }

    pub fn clear(&mut self) {
        self.scope(|scope| scope.clear());
    }
}

impl Drop for LocalScope {
    fn drop(&mut self) {
        trace!("popping local scope");
        self.locals.borrow_mut().pop();
    }
}

impl Debug for LocalScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "LocalScope {{")?;
        self.scope(|scope| {
            for (key, value) in scope.iter() {
                writeln!(f, "    {} = {}", key.into_inner(), value)?;
            }
            Ok(())
        })?;
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl Display for LocalScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

pub struct GlobalScope {
    globals: Scope,
    locals: Locals,
    interner: Rc<Interner>,
}

impl GlobalScope {
    pub fn new(interner: Rc<Interner>) -> GlobalScope {
        GlobalScope {
            globals: IntMap::from_iter(
                BUILTINS
                    .iter()
                    .map(|(k, v)| (interner.borrow_mut().get_or_intern(k), Value::Builtin(*v))),
            ),
            locals: Rc::new(RefCell::new(vec![])),
            interner,
        }
    }

    pub fn with_globals(interner: Rc<Interner>, globals: Scope) -> GlobalScope {
        let mut scope = GlobalScope::new(interner);
        scope.globals.extend(globals);
        scope
    }

    pub fn insert(&mut self, key: HashlessMicroSpur, value: Value) {
        trace!("defining new global {} = {}", key.into_inner(), value);
        self.globals.insert(key, value);
    }

    pub fn local(&self) -> LocalScope {
        trace!("pushing local scope");
        self.locals.borrow_mut().push(IntMap::default());
        LocalScope {
            locals: self.locals.clone(),
        }
    }

    pub fn lookup(&self, name: &HashlessMicroSpur) -> ValueResult {
        self.locals
            .borrow()
            .last()
            .and_then(|scope| scope.get(*name))
            .or_else(|| self.globals.get(*name))
            .ok_or_else(|| {
                trace!(
                    "failed to look up name {}! current scopes: {}",
                    name.into_inner(),
                    self
                );
                Error::UndefinedName(
                    self.interner
                        .borrow_mut()
                        .try_resolve(name)
                        .map(|v| v.to_owned()),
                )
            })
            .cloned()
    }

    pub fn global_lookup(&self, name: &HashlessMicroSpur) -> Option<&Value> {
        self.globals.get(*name)
    }
}

impl Debug for GlobalScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "GlobalScope {{")?;

        writeln!(f, "  globals:")?;
        for (key, value) in self.globals.iter() {
            writeln!(f, "    {} = {}", key.into_inner(), value)?;
        }
        writeln!(f)?;

        {
            let locals = self.locals.borrow();
            for (index, scope) in locals.iter().enumerate() {
                writeln!(f, "  local scope {}:", index)?;
                if scope.is_empty() {
                    writeln!(f, "    <empty>")?;
                } else {
                    for (key, value) in scope.iter() {
                        writeln!(f, "    {} = {}", key.into_inner(), value)?;
                    }
                }
                writeln!(f)?;
            }
        }

        writeln!(f, "}}")?;
        Ok(())
    }
}

impl Display for GlobalScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}
