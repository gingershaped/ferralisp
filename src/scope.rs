//! HashMap wrappers for storing a machine's global and local scopes.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{builtins::BUILTINS, machine::{Error, ValueResult}, value::Value};

type Scope = HashMap<String, Rc<Value>>;
type Locals = Rc<RefCell<Vec<Scope>>>;

pub struct LocalScope {
    locals: Locals
}

impl LocalScope {
    fn scope<R, F: FnOnce(&mut Scope) -> R>(&self, f: F) -> R{
        let mut scopes = self.locals.borrow_mut();
        f(scopes.last_mut().expect("local scope stack underflow"))
    }

    pub fn insert(&self, key: String, value: Rc<Value>) {
        self.scope(|scope| scope.insert(key, value));
    }
    
    pub fn clear(&mut self) {
        self.scope(|scope| scope.clear());
    }
}

impl Drop for LocalScope {
    fn drop(&mut self) {
        self.locals.borrow_mut().pop();
    }
}

pub struct GlobalScope {
    globals: Scope,
    locals: Locals,
}

impl GlobalScope {
    pub fn new() -> GlobalScope {
        GlobalScope {
            globals: HashMap::from_iter(
                BUILTINS
                    .iter()
                    .map(|(k, v)| ((*k).into(), Rc::new(Value::Builtin(*v)))),
            ),
            locals: Rc::new(RefCell::new(vec![])),
        }
    }

    pub fn with_globals(globals: Scope) -> GlobalScope {
        let mut scope = GlobalScope::new();
        scope.globals.extend(globals);
        scope
    }

    pub fn insert(&mut self, key: String, value: Rc<Value>) {
        self.globals.insert(key, value);
    }

    pub fn local(&self) -> LocalScope {
        self.locals.borrow_mut().push(HashMap::new());
        LocalScope { locals: self.locals.clone() }
    }

    pub fn lookup(&self, name: &String) -> ValueResult {
        self
            .locals
            .borrow()
            .last()
            .and_then(|scope| scope.get(name))
            .or_else(|| self.globals.get(name))
            .ok_or_else(|| Error::UndefinedName(name.to_string()))
            .cloned()
    }

    pub fn global_lookup(&self, name: &String) -> Option<&Rc<Value>> {
        self.globals.get(name)
    }
}