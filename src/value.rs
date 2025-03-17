//! values are the units of data which exist inside the machine. there are four kinds of them:
//! - List: a singly-linked list of values
//! - Builtin: a built-in function or macro
//! - Integer: a signed 64-bit integer
//! - Name: a name
//! all values are truthy for the purposes of builtins like `i`,
//! except for the number zero and the empty list (nil).

use std::{
    convert::Infallible,
    fmt::{Debug, Display},
    mem::MaybeUninit,
    num::NonZero,
    ops::Deref,
    rc::{Rc, Weak},
};

use lasso::MiniSpur;
use refpool::{Pool, PoolDefault, PoolRef};
use strum_macros::IntoStaticStr;

use crate::{
    builtins::Builtin,
    machine::{Interner, Machine},
};

trait DisplayWithContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, interner: Option<&Rc<Interner>>) -> std::fmt::Result;
}

#[derive(Clone, IntoStaticStr, PartialEq)]
pub enum Value {
    List(NodePtr),
    Builtin(Builtin),
    Integer(i64),
    Name(HashlessMiniSpur),
}

impl Value {
    pub fn truthy(&self) -> bool {
        match self {
            Value::Integer(0) => false,
            Value::List(list) => !list.is_nil(),
            _ => true,
        }
    }

    pub const fn nil() -> Value {
        Value::List(NodePtr::NIL)
    }

    pub fn from_iter<T: IntoIterator<Item = Value>>(pool: &Pool<Node>, iter: T) -> Self {
        Value::List(NodePtr::from_iter(pool, iter))
    }

    pub fn contextualize(&self, machine: &Machine) -> ContextValue {
        ContextValue {
            value: self.clone(),
            interner: Rc::downgrade(&machine.interner),
        }
    }

}

impl DisplayWithContext for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, interner: Option<&Rc<Interner>>) -> std::fmt::Result {
        match self {
            Value::List(values) => DisplayWithContext::fmt(values, f, interner),
            Value::Builtin(builtin) => {
                write!(
                    f,
                    "<builtin {} at {:p}>",
                    if builtin.is_macro {
                        "macro"
                    } else {
                        "function"
                    },
                    builtin.body
                )
            }
            Value::Integer(value) => write!(f, "{}", value),
            Value::Name(name) => match interner {
                Some(interner) => match interner.borrow().try_resolve(name) {
                    Some(name) => write!(f, "{}", name),
                    None => write!(f, "<invalid key {}>", name.into_inner()),
                },
                None => write!(f, "<unbound key {}>", name.into_inner()),
            },
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        DisplayWithContext::fmt(self, f, None)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<HashlessMiniSpur> for Value {
    fn from(value: HashlessMiniSpur) -> Self {
        Value::Name(value)
    }
}

#[derive(Clone)]
pub struct ContextValue {
    value: Value,
    interner: Weak<Interner>,
}

impl Deref for ContextValue {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl Display for ContextValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        DisplayWithContext::fmt(&**self, f, self.interner.upgrade().as_ref())
    }
}

impl Debug for ContextValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl PartialEq for ContextValue {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

#[derive(Clone, Default, PartialEq)]
#[repr(transparent)]
pub struct NodePtr(Option<PoolRef<Node>>);

#[repr(C)]
#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    value: Value,
    next: NodePtr,
}

impl NodePtr {
    pub const NIL: NodePtr = NodePtr(None);

    pub fn head(&self) -> Option<&Value> {
        match &self.0 {
            Some(node) => Some(&node.value),
            None => None,
        }
    }

    pub fn tail(&self) -> Option<NodePtr> {
        match &self.0 {
            Some(node) => Some(node.next.clone()),
            None => None,
        }
    }

    pub fn divide<'a>(&'a self) -> Option<(&'a Value, &'a NodePtr)> {
        match &self.0 {
            Some(node) => Some((&node.value, &node.next)),
            None => None,
        }
    }

    pub fn cons(&self, pool: &Pool<Node>, value: Value) -> NodePtr {
        let node = Node {
            value,
            next: self.clone(),
        };
        NodePtr(Some(PoolRef::new(pool, node)))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self.0, None)
    }

    pub fn iter(&self) -> ListIterator {
        self.into_iter()
    }
}

impl PoolDefault for NodePtr {
    unsafe fn default_uninit(target: &mut MaybeUninit<Self>) {
        target.write(NodePtr(None));
    }
}

pub struct ListIterator<'a>(Option<&'a Node>);

impl<'a> IntoIterator for &'a NodePtr {
    type Item = &'a Value;
    type IntoIter = ListIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ListIterator(self.0.as_deref())
    }
}

impl<'a> Iterator for ListIterator<'a> {
    type Item = &'a Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.take() {
            Some(node) => {
                self.0 = node.next.0.as_deref();
                Some(&node.value)
            }
            _ => None,
        }
    }
}

impl DisplayWithContext for NodePtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, interner: Option<&Rc<Interner>>) -> std::fmt::Result {
        write!(f, "(")?;
        let mut iter = self.iter();
        let mut item = iter.next();
        while let Some(value) = item {
            DisplayWithContext::fmt(value, f, interner)?;
            item = iter.next();
            if item.is_some() {
                write!(f, " ")?;
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl Debug for NodePtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        DisplayWithContext::fmt(self, f, None)
    }
}

#[repr(C)]
pub struct UninitNode {
    value: Value,
    next: Option<PoolRef<MaybeUninit<UninitNode>>>,
}

impl NodePtr {
    pub fn from_iter<T: IntoIterator<Item = Value>>(pool: &Pool<Node>, iter: T) -> Self {
        let Ok(list) = Self::try_from_iter(pool, iter.into_iter().map(Ok::<Value, Infallible>));
        list
    }

    pub fn try_from_iter<E, T: IntoIterator<Item = Result<Value, E>>>(
        pool: &Pool<Node>,
        iter: T,
    ) -> Result<Self, E> {
        let pool: Pool<MaybeUninit<UninitNode>> = pool.cast();
        let mut iter = iter.into_iter();
        let mut root_node = if let Some(first) = iter.next() {
            UninitNode {
                value: first?,
                next: Some(PoolRef::new(&pool, MaybeUninit::uninit())),
            }
        } else {
            return Ok(NodePtr::NIL);
        };
        let mut node = &mut root_node;

        for item in iter {
            match item {
                Ok(value) => {
                    let Some(ref mut ptr) = node.next else {
                        unreachable!()
                    };
                    let next_node = UninitNode {
                        value,
                        next: Some(PoolRef::new(&pool, MaybeUninit::uninit())),
                    };
                    node = PoolRef::get_mut(ptr).unwrap().write(next_node);
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        node.next = None;

        let root_node = unsafe {
            // SAFETY: UninitNode and Node have the same memory layout,
            // and root_node is now fully initialized
            std::mem::transmute::<UninitNode, Node>(root_node)
        };

        Ok(NodePtr(Some(PoolRef::new(&pool.cast(), root_node))))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct HashlessMiniSpur(MiniSpur);

impl HashlessMiniSpur {
    #[inline]
    pub const fn into_inner(&self) -> NonZero<u16> {
        self.0.into_inner()
    }
}

unsafe impl lasso::Key for HashlessMiniSpur {
    #[inline]
    fn into_usize(self) -> usize {
        self.0.into_usize()
    }

    #[inline]
    fn try_from_usize(int: usize) -> Option<Self> {
        MiniSpur::try_from_usize(int).map(Self)
    }
}

impl intmap::IntKey for HashlessMiniSpur {
    type Int = u16;

    const PRIME: Self::Int = u16::PRIME;

    #[inline(always)]
    fn into_int(self) -> Self::Int {
        self.into_inner().into()
    }
}

impl Debug for HashlessMiniSpur {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(test)]
mod test {
    use refpool::{Pool, PoolRef};

    use super::{Node, NodePtr};

    #[test]
    fn list_ops() {
        let pool = Pool::new(0);

        let test_list = NodePtr(Some(PoolRef::new(
            &pool,
            Node {
                value: 1.into(),
                next: NodePtr(Some(PoolRef::new(
                    &pool,
                    Node {
                        value: 2.into(),
                        next: NodePtr(Some(PoolRef::new(
                            &pool,
                            Node {
                                value: 3.into(),
                                next: NodePtr::NIL,
                            },
                        ))),
                    },
                ))),
            },
        )));
        let head = &1.into();
        let tail = NodePtr(Some(PoolRef::new(
            &pool,
            Node {
                value: 2.into(),
                next: NodePtr(Some(PoolRef::new(
                    &pool,
                    Node {
                        value: 3.into(),
                        next: NodePtr::NIL,
                    },
                ))),
            },
        )));

        assert_eq!(test_list.head(), Some(head));
        assert_eq!(test_list.tail(), Some(tail.clone()));
        assert_eq!(test_list.divide(), Some((head, &tail)));
        assert_eq!(
            NodePtr::from_iter(&pool, vec![1.into(), 2.into(), 3.into()]),
            test_list
        );
    }
}
