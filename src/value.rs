//! values are the units of data which exist inside the machine. there are four kinds of them:
//! - List: a singly-linked list of values
//! - Builtin: a built-in function or macro
//! - Integer: a signed 64-bit integer
//! - Name: a name
//!   all values are truthy for the purposes of builtins like `i`,
//!   except for the number zero and the empty list (nil).

use std::{
    fmt::{Debug, Display}, hash::Hash, mem::MaybeUninit, num::NonZero, rc::{Rc, Weak}
};

use lasso::MicroSpur;
use strum_macros::IntoStaticStr;

use crate::{builtins::Builtin, machine::Interner};

#[derive(Clone, IntoStaticStr)]
pub enum Value {
    List(Rc<List>),
    Builtin(Builtin),
    Integer(i64),
    Name((HashlessMicroSpur, Weak<Interner>)),
}

impl Value {
    pub fn truthy(&self) -> bool {
        match self {
            Value::Integer(0) => false,
            Value::List(list) => !list.is_nil(),
            _ => true,
        }
    }

    pub fn nil() -> Value {
        Value::List(Rc::new(List::Nil))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::List(l0), Self::List(r0)) => l0 == r0,
            (Self::Builtin(l0), Self::Builtin(r0)) => l0 == r0,
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Name((l0, _)), Self::Name((r0, _))) => l0 == r0,
            _ => false,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(values) => Debug::fmt(values, f),
            Value::Builtin(builtin) => {
                write!(
                    f,
                    "<builtin {} \"{}\">",
                    if builtin.is_macro {
                        "macro"
                    } else {
                        "function"
                    },
                    builtin.name
                )
            }
            Value::Integer(value) => write!(f, "{}", value),
            Value::Name((name, interner)) => match interner.upgrade() {
                Some(interner) => match interner.borrow().try_resolve(name) {
                    Some(name) => write!(f, "{}", name),
                    None => write!(f, "<invalid key {}>", name.into_inner()),
                },
                None => write!(f, "<unbound key {}>", name.into_inner()),
            },
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<HashlessMicroSpur> for Value {
    fn from(value: HashlessMicroSpur) -> Self {
        Value::Name((value, Weak::new()))
    }
}

impl FromIterator<Value> for Value {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Value::List(Rc::new(iter.into_iter().collect()))
    }
}

#[derive(Clone, PartialEq)]
#[repr(C)]
pub enum List {
    Cons(Value, Rc<List>),
    Nil,
}

impl List {
    pub fn head(&self) -> Option<&Value> {
        match self {
            List::Cons(value, _) => Some(value),
            List::Nil => None,
        }
    }

    pub fn tail(&self) -> Option<Rc<List>> {
        match self {
            List::Cons(_, tail) => Some(tail.clone()),
            List::Nil => None,
        }
    }

    pub fn divide(&self) -> Option<(&Value, Rc<List>)> {
        match self {
            List::Cons(head, tail) => Some((head, tail.clone())),
            List::Nil => None,
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, List::Nil)
    }

    pub fn iter(&self) -> ListIterator {
        self.into_iter()
    }
}

pub struct ListIterator<'a>(Option<&'a List>);

impl<'a> IntoIterator for &'a List {
    type Item = &'a Value;
    type IntoIter = ListIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ListIterator(Some(&self))
    }
}

impl<'a> Iterator for ListIterator<'a> {
    type Item = &'a Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.take() {
            Some(List::Cons(value, list)) => {
                self.0 = Some(list.as_ref());
                Some(value)
            }
            _ => None,
        }
    }
}

#[repr(C)]
pub enum SpicyList {
    Cons(Value, Rc<MaybeUninit<SpicyList>>),
    Nil,
}

impl FromIterator<Value> for List {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let mut root_node = if let Some(first) = iter.next() {
            SpicyList::Cons(first, Rc::new_uninit())
        } else {
            return List::Nil;
        };
        let mut node = &mut root_node;

        for item in iter {
            let SpicyList::Cons(_, ref mut ptr) = node else {
                unreachable!()
            };
            let next_node = SpicyList::Cons(item, Rc::new_uninit());
            node = Rc::get_mut(ptr).unwrap().write(next_node);
        }

        let SpicyList::Cons(_, ref mut ptr) = node else {
            unreachable!()
        };
        Rc::get_mut(ptr).unwrap().write(SpicyList::Nil);

        unsafe {
            // SAFETY: SpicyList and List have the same memory layout,
            // and root_node is now fully initialized
            std::mem::transmute::<SpicyList, List>(root_node)
        }
    }
}

impl Debug for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let mut iter = self.iter();
        let mut item = iter.next();
        while let Some(value) = item {
            Debug::fmt(value, f)?;
            item = iter.next();
            if matches!(item, Some(_)) {
                write!(f, " ")?;
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct HashlessMicroSpur(MicroSpur);

impl HashlessMicroSpur {
    #[inline]
    pub const fn into_inner(&self) -> NonZero<u8> {
        self.0.into_inner()
    }
}

impl Hash for HashlessMicroSpur {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u8(self.0.into_inner().into());
    }
}

impl nohash_hasher::IsEnabled for HashlessMicroSpur {}

unsafe impl lasso::Key for HashlessMicroSpur {
    #[inline]
    fn into_usize(self) -> usize {
        self.0.into_usize()
    }

    #[inline]
    fn try_from_usize(int: usize) -> Option<Self> {
        MicroSpur::try_from_usize(int).map(|spur| Self(spur))
    }
}

impl Debug for HashlessMicroSpur {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use super::List;

    #[test]
    fn list_ops() {
        let test_list = List::Cons(
            1.into(),
            Rc::new(List::Cons(
                2.into(),
                Rc::new(List::Cons(3.into(), Rc::new(List::Nil))),
            )),
        );
        let head = &1.into();
        let tail = Rc::new(List::Cons(
            2.into(),
            Rc::new(List::Cons(3.into(), Rc::new(List::Nil))),
        ));

        assert_eq!(test_list.head(), Some(head));
        assert_eq!(test_list.tail(), Some(tail.clone()));
        assert_eq!(test_list.divide(), Some((head, tail)));
        assert_eq!(List::from_iter(vec![1.into(), 2.into(), 3.into()]), test_list);
        println!("{:?}", List::from_iter(vec![1.into(), 2.into(), 3.into()]))
    }
}
