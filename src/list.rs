//! an implementation of a singly-linked list

use std::{collections::VecDeque, fmt::Display, ops::Index, rc::Rc};

#[derive(Clone, Debug, PartialEq, Eq)]
enum Node<T> {
    Cons(T, Box<Node<T>>),
    Nil,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct List<T> {
    node: Node<T>,
    length: usize,
}

impl<T> List<T> {
    /// return the first element of the list, or None if the list is nil
    pub fn head(&self) -> Option<&T> {
        match &self.node {
            Node::Cons(value, _) => Some(value),
            Node::Nil => None,
        }
    }

    /// return the list without its first element, or None if the list is nil
    pub fn tail(&self) -> Option<List<&T>> {
        match &self.node {
            Node::Cons(_, node) => Some(List {
                node: node.contents(),
                length: self.length - 1,
            }),
            Node::Nil => None,
        }
    }

    /// return a pair of the head of the list and the rest of the elements, or None if the list is nil
    pub fn divide(&self) -> Option<(&T, List<&T>)> {
        match &self.node {
            Node::Cons(value, list) => Some((
                &value,
                List {
                    node: list.contents(),
                    length: self.length - 1,
                },
            )),
            Node::Nil => None,
        }
    }

    pub fn cons(self, element: T) -> List<T> {
        List {
            node: Node::Cons(element, Box::new(self.node)),
            length: self.length + 1,
        }
    }

    /// return the last element of the list
    pub fn last(&self) -> Option<&T> {
        self.node.last()
    }

    /// return the number of elements in the list
    pub fn len(&self) -> usize {
        self.length
    }

    pub const fn nil() -> List<T> {
        List {
            node: Node::Nil,
            length: 0,
        }
    }
}

impl<T> Node<T> {
    fn last(&self) -> Option<&T> {
        match self {
            Node::Cons(value, list) => match list.as_ref() {
                Node::Cons(_, _) => list.last(),
                Node::Nil => Some(value),
            },
            Node::Nil => None,
        }
    }

    pub fn contents(&self) -> Node<&T> {
        match self {
            Node::Cons(value, node) => Node::Cons(value, Box::new(node.contents())),
            Node::Nil => Node::Nil,
        }
    }
}

impl<T> Index<usize> for List<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.node.index(index)
    }
}

impl<T> Index<usize> for Node<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            Node::Cons(item, node) => {
                if index == 0 {
                    item
                } else {
                    node.index(index - 1)
                }
            }
            Node::Nil => panic!("List index {} out of bounds", index),
        }
    }
}

impl<'a, T> IntoIterator for &'a List<T> {
    type Item = &'a T;
    type IntoIter = ListIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        ListIterator(Some(&self.node))
    }
}

impl<T> IntoIterator for List<T> {
    type Item = T;
    type IntoIter = ListConsumingIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        ListConsumingIterator(Some(self.node))
    }
}

pub struct ListIterator<'a, T>(Option<&'a Node<T>>);

impl<'a, T> Iterator for ListIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.take() {
            Some(Node::Cons(value, list)) => {
                self.0 = Some(list.as_ref());
                Some(value)
            }
            _ => None,
        }
    }
}

pub struct ListConsumingIterator<T>(Option<Node<T>>);

impl<T> Iterator for ListConsumingIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.take() {
            Some(Node::Cons(value, list)) => {
                self.0 = Some(*list);
                Some(value)
            }
            _ => None,
        }
    }
}

impl<T> From<VecDeque<T>> for Node<T> {
    fn from(mut deque: VecDeque<T>) -> Self {
        match deque.pop_front() {
            Some(value) => Node::Cons(value, Box::new(deque.into())),
            None => Node::Nil,
        }
    }
}

impl<T> From<VecDeque<T>> for List<T> {
    fn from(vec: VecDeque<T>) -> Self {
        List {
            length: vec.len(),
            node: Node::from(vec),
        }
    }
}

impl<T> From<Vec<T>> for List<T> {
    fn from(vec: Vec<T>) -> Self {
        List::from(VecDeque::from(vec))
    }
}

impl<T> FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        // TODO: consider using unsafe fuckery to do this without allocating that vec
        let mut items = iter.into_iter().collect::<Vec<_>>();
        items.reverse();
        let mut node = Node::Nil;
        let mut length = 0;

        for i in items {
            node = Node::Cons(i, Box::new(node));
            length += 1;
        }

        List { node, length }
    }
}

impl<'a, T> From<List<&Rc<T>>> for List<Rc<T>> {
    fn from(value: List<&Rc<T>>) -> Self {
        value.into_iter().cloned().collect()
    }
}

impl<T: Display> Display for List<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (index, value) in self.into_iter().enumerate() {
            value.fmt(f)?;
            if index != self.len() {
                write!(f, " ")?;
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_list() -> List<usize> {
        List::from(vec![1, 2, 3])
    }

    #[test]
    fn node_into() {
        assert_eq!(
            Node::from(VecDeque::from(vec![1, 2, 3])),
            Node::Cons(
                1,
                Box::new(Node::Cons(2, Box::new(Node::Cons(3, Box::new(Node::Nil))))),
            )
        );
    }

    #[test]
    fn list_ops() {
        assert_eq!(test_list().head(), Some(&1));
        assert_eq!(test_list().tail(), Some(List::from(vec![&2, &3])));
        assert_eq!(test_list().last(), Some(&3));
        assert_eq!(test_list().divide(), Some((&1, List::from(vec![&2, &3]))));
        assert_eq!(test_list().cons(0), List::from(vec![0, 1, 2, 3]));
    }
}
