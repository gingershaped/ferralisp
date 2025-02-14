use std::{collections::VecDeque, convert::Infallible};


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

pub const NIL: List<Infallible> = List { node: Node::Nil, length: 0 };

impl<T> List<T> {
    pub fn head(&self) -> Option<&T> {
        match &self.node {
            Node::Cons(value, _) => Some(value),
            Node::Nil => None,
        }
    }

    pub fn divide(&self) -> Option<(&T, List<&T>)> {
        match &self.node {
            Node::Cons(value, list) =>
                Some((&value, List { node: list.contents(), length: self.length - 1 })),
            Node::Nil => None,
        }
    }

    pub fn last(&self) -> Option<&T> {
        self.node.last()
    }

    pub fn len(&self) -> usize {
        self.length
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

// impl<T: Display> Display for Node<T> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "[")?;
//         for (index, value) in self.iter().enumerate() {
//             value.fmt(f)?;
//             if index != self.len() {
//                 write!(f, " ")?;
//             }
//         }
//         write!(f, "]")?;
//         Ok(())
//     }
// }

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::value::Value;

    use super::*;

    #[test]
    fn list_into() {
        assert_eq!(
            Node::from(VecDeque::from(vec![
                Rc::new(Value::Integer(1)),
                Rc::new(Value::Integer(2)),
                Rc::new(Value::Integer(3)),
            ])),
            Node::Cons(
                Rc::new(Value::Integer(1)),
                Box::new(Node::Cons(
                    Rc::new(Value::Integer(2)),
                    Box::new(Node::Cons(Rc::new(Value::Integer(3)), Box::new(Node::Nil)))
                )),
            )
        );
    }

    #[test]
    fn list_ops() {
        assert_eq!(
            List::from(vec![
                Rc::new(Value::Integer(1)),
                Rc::new(Value::Integer(2)),
                Rc::new(Value::Integer(3))
            ])
            .head(),
            Some(&Rc::new(Value::Integer(1)))
        );
        assert_eq!(
            List::from(vec![
                Rc::new(Value::Integer(1)),
                Rc::new(Value::Integer(2)),
                Rc::new(Value::Integer(3))
            ])
            .last(),
            Some(&Rc::new(Value::Integer(3)))
        );
    }
}