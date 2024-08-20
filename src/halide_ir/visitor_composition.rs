use std::marker::PhantomData;

use super::Visitor;

pub struct ComposeVisitor<V, T>
where
    V: Visitor<T>,
{
    visitor: V,
    phantom: PhantomData<T>,
}

impl<V, T> ComposeVisitor<V, T>
where
    V: Visitor<T>,
{
    pub fn new(visitor: V) -> Self {
        Self {
            visitor,
            phantom: PhantomData,
        }
    }
}

impl<V, T> Default for ComposeVisitor<V, T>
where
    V: Visitor<T> + Default,
{
    fn default() -> Self {
        Self::new(V::default())
    }
}

impl<V, T> Visitor<T> for ComposeVisitor<V, T>
where
    V: Visitor<T>,
    T: Clone,
{
    type Output = (T, V::Output);

    fn default_u(&mut self, data: T) -> Self::Output {
        (data.clone(), self.visitor.default_u(data))
    }
}

pub struct ProjectRightVisitor<T, U> {
    phantom: PhantomData<(T, U)>,
}

impl<T, U> Default for ProjectRightVisitor<T, U> {
    fn default() -> Self {
        Self {
            phantom: PhantomData,
        }
    }
}

impl<T, U> Visitor<(T, U)> for ProjectRightVisitor<T, U> {
    type Output = U;

    fn default_u(&mut self, data: (T, U)) -> Self::Output {
        let (_t, u) = data;
        u
    }
}

pub struct SetData<T>
where
    T: Clone,
{
    value: T,
}

impl<T> SetData<T>
where
    T: Clone,
{
    pub fn new(value: T) -> Self {
        Self { value }
    }
}

impl<X, T> Visitor<X> for SetData<T>
where
    T: Clone,
{
    type Output = T;

    fn default_u(&mut self, _data: X) -> Self::Output {
        self.value.clone()
    }
}
