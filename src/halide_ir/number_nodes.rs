use super::Visitor;

#[derive(Default)]
pub struct NumberNodes {
    count: u64,
}

impl<T> Visitor<T> for NumberNodes {
    type Output = u64;

    fn default_u(&mut self, _data: T) -> u64 {
        self.count += 1;
        self.count
    }
}
