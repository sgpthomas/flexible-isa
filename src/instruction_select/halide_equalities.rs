use crate::HalideLang;
use egg::rewrite as rw;

#[derive(Default)]
pub struct HalideEqualities;

impl HalideEqualities {
    pub fn rewrites<N>() -> Vec<egg::Rewrite<HalideLang, N>>
    where
        N: egg::Analysis<HalideLang>,
    {
        // let bidirectional = vec![rw!("neg-eq"; "(neg (== ?a ?b))" <=> "(!= ?a ?b)")];
        let bidirectional: Vec<Vec<egg::Rewrite<_, _>>> = vec![];

        // let unidirectional = vec![rw!("add0"; "(+ ?a 0)" => "?a")];
        let unidirectional: Vec<egg::Rewrite<_, _>> = vec![];

        bidirectional
            .into_iter()
            .flatten()
            .chain(unidirectional)
            .collect()
    }
}
