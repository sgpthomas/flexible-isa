use derive_deftly::define_derive_deftly;

pub trait Annotation<T> {
    fn data(&self) -> &T;
    fn data_mut(&mut self) -> &mut T;
}

define_derive_deftly! {
    export Annotation:

    ${select1
      is_struct {
          impl<$tgens> $crate::halide_ir::Annotation<$tgens> for $ttype where $twheres {
              fn data(&self) -> &T {
                  $(
                      ${when fmeta(data)} &self.$fname
                  )
              }

              fn data_mut(&mut self) -> &mut T {
                  $(
                      ${when fmeta(data)} &mut self.$fname
                  )
              }
          }
      }
      is_enum {
          impl<$tgens> $crate::halide_ir::Annotation<$tgens> for $ttype where $twheres {
              fn data(&self) -> &T {
                  #[allow(unused)]
                  match self {
                      $(
                          $vpat => { ${for fields {
                              ${when fmeta(data)} $fpatname
                          }} },
                      )
                  }
              }

              fn data_mut(&mut self) -> &mut T {
                  #[allow(unused)]
                  match self {
                      $(
                          $vpat => { ${for fields {
                              ${when fmeta(data)} $fpatname
                          }} },
                      )
                  }
              }
          }
      }
    }

}
