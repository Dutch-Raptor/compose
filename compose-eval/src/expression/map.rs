use crate::{Eval, Machine};
use compose_library::diag::{bail, SourceResult};
use compose_library::{MapValue, Value, Vm};
use compose_syntax::ast;
use compose_syntax::ast::{AstNode, Expr};
use std::collections::HashMap;
use crate::evaluated::Evaluated;

impl Eval for ast::MapLiteral<'_> {
    fn eval(self, vm: &mut Machine) -> SourceResult<Evaluated> {
        let mut map = HashMap::new();

        for entry in self.entries() {
            let key = match entry.key() {
                Expr::Ident(i) => i.get().clone(),
                other => {
                    let evaluated = other.eval(vm)?;

                    let Value::Str(as_str) = &evaluated.value else {
                        bail!(entry.key().span(), "map keys must be strings");
                    };

                    as_str.0.clone()
                }
            };

            let value = entry.value().eval(vm)?.value;

            map.insert(key, value);
        }

        Ok(Evaluated::mutable(Value::Map(MapValue::from(
            vm.heap_mut(),
            map.into(),
        ))))
    }
}
