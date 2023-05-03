use std::collections::HashMap;

use crate::analysis::{self, BindingId, BindingKind, BindingMap, DefinitionLocation};
use crate::ir::mem::ArenaRef;
use crate::ir::ty::{IrClassName, IrTyRegistry, MaybeSelfTy};

use super::class::lower_object_ty;

#[derive(Debug, Clone)]
struct ClassBindings<'a> {
    bindings: Vec<Binding<'a>>,
    self_binding: BindingId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding<'a> {
    pub id: BindingId,
    pub kind: BindingKind,
    pub ty: MaybeSelfTy<'a>,
    pub location: DefinitionLocation,
}

#[derive(Debug, Clone)]
pub struct Bindings<'a> {
    bindings: HashMap<IrClassName<'a>, ClassBindings<'a>>,
}

impl<'a> Bindings<'a> {
    pub fn new(
        arena: ArenaRef<'a>,
        ty_registry: &mut IrTyRegistry<'a>,
        bindings: HashMap<analysis::ClassName<'_>, BindingMap<'_>>,
    ) -> Self {
        let bindings = bindings
            .into_iter()
            .map(|(class_name, binding_map)| {
                let Some(class) = ty_registry.get_class_by_class_name(&class_name) else {
                    panic!("unknown class {}", class_name);
                };

                let mut self_binding = None;
                let class_bindings = binding_map
                    .into_bindings()
                    .inspect(|&(id, ref binding)| {
                        if binding.kind == BindingKind::SelfRef {
                            self_binding = Some(id);
                        }
                    })
                    .map(|(id, binding)| Binding {
                        id,
                        kind: binding.kind,
                        ty: lower_object_ty(arena, ty_registry, class.name, &binding.ty),
                        location: binding.location,
                    })
                    .collect();
                let class_bindings = ClassBindings {
                    bindings: class_bindings,
                    self_binding: self_binding.unwrap(),
                };

                (class.name, class_bindings)
            })
            .collect();

        Self { bindings }
    }

    pub fn get(&self, class_name: IrClassName<'a>, binding_id: BindingId) -> &Binding<'a> {
        &self.bindings[&class_name].bindings[binding_id.idx().get() - 1]
    }

    pub fn get_self_binding(&self, class_name: IrClassName<'a>) -> &Binding<'a> {
        self.get(class_name, self.bindings[&class_name].self_binding)
    }
}
