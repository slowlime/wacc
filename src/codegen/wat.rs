/// A horrible macro crime, or a macro-based parser of WAT-like syntax.
///
/// Choose whichever you prefer.
macro_rules! quote_wat {
    (
        $((pre {$($pre:tt)+}))?
        (span $pos:expr)
        (func (type :$type:ident) (local {$($local:tt)+}) $($sexpr:tt)*)
    ) => ({
        #[allow(unused_imports)]
        use wast::core::*;
        #[allow(unused_imports)]
        use wast::token::Index as WasmIndex;
        #[allow(unused_imports)]
        use wast::token::Span as WasmSpan;

        quote_wat!([$pos] func/body $((pre {$($pre)+}))? (type :$type) (local {$($local)+}) @ $($sexpr)*)
    });

    ([$pos:expr] type_use @ :$type:ident) => (TypeUse {
        index: Some(quote_wat!([$pos] ty_id @ :$type).to_wasm_index($pos)),
        inline: None,
    });

    ([$pos:expr] ty_id @ :$binding:ident) => ($binding);

    ([$pos:expr] local @ $ty:tt :$binding:ident) => ((Local {
        id: None,
        name: None,
        ty: quote_wat!([$pos] val_ty @ $ty),
    }, $binding));

    ([$pos:expr] heap_type @ extern) => (HeapType::Extern);
    ([$pos:expr] heap_type @ :$binding:ident) => ($binding.to_heap_type($pos));

    ([$pos:expr] cast_arg @ :$binding:ident) => (CastArg {
        nullable: false,
        heap: $binding.to_heap_type($pos),
    });
    ([$pos:expr] cast_arg @ null :$binding:ident) => (CastArg {
        nullable: true,
        heap: $binding.to_heap_type($pos),
    });

    ([$pos:expr] val_ty @ i32) => (ValType::I32);
    ([$pos:expr] val_ty @ (ref $($null:ident)? $($heap_ty:tt)+)) => (ValType::Ref(RefType {
        nullable: quote_wat!(null @ $($null)?),
        heap: quote_wat!([$pos] heap_type @ $($heap_ty)+),
    }));

    (null @ null) => (true);
    (null @) => (false);

    ([$pos:expr] block_type @ :$binding:ident) => (BlockType {
        label: None,
        label_name: None,
        ty: quote_wat!([$pos] type_use @ :$binding),
    });

    ([$pos:expr] type_use @ :$ty_id:ident) => (TypeUse {
        index: Some($ty_id.to_wasm_index($pos)),
        inline: None,
    });

    ([$pos:expr] func/body $((pre {$($pre:tt)+}))? (type :$type:ident) (local {$($local:tt)+}) @ $($instr:tt)*) => ({
        #[allow(unused_mut)]
        let mut instrs = vec![];
        {
            $($($pre)+)?
            $(
                quote_wat!([$pos] instr instrs @ $instr);
            )*
        }

        let locals = ($($local)+);

        Func {
            span: WasmSpan::from_offset($pos),
            id: None,
            name: None,
            exports: InlineExport { names: vec![] },
            kind: FuncKind::Inline {
                locals,
                expression: Expression {
                    instrs: instrs.into(),
                },
            },
            ty: quote_wat!([$pos] type_use @ :$type),
        }
    });

    ([$pos:expr] instr $instrs:ident @ (local.get :$id:ident)) => ($instrs.push($id.wasm_get($pos)));
    ([$pos:expr] instr $instrs:ident @ (local.set :$id:ident)) => ($instrs.push($id.wasm_set($pos)));
    ([$pos:expr] instr $instrs:ident @ (local.tee :$id:ident)) => ($instrs.push($id.wasm_tee($pos)));
    ([$pos:expr] instr $instrs:ident @ (array.len)) => ($instrs.push(Instruction::ArrayLen));
    ([$pos:expr] instr $instrs:ident @ (i32.add)) => ($instrs.push(Instruction::I32Add));
    ([$pos:expr] instr $instrs:ident @ (i32.sub)) => ($instrs.push(Instruction::I32Sub));
    ([$pos:expr] instr $instrs:ident @ (i32.const $value:expr)) => ($instrs.push(Instruction::I32Const($value)));
    ([$pos:expr] instr $instrs:ident @ (i32.eq)) => ($instrs.push(Instruction::I32Eq));
    ([$pos:expr] instr $instrs:ident @ (i32.gt_s)) => ($instrs.push(Instruction::I32GtS));
    ([$pos:expr] instr $instrs:ident @ (array.new_canon_default :$ty_id:ident)) => ($instrs.push(Instruction::ArrayNewDefault(
        $ty_id.to_wasm_index($pos),
    )));
    ([$pos:expr] instr $instrs:ident @ (ref.cast $($args:tt)+)) => ($instrs.push(
        Instruction::RefCast(quote_wat!([$pos] cast_arg @ $($args)+))
    ));
    ([$pos:expr] instr $instrs:ident @ (extern.externalize)) => ($instrs.push(Instruction::ExternExternalize));
    ([$pos:expr] instr $instrs:ident @ (extern.internalize)) => ($instrs.push(Instruction::ExternInternalize));

    ([$pos:expr] instr $instrs:ident @ (br $idx:literal)) => ($instrs.push(Instruction::Br(WasmIndex::Num(
        $idx, WasmSpan::from_offset($pos)
    ))));

    ([$pos:expr] instr $instrs:ident @ (br_if $idx:literal)) => ($instrs.push(Instruction::BrIf(WasmIndex::Num(
        $idx, WasmSpan::from_offset($pos)
    ))));

    ([$pos:expr] instr $instrs:ident @ (return)) => ($instrs.push(Instruction::Return));

    ([$pos:expr] instr $instrs:ident @ (block (type :$id:ident) $($sexpr:tt)*)) => ({
        $instrs.push(Instruction::Block(quote_wat!([$pos] block_type @ :$id)));

        $(
            quote_wat!([$pos] instr $instrs @ $sexpr);
        )*

        $instrs.push(Instruction::End(None));
    });

    ([$pos:expr] instr $instrs:ident @ (loop (type :$id:ident) $($sexpr:tt)*)) => ({
        $instrs.push(Instruction::Loop(quote_wat!([$pos] block_type @ :$id)));

        $(
            quote_wat!([$pos] instr $instrs @ $sexpr);
        )*

        $instrs.push(Instruction::End(None));
    });

    ([$pos:expr] instr $instrs:ident @ (if (type :$id:ident) $then:tt $($else:tt)?)) => ({
        $instrs.push(Instruction::If(quote_wat!([$pos] block_type @ :$id)));
        quote_wat!([$pos] instr/if $instrs @ $then);

        $(
            $instrs.push(Instruction::Else(None));
            quote_wat!([$pos] instr/if $instrs @ $else);
        )?

        $instrs.push(Instruction::End(None));
    });

    ([$pos:expr] instr/if $instrs:ident @ (then $($sexpr:tt)*)) => ({$(
        quote_wat!([$pos] instr $instrs @ $sexpr);
    )*});

    ([$pos:expr] instr/if $instrs:ident @ (else $($sexpr:tt)*)) => ({$(
        quote_wat!([$pos] instr $instrs @ $sexpr);
    )*});

    ([$pos:expr] instr $instrs:ident @ (array.get :$id:ident)) => ($instrs.push(Instruction::ArrayGet(
        $id.to_wasm_index($pos)
    )));

    ([$pos:expr] instr $instrs:ident @ (array.get_u :$id:ident)) => ($instrs.push(Instruction::ArrayGetU(
        $id.to_wasm_index($pos)
    )));

    ([$pos:expr] instr $instrs:ident @ (array.set :$id:ident)) => ($instrs.push(Instruction::ArraySet(
        $id.to_wasm_index($pos)
    )));
}

pub(crate) use quote_wat;
