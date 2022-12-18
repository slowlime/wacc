use super::ctx::ty::RegularTy;
use super::ctx::ty::WasmTy;
use super::ctx::ty::WellKnownTyKey;
use super::ctx::ty::WELL_KNOWN_TYS;
use super::ctx::LocalCtx;
use super::ctx::TyKind;
use super::funcs::BuiltinFuncKey;
use super::funcs::BUILTIN_FUNCS;
use super::process_locals;
use super::wat::quote_wat;
use super::Codegen;

impl<'buf> Codegen<'_, 'buf, WasmTy<'buf>> {
    pub(super) fn generate_string_eq(&self) -> wast::core::Func<'static> {
        let func = BUILTIN_FUNCS.get(BuiltinFuncKey::StringEq);
        let func_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();
        let bytes_ty_id = self
            .ty_index
            .get_by_wasm_ty(&RegularTy::ByteArray.into())
            .unwrap();

        let locals = LocalCtx::new();

        let func_to_i32_ty_id = self
            .ty_index
            .get_by_wasm_ty(&WELL_KNOWN_TYS.get(WellKnownTyKey::FuncToI32))
            .unwrap();

        quote_wat! {
            (pre {
                let lhs_local = locals.bind(None, bytes_ty_id);
                let rhs_local = locals.bind(None, bytes_ty_id);

                let idx_local = locals.bind(None, TyKind::I32);
            })
            (span 0)
            (func (type :func_ty_id) (local {process_locals(locals, 2, 0)})
              (local.get :lhs_local) // <lhs: $bytes>
              (array.len)            // <lhs.len: i32>
              (local.get :rhs_local) // <lhs.len: i32> <rhs: $bytes>
              (array.len)            // <lhs.len: i32> <rhs.len: i32>
              (i32.eq)               // <lhs.len == rhs.len: i32>

              (if (type :func_to_i32_ty_id)
                (then // lhs.len == rhs.len // ø
                  (i32.const 0) // <0: i32>
                  (local.set :idx_local) // ø

                  (loop (type :func_to_i32_ty_id)
                    (local.get :idx_local) // <i: i32>
                    (local.get :lhs_local) // <i: i32> <lhs: $bytes>
                    (array.len)            // <i: i32> <lhs.len: i32>
                    (i32.eq)               // <i == lhs.len: i32>

                    (if (type :func_to_i32_ty_id)
                      (then
                        (i32.const 1)
                        (br 2))

                      (else
                        (local.get :lhs_local) // <lhs: $bytes>
                        (local.get :idx_local) // <lhs: $bytes> <i: i32>
                        (array.get_u :bytes_ty_id) // <lhs[i]: i32>
                        (local.get :rhs_local)     // <lhs[i]: i32> <rhs: $bytes>
                        (local.get :idx_local)     // <lhs[i]: i32> <rhs: $bytes> <i: i32>
                        (array.get_u :bytes_ty_id) // <lhs[i]: i32> <rhs[i]: i32>
                        (i32.eq)                   // <lhs[i] == rhs[i]: i32>

                        (if (type :func_to_i32_ty_id)
                          (then
                            (local.get :idx_local) // <i: i32>
                            (i32.const 1)          // <i: i32> <1: i32>
                            (i32.add)              // <i + 1: i32>
                            (local.set :idx_local) // ø
                            (br 2))

                          (else
                            (i32.const 0)
                            (br 3)))))))

                (else // lhs.len != rhs.len
                  (i32.const 0)
                  (br 0))))
        }
    }

    pub(super) fn generate_string_concat(&self) -> wast::core::Func<'static> {
        let func = BUILTIN_FUNCS.get(BuiltinFuncKey::StringConcat);
        let func_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();
        let bytes_ty_id = self
            .ty_index
            .get_by_wasm_ty(&RegularTy::ByteArray.into())
            .unwrap();

        let locals = LocalCtx::new();

        let func_empty_ty_id = self
            .ty_index
            .get_by_wasm_ty(&WELL_KNOWN_TYS.get(WellKnownTyKey::FuncEmpty))
            .unwrap();

        quote_wat! {
            (pre {
                let lhs_local = locals.bind(None, bytes_ty_id);
                let rhs_local = locals.bind(None, bytes_ty_id);

                let idx_local = locals.bind(None, TyKind::I32);
                let result_local = locals.bind(None, bytes_ty_id);
            })
            (span 0)
            (func (type :func_ty_id) (local {process_locals(locals, 2, 0)})
              (local.get :lhs_local) // <lhs: $bytes>
              (array.len)            // <lhs.len: i32>
              (local.get :rhs_local) // <lhs.len: i32> <rhs: $bytes>
              (array.len)            // <lhs.len: i32> <rhs.len: i32>
              (i32.add)              // <len: i32>
              (array.new_canon_default :bytes_ty_id) // <result: $bytes>
              (i32.const 0)          // <result: $bytes> <0: i32>
              (local.set :idx_local) // <result: $bytes>
              (local.set :result_local) // ø

              (block (type :func_empty_ty_id)  // ø
                (loop (type :func_empty_ty_id) // ø
                  (local.get :idx_local) // <i: i32>
                  (local.get :lhs_local) // <i: i32> <lhs: $bytes>
                  (array.len)            // <i: i32> <lhs.len: i32>
                  (i32.eq)               // <i == lhs.len: i32>

                  (if (type :func_empty_ty_id) // ø
                    (then
                      (local.get :result_local) // <result: $bytes>
                      (br 2))

                    (else
                      (local.get :result_local) // <result: $bytes>
                      (local.get :idx_local)    // <result: $bytes> <i: i32>
                      (local.get :lhs_local)    // <result: $bytes> <i: i32> <lhs: $bytes>
                      (local.get :idx_local)    // <result: $bytes> <i: i32> <lhs: $bytes> <i: i32>
                      (array.get_u :bytes_ty_id) // <result: $bytes> <i: i32> <lhs[i]: i32>
                      (array.set :bytes_ty_id)   // ø
                      (local.get :idx_local)     // <i: i32>
                      (i32.const 1)              // <i: i32> <1: i32>
                      (i32.add)                  // <i + 1: i32>
                      (local.set :idx_local)  // ø
                      (br 1)))))

              (block (type :func_empty_ty_id)  // ø
                (loop (type :func_empty_ty_id) // ø
                  (local.get :idx_local) // <i: i32>
                  (local.get :result_local) // <i: i32> <result: $bytes>
                  (array.len)               // <i: i32> <result.len: i32>
                  (i32.eq)                  // <i == result.len: i32>

                  (if (type :func_empty_ty_id) // ø
                    (then
                      (local.get :result_local) // <result: $bytes>
                      (br 2))

                    (else
                      (local.get :result_local) // <result: $bytes>
                      (local.get :idx_local)    // <result: $bytes> <i: i32>
                      (local.get :rhs_local)    // <result: $bytes> <i: i32> <rhs: $bytes>
                      (local.get :idx_local)    // <result: $bytes> <i: i32> <rhs: $bytes> <i: i32>
                      (local.get :lhs_local)    // <result: $bytes> <i: i32> <rhs: $bytes> <i: i32> <lhs: $bytes>
                      (array.len)               // <result: $bytes> <i: i32> <rhs: $bytes> <i: i32> <lhs.len: i32>
                      (i32.sub)                 // <result: $bytes> <i: i32> <rhs: $bytes> <i - lhs.len: i32>
                      (array.get_u :bytes_ty_id) // <result: $bytes> <i: i32> <rhs[i - lhs.len]: i32>
                      (array.set :bytes_ty_id)   // ø
                      (local.get :idx_local)     // <i: i32>
                      (i32.const 1)              // <i: i32> <1: i32>
                      (i32.add)                  // <i + 1: i32>
                      (local.set :idx_local)     // ø
                      (br 1)))))

              (local.get :result_local))
        }
    }

    pub(super) fn generate_string_substr(&self) -> wast::core::Func<'static> {
        let func = BUILTIN_FUNCS.get(BuiltinFuncKey::StringSubstr);
        let func_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();
        let bytes_ty_id = self
            .ty_index
            .get_by_wasm_ty(&RegularTy::ByteArray.into())
            .unwrap();

        let locals = LocalCtx::new();

        let func_to_i32_ty_id = self.ty_index.get_by_wasm_ty(&WELL_KNOWN_TYS.get(WellKnownTyKey::FuncToI32)).unwrap();
        let func_empty_ty_id = self
            .ty_index
            .get_by_wasm_ty(&WELL_KNOWN_TYS.get(WellKnownTyKey::FuncEmpty))
            .unwrap();

        quote_wat! {
            (pre {
                let s_local = locals.bind(None, bytes_ty_id);
                let start_local = locals.bind(None, TyKind::I32);
                let len_local = locals.bind(None, TyKind::I32);

                let idx_local = locals.bind(None, TyKind::I32);
                let result_local = locals.bind(None, bytes_ty_id);
            })
            (span 0)
            (func (type :func_ty_id) (local {process_locals(locals, 3, 0)})
              // we're going to allocate an array of length `len`
              // so make sure it's at least not larger than the input array...
              (local.get :len_local) // <len: i32>
              (local.get :s_local)   // <len: i32> <s: $bytes>
              (array.len)            // <len: i32> <s.len: i32>
              (i32.gt_s)             // <len > s.len: i32>

              (if (type :func_to_i32_ty_id)
                (then
                  (local.get :s_local) // <s: $bytes>
                  (array.len))         // <s.len: i32>

                (else
                  (local.get :len_local))) // <len: i32>

              (array.new_canon_default :bytes_ty_id) // <result: $bytes>
              (local.set :result_local) // ø

              (i32.const 0) // <0: i32>
              (local.set :idx_local) // ø

              (loop (type :func_empty_ty_id)
                (local.get :idx_local) // <i: i32>
                (local.get :len_local) // <i: i32> <len: i32>
                (i32.eq)               // <i == len: i32>

                (if (type :func_empty_ty_id) // i == len
                  (then
                    (local.get :result_local) // <result: $bytes>
                    (return))

                  (else
                    (local.get :result_local) // <result: $bytes>
                    (local.get :idx_local)    // <result: $bytes> <i: i32>
                    (local.get :s_local)      // <result: $bytes> <i: i32> <s: $bytes>
                    (local.get :idx_local)    // <result: $bytes> <i: i32> <s: $bytes> <i: i32>
                    (local.get :start_local)  // <result: $bytes> <i: i32> <s: $bytes> <i: i32> <start: i32>
                    (i32.add)                 // <result: $bytes> <i: i32> <s: $bytes> <start + i: i32>
                    (array.get_u :bytes_ty_id) // <result: $bytes> <i: i32> <s[start + i]: i32>
                    (array.set :bytes_ty_id)   // ø
                    (local.get :idx_local)     // <i: i32>
                    (i32.const 1)              // <i: i32> <1: i32>
                    (i32.add)                  // <i + 1: i32>
                    (local.set :idx_local)     // ø
                    (br 1))))

              // this is technically unreachable
              (local.get :result_local)) // <result: $bytes>
        }
    }

    pub(super) fn generate_bytes_new(&self) -> wast::core::Func<'static> {
        let func = BUILTIN_FUNCS.get(BuiltinFuncKey::BytesNew);
        let func_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();
        let bytes_ty_id = self
            .ty_index
            .get_by_wasm_ty(&RegularTy::ByteArray.into())
            .unwrap();

        let locals = LocalCtx::new();

        quote_wat! {
            (pre {
                let len_local = locals.bind(None, TyKind::I32);
            })
            (span 0)
            (func (type :func_ty_id) (local {process_locals(locals, 1, 0)})
              (local.get :len_local)
              (array.new_canon_default :bytes_ty_id)
              (extern.externalize))
        }
    }

    pub(super) fn generate_bytes_len(&self) -> wast::core::Func<'static> {
        let func = BUILTIN_FUNCS.get(BuiltinFuncKey::BytesLen);
        let func_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();
        let bytes_ty_id = self
            .ty_index
            .get_by_wasm_ty(&RegularTy::ByteArray.into())
            .unwrap();

        let locals = LocalCtx::new();

        quote_wat! {
            (pre {
                let bytes_local = locals.bind(None, TyKind::Extern);
            })
            (span 0)
            (func (type :func_ty_id) (local {process_locals(locals, 1, 0)})
              (local.get :bytes_local)
              (extern.internalize)
              (ref.as_data)
              (ref.cast :bytes_ty_id)
              (array.len))
        }
    }

    pub(super) fn generate_bytes_get(&self) -> wast::core::Func<'static> {
        let func = BUILTIN_FUNCS.get(BuiltinFuncKey::BytesGet);
        let func_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();
        let bytes_ty_id = self
            .ty_index
            .get_by_wasm_ty(&RegularTy::ByteArray.into())
            .unwrap();

        let locals = LocalCtx::new();

        quote_wat! {
            (pre {
                let bytes_local = locals.bind(None, TyKind::Extern);
                let idx_local = locals.bind(None, TyKind::I32);
            })
            (span 0)
            (func (type :func_ty_id) (local {process_locals(locals, 2, 0)})
              (local.get :bytes_local)
              (extern.internalize)
              (ref.as_data)
              (ref.cast :bytes_ty_id)
              (local.get :idx_local)
              (array.get_u :bytes_ty_id))
        }
    }

    pub(super) fn generate_bytes_set(&self) -> wast::core::Func<'static> {
        let func = BUILTIN_FUNCS.get(BuiltinFuncKey::BytesSet);
        let func_ty_id = self.ty_index.get_by_wasm_ty(&func.ty).unwrap();
        let bytes_ty_id = self
            .ty_index
            .get_by_wasm_ty(&RegularTy::ByteArray.into())
            .unwrap();

        let locals = LocalCtx::new();

        quote_wat! {
            (pre {
                let bytes_local = locals.bind(None, bytes_ty_id);
                let idx_local = locals.bind(None, TyKind::I32);
                let value_local = locals.bind(None, TyKind::I32);
            })
            (span 0)
            (func (type :func_ty_id) (local {process_locals(locals, 3, 0)})
              (local.get :bytes_local)
              (extern.internalize)
              (ref.as_data)
              (ref.cast :bytes_ty_id)
              (local.get :idx_local)
              (local.get :value_local)
              (array.set :bytes_ty_id))
        }
    }
}
