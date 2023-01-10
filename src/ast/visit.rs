use super::*;

pub trait AstRecurse<'buf> {
    fn recurse<V: Visitor<'buf, Output = ()>>(&self, visitor: &mut V);
    fn recurse_mut<V: VisitorMut<'buf, Output = ()>>(&mut self, visitor: &mut V);
}

macro_rules! define_visitor {
    ($( $type:ident { $( $name:ident ( $arg:ident : $ty:ty ) );+ $(;)? } )+) => {
        pub trait Visitor<'buf>
        where
            Self: Sized,
        {
            type Output;

            $(
                $(
                    fn $name(&mut self, $arg: &$ty) -> Self::Output;
                )+
            )+
        }

        pub trait VisitorMut<'buf>
        where
            Self: Sized,
        {
            type Output;

            $(
                $(
                    fn $name(&mut self, $arg: &mut $ty) -> Self::Output;
                )+
            )+
        }

        pub trait DefaultVisitor<'buf>
        where
            Self: Sized,
        {
            $( define_visitor!(@ $type { $( $name ( $arg : &$ty ) => recurse; )+ } ); )+
        }

        impl<'buf, T> Visitor<'buf> for T
        where
            T: DefaultVisitor<'buf>,
        {
            type Output = ();

            $(
                $(
                    fn $name(&mut self, $arg: &$ty) {
                        <Self as DefaultVisitor<'buf>>::$name(self, $arg);
                    }
                )+
            )+
        }

        pub trait DefaultVisitorMut<'buf>
        where
            Self: Sized,
        {
            $( define_visitor!(@ $type { $( $name ( $arg : &mut $ty ) => recurse_mut; )+ } ); )+
        }

        impl<'buf, T> VisitorMut<'buf> for T
        where
            T: DefaultVisitorMut<'buf>,
        {
            type Output = ();

            $(
                $(
                    fn $name(&mut self, $arg: &mut $ty) {
                        <Self as DefaultVisitorMut<'buf>>::$name(self, $arg);
                    }
                )+
            )+
        }
    };

    (@ NonTerminal { $( $name:ident ( $arg:ident : $ty:ty ) => $recurse:ident; )+ }) => {
        $(
            fn $name(&mut self, $arg: $ty) {
                $arg.$recurse(self);
            }
        )+
    };

    (@ Terminal { $( $name:ident ( $arg:ident : $ty:ty ) => $recurse:ident; )+ }) => {
        $(
            #[allow(unused_variables)]
            fn $name(&mut self, $arg: $ty) {}
        )+
    };
}

define_visitor! {
    NonTerminal {
        // statements
        visit_program(program: Program<'buf>);
        visit_class(class: Class<'buf>);
        visit_feature(feature: Feature<'buf>);
        visit_method(method: Method<'buf>);
        visit_field(field: Field<'buf>);

        // expr
        visit_expr(expr: Expr<'buf>);
        visit_assignment(expr: Assignment<'buf>);
        visit_call(expr: Call<'buf>);
        visit_if(expr: If<'buf>);
        visit_while(expr: While<'buf>);
        visit_block(expr: Block<'buf>);
        visit_let(expr: Let<'buf>);
        visit_case(expr: Case<'buf>);
        visit_new(expr: New<'buf>);
        visit_bin_op(expr: BinOpExpr<'buf>);
        visit_un_op(expr: UnOpExpr<'buf>);
        visit_name_expr(expr: NameExpr<'buf>);

        // neither stmt nor expr
        visit_formal(formal: Formal<'buf>);
        visit_receiver(recv: Receiver<'buf>);
        visit_case_arm(arm: CaseArm<'buf>);
        visit_ty_name(ty_name: TyName<'buf>);
        visit_binding(binding: Binding<'buf>);
    }

    Terminal {
        visit_name(name: Name<'buf>);
        visit_int_lit(expr: IntLit);
        visit_string_lit(expr: StringLit<'buf>);
        visit_bool_lit(expr: BoolLit);
    }
}
