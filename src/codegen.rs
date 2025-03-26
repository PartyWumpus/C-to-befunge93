use std::collections::HashMap;

use crate::{
    builder::OpBuilder,
    ir::{BinOp, BranchType, FuncInfo, IROp, IRTopLevel, IRType, IRValue, UnaryOp},
    Args,
};

static PRE_INIT_PRELUDE: &str = r##"v!R#######
v#########    main stack ->      !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v#########    call stack ->      !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v######### static memory ->      !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v#########        // IN FUTURE, malloc() mem could be a 4th mem location here
v#########
v#########  below are the bit stacks, for bitshifts and bitwise operations
v#########  the last 64 bits are just zeros for cheaper bitshifts
v bit stack A
v bit stack B
>"!"00p 010g2-2pv
v               <
>0>:0\9p:0\8p1+v
  ^_v#-+"@A":  <
v $ <

"##;

static POST_INIT_PRELUDE: &str = "0
1
>v
 >:#v_  $$ 55+ , 20g . @
 v  <";

pub struct CodeGen {
    builder: OpBuilder,
    function_map: HashMap<String, FuncInfo>,
}

impl CodeGen {
    pub fn compile_program(
        program: Vec<IRTopLevel>,
        function_map: HashMap<String, FuncInfo>,
        args: &Args,
    ) -> Vec<String> {
        let mut cg = Self {
            builder: OpBuilder::new(false),
            function_map,
        };

        let mut out: Vec<String> = vec![];
        out.extend(PRE_INIT_PRELUDE.lines().map(ToOwned::to_owned));
        let mut funcs: Vec<String> = vec![];
        let mut inits: Vec<String> = vec![];
        for func in program {
            let init = func.is_initializer;
            let x = cg.compile_top_level(func);
            if init {
                inits.extend(x);
            } else {
                funcs.extend(x);
            }
        }
        out.extend(inits);
        out.extend(POST_INIT_PRELUDE.lines().map(ToOwned::to_owned));
        let func_finder_pos = (3, out.len() - 2);
        out.extend(funcs);

        // Sneaky stick filename at top
        out[0] += &(" file: ".to_owned() + &args.filename);

        // Stick preproccesor info at the bottom
        if args.preprocessor_info {
            out.extend(vec![
                "#$watch[0,0]:int = stack".to_owned(),
                "#$watch[1,0]:int = call stack".to_owned(),
                "#$watch[2,0]:int = return".to_owned(),
                format!("#$break[{},{}]", func_finder_pos.0, func_finder_pos.1),
            ]);
        }
        out
    }

    fn compile_top_level(&mut self, func: IRTopLevel) -> Vec<String> {
        self.builder = OpBuilder::new(!func.is_initializer);
        for op in func.ops {
            match &op {
                IROp::FunctionLabel(_) => (),
                IROp::Call(called_func_name, vals) => {
                    assert!(!func.is_initializer, "Non static call in static context");
                    // TODO: improve error on unknown func call
                    let Some(called_func) = self.function_map.get(called_func_name) else {
                        panic!("Function '{called_func_name}' not found");
                    };

                    self.builder
                        .call(self.function_map[&func.name], *called_func, vals);
                }
                IROp::Return(val) => {
                    self.builder.return_(val, func.stack_frame_size);
                }
                IROp::Cast(_ctype, _val, _output) => {
                    // FIXME: Casts are currently no op, through the power of being incorrect
                }
                IROp::Label(label) => self.builder.label(label.to_owned()),
                IROp::InlineBefunge(lines) => self.builder.insert_inline_befunge(lines),
                IROp::CondBranch(flavour, label, val) => match flavour {
                    BranchType::Zero => self.builder.zero_branch(val, label.to_string()),
                    BranchType::NonZero => self.builder.not_zero_branch(val, label.to_string()),
                },
                IROp::AlwaysBranch(label) => self.builder.unconditional_branch(label.to_owned()),
                IROp::AddressOf(a, out) => {
                    self.builder.address_of(a);
                    //self.builder.constrain_to_range(&IRValue::BefungeStack, IRType::Signed(64));
                    self.builder.copy(&IRValue::BefungeStack, out);
                }
                IROp::One(op, a, out, irtype) => {
                    match op {
                        UnaryOp::Copy => self.builder.copy(a, &IRValue::BefungeStack),
                        UnaryOp::Minus => self.builder.unary_minus(a),
                        UnaryOp::Complement => self.builder.bitwise_complement(a),
                        UnaryOp::BooleanNegate => self.builder.boolean_negate(a),
                        UnaryOp::Dereference => self.builder.dereference(a),
                    }
                    self.builder
                        .constrain_to_range(&IRValue::BefungeStack, *irtype);
                    self.builder.copy(&IRValue::BefungeStack, out);
                }
                IROp::Two(op, a, b, out, irtype) => {
                    match op {
                        BinOp::Add => self.builder.add(a, b),
                        BinOp::Sub => self.builder.sub(a, b),
                        BinOp::Mult => self.builder.multiply(a, b),
                        BinOp::Div => self.builder.divide(a, b),
                        BinOp::Mod => self.builder.modulo(a, b),
                        BinOp::Equal => self.builder.is_equal(a, b),
                        BinOp::NotEqual => self.builder.is_not_equal(a, b),
                        BinOp::LessThan => self.builder.is_less_than(a, b),
                        BinOp::LessOrEqual => self.builder.is_less_or_equal(a, b),
                        BinOp::GreaterThan => self.builder.is_greater_than(a, b),
                        BinOp::GreaterOrEqual => self.builder.is_greater_or_equal(a, b),

                        BinOp::BitwiseAnd => self.builder.bit_and(a, b),
                        BinOp::BitwiseOr => self.builder.bit_or(a, b),
                        BinOp::BitwiseXor => self.builder.bit_xor(a, b),

                        BinOp::ShiftLeft => {
                            self.builder.bitshift_left(a, b);
                        }
                        BinOp::ShiftRight => {
                            self.builder.bitshift_right(a, b);
                        }
                    }
                    self.builder
                        .constrain_to_range(&IRValue::BefungeStack, *irtype);
                    self.builder.copy(&IRValue::BefungeStack, out);
                }
            }
            self.builder.add_space();
        }

        self.builder.finalize_function()
    }
}
