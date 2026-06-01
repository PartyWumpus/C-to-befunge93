use std::collections::HashMap;

use crate::{
    ARGS,
    builder::OpBuilder,
    ir::{BinOp, BranchType, CmpOp, FuncInfo, IROp, IRTopLevel, IRType, IRValue, UnaryOp},
};

static PRE_INIT_PRELUDE: &str = r##"
v!R#######    main stack    =>   !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v#########    static memory =>   !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v#########    dynamic alloc =>   !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v#########    call stack    =>   !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~ ...etc
v#########    filename
v#########    compiled by: c-to-befunge
v#########  below are the bit stacks, for bitshifts and bitwise operations
v#########  the last 64 bits are just zeros for cheaper bitshifts
v bit stack A
v bit stack B
>"!"00p 010g3-3pv
v               <
"##;

static SETUP_BIT_STACKS: &str = r#"
>0>:0\9p:0\8p1+v
  ^_v#-+"@A":  <
v $ <
"#;

static POST_INIT_PRELUDE: &str = "
0
1
>v
 >:#v_  $$ 55+ , 20g . @
 v  <
";

pub struct CodeGen {
    builder: OpBuilder,
    function_map: HashMap<String, FuncInfo>,
}

impl CodeGen {
    pub fn compile_program(
        program: Vec<IRTopLevel>,
        function_map: HashMap<String, FuncInfo>,
    ) -> Vec<String> {
        let mut cg = Self {
            builder: OpBuilder::new(false),
            function_map,
        };

        let mut out: Vec<String> = vec![];
        out.extend(PRE_INIT_PRELUDE.lines().skip(1).map(ToOwned::to_owned));
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
        // consider being smart here
        if !ARGS.disable_bitwise_ops {
            out.extend(SETUP_BIT_STACKS.lines().map(ToOwned::to_owned));
        }
        out.extend(POST_INIT_PRELUDE.lines().map(ToOwned::to_owned));
        let func_finder_pos = (3, out.len() - 2);
        out.extend(funcs);

        // out[4] == "filename"
        if ARGS.filenames.len() == 1 {
            out[4] += ": ";
            out[4] += &ARGS.filenames[0];
        } else {
            out[4] += "s: ";
            out[4] += &ARGS.filenames.join(", ");
        }

        // Stick preproccesor info at the bottom
        if ARGS.preprocessor_info {
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
                IROp::Call(called_func_name, vals) => {
                    assert!(!func.is_initializer, "Non static call in static context");
                    let Some(called_func) = self.function_map.get(called_func_name) else {
                        unreachable!(
                            "Function '{called_func_name}' not found LATE. should be caught in linker"
                        );
                    };

                    self.builder.call(
                        self.function_map[&func.name],
                        *called_func,
                        &vals
                            .iter()
                            .map(|(a, b)| (a.clone(), b.get()))
                            .collect::<Vec<_>>(),
                    );
                }
                IROp::GetIdOfFunction(called_func_name, out) => {
                    let Some(called_func) = self.function_map.get(called_func_name) else {
                        unreachable!(
                            "Function '{called_func_name}' not found LATE. should be caught in linker"
                        );
                    };

                    self.builder.copy(&IRValue::int(called_func.id), out, 1);
                }
                IROp::Return(val, size) => {
                    self.builder.return_(val, size.get());
                }
                IROp::GetReturnValue(out, size) => {
                    self.builder.load_return_val(out, size.get());
                }
                IROp::Cast(irtype, (val, original_irtype), output) => {
                    match (original_irtype, irtype) {
                        // TODO: this is wrong for -0.0, it should also become 0
                        // bool happens no matter what
                        (_, IRType::Signed(1) | IRType::Unsigned(1)) => {
                            self.builder.constrain_to_range(val, *irtype, false);
                            self.builder.copy(&IRValue::BefungeStack, output, 1);
                        }
                        (IRType::Double, IRType::Signed(..) | IRType::Unsigned(..)) => {
                            if let IRValue::Immediate(val) = val {
                                self.builder.copy(
                                    &IRValue::int(f64::from_bits(*val as u64) as usize),
                                    output,
                                    1,
                                );
                            } else {
                                self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_f64_to_i64"],
                                    &[(val.clone(), 1)],
                                );
                                self.builder.load_return_val(output, 1);
                            }
                        }
                        (IRType::Signed(..), IRType::Double) => {
                            if let IRValue::Immediate(val) = val {
                                self.builder
                                    .copy(&IRValue::float(*val as isize as f64), output, 1);
                            } else {
                                self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_i64_to_f64"],
                                    &[(val.clone(), 1)],
                                );
                                self.builder.load_return_val(output, 1);
                            }
                        }
                        (IRType::Unsigned(..), IRType::Double) => {
                            if let IRValue::Immediate(val) = val {
                                self.builder.copy(&IRValue::float(*val as f64), output, 1);
                            } else {
                                self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_ui64_to_f64"],
                                    &[(val.clone(), 1)],
                                );
                                self.builder.load_return_val(output, 1);
                            }
                        }
                        _ => {
                            self.builder.constrain_to_range(val, *irtype, false);
                            self.builder.copy(&IRValue::BefungeStack, output, 1);
                        }
                    }
                }
                IROp::Label(label) => self.builder.label(label.to_owned()),
                IROp::InlineBefunge(lines) => self.builder.insert_inline_befunge(lines),
                IROp::CondBranch(flavour, label, val) => match flavour {
                    BranchType::Zero => self.builder.zero_branch(val, label.to_owned()),
                    BranchType::NonZero => self.builder.not_zero_branch(val, label.to_owned()),
                },
                IROp::AlwaysBranch(label) => self.builder.unconditional_branch(label.to_owned()),
                IROp::AddressOf(a, out) => {
                    self.builder.address_of(a);
                    self.builder.copy(&IRValue::BefungeStack, out, 1);
                }
                IROp::Dereference(a, out, size) => {
                    self.builder.dereference(a, out, size.get());
                }
                IROp::Copy(a, out, size) => {
                    self.builder.copy(a, out, size.get());
                }
                IROp::Store(a, out, size) => {
                    self.builder.store(a, out, size.get());
                }
                IROp::One(op, a, out, irtype) => {
                    match op {
                        UnaryOp::Minus => {
                            if matches!(irtype, IRType::Double) {
                                self.builder.add(a, &IRValue::float(-0.0));
                                self.builder.copy(&IRValue::BefungeStack, out, 1);
                                continue;
                            }

                            self.builder.unary_minus(a);
                        }
                        UnaryOp::Complement => self.builder.bitwise_complement(a),
                        UnaryOp::BooleanNegate => self.builder.boolean_negate(a),
                    }
                    if !matches!(irtype, IRType::Double) {
                        self.builder
                            .constrain_to_range(&IRValue::BefungeStack, *irtype, false);
                    }
                    self.builder.copy(&IRValue::BefungeStack, out, 1);
                }
                IROp::Cmp(op, a, b, out, irtype) => match irtype {
                    IRType::Signed(..) | IRType::Unsigned(..) => {
                        match op {
                            CmpOp::Equal => self.builder.is_equal(a, b),
                            CmpOp::NotEqual => self.builder.is_not_equal(a, b),
                            CmpOp::LessThan => self.builder.is_less_than(a, b, *irtype),
                            CmpOp::LessOrEqual => self.builder.is_less_or_equal(a, b, *irtype),
                            CmpOp::GreaterThan => self.builder.is_greater_than(a, b, *irtype),
                            CmpOp::GreaterOrEqual => {
                                self.builder.is_greater_or_equal(a, b, *irtype);
                            }
                        }
                        self.builder.copy(&IRValue::BefungeStack, out, 1);
                    }
                    IRType::Double => {
                        if ARGS.enable_softfloat {
                            match op {
                                CmpOp::Equal => self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_double_is_equal"],
                                    &[(a.clone(), 1), (b.clone(), 1)],
                                ),
                                CmpOp::NotEqual => self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_double_is_not_equal"],
                                    &[(a.clone(), 1), (b.clone(), 1)],
                                ),
                                CmpOp::LessThan => self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_double_is_less_than"],
                                    &[(a.clone(), 1), (b.clone(), 1)],
                                ),
                                CmpOp::LessOrEqual => self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_double_is_less_or_equal"],
                                    &[(a.clone(), 1), (b.clone(), 1)],
                                ),
                                CmpOp::GreaterThan => self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_double_is_greater_than"],
                                    &[(a.clone(), 1), (b.clone(), 1)],
                                ),
                                CmpOp::GreaterOrEqual => self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_double_is_greater_or_equal"],
                                    &[(a.clone(), 1), (b.clone(), 1)],
                                ),
                            }

                            self.builder.load_return_val(out, 1);
                        } else {
                            // often good enough
                            match op {
                                CmpOp::Equal => {
                                    self.builder.is_equal(a, b);
                                }
                                CmpOp::NotEqual => {
                                    self.builder.is_not_equal(a, b);
                                }
                                _ => panic!(
                                    "Softfloat is not enabled, but a floating point operation was used"
                                ),
                            }

                            self.builder.copy(&IRValue::BefungeStack, out, 1);
                        }
                    }
                },
                IROp::Two(op, a, b, out, irtype) => match irtype {
                    IRType::Signed(..) | IRType::Unsigned(..) => {
                        match op {
                            BinOp::Add => self.builder.add(a, b),
                            BinOp::Sub => self.builder.sub(a, b),
                            BinOp::Mult => self.builder.multiply(a, b),
                            BinOp::Div => self.builder.divide(a, b),
                            BinOp::Mod => self.builder.modulo(a, b),

                            BinOp::BitwiseAnd => self.builder.bit_and(a, b),
                            BinOp::BitwiseOr => self.builder.bit_or(a, b),
                            BinOp::BitwiseXor => self.builder.bit_xor(a, b),

                            BinOp::ShiftLeft => {
                                assert!(
                                    !func.is_initializer,
                                    "Bitshfits can't yet be used in init"
                                );
                                self.builder.call(
                                    self.function_map[&func.name],
                                    self.function_map["_bf_bitshift_left"],
                                    &[(a.clone(), 1), (b.clone(), 1)],
                                );
                                self.builder.load_return_val(out, 1);
                                continue;
                            }
                            BinOp::ShiftRight => {
                                assert!(
                                    !func.is_initializer,
                                    "Bitshfits can't yet be used in init"
                                );
                                if matches!(irtype, IRType::Unsigned(64)) {
                                    self.builder.call(
                                        self.function_map[&func.name],
                                        self.function_map["_bf_unsigned_long_bitshift_right"],
                                        &[(a.clone(), 1), (b.clone(), 1)],
                                    );
                                } else {
                                    self.builder.call(
                                        self.function_map[&func.name],
                                        self.function_map["_bf_signed_bitshift_right"],
                                        &[(a.clone(), 1), (b.clone(), 1)],
                                    );
                                }
                                self.builder.load_return_val(out, 1);
                                continue;
                            }
                        }
                        self.builder
                            .constrain_to_range(&IRValue::BefungeStack, *irtype, false);
                        self.builder.copy(&IRValue::BefungeStack, out, 1);
                    }
                    IRType::Double => {
                        assert!(!func.is_initializer, "Floats can't yet be used in init");
                        assert!(
                            ARGS.enable_softfloat,
                            "Softfloat is not enabled, but a floating point operation was used"
                        );
                        match op {
                            BinOp::Add => self.builder.call(
                                self.function_map[&func.name],
                                self.function_map["_bf_double_add"],
                                &[(a.clone(), 1), (b.clone(), 1)],
                            ),
                            BinOp::Sub => self.builder.call(
                                self.function_map[&func.name],
                                self.function_map["_bf_double_sub"],
                                &[(a.clone(), 1), (b.clone(), 1)],
                            ),
                            BinOp::Mult => self.builder.call(
                                self.function_map[&func.name],
                                self.function_map["_bf_double_multiply"],
                                &[(a.clone(), 1), (b.clone(), 1)],
                            ),
                            BinOp::Div => self.builder.call(
                                self.function_map[&func.name],
                                self.function_map["_bf_double_divide"],
                                &[(a.clone(), 1), (b.clone(), 1)],
                            ),
                            BinOp::Mod => self.builder.call(
                                self.function_map[&func.name],
                                self.function_map["_bf_double_modulo"],
                                &[(a.clone(), 1), (b.clone(), 1)],
                            ),

                            BinOp::BitwiseAnd => panic!("cannot bit_and floats"),
                            BinOp::BitwiseOr => panic!("cannot bit_or floats"),
                            BinOp::BitwiseXor => panic!("cannot bit_xor floats"),

                            BinOp::ShiftLeft => panic!("cannot bitshift_left floats"),
                            BinOp::ShiftRight => panic!("cannot bitshift_right floats"),
                        }

                        self.builder.load_return_val(out, 1);
                    }
                },
                IROp::CopyWithOffset(
                    (source, source_offset),
                    (destination, destination_offset),
                ) => {
                    self.builder.copy_with_offset(
                        (source, *source_offset),
                        (destination, *destination_offset),
                    );
                }
                IROp::AddPtr(ptr, b, out, size) => {
                    self.builder.add_ptr(ptr, b, size.get());
                    self.builder.copy(&IRValue::BefungeStack, out, 1);
                }
            }
            self.builder.add_space();
        }

        self.builder.finalize_function(&func.name)
    }
}
