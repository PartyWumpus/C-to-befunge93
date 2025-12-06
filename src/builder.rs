#![allow(
    clippy::needless_pass_by_ref_mut,
    clippy::needless_raw_string_hashes,
    clippy::needless_raw_strings
)]
use std::collections::HashMap;

use crate::{
    ARGS,
    ir::{FuncInfo, IRType, IRValue},
    number_generation::int_to_befunge_str,
};

pub enum BuilderErrorType {
    InvalidRegister,
    NonEmptyBstack,
    BefungeStackUsedWhenEmpty,
    ZeroSizedCopy,
    PsuedoInFinalIR,
    AttemptToModifyConstant,
}

#[derive(Debug)]
pub struct OpBuilder {
    ops: Vec<char>,
    // Extra lines that go below the ops, and above any branches
    extra_ops: Vec<Vec<char>>,
    is_function: bool,
    branch_labels: HashMap<String, usize>,
    branch_points: HashMap<String, Vec<usize>>,
    exit_points: Vec<usize>,
    return_points: Vec<usize>,
    pub current_stack_size: usize,
}

impl OpBuilder {
    pub fn new(is_function: bool) -> Self {
        let start = if is_function { " v   _$ >:#^_$" } else { ">" }
            .chars()
            .collect::<Vec<_>>();

        Self {
            ops: start,
            is_function,
            extra_ops: vec![],
            branch_labels: HashMap::new(),
            branch_points: HashMap::new(),
            exit_points: Vec::new(),
            return_points: Vec::new(),
            current_stack_size: 0,
        }
    }

    fn str(&mut self, str: &str) {
        self.ops.extend(str.chars());
    }

    fn char(&mut self, char: char) {
        self.ops.push(char);
    }

    pub fn add_space(&mut self) {
        self.char(' ');
    }

    /// Puts stack ptr on bstack
    fn load_stack_ptr(&mut self) {
        self.str("00g");
        self.current_stack_size += 1;
    }

    /// Set stack ptr to top of bstack
    fn set_stack_ptr(&mut self) {
        self.str("00p");
        self.current_stack_size += 1;
    }

    /// Puts stack ptr on bstack
    fn load_call_stack_ptr(&mut self) {
        self.str("10g");
        self.current_stack_size += 1;
    }

    /// Set stack ptr to top of bstack
    fn set_call_stack_ptr(&mut self) {
        self.str("10p");
        self.current_stack_size += 1;
    }

    /// Puts return value on bstack
    pub fn load_return_val(&mut self, size: usize) {
        assert!(size == 1);
        self.str("20g");
        self.current_stack_size += 1;
    }

    fn set_return_val(&mut self, loc: &IRValue, size: usize) {
        assert!(size == 1);
        self.load_val(loc);
        self.str("20p");
        self.current_stack_size -= 1;
    }

    /// Puts num on bstack
    pub fn load_number(&mut self, num: usize) {
        let x = int_to_befunge_str(num as u64, ARGS.optimization_level > 1);
        self.str(&x);
        self.current_stack_size += 1;
    }

    fn index_register(&mut self, id: usize) {
        assert!(id <= 99, "attempt to index register > 99");
        self.str(&format!("{}{}", id / 10, id % 10));
        self.current_stack_size += 2;
    }

    fn load_register_val(&mut self, id: usize) {
        self.index_register(id);
        self.char('g');
        self.current_stack_size -= 1;
    }

    fn set_register_val(&mut self, id: usize) {
        self.index_register(id);
        self.char('p');
        self.current_stack_size -= 3;
    }

    fn load_stack_val(&mut self, offset: usize) {
        self.load_stack_ptr();
        self.load_number(offset);
        self.str("+0g");
        self.current_stack_size -= 1;
    }

    fn set_stack_val(&mut self, offset: usize) {
        self.load_stack_ptr();
        self.load_number(offset);
        self.str("+0p");
        self.current_stack_size -= 3;
    }

    /// Puts data value on bstack
    fn load_data_val(&mut self, position: usize) {
        self.load_number(position + 31); // + 31 to avoid the register space
        self.str("1g");
        self.current_stack_size += 0;
    }

    /// Set data value to top of bstack
    fn set_data_val(&mut self, position: usize) {
        self.load_number(position + 31); // + 31 to avoid the register space
        self.str("1p");
        self.current_stack_size -= 2;
    }

    pub fn label(&mut self, label: String) {
        self.char('>');
        self.branch_labels.insert(label, self.ops.len() - 1);
    }

    pub fn unconditional_branch(&mut self, label: String) {
        self.char('v');
        self.branch_points
            .entry(label)
            .or_default()
            .push(self.ops.len() - 1);
        assert!(self.current_stack_size == 0);
    }

    pub fn not_zero_branch(&mut self, val: &IRValue, label: String) {
        self.load_val(val);
        self.str("#v_");
        self.branch_points
            .entry(label)
            .or_default()
            .push(self.ops.len() - 2);
        self.current_stack_size -= 1;
        assert!(self.current_stack_size == 0);
    }

    pub fn zero_branch(&mut self, val: &IRValue, label: String) {
        self.load_val(val);
        self.char('!');
        self.not_zero_branch(&IRValue::BefungeStack, label);
    }

    //// Function Calls

    pub fn increment_stack_ptr(&mut self, amount: usize) {
        self.load_stack_ptr();
        self.load_number(amount);
        self.char('+');
        self.set_stack_ptr();
    }

    pub fn decrement_stack_ptr(&mut self, amount: usize) {
        self.load_stack_ptr();
        self.load_number(amount);
        self.char('-');
        self.set_stack_ptr();
    }

    fn exit(&mut self) {
        self.char('^');
        self.exit_points.push(self.ops.len() - 1);
        self.current_stack_size = 0;
    }

    pub fn return_(&mut self, val: &IRValue, size: usize) {
        self.set_return_val(val, size);
        self.load_call_stack_ptr();

        self.str(r"1-:3g");
        self.set_stack_ptr();

        self.str(r"1-:3g\1-:3g\");
        self.set_call_stack_ptr();

        self.exit();
    }

    fn call_exit(&mut self) {
        self.str("^>");
        self.exit_points.push(self.ops.len() - 2);
        self.return_points.push(self.ops.len() - 1);
        self.current_stack_size = 0;
    }

    pub fn call(&mut self, caller: FuncInfo, calle: FuncInfo, params: &[(IRValue, usize)]) {
        self.load_stack_ptr();
        self.set_register_val(22);

        for (val, size) in params {
            if matches!(val, IRValue::BefungeStack) {
                panic!("function param cannot be assumed to be on the bstack")
            }
            if *size != 1 {
                todo!("Structs cannot yet be used as function args")
            }
            self.load_val(val);
        }

        self.increment_stack_ptr(caller.stack_frame_size);
        for i in (0..params.len()).rev() {
            self.put_val(&IRValue::Stack(i + 1));
        }

        self.load_number(0);
        self.load_number(calle.id);

        // Load my ID onto the call stack
        // TODO: optimize with swap op
        self.char(' ');
        self.load_number(caller.id);
        self.load_call_stack_ptr();
        self.str("3p");

        // Load current location in function onto the call stack
        self.load_number(self.return_points.len() + 1);
        self.load_call_stack_ptr();
        self.str("1+3p");

        // Put saved stack ptr value onto the call stack
        self.load_register_val(22);
        self.load_call_stack_ptr();
        self.str("2+3p");

        // Increment call stack ptr
        self.load_call_stack_ptr();
        self.str("3+");
        self.set_call_stack_ptr();

        self.call_exit();
    }

    // Bitwise info:
    // there are free zeros to the left of the fungespace
    // actual values from 0 (0) to 63 (?)
    // then zeros from 64 (@) to 128

    // Puts sign bit and abs(a) on the bstack
    fn absolute_value_and_sign_bit(&mut self, a: &IRValue) {
        self.load_val(a);

        self.insert_inline_befunge(&[
            r#"v    >>\ "#.to_owned(),
            r#">:0`:|^0<"#.to_owned(),
            r#"     >\-^"#.to_owned(),
        ]);

        self.current_stack_size += 1;
    }

    // Leaves sign bit on the stack!
    fn load_bit_stack(&mut self, a: &IRValue, first: bool) {
        self.load_val(a);

        self.absolute_value_and_sign_bit(&IRValue::BefungeStack);

        // TODO: clobbers 97p and 87p, as well as writing to the special bit stacks
        if first {
            self.insert_inline_befunge(&[
                r#":97p01-\>2%87p1+:87g\9p97gv>"#.to_owned(),
                r#"        ^p79:\_v#-"?":\/2 < "#.to_owned(),
                r#"               > $$        ^"#.to_owned(),
            ]);
        } else {
            self.insert_inline_befunge(&[
                r#":97p01-\>2%87p1+:87g\8p97gv>"#.to_owned(),
                r#"        ^p79:\_v#-"?":\/2 < "#.to_owned(),
                r#"               > $$        ^"#.to_owned(),
            ]);
        }
        self.current_stack_size -= 1;
    }

    pub fn bit_and(&mut self, a: &IRValue, b: &IRValue) {
        self.load_bit_stack(a, true);
        self.load_bit_stack(b, false);
        // Calculate sign for later: (a*b)*2-1
        self.str(r"*2*1-");
        self.current_stack_size -= 1;

        // For each bit, do a * b
        self.insert_inline_befunge(&[
            r#"097p"?">::9g\8g*97g2v>"#.to_owned(),
            r#"       ^-1_v#:p79+* < "#.to_owned(),
            r#"           > $  97g  ^"#.to_owned(),
        ]);
        self.current_stack_size += 1;

        // Times by the sign from earlier
        self.char('*');
        self.current_stack_size -= 1;
    }

    pub fn bit_xor(&mut self, a: &IRValue, b: &IRValue) {
        self.load_bit_stack(a, true);
        self.load_bit_stack(b, false);
        // Calculate sign for later: (a+b mod 2) * -2 + 1
        self.str("+2%02-*1+");
        self.current_stack_size -= 1;

        // For each bit, do (a + b) mod 2
        self.insert_inline_befunge(&[
            r#"097p"?">::9g\8g+2%97gv>"#.to_owned(),
            r#"       ^-1_v#:p79+*2 < "#.to_owned(),
            r#"           >   $  97g ^"#.to_owned(),
        ]);
        self.current_stack_size += 1;

        // Times by the sign from earlier
        self.char('*');
        self.current_stack_size -= 1;
    }

    pub fn bit_or(&mut self, a: &IRValue, b: &IRValue) {
        self.load_bit_stack(a, true);
        self.load_bit_stack(b, false);
        // If both one, -> 1 else -1
        self.str(r"+1`2*1-");
        self.current_stack_size -= 1;

        // For each bit, do not( (a + b) > 1 )
        self.insert_inline_befunge(&[
            r#"097p"?">::9g\8g+1\`!97v>"#.to_owned(),
            r#"       ^-1_v#:p79+*2 g< "#.to_owned(),
            r#"           >   $  97g  ^"#.to_owned(),
        ]);
        self.current_stack_size += 1;

        // Times by the sign from earlier
        self.char('*');
        self.current_stack_size -= 1;
    }

    pub fn bitshift_left(&mut self, a: &IRValue, b: &IRValue) {
        self.load_bit_stack(a, true);
        // sign bit (0/1) to sign (-1/1)
        self.str("2*1-");
        self.load_val(b);
        self.insert_inline_befunge(&[
            r#"097p:87p"?"+>:9g97g2*+97pv>"#.to_owned(),
            r#"            ^-1_v#+g78:  < "#.to_owned(),
            r#"                >   $ 97g ^"#.to_owned(),
        ]);

        // Times by the sign from earlier
        self.char('*');
        self.current_stack_size -= 1;
    }

    // NOTE: this does not match gcc.
    // gcc does sign extension (extend with 1s) but too much effort
    pub fn bitshift_right(&mut self, a: &IRValue, b: &IRValue) {
        self.load_bit_stack(a, true);
        // sign bit (0/1) to sign (-1/1)
        self.str("2*1-");
        self.load_val(b);
        self.insert_inline_befunge(&[
            r#"097p:87p"?"\->:9g97g2*+97pv>"#.to_owned(),
            r#"             ^-1_v#-\g78: < "#.to_owned(),
            r#"                 >   $ 97g ^"#.to_owned(),
        ]);

        // Times by the sign from earlier
        self.char('*');
    }

    pub fn constrain_to_range(&mut self, value: &IRValue, size: IRType, bounded: bool) {
        self.load_val(value);
        match size {
            // FIXME: handles casts from signed -> unsigned wrong
            IRType::Unsigned(size) => {
                assert!(size <= 64);
                // jank alert TODO: FIXME:
                if size < 32 {
                    self.load_number(2_usize.pow(size as u32));
                    self.char('%');
                    self.current_stack_size -= 1;
                }
            }
            IRType::Signed(size) => {
                assert!(size <= 64);
                if size == 64 {
                    // Don't need to do anything if it's signed 64 bit,
                    // because everything already is!
                } else {
                    // jank alert TODO: FIXME:
                    if size < 32 {
                        self.load_number(2_usize.pow(size as u32 - 1));
                        self.add(&IRValue::BefungeStack, &IRValue::BefungeStack);
                        self.load_number(2_usize.pow(size as u32));
                        self.modulo(&IRValue::BefungeStack, &IRValue::BefungeStack);
                        self.load_number(2_usize.pow(size as u32 - 1));
                        self.sub(&IRValue::BefungeStack, &IRValue::BefungeStack);
                    }
                }
            }
            IRType::Double => todo!("floats"),
        }
    }

    pub fn insert_inline_befunge(&mut self, lines: &[String]) {
        let initial_length = self.ops.len();
        if let Some(first_line) = lines.first() {
            self.str(first_line);
        }
        let mut longest_extra = 0;
        for (i, line) in lines[1..].iter().enumerate() {
            if self.extra_ops.len() == i {
                self.extra_ops.push(vec![' '; initial_length]);
            }

            if self.extra_ops[i].len() < initial_length {
                self.extra_ops[i].resize_with(initial_length, || ' ');
            }
            assert!(
                self.extra_ops[i].len() == initial_length,
                "extra ops row should be the same length (or shorter) than main ops row"
            );

            self.extra_ops[i].extend(line.chars());
            let len = self.extra_ops[i].len();
            if len > longest_extra {
                longest_extra = len;
            }
        }

        // Top row must extend to at least the length of all the extra rows
        if self.ops.len() < longest_extra {
            self.ops.resize_with(longest_extra, || ' ');
        }
    }

    //// Finalize

    pub fn finalize_function(&self, name: &str) -> Vec<String> {
        let row_length = self.ops.len();
        let mut rows: Vec<Vec<char>> = vec![];
        let mut title_row = vec![' '; 8];
        title_row.append(&mut vec!['@'; 4]);
        title_row.append(&mut vec![' '; 2]);
        title_row.append(&mut name.to_string().chars().collect());
        if self.is_function {
            title_row.append(&mut vec!['(', ')']);
        }
        rows.push(title_row);

        if self.is_function {
            let mut entry_row = vec![' ', '>', '1', '-', ':', 'v'];
            entry_row.append(&mut vec![' '; row_length]);
            rows.push(entry_row);

            // Add return points
            for (i, pos) in self.return_points.iter().rev().enumerate() {
                if i < self.return_points.len() {
                    let mut resposition_row = vec![' '; row_length];
                    resposition_row[8] = '^';
                    resposition_row[9] = '-';
                    resposition_row[10] = '1';
                    resposition_row[11] = '<';
                    rows.push(resposition_row);
                }
                let mut entry_row = vec![' '; row_length];
                entry_row[8] = '>';
                entry_row[9] = ':';
                entry_row[10] = '#';
                entry_row[11] = '^';
                entry_row[12] = '_';
                entry_row[13] = '$';

                entry_row[*pos] = 'v';
                rows.push(entry_row);
            }

            // Add function exit points
            let mut exit_row = vec![' '; row_length];
            exit_row[0] = '^';
            for pos in self.exit_points.clone() {
                exit_row[pos] = '<';
            }
            rows.push(exit_row);

            if !self.return_points.is_empty() {
                let mut row = vec![' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '^', '-', '1', '<'];
                row.append(&mut vec![' '; row_length]);
                rows.push(row);
            }
        }

        let mut ops = self.ops.clone();
        if !self.is_function {
            ops.push('v');
        }
        rows.push(ops);
        rows.extend(self.extra_ops.clone());

        if !self.is_function {
            let mut row = vec![' '; row_length];
            row[0] = 'v';
            row.push('<');
            rows.push(row);
        }

        // quick correctness check
        for branch in self.branch_points.keys() {
            assert!(self.branch_labels.contains_key(branch));
        }

        // Add branches
        for (label, label_pos) in self.branch_labels.clone() {
            let mut row = vec![' '; row_length];
            row[label_pos] = '^';
            for branch_pos in self.branch_points.get(&label).unwrap_or(&Vec::new()) {
                if *branch_pos > label_pos {
                    row[*branch_pos] = '<';
                } else if *branch_pos < label_pos {
                    row[*branch_pos] = '>';
                }
            }
            rows.push(row);
        }
        rows.iter()
            .map(|row| row.iter().collect::<String>().trim_end().to_owned())
            .collect()
    }
}

impl OpBuilder {
    fn load_val(&mut self, val: &IRValue) {
        match val {
            IRValue::Stack(offset) => self.load_stack_val(*offset),
            IRValue::Register(id) => self.load_register_val(*id),
            IRValue::Immediate(value) => self.load_number(*value),
            IRValue::Data(position) => self.load_data_val(*position),
            IRValue::BefungeStack => {
                assert!(
                    (self.current_stack_size != 0),
                    "Attempt to use a befunge stack value when the stack is empty!"
                );
            }
            IRValue::Psuedo { .. } | IRValue::StaticPsuedo { .. } => {
                panic!("Psuedo registers should be removed by befunge generation time")
            }
        }
    }

    fn put_val(&mut self, val: &IRValue) {
        match val {
            IRValue::Stack(offset) => self.set_stack_val(*offset),
            IRValue::Register(id) => self.set_register_val(*id),
            IRValue::Immediate(_) => panic!("Immediate value as output location"),
            IRValue::Data(position) => self.set_data_val(*position),
            IRValue::BefungeStack => (),
            IRValue::Psuedo { .. } | IRValue::StaticPsuedo { .. } => {
                panic!("Psuedo registers should be removed by befunge generation time")
            }
        }
    }

    // TODO: add special optimizations
    fn get_two_ordered(&mut self, a: &IRValue, b: &IRValue) {
        if matches!(b, IRValue::BefungeStack) && !matches!(a, IRValue::BefungeStack) {
            self.load_val(b);
            self.load_val(a);
            self.char('\\');
        } else {
            self.load_val(a);
            self.load_val(b);
        }
    }

    fn get_two_reorderable(&mut self, a: &IRValue, b: &IRValue) {
        if matches!(b, IRValue::BefungeStack) {
            self.load_val(b);
            self.load_val(a);
        } else {
            self.load_val(a);
            self.load_val(b);
        }
    }

    pub fn copy(&mut self, a: &IRValue, b: &IRValue, size: usize) {
        assert_ne!(size, 0);
        if size == 1 {
            self.load_val(a);
            self.put_val(b);
        } else {
            for offset in 0..size {
                match a {
                    IRValue::Stack(position) => self.load_stack_val(*position + offset),
                    IRValue::Data(position) => self.load_data_val(*position + offset),
                    IRValue::Register(..) => panic!("Cannot copy with offset into a register"),
                    IRValue::BefungeStack => {
                        panic!("Cannot copy with offset into the befunge stack")
                    }
                    IRValue::Immediate(_) => panic!("Immediate value as output location"),
                    IRValue::Psuedo { .. } | IRValue::StaticPsuedo { .. } => {
                        panic!("Psuedo registers should be removed by befunge generation time")
                    }
                }
                match b {
                    IRValue::Stack(position) => self.set_stack_val(*position + offset),
                    IRValue::Data(position) => self.set_data_val(*position + offset),
                    IRValue::Register(..) => panic!("Cannot copy with offset into a register"),
                    IRValue::BefungeStack => {
                        panic!("Cannot copy with offset into the befunge stack")
                    }
                    IRValue::Immediate(_) => panic!("Immediate value as output location"),
                    IRValue::Psuedo { .. } | IRValue::StaticPsuedo { .. } => {
                        panic!("Psuedo registers should be removed by befunge generation time")
                    }
                }
            }
        }
    }

    pub fn copy_with_offset(&mut self, a: &IRValue, b: &IRValue, offset: usize) {
        // TODO: optimize here when loading an immediate value
        self.load_val(a);
        match b {
            IRValue::Stack(position) => self.set_stack_val(*position + offset),
            IRValue::Data(position) => self.set_data_val(*position + offset),
            IRValue::Register(..) => panic!("Cannot copy with offset into a register"),
            IRValue::BefungeStack => panic!("Cannot copy with offset into the befunge stack"),
            IRValue::Immediate(_) => panic!("Immediate value as output location"),
            IRValue::Psuedo { .. } | IRValue::StaticPsuedo { .. } => {
                panic!("Psuedo registers should be removed by befunge generation time")
            }
        }
    }

    pub fn copy_from_offset(&mut self, a: &IRValue, b: &IRValue, offset: usize) {
        match a {
            IRValue::Stack(position) => self.load_stack_val(*position + offset),
            IRValue::Data(position) => self.load_data_val(*position + offset),
            IRValue::Register(..) => panic!("Cannot copy with offset into a register"),
            IRValue::BefungeStack => panic!("Cannot copy with offset into the befunge stack"),
            IRValue::Immediate(_) => panic!("Immediate value as output location"),
            IRValue::Psuedo { .. } | IRValue::StaticPsuedo { .. } => {
                panic!("Psuedo registers should be removed by befunge generation time")
            }
        }
        self.put_val(b);
    }

    // 0 - x
    pub fn unary_minus(&mut self, a: &IRValue) {
        self.load_number(0);
        self.load_val(a);
        self.char('-');
        self.current_stack_size -= 1;
    }

    // TODO: improve. x -> -1 - x
    pub fn bitwise_complement(&mut self, a: &IRValue) {
        self.load_number(0);
        self.load_val(a);
        self.str("-1-");
        self.current_stack_size -= 1;
    }

    pub fn boolean_negate(&mut self, a: &IRValue) {
        self.load_val(a);
        self.char('!');
    }

    pub fn address_of(&mut self, a: &IRValue) {
        match a {
            IRValue::Stack(offset) => {
                // TODO: optimize this
                self.load_number(*offset);
                self.load_stack_ptr();
                self.add(&IRValue::BefungeStack, &IRValue::BefungeStack);
            }
            IRValue::Data(position) => {
                // TODO:
                //self.load_number(0b001 * 2_usize.pow(61) + *position);
                self.add(
                    &IRValue::int(2_usize.pow(61)),
                    &IRValue::int(*position + 31),
                );
            }
            IRValue::Register(_)
            | IRValue::Psuedo { .. }
            | IRValue::StaticPsuedo { .. }
            | IRValue::Immediate(_)
            | IRValue::BefungeStack => panic!("{a:?} is an invalid location to get the address of"),
        }
    }

    pub fn dereference(&mut self, a: &IRValue, size: usize) {
        // TODO: consider moving this top level
        self.load_number(2_usize.pow(61));
        self.put_val(&IRValue::Register(61));

        self.load_val(a);
        for _ in 1..size * 2 {
            self.char(':');
            self.current_stack_size += 1;
        }

        for _ in 0..size {
            self.load_val(&IRValue::Register(61));
            self.modulo(&IRValue::BefungeStack, &IRValue::BefungeStack);

            self.char('\\');

            self.load_val(&IRValue::Register(61));
            self.divide(&IRValue::BefungeStack, &IRValue::BefungeStack);

            self.char('g');
            self.current_stack_size -= 1;
        }
    }

    /// follows pointer
    pub fn store(&mut self, val: &IRValue, loc: &IRValue) {
        self.load_val(val);

        self.load_val(loc);
        self.char(':');
        self.current_stack_size += 1;

        self.load_number(2_usize.pow(61));
        self.modulo(&IRValue::BefungeStack, &IRValue::BefungeStack);

        self.char('\\');

        self.load_number(2_usize.pow(61));
        self.divide(&IRValue::BefungeStack, &IRValue::BefungeStack);

        self.char('p');
        self.current_stack_size -= 3;
    }

    pub fn add(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_reorderable(a, b);
        self.char('+');
        self.current_stack_size -= 1;
    }
    pub fn sub(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_ordered(a, b);
        self.char('-');
        self.current_stack_size -= 1;
    }
    pub fn multiply(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_reorderable(a, b);
        self.char('*');
        self.current_stack_size -= 1;
    }
    pub fn divide(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_ordered(a, b);
        self.char('/');
        self.current_stack_size -= 1;
    }
    pub fn modulo(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_ordered(a, b);
        self.char('%');
        self.current_stack_size -= 1;
    }
    pub fn is_equal(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_reorderable(a, b);
        self.str("-!");
        self.current_stack_size -= 1;
    }
    pub fn is_not_equal(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_reorderable(a, b);
        self.str("-!!");
        self.current_stack_size -= 1;
    }
    pub fn is_less_than(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_ordered(b, a); // swapped load order
        self.str("`");
        self.current_stack_size -= 1;
    }
    pub fn is_less_or_equal(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_ordered(a, b);
        self.str("`!");
        self.current_stack_size -= 1;
    }
    pub fn is_greater_than(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_ordered(a, b);
        self.char('`');
        self.current_stack_size -= 1;
    }
    pub fn is_greater_or_equal(&mut self, a: &IRValue, b: &IRValue) {
        self.get_two_ordered(b, a); // swapped load order
        self.str("`!");
        self.current_stack_size -= 1;
    }
    pub fn add_ptr(&mut self, ptr: &IRValue, b: &IRValue, size: usize) {
        self.get_two_ordered(ptr, b);
        //self.address_of(ptr);
        //self.get_val(b);
        self.load_number(size);
        self.str("*+");
        self.current_stack_size -= 2;
    }
}
