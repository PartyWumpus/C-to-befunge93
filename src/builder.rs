use std::collections::HashMap;

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
        }
    }

    pub fn str(&mut self, str: &str) {
        self.ops.extend(str.chars());
    }

    pub fn char(&mut self, char: char) {
        self.ops.push(char);
    }

    /// Puts stack ptr on bstack
    fn load_stack_ptr(&mut self) {
        self.str("00g");
    }

    /// Set stack ptr to top of bstack
    fn set_stack_ptr(&mut self) {
        self.str("00p");
    }

    /// Puts stack ptr on bstack
    fn load_call_stack_ptr(&mut self) {
        self.str("10g");
    }

    /// Set stack ptr to top of bstack
    fn set_call_stack_ptr(&mut self) {
        self.str("10p");
    }

    /// Puts return value on bstack
    pub fn load_return_val(&mut self) {
        self.str("20g");
    }

    /// Set return value to top of bstack
    fn set_return_val(&mut self) {
        self.str("20p");
    }

    /// Puts num on bstack
    pub fn load_number(&mut self, num: usize) {
        let x = match num {
            0..10 => num.to_string(),
            10..19 => (num - 9).to_string() + "9+",
            19 => "55*1-".to_owned(),
            // This is the newline char
            20 => "45*".to_owned(),
            // This is the " char
            34 => "98+2*".to_owned(),
            x => number_to_bf_string(x),
        };
        self.str(&x);
    }

    fn index_register(&mut self, id: usize) {
        let id = id + 2;
        assert!(id < 99, "attempt to index register > 99");
        self.str(&format!("{}{}", id % 10, id / 10));
    }

    pub fn load_register_val(&mut self, id: usize) {
        self.index_register(id);
        self.char('g');
    }

    pub fn set_register_val(&mut self, id: usize) {
        self.index_register(id);
        self.char('p');
    }

    pub fn load_stack_val(&mut self, offset: usize) {
        self.load_stack_ptr();
        self.load_number(offset);
        self.str("-1g");
    }

    pub fn set_stack_val(&mut self, offset: usize) {
        self.load_stack_ptr();
        self.load_number(offset);
        self.str("-1p");
    }

    /// Puts data value on bstack
    pub fn load_data_val(&mut self, position: usize) {
        self.load_number(position + 30); // + 30 to avoid the register space
        self.str("3g");
    }

    /// Set data value to top of bstack
    pub fn set_data_val(&mut self, position: usize) {
        self.load_number(position + 30); // + 30 to avoid the register space
        self.str("3p");
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
    }

    pub fn not_zero_branch(&mut self, label: String) {
        self.str("#v_");
        self.branch_points
            .entry(label)
            .or_default()
            .push(self.ops.len() - 2);
    }

    pub fn zero_branch(&mut self, label: String) {
        self.char('!');
        self.not_zero_branch(label);
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
    }

    pub fn return_(&mut self, stack_frame_size: usize) {
        self.set_return_val();
        self.decrement_stack_ptr(stack_frame_size);

        self.load_call_stack_ptr();
        self.str(r"1-:2g\1-:2g\");
        self.set_call_stack_ptr();

        self.exit();
    }

    fn call_exit(&mut self) {
        self.str("^>");
        self.exit_points.push(self.ops.len() - 2);
        self.return_points.push(self.ops.len() - 1);
    }

    pub fn call(&mut self) {
        // TODO: optimize with swap op
        self.load_call_stack_ptr();
        self.str("2p");

        self.load_number(self.return_points.len() + 1);
        self.load_call_stack_ptr();
        self.str("1+2p");

        self.load_call_stack_ptr();
        self.str("2+");
        self.set_call_stack_ptr();

        self.call_exit();
    }

    fn load_bit_stack(&mut self, alt: bool) {
        // TODO: clobbers 98p and 99p, as well as writing to the special bit stacks
        if alt {
            self.insert_inline_befunge(&[
                r#":98p"!"\>2%88p1+:88g\9p98gv>"#.to_owned(),
                r#"        ^p89:\_v#-"a":\/2 < "#.to_owned(),
                r#"               > $$        ^"#.to_owned(),
            ])
        } else {
            self.insert_inline_befunge(&[
                r#":98p"!"\>2%88p1+:88g\8p98gv>"#.to_owned(),
                r#"        ^p89:\_v#-"a":\/2 < "#.to_owned(),
                r#"               > $$        ^"#.to_owned(),
            ])
        }
    }

    // Assumes two values on stack
    pub fn bit_and(&mut self) {
        self.load_bit_stack(false);
        self.load_bit_stack(true);
        // For each bit, do a * b
        self.insert_inline_befunge(&[
            r#"098p"a">::9g\8g*98g2* v>"#.to_owned(),
            r#"       ^_v#-"!":-1p89+< "#.to_owned(),
            r#"         >   $  98g    ^"#.to_owned(),
        ])
    }

    // Assumes two values on stack
    pub fn bit_xor(&mut self) {
        self.load_bit_stack(false);
        self.load_bit_stack(true);
        // For each bit, do (a + b) mod 2
        self.insert_inline_befunge(&[
            r#"098p"a">::9g\8g+2%98g2*v>"#.to_owned(),
            r#"       ^_v#-"!":-1p89+ < "#.to_owned(),
            r#"         >   $  98g     ^"#.to_owned(),
        ])
    }

    // Assumes two values on stack
    pub fn bit_or(&mut self) {
        self.load_bit_stack(false);
        self.load_bit_stack(true);
        // For each bit, do not( (a + b) > 1 )
        self.insert_inline_befunge(&[
            r#"098p"a">::9g\8g+1\`!98g2v>"#.to_owned(),
            r#"       ^_v#-"!":-1p89+* < "#.to_owned(),
            r#"         >   $  98g      ^"#.to_owned(),
        ])
    }

    pub fn insert_inline_befunge(&mut self, lines: &[String]) {
        let initial_length = self.ops.len();
        if let Some(first_line) = lines.get(0) {
            self.str(first_line)
        }
        let mut longest_extra = 0;
        for (i, line) in lines[1..].iter().enumerate() {
            if self.extra_ops.len() == i {
                self.extra_ops.push(vec![' '; initial_length])
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
                longest_extra = len
            }
        }

        // Top row must extend to at least the length of all the extra rows
        if self.ops.len() < longest_extra {
            self.ops.resize_with(longest_extra, || ' ');
        }
    }

    //// Finalize

    pub fn finalize_function(&self) -> Vec<String> {
        let row_length = self.ops.len();
        let mut rows: Vec<Vec<char>> = vec![];

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
            .map(|row| row.iter().collect::<String>())
            .collect()
    }
}

// NOTE: to reader, this is able to be much simpler than Mikescher's BefunRep
// as I am allowed to use any valid character up to 65_535, where as they
// only use ascii, which is likely compatible with more interpreters

// This is not optimal, but it should do a reasonably good job at finding
// a reasonably short expression (max about 10 chars)
fn number_to_bf_string(num: usize) -> String {
    let mut options = vec![];
    if let Some(res) = num_to_bf_chars(num) {
        let mult = str::repeat("*", res.chars().count() - 1);
        options.push("\"".to_owned() + &res + "\"" + &mult);
    }

    // make n + i, and then take away i
    for i in 0..10 {
        if let Some(res) = num_to_bf_chars(num + i) {
            let mult = str::repeat("*", res.chars().count() - 1);
            options.push("\"".to_owned() + &res + "\"" + &mult + &i.to_string() + "-");
        }
    }

    // make n - i, and then add i
    for i in 0..10 {
        if let Some(res) = num_to_bf_chars(num - i) {
            let mult = str::repeat("*", res.chars().count() - 1);
            options.push("\"".to_owned() + &res + "\"" + &mult + &i.to_string() + "+");
        }
    }

    // find shortest by char length (not .len())
    let mut options = options.iter();
    let mut shortest = match options.next() {
        None => panic!("Failed to represent {num} in befunge. This is unlikely, if not impossible. (Checked the first 20,000,000 values with zero failures)"),
        Some(x) => x,
    };
    let mut shortest_length = shortest.chars().count();

    for option in options {
        let length = option.chars().count();
        if length < shortest_length {
            shortest_length = length;
            shortest = option;
        }
    }
    return shortest.to_string();
}

fn num_to_bf_chars(num: usize) -> Option<String> {
    // 2^16 - 1
    if num < 65_535 {
        if let Some(char) = char::from_u32(num as u32) {
            return Some(char.to_string());
        }
    }
    let (a, b) = get_highest_divisors(num);
    // If prime or newline or carrage return or speech mark, it's an invalid repr
    if a == 1 || a == 10 || a == 13 || a == 34 || b == 1 || b == 10 || b == 13 || b == 34 {
        None
    } else {
        Some(num_to_bf_chars(a)? + &num_to_bf_chars(b)?)
    }
}

fn get_highest_divisors(n: usize) -> (usize, usize) {
    let mut a = (n as f64).sqrt() as usize;
    while n % a > 0 {
        a -= 1;
    }
    (a, n / a)
}
