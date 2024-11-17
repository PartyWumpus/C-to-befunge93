use std::collections::HashMap;

pub struct OpBuilder {
    ops: String,
    branch_labels: HashMap<String, usize>,
    branch_points: HashMap<String, Vec<usize>>,
    exit_points: Vec<usize>,
    return_points: Vec<usize>,
}

impl OpBuilder {
    pub fn new() -> Self {
        Self {
            ops: " v   _$ >:#^_$".to_owned(),
            branch_labels: HashMap::new(),
            branch_points: HashMap::new(),
            exit_points: Vec::new(),
            return_points: Vec::new(),
        }
    }

    pub fn append_str(&mut self, str: &str) {
        self.ops += str
    }

    pub fn append_char(&mut self, char: char) {
        self.ops.push(char.into());
    }

    /// Puts stack ptr on bstack
    fn load_stack_ptr(&mut self) {
        self.append_str("00g");
    }

    /// Set stack ptr to top of bstack
    fn set_stack_ptr(&mut self) {
        self.append_str("00p")
    }

    /// Puts stack ptr on bstack
    fn load_call_stack_ptr(&mut self) {
        self.append_str("10g");
    }

    /// Set stack ptr to top of bstack
    fn set_call_stack_ptr(&mut self) {
        self.append_str("10p")
    }

    /// Puts return value on bstack
    pub fn load_return_val(&mut self) {
        self.append_str("20g");
    }

    /// Set return value to top of bstack
    fn set_return_val(&mut self) {
        self.append_str("20p")
    }

    /// Puts num on bstack
    pub fn load_number(&mut self, num: usize) {
        let x = match num {
            0..9 => num.to_string(),
            x => format!(
                r#""{}""#,
                char::from_u32(x as u32).expect("failed to convert number to befunge string")
            ),
        };
        self.append_str(&x);
    }

    fn index_register(&mut self, id: usize) {
        let id = id + 2;
        if id > 99 {
            panic!("attempt to index register > 99")
        }
        self.append_str(&format!("{}{}", id % 10, id / 10))
    }

    pub fn load_register_val(&mut self, id: usize) {
        self.index_register(id);
        self.append_char('g');
    }

    pub fn set_register_val(&mut self, id: usize) {
        self.index_register(id);
        self.append_char('p');
    }

    pub fn load_stack_val(&mut self, offset: usize) {
        self.load_stack_ptr();
        self.load_number(offset);
        self.append_str("+1g");
    }

    pub fn set_stack_val(&mut self, offset: usize) {
        self.load_stack_ptr();
        self.load_number(offset);
        self.append_str("+1p");
    }

    pub fn label(&mut self, label: String) {
        self.append_char('>');
        self.branch_labels.insert(label, self.ops.len() - 1);
    }

    pub fn unconditional_branch(&mut self, label: String) {
        self.append_char('v');
        self.branch_points
            .entry(label)
            .or_default()
            .push(self.ops.len() - 1);
    }

    pub fn not_zero_branch(&mut self, label: String) {
        self.append_str("#v_");
        self.branch_points
            .entry(label)
            .or_default()
            .push(self.ops.len() - 2);
    }

    pub fn zero_branch(&mut self, label: String) {
        self.append_char('!');
        self.not_zero_branch(label);
    }

    //// Function Calls

    pub fn increment_stack_ptr(&mut self, amount: usize) {
        self.load_stack_ptr();
        self.load_number(amount);
        self.append_char('+');
        self.set_stack_ptr();
    }

    pub fn decrement_stack_ptr(&mut self, amount: usize) {
        self.load_stack_ptr();
        self.load_number(amount);
        self.append_char('-');
        self.set_stack_ptr();
    }

    fn exit(&mut self) {
        self.append_char('^');
        self.exit_points.push(self.ops.len() - 1);
    }

    pub fn return_(&mut self, stack_frame_size: usize) {
        self.set_return_val();
        self.decrement_stack_ptr(stack_frame_size);

        self.load_call_stack_ptr();
        self.append_str(r#"1-:2g\1-:2g\"#);
        self.set_call_stack_ptr();

        self.exit();
    }

    fn call_exit(&mut self) {
        self.append_str("^>");
        self.exit_points.push(self.ops.len() - 2);
        self.return_points.push(self.ops.len() - 1);
    }

    pub fn call(&mut self) {
        // TODO: optimize with swap op
        self.load_call_stack_ptr();
        self.append_str("2p");

        self.load_number(self.return_points.len() + 1);
        self.load_call_stack_ptr();
        self.append_str("1+2p");

        self.load_call_stack_ptr();
        self.append_str("2+");
        self.set_call_stack_ptr();

        self.call_exit();
    }

    //// Finalize

    pub fn finalize_function(self) -> Vec<String> {
        let row_length = self.ops.len();
        let mut rows: Vec<Vec<char>> = vec![];

        let mut entry_row = vec![' ', '>', '1', '-', ':', 'v'];
        entry_row.append(&mut vec![' '; row_length]);
        rows.push(entry_row);

        // Add return points
        for (i, pos) in self.return_points.iter().enumerate() {
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
        for pos in self.exit_points {
            exit_row[pos] = '<';
        }

        rows.push(exit_row);

        if self.return_points.len() > 0 {
            let mut row = vec![' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '^', '-', '1', '<'];
            row.append(&mut vec![' '; row_length]);
            rows.push(row);
        }

        rows.push(self.ops.chars().collect());

        // Add branches
        for (label, label_pos) in self.branch_labels {
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
