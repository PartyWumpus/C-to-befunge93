use std::collections::HashMap;

pub struct OpBuilder {
    ops: String,
    branch_labels: HashMap<String, usize>,
    branch_points: HashMap<String, Vec<usize>>,
}

impl OpBuilder {
    pub fn new() -> Self {
        Self {
            ops: "".to_owned(),
            branch_labels: HashMap::new(),
            branch_points: HashMap::new(),
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

    /// Puts return value on bstack
    pub fn load_return_val(&mut self) {
        self.append_str("10g");
    }

    /// Set return value to top of bstack
    pub fn set_return_val(&mut self) {
        self.append_str("10p")
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
            panic!("attempt to index register > 96")
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

    pub fn call(&mut self) {}

    pub fn finalize_function(self) -> Vec<String> {
        let row_length = self.ops.len();
        let mut rows = vec![self.ops];
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
            rows.push(row.iter().collect::<String>());
        }
        rows
    }
}
