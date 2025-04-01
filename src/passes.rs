use crate::ir::{FuncInfo, IROp, IRTopLevel, IRValue};
use std::collections::HashMap;

struct PsuedoMap<'a> {
    stack_map: HashMap<String, usize>,
    stack_count: usize,
    data_count: &'a mut usize,
    data_map: &'a mut HashMap<String, usize>,
}

#[allow(clippy::match_same_arms, clippy::redundant_closure)]
fn apply_to_all_ir_values(ops: &mut IRTopLevel, func: &mut impl FnMut(&mut IRValue)) {
    for i in 0..ops.ops.len() {
        match &mut ops.ops[i] {
            IROp::Return(o) => func(o),
            IROp::Call(_, ops) => ops.iter_mut().for_each(|x| func(x)),
            IROp::CondBranch(_, _, a) => func(a),
            IROp::One(_, a, out) => {
                func(a);
                func(out);
            }
            IROp::Two(_, a, b, out) => {
                func(a);
                func(b);
                func(out);
            }
            IROp::Cast(_, (a, _), out) => {
                func(a);
                func(out);
            }
            _ => (),
        }
    }
}

fn pseudo_removal(
    func: &mut IRTopLevel,
    data_count: &mut usize,
    data_map: &mut HashMap<String, usize>,
) {
    let mut map = PsuedoMap {
        stack_map: HashMap::new(),
        stack_count: func.parameters,
        data_count,
        data_map,
    };
    let mut get = |val: &mut IRValue| *val = map.get(val);
    apply_to_all_ir_values(func, &mut get);
}

impl PsuedoMap<'_> {
    fn get(&mut self, val: &IRValue) -> IRValue {
        if let IRValue::Psuedo { name } = val {
            if let Some(id) = self.stack_map.get(name) {
                return IRValue::Stack(*id);
            }
            self.stack_count += 1;
            self.stack_map.insert(name.to_owned(), self.stack_count);
            return IRValue::Stack(self.stack_count);
        }
        if let IRValue::StaticPsuedo { name, .. } = val {
            if let Some(id) = self.data_map.get(name) {
                return IRValue::Data(*id);
            }
            *self.data_count += 1;
            self.data_map.insert(name.to_owned(), *self.data_count);
            return IRValue::Data(*self.data_count);
        }
        val.clone()
    }
}

pub fn pseudo_removal_pass(funcs: &mut Vec<IRTopLevel>) {
    let mut data_count = 0;
    let mut data_map = HashMap::new();
    for func in funcs {
        pseudo_removal(func, &mut data_count, &mut data_map);
    }
}

pub fn the_linkening(files: Vec<Vec<IRTopLevel>>) -> Vec<IRTopLevel> {
    let mut out = vec![];
    for (i, file) in files.into_iter().enumerate() {
        for mut func in file {
            append_to_all_psuedos(&mut func, &(".".to_owned() + &i.to_string()));
            out.push(func);
        }
    }
    out
}

fn append_to_all_psuedos(func: &mut IRTopLevel, new_suffix: &str) {
    apply_to_all_ir_values(func, &mut |value: &mut IRValue| {
        append_to_ir_value(value, new_suffix);
    });
}

fn append_to_ir_value(value: &mut IRValue, new_suffix: &str) {
    match value {
        IRValue::StaticPsuedo {
            name,
            linkable: false,
        }
        | IRValue::Psuedo { name } => {
            *name = name.clone() + new_suffix;
        }
        _ => (),
    }
}

fn stack_size_recalculator(func: &mut IRTopLevel) -> usize {
    // find biggest Stack value
    let mut counter = 0;
    let mut check = |val: &mut IRValue| {
        if let IRValue::Stack(val) = val {
            if *val > counter {
                counter = *val;
            }
        }
    };
    apply_to_all_ir_values(func, &mut check);
    std::cmp::max(counter, func.parameters)
}

pub fn stack_size_reducer_pass(funcs: &mut Vec<IRTopLevel>) {
    for func in funcs {
        func.stack_frame_size = stack_size_recalculator(func);
    }
}

pub fn function_id_mapping_pass(funcs: &[IRTopLevel]) -> HashMap<String, FuncInfo> {
    let mut map = HashMap::new();
    let mut i = 1;
    for func in funcs {
        if !func.is_initializer {
            map.insert(
                func.name.clone(),
                FuncInfo {
                    id: i,
                    stack_frame_size: func.stack_frame_size,
                },
            );
            i += 1;
        }
    }
    map
}

pub fn sort_functions_pass(funcs: Vec<IRTopLevel>) -> Vec<IRTopLevel> {
    let mut out = vec![];
    let main_found = false;
    for func in funcs {
        if func.name == "main" {
            assert!(!main_found, "Only one main function may exist");
            out.insert(0, func);
        } else {
            out.push(func);
        }
    }
    out
}
