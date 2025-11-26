use prime_factorization::Factorization;

const MAX: u64 = 1_114_111;
const ISIZE_MAX: isize = 1_114_111;

pub fn int_to_befunge_str(num: u64) -> String {
    match num {
        0..10 => num.to_string(),
        10..19 => (num - 9).to_string() + "9+",
        19 => "45*1-".to_owned(),
        20 => "45*".to_owned(),
        34 => "98+2*".to_owned(),
        _ => {
            if num < MAX
                && let Some(char) = char::from_u32(num as u32)
            {
                return format!("\"{char}\"");
            }

            let mut len = usize::MAX;
            let mut best = None;

            // TODO: enable with flag
            if true {
                for i in -9..=9 {
                    let x = with_offset(num, i, sqrt);
                    if let Some(x) = x {
                        return x;
                    }
                }

                if let Some(n) = find_nearby_square(num) {
                    return n;
                }

                for i in -9..=9 {
                    let x = with_offset(num, i, factors);
                    if let Some(x) = x
                        && x.chars().count() < len
                    {
                        len = x.chars().count();
                        best = Some(x);
                    }
                }

                if best.is_none() {
                    // TODO: investigate some smarter search
                    for i in -1000..=-10 {
                        let x = with_offset(num, i, factors);
                        if let Some(x) = x {
                            return x;
                        }
                    }
                }
            }

            best.unwrap_or_else(|| base_9(num))
        }
    }
}

fn base_9(num: u64) -> String {
    let mut result = String::new();
    let mut x = num;
    loop {
        let m = x % 9;
        x /= 9;

        result.push(char::from_digit(m as u32, 9).unwrap());
        result.push('9');
        if x == 0 {
            break;
        }
    }
    result.pop(); // remove extra 9 at the end
    for _ in 0..result.len() / 2 {
        result.push('*');
        result.push('+');
    }
    result
}

fn factors(num: u64) -> Option<String> {
    if num < 32 {
        return None;
    }

    let mut factors = Factorization::run(num).factors;
    if factors.iter().any(|x| *x >= MAX) {
        return None;
    }

    let len = factors.len();
    // TODO: make this faster
    for _ in 0..len {
        let mut left = 0;
        'loopy: while left < factors.len() {
            left += 1;
            let mut right = factors.len();
            while right > left {
                right -= 1;
                if factors[left - 1] * factors[right] < MAX
                    && char::from_u32((factors[left - 1] * factors[right]) as u32).is_some()
                {
                    factors[left - 1] *= factors[right];
                    factors.remove(right);
                    continue 'loopy;
                }
            }
        }
    }

    let mut result = "\"".to_string();
    let len = factors.len();
    for factor in factors {
        if factor == '"'.into() || factor == '\r'.into() || factor == '\n'.into() {
            return None;
        }
        result.push(char::from_u32(factor as u32)?);
    }
    result.push('"');
    for _ in 0..len - 1 {
        result.push('*');
    }

    Some(result)
}

fn with_offset(
    num: u64,
    offset: isize,
    mut func: impl FnMut(u64) -> Option<String>,
) -> Option<String> {
    let mut str = func(num.wrapping_add_signed(offset as i64))?;
    match offset {
        0 => (),
        1..=9 => {
            str.push(offset.to_string().chars().next().unwrap());
            str.push('-');
        }
        10.. => {
            let offset = offset as u64;
            if !str.starts_with('"') {
                return None;
            }
            if offset != '"'.into()
                && offset != '\r'.into()
                && offset != '\n'.into()
                && let Some(n) = char::from_u32(offset as u32)
            {
                str.insert(1, n);
                str.push('\\');
                str.push('-');
            } else {
                return None;
            }
        }
        -9..=-1 => {
            str.push((-offset).to_string().chars().next().unwrap());
            str.push('+');
        }
        ..=-10 => {
            let offset = (-offset) as u64;
            if !str.starts_with('"') {
                return None;
            }

            if offset != '"'.into()
                && offset != '\r'.into()
                && offset != '\n'.into()
                && let Some(n) = char::from_u32(offset as u32)
            {
                str.insert(1, n);
                str.push('\\');
                str.push('+');
            } else {
                return None;
            }
        }
    }
    Some(str)
}

fn sqrt(num: u64) -> Option<String> {
    let sqrt = num.isqrt();
    if sqrt * sqrt != num {
        return None;
    }

    let mut str = int_to_befunge_str(sqrt);
    str.push(':');
    str.push('*');
    Some(str)
}

fn find_nearby_square(num: u64) -> Option<String> {
    let isqrt = num.isqrt();
    let (lower, higher) = (isqrt * isqrt, (isqrt + 1) * (isqrt + 1));
    if (num - lower) < MAX
        && let Some(x) = with_offset(num, (num - lower) as isize, sqrt)
    {
        return Some(x);
    }

    if (higher - num) < MAX
        && let Some(x) = with_offset(num, (higher - num) as isize, sqrt)
    {
        return Some(x);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::{Rng, rngs::StdRng};

    fn check_str(str: &str, num: u64) {
        let mut stack: Vec<i64> = vec![];
        let mut string_mode = false;
        for char in str.chars() {
            if string_mode {
                assert_ne!(char, '\n');
                assert_ne!(char, '\r');
                if char == '"' {
                    string_mode = false;
                } else {
                    stack.push(char as i64);
                }
            } else {
                match char {
                    '0'..='9' => stack.push((char as u8 - b'0').into()),
                    '"' => string_mode = true,
                    '+' => {
                        let (a, b) = (stack.pop().unwrap_or(0), stack.pop().unwrap_or(0));
                        stack.push(b + a);
                    }
                    '*' => {
                        let (a, b) = (stack.pop().unwrap_or(0), stack.pop().unwrap_or(0));
                        stack.push(b * a);
                    }
                    '-' => {
                        let (a, b) = (stack.pop().unwrap_or(0), stack.pop().unwrap_or(0));
                        stack.push(b - a);
                    }
                    ':' => {
                        let a = stack.pop().unwrap_or(0);
                        stack.push(a);
                        stack.push(a);
                    }
                    '\\' => {
                        let (a, b) = (stack.pop().unwrap_or(0), stack.pop().unwrap_or(0));
                        stack.push(a);
                        stack.push(b);
                    }
                    other => panic!("{other} is not a valid befunge operation {num}"),
                }
            }
        }
        assert_eq!(stack.len(), 1, "{num}");
        assert!(stack[0] > 0);
        assert_eq!(stack[0], num as i64, "{num}, {str}");
    }

    fn check_all(num: u64) {
        check_str(&base_9(num), num);
        if let Some(str) = factors(num) {
            check_str(&str, num);
        }
        if let Some(str) = sqrt(num) {
            check_str(&str, num);
        }
        if let Some(str) = find_nearby_square(num) {
            check_str(&str, num);
        }
    }

    #[test]
    fn small_values() {
        for i in 1..300 {
            check_all(i);
        }
    }

    #[test]
    fn medium_values() {
        for i in 65_500..66_500 {
            check_all(i);
        }
    }

    #[test]
    fn squares() {
        for i in 600..1000 {
            check_all(i * i);
            check_all((i * i) - 125);
        }
    }

    #[test]
    fn test_perfect() {
        let other_part = 2u64.pow(30) - 1;
        let num: u64 = 2u64.pow(31) * other_part;
        check_all(num);
    }

    #[test]
    fn test_large() {
        for i in 2u64.pow(60) - 50..2u64.pow(60) + 50 {
            check_all(i);
        }
    }

    #[test]
    fn test_max() {
        for i in i64::MAX - 100..=i64::MAX {
            check_all(i as u64);
        }
    }

    #[test]
    fn test_random() {
        use rand::SeedableRng;
        let mut rng = StdRng::seed_from_u64(42);
        for _ in 0..10_000 {
            let x: i64 = rng.random();
            check_all(x.abs() as u64);
        }
    }

    #[test]
    fn final_sanity_check() {
        use rand::SeedableRng;
        let mut rng = StdRng::seed_from_u64(42);
        let mut vals = vec![];
        for _ in 0..1_000 {
            let x: i64 = rng.random();
            vals.push(int_to_befunge_str(x.abs() as u64).chars().count());
        }
        println!(
            "average: {}",
            vals.iter().sum::<usize>() as f64 / vals.len() as f64
        );
    }
}
