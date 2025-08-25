use prime_factorization::Factorization;
use std::num::NonZeroU64;

#[derive(Debug, Clone)]
struct Factors {
    offset: i8,
    direct_factors: Box<[u64]>,
    offset_factors: Box<[Factors]>,
}

pub fn int_to_befunge_str(num: u64) -> String {
    match num {
        0..10 => num.to_string(),
        10..19 => (num - 9).to_string() + "9+",
        19 => "45*1-".to_owned(),
        20 => "45*".to_owned(),
        34 => "98+2*".to_owned(),
        _ => {
            let factors = highest_valid_factors(NonZeroU64::new(num).unwrap());
            factors_to_befunge_str(&factors)
        }
    }
}

fn factors_to_befunge_str(res: &Factors) -> String {
    let (mut chars, mut ops) = (vec![], vec![]);
    factors_to_befunge_str_rec(res, &mut chars, &mut ops);
    format!(
        r#""{}"{}"#,
        chars.iter().collect::<String>(),
        &ops.iter().collect::<String>()
    )
}

fn factors_to_befunge_str_rec(res: &Factors, chars: &mut Vec<char>, ops: &mut Vec<char>) {
    let mut at_least_one = false;
    for i in &res.direct_factors {
        let j = char::from_u32((*i).try_into().unwrap()).unwrap();
        chars.push(j);
        if !at_least_one {
            at_least_one = true;
        } else {
            ops.push('*');
        }
    }

    for i in &res.offset_factors {
        factors_to_befunge_str_rec(i, chars, ops);
        match i.offset {
            0 => (),
            1..=9 => {
                ops.push(i.offset.to_string().chars().next().unwrap());
                ops.push('+');
            }
            -9..=-1 => {
                ops.push((-i.offset).to_string().chars().next().unwrap());
                ops.push('-');
            }
            _ => unreachable!(),
        }
        if !at_least_one {
            at_least_one = true;
        } else {
            ops.push('*');
        }
    }
}

fn resolve(x: Factors) -> u64 {
    let mut val: u64 = 1;
    for i in x.direct_factors {
        assert_ne!(i, 10);
        assert_ne!(i, 13);
        assert_ne!(i, 34);
        val *= i;
    }
    for i in x.offset_factors {
        val *= resolve(i);
    }

    val.wrapping_add_signed(i64::from(x.offset))
}

impl Factors {
    fn size(&self) -> usize {
        self.offset_factors
            .iter()
            .map(|x| x.size() + 3) // 3 for *, offset and +/-
            .sum::<usize>()
            + self.direct_factors.len() * 2 // 2 for * and factor
            - 1 // -1 because there's one less * than numbers
    }
}

fn highest_valid_factors(n: NonZeroU64) -> Factors {
    highest_valid_factors_rec(n, 0)
}

// TODO: CONSIDER:
//const MAX: u64 = 1_114_111;
const MAX: u64 = 65_535;
fn highest_valid_factors_rec(n: NonZeroU64, offset: i8) -> Factors {
    let n = n.get();
    if n == 1 {
        return Factors {
            offset: 0,
            direct_factors: [1].into(),
            offset_factors: [].into(),
        };
    }
    // TODO: consider
    //let factor_repr = if n < 2_u64.pow(32) {
    //    Factorization::run(n as u32).factors.iter().map(|x| *x as u64).collect()
    //} else {
    //    Factorization::run(n).factors
    //};
    let factor_repr = Factorization::run(n).factors;
    let mut direct_factors = vec![];
    let mut offset_factors = vec![];
    let mut in_progress = 1;
    for factor in &factor_repr {
        if *factor < MAX / 2 && char::from_u32(u32::try_from(*factor).unwrap()).is_some() {
            let combined = in_progress * *factor;
            if combined < MAX && char::from_u32(u32::try_from(combined).unwrap()).is_some() {
                in_progress = combined;
            } else {
                direct_factors.push(in_progress);
                in_progress = *factor;
            }
        } else if *factor < MAX && char::from_u32(u32::try_from(*factor).unwrap()).is_some() {
            direct_factors.push(*factor);
        } else {
            // In theory it's safe to subtract one and two always, because 1 and 0 are never factors.
            // Adding up to 9 is always okay because the largest prime is 59 away from U64::MAX
            // In reality though this is very, very slow, for not great gains.

            // Worth investigating if a heuristic could be used here to only try more
            // things if the current solution is outside a reasonable range or something.
            let a = highest_valid_factors_rec(NonZeroU64::new(*factor - 1).unwrap(), 1);
            let b = highest_valid_factors_rec(NonZeroU64::new(*factor + 1).unwrap(), -1);
            if a.size() < b.size() {
                offset_factors.push(a);
            } else {
                offset_factors.push(b);
            }
        }
    }
    if in_progress != 1 {
        direct_factors.push(in_progress);
    }
    sanitise_factors(&mut direct_factors, &mut offset_factors);
    Factors {
        direct_factors: direct_factors.into(),
        offset_factors: offset_factors.into(),
        offset,
    }
}

fn sanitise_factors(arr: &mut Vec<u64>, bigs: &mut Vec<Factors>) {
    let mut i = 0;
    while i < arr.len() {
        if arr[i] == 10 {
            arr[i] = 2;
            arr.push(5);
        } else if arr[i] == 13 {
            arr.remove(i);
            bigs.push(Factors {
                direct_factors: Box::new([6, 2]),
                offset_factors: Box::new([]),
                offset: 1,
            });
            continue;
        } else if arr[i] == 34 {
            arr[i] = 2;
            arr.push(17);
        }
        i += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(n: u64) {
        let res = highest_valid_factors(NonZeroU64::new(n).unwrap());
        assert_eq!(resolve(res), n);
    }

    #[test]
    fn small_values() {
        for i in 1..100 {
            check(i);
        }
    }

    #[test]
    fn medium_values() {
        for i in 65_500..66_500 {
            check(i);
        }
    }

    #[test]
    fn test_perfect() {
        let other_part = 2u64.pow(30) - 1;
        let num: u64 = 2u64.pow(31) * other_part;
        check(num);
    }

    #[test]
    fn test_large() {
        for i in 2u64.pow(60) - 50..2u64.pow(60) + 50 {
            check(i);
        }
    }

    #[test]
    fn test_max() {
        for i in u64::MAX - 100..=u64::MAX {
            check(i);
        }
    }
}
