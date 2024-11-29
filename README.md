# A C to befunge compiler

## Big thanks to: 

- Nora Sandler's [Writing a C Compiler](https://norasandler.com/book/) book for helping guide this project (and for being a great read regardless)
- Mikescher's [BefunExec](https://github.com/Mikescher/BefunExec) for being an excellent befunge debugger
- Notes from [Jeffrey Lee's site](https://www.phlamethrower.co.uk/index.php) about their [prototype befunge -> C compiler](https://www.phlamethrower.co.uk/befunge/c2b.php) and their [general befunge insights](https://www.phlamethrower.co.uk/befunge/)
- The [lang_c](https://docs.rs/lang-c/latest/lang_c/index.html) crate for doing the lexing and parsing of c for me (i love skipping 1/2 of the work)

## Notes about the implementation
The top left 10x10 corner is a sort of zero page as it can be indexed with only 3 characters (ie 05g for getting the value at 0, 5), and is used as the registers (more specifically, `00` is the stack pointer, `10` is the call stack pointer, `20` is the return value, and the rest are general purpose, so register ID 1 is `30`, register 8 is `01`)

As zero is always on the stack (if empty), I could theoretically make getting the stack register just `g` instead of `00g`, but that's going to cause mayhem if the stack ever leaks even a single value. (similarly i could make `1g`, `2g` be the call stack & return value, but they're less relevant really)

Function calls/returns use *two* indexes, one for the function ID (where 1 is main, and everything else is arbitrary), and the return location in the function (where 0 is the start) because after a call you need to return back to the position in the function where you were.
Calling convention: Return values just go in `20` in the zero-page, and function parameters go onto the start of the function's stack, so if a function takes args, stack(0) will be the value of the first one.
For ease of C compilation we just always move the value from the return register onto some stack value but that would theoretically be optimizable away in a register allocation pass, so I think it's the better choice.

## Limitations (note many of these are fixable and I plan to fix them)
- Only supports ints as types
- Scoping of variables is broken (i am lazy)
- Doesn't support C loops (note the IR is perfectly capable of loops, I just haven't got the C parsing bit done)
- Doesn't do a register allocation pass, which would probably make it a decent bit faster
- No linking of any sort, all has to be in one file
- No stdlib or anything from libc at all (might fudge some stdio in just for convinience before doing it properly)
- No arbitrarily accessable memory (doable, just need a 3rd stack and some more IR)
- It's not particularly good
- Missing some random operators because I haven't bothered to implement them
- TODO: figure out how the hell automated tests are going to work

## Examples

[ TODO ]
