# A C to befunge compiler

## Big thanks to: 

- Nora Sandler's [Writing a C Compiler](https://norasandler.com/book/) book for helping guide this project (and for being a great read regardless)
- Mikescher's [BefunExec](https://github.com/Mikescher/BefunExec) for being an excellent befunge debugger
- Notes from [Jeffrey Lee's site](https://www.phlamethrower.co.uk/index.php) about their [prototype befunge -> C compiler](https://www.phlamethrower.co.uk/befunge/c2b.php) and their [general befunge insights](https://www.phlamethrower.co.uk/befunge/)
- The [lang_c](https://docs.rs/lang-c/latest/lang_c/index.html) crate for doing the lexing and parsing of c for me (i love skipping 1/2 of the work)

## Notes about the implementation
The top left 10x10 corner is a sort of zero page as it can be indexed with only 3 characters (ie 05g for getting the value at 0, 5), and is used as the registers (more specifically, `00` is the stack pointer, `10` is the call stack pointer, `20` is the return value, and the rest are general purpose, so register ID 1 is `30`, register 8 is `01`)

There is a custom stack, which is where *all state* is stored. The actual befunge stack is only used to perform individual operations, with data going right back onto the cstack. Indexing of this stack is stack frame relative only, which makes it nice and easy to work with.

As zero is always on the stack (if empty), I could theoretically make getting the stack register just `g` instead of `00g`, but that's going to cause mayhem if the stack ever leaks even a single value. (similarly i could make `1g`, `2g` be the call stack & return value, but they're less relevant really)

There is also a call stack which keeps track of previously called functions, and where you exited from them.

Function calls/returns use *two* indexes, one for the function ID (where 1 is main, and everything else is arbitrary), and the return location in the function (where 0 is the start) because after a call you need to return back to the position in the function where you were.

Calling convention: Return values just go in `20` in the zero-page, and function parameters go onto the start of the function's stack, so if a function takes args, stack(0) will be the value of the first one. This allows an arbitrarily large number of args, if needed.

For ease of C compilation we just always move the value from the return register onto some stack value but that would theoretically be optimizable away in a register allocation pass, so I think it's the best choice.

While I haven't actually implemented it for the C -> IR phase, the IR would fully support inline loops, gotos, etc.

### TLDR: Stuff it supports
- Stack frames for each function
- Call stack for returning
- Function calling with an arbitrary number of args
- Returning can go to specific parts of a function
- Inline jumps and loops

### Current Limitations
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

None of these are full architectural failures though (aside from perhaps linking) so should be resolvable.

## Examples

### One function
This C
```c
int main(void) {
 int a = 10 * 25;
 return a * a;
}
```
Becomes this IR
```
FUNC "main"
Two(Mult, Immediate(10), Immediate(25), Stack(1))
One(Copy, Stack(1), Stack(2))
Two(Mult, Stack(2), Stack(2), Stack(3))
Return(Stack(3))
```
Which then becomes this befunge
```b93
v!R#######
v#########            STACK -> xx###################
v#########       CALL STACK -> xx###################
v#########
v#########
v#########
v#########
v#########
v#########
v#########
>"!"00p 010g2-2pv
>v  10          <
 >:#v_  $$ 20g . @
 v  <
 >1-:v                                                                                                         
^                                                                                                      < 
 v   _$ >:#^_$19+""*00g1+1p 00g1+1g00g2+1p 00g2+1g00g2+1g*00g3+1p 00g3+1g20p00g3-00p10g1-:2g\1-:2g\10p^ 
```

### Recursive fib
This C
```c
int main(void) {
 int b = 10;
 return fib(b);
}


int fib(int i) {
  if (i == 0 || i == 1) {
    return 1;
  } else {
    return fib(i - 1) + fib(i - 2);
  }
}
```
Becomes this IR
```
FUNC "main"
One(Copy, Immediate(10), Stack(1))
Call("fib", [Stack(1)])
One(Copy, Register(0), Stack(2))
Return(Stack(2))

FUNC "fib"
Two(Equal, Stack(1), Immediate(0), Stack(2))
CondBranch(NonZero, "skip.1", Stack(2))
Two(Equal, Stack(1), Immediate(1), Stack(3))
CondBranch(NonZero, "skip.1", Stack(3))
One(Copy, Immediate(0), Stack(4))
AlwaysBranch("end.2")
Label("skip.1")
One(Copy, Immediate(1), Stack(4))
Label("end.2")
CondBranch(Zero, "else.10", Stack(4))
Return(Immediate(1))
AlwaysBranch("else.11")
Label("else.10")
Two(Sub, Stack(1), Immediate(1), Stack(5))
Call("fib", [Stack(5)])
One(Copy, Register(0), Stack(6))
Two(Sub, Stack(1), Immediate(2), Stack(7))
Call("fib", [Stack(7)])
One(Copy, Register(0), Stack(8))
Two(Add, Stack(6), Stack(8), Stack(9))
Return(Stack(9))
Label("else.11")
```
Which then becomes this befunge
```b93
v!R#######
v#########            STACK -> xx###################
v#########       CALL STACK -> xx###################
v#########
v#########
v#########
v#########
v#########
v#########
v#########
>"!"00p 010g2-2pv
>v  10          <
 >:#v_  $$ 20g . @
 v  <
 >1-:v                                                                                                                           
        ^-1<                                                                                                               
        >:#^_$                                                          v                                                  
^                                                                      <                                                 < 
        ^-1<                                                                                                                           
 v   _$ >:#^_$19+00g1+1p 00g1+1g00g3+1p0200g2+00p110g2p110g1+2p10g2+10p^> 20g00g2+1p 00g2+1g20p00g2-00p10g1-:2g\1-:2g\10p^ 
 >1-:v                                                                                                                                                                                                                                                                                                                                                                             
        ^-1<                                                                                                                                                                                                                                                                                                                                                                 
        >:#^_$                                                                                                                                                                                                                                                                                   v                                                                           
        ^-1<                                                                                                                                                                                                                                                                                                                                                                 
        >:#^_$                                                                                                                                                                                                    v                                                                                                                                                          
^                                                                                                                                         <                                                                      <                                                                              <                                                                        <   
        ^-1<                                                                                                                                                                                                                                                                                                                                                                             
 v   _$ >:#^_$00g1+1g0-!00g2+1p 00g2+1g#v_ 00g1+1g1-!00g3+1p 00g3+1g#v_ 000g4+1p v > 100g4+1p > 00g4+1g!#v_ 120p00g9-00p10g1-:2g\1-:2g\10p^ v > 00g1+1g1-00g5+1p 00g5+1g00g19++1p0200g9+00p210g2p110g1+2p10g2+10p^> 20g00g6+1p 00g1+1g2-00g7+1p 00g7+1g00g19++1p0200g9+00p210g2p210g1+2p10g2+10p^> 20g00g8+1p 00g6+1g00g8+1g+00g9+1p 00g9+1g20p00g9-00p10g1-:2g\1-:2g\10p^ > 
                                                                                                                                            >                                                                                                                                                                                                                              ^ 
                                        >                            >             ^                                                                                                                                                                                                                                                                                         
                                                                                 >            ^                                                                                                                                                                                                                                                                              
                                                                                                         >                                    ^                                                                                                                                                                                                                              
```

### A bunch o functions
```c
int main(void) {
 int b = 10;
 return func_d(func_d(b));
}

int func_a(int a) {
  return func_b(a + 1);
}

int func_b(int a) {
  return a + 1;
}

int func_c(int a) {
  return func_a(a + 1);
}

int func_d(int a) {
  return func_c(a + 1);
}
```
->
```
FUNC "main"
One(Copy, Immediate(10), Stack(1))
Call("func_d", [Stack(1)])
One(Copy, Register(0), Stack(2))
Call("func_d", [Stack(2)])
One(Copy, Register(0), Stack(3))
Return(Stack(3))

FUNC "func_a"
Two(Add, Stack(1), Immediate(1), Stack(2))
Call("func_b", [Stack(2)])
One(Copy, Register(0), Stack(3))
Return(Stack(3))

FUNC "func_b"
Two(Add, Stack(1), Immediate(1), Stack(2))
Return(Stack(2))

FUNC "func_c"
Two(Add, Stack(1), Immediate(1), Stack(2))
Call("func_a", [Stack(2)])
One(Copy, Register(0), Stack(3))
Return(Stack(3))

FUNC "func_d"
Two(Add, Stack(1), Immediate(1), Stack(2))
Call("func_c", [Stack(2)])
One(Copy, Register(0), Stack(3))
Return(Stack(3))
```
->
```b93
v!R#######
v#########            STACK -> xx###################
v#########       CALL STACK -> xx###################
v#########
v#########
v#########
v#########
v#########
v#########
v#########
>"!"00p 010g2-2pv
>v  10          <
 >:#v_  $$ 20g . @
 v  <
 >1-:v                                                                                                                                                                                       
        ^-1<                                                                                                                                                                           
        >:#^_$                                                                                                                      v                                                  
        ^-1<                                                                                                                                                                           
        >:#^_$                                                          v                                                                                                              
^                                                                      <                                                           <                                                 < 
        ^-1<                                                                                                                                                                                       
 v   _$ >:#^_$19+00g1+1p 00g1+1g00g4+1p0500g3+00p110g2p110g1+2p10g2+10p^> 20g00g2+1p 00g2+1g00g4+1p0500g3+00p110g2p210g1+2p10g2+10p^> 20g00g3+1p 00g3+1g20p00g3-00p10g1-:2g\1-:2g\10p^ 
 >1-:v                                                                                                                                 
        ^-1<                                                                                                                     
        >:#^_$                                                                v                                                  
^                                                                            <                                                 < 
        ^-1<                                                                                                                                 
 v   _$ >:#^_$00g1+1g1+00g2+1p 00g2+1g00g4+1p0300g3+00p210g2p110g1+2p10g2+10p^> 20g00g3+1p 00g3+1g20p00g3-00p10g1-:2g\1-:2g\10p^ 
 >1-:v                                                                     
^                                                                  < 
 v   _$ >:#^_$00g1+1g1+00g2+1p 00g2+1g20p00g2-00p10g1-:2g\1-:2g\10p^ 
 >1-:v                                                                                                                                 
        ^-1<                                                                                                                     
        >:#^_$                                                                v                                                  
^                                                                            <                                                 < 
        ^-1<                                                                                                                                 
 v   _$ >:#^_$00g1+1g1+00g2+1p 00g2+1g00g4+1p0200g3+00p410g2p110g1+2p10g2+10p^> 20g00g3+1p 00g3+1g20p00g3-00p10g1-:2g\1-:2g\10p^ 
 >1-:v                                                                                                                                 
        ^-1<                                                                                                                     
        >:#^_$                                                                v                                                  
^                                                                            <                                                 < 
        ^-1<                                                                                                                                 
 v   _$ >:#^_$00g1+1g1+00g2+1p 00g2+1g00g4+1p0400g3+00p510g2p110g1+2p10g2+10p^> 20g00g3+1p 00g3+1g20p00g3-00p10g1-:2g\1-:2g\10p^ 
```
