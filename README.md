# A C to befunge compiler

It's a C compiler that outputs (64 bit) befunge93 programs instead of assembly. Roughly targeting c11, but strict conformance is not a primary goal.

## Big thanks to: 

- Nora Sandler's [Writing a C Compiler](https://norasandler.com/book/) book for helping guide this project (and for being a great read regardless)
- Mikescher's [BefunExec](https://github.com/Mikescher/BefunExec) for being an excellent befunge debugger
- Notes from [Jeffrey Lee's site](https://www.phlamethrower.co.uk/index.php) about their [prototype befunge -> C compiler](https://www.phlamethrower.co.uk/befunge/c2b.php) and their [general befunge insights](https://www.phlamethrower.co.uk/befunge/)
- The [lang_c](https://docs.rs/lang-c/latest/lang_c/index.html) crate for doing the lexing and parsing of c for me (i love skipping 1/2 of the work)
- The [Berkeley SoftFloat](https://www.jhauser.us/arithmetic/SoftFloat.html) library, which is used for floating point operations in befunge

## Befunge interpreter assumptions
I've been testing this with [BefunExec](https://github.com/Mikescher/BefunExec), [RBeJ](https://github.com/PartyWumpus/RBeJ) and my [befunge-editor](https://github.com/PartyWumpus/befunge-editor), so I'd recommend one of those.
- Infinite (or at least a lot of) positive fungespace (right and downwards from 0,0)
- All negative fungespace positions are zeros (that assumption used for bitshifts)
- 64 bit signed values on the stack
- 64 bit values in fungespace

## Important usage note
Requires being run in this repo, because the preproccessor step (for now) requires the befunge_libc/ folder to exist.
The befunge_libc folder contains:
- `stdlib`: a minimal copy of libc
- `internal`: various c functions that only the compiler should insert calls to
- `softfloat`: the Berkeley SoftFloat (version 3e) library, internally used for all double/float operations

## Notes about the implementation

The top left 10x10 corner is a sort of zero page as it can be indexed with only 3 characters (ie 05g for getting the value at 0, 5), and is used as the registers (more specifically, `00` is the stack pointer, `10` is the call stack pointer, `20` is the return value, and the rest are general purpose, so register ID 1 is `30`, register 8 is `01`)

There is a custom stack (cstack), which is where most state is stored. The actual befunge stack (bstack) is only used to perform individual operations, with data going right back onto the cstack. Indexing of this stack is stack frame relative only, which makes it nice and easy to work with.

There is also a call stack which keeps track of previously called functions, and where you exited from them.

Function calls/returns use *two* indexes, one for the function ID (where 1 is main, and everything else is arbitrary), and the return location in the function (where 0 is the start) because after a call you need to return back to the position in the function where you were.

Note: As zero is always on the bstack (if empty), I could theoretically make calls to functions just be the ID, as the initial entry position is 0, and that'd be included for free. This would cause mayhem if any value ever leaked onto the bstack though, so idk.

Calling convention: Return values just go in `20` in the zero-page, and function parameters go onto the start of the function's stack, so if a function takes args, stack(0) will be the value of the first one. This allows an arbitrarily large number of args, if needed.

For ease of C compilation we just always move the value from the return register onto some stack value but that would theoretically be optimizable away in a register allocation pass, so I think it's the best choice.

There's also a static memory space used for globals.

### TLDR: Stuff it supports
- All c operators, aside from `alignof` and some uses of `,`
- Stack frames for each function
- Call stack for returning, including returning to the middle of a function
- Function calling with an arbitrary number of args (no varargs yet)
- Inline if statements and loops
- Pointers to stack locals, static data and dynamically allocated memory
- Multidimensional arrays
- Clear error messages
- Global variables

### Current Limitations
None of these are full architectural failures so will eventually be resolved.
- Enums are incorrectly implemented, but largely functional
- Doubles can only be created and pointed to
- Overflow is incorrectly handled for ints and unsigned longs (correct for all chars and signed longs though)
- No var args
- Some invalid lvalues are incorrectly allowed (stuff like `func().a = 5`)
- Function pointers can't be used (but can be constructed)
- Type qualifiers like `const`, `volatile` and `restrict` are not supported
- Bitshifts and bitwise operations are wrong (i think?) for negative values
- `malloc` is just a dumb linear allocator with `free()` being a no-op
- Doesn't do a register allocation pass or any other optimizations, which would probably make it a decent bit faster
- Uses gcc for preprocessing (ie #includes), so requires gcc installed to work (step one to making a c compiler, use another c compiler...)

### ASM support
This compiler allows for use of the GNU ASM extensions for inline befunge. (either for extra optimized code or for using special befunge operators like `,`, `.` for output and `&`, `~` for input).

There are two important assumptions to keep in mind when writing inline befunge:
- The entry point is always the the top left heading right, and the exit point **must** be the top right heading right. All compiler produced branches from the rest of your function and similar will be moved downwards below your inline befunge, feel free to make it as long and as tall as you want.
- The bstack should be left *empty* on end, as optimizations may (although do not yet) assume the bstack is empty.

#### Basic asm support
[Basic GNU asm](https://gcc.gnu.org/onlinedocs/gcc/Basic-Asm.html) can be used to insert inline befunge, although I don't see it being particularly useful without being able to access C values.
##### Example
```c
int main(void) {
 int a = 10;
 __asm__(
  "v INLINE BEFUNGE :3 >"
  ">    76*::::....    ^"
 );
 return;
}
```
->
```
19+00g1-1p v INLINE BEFUNGE :3 > 020p00g1-00p10g1-:2g\1-:2g\10p^ 
           >    76*::::....    ^
```

And just for clarity, here's the same example without inline befunge
```c
int main(void) {
 int a = 10;
 return;
}
```
-> 
```b93
19+00g1-1p 020p00g1-00p10g1-:2g\1-:2g\10p^ 
```

#### Advanced asm support
[Advanced GNU ASM](https://gcc.gnu.org/onlinedocs/gcc/Extended-Asm.html) is more useful, as it allows you to tell the compiler which C values you want to use.

The normal way the ASM extension works is by templating your string using the inputs/outputs you've defined, but templating befunge is kinda difficult, so instead your inputs are loaded to your given registers before the asm runs and your outputs are loaded from the registers to the c values.

The empty `""` is for 'constraints' which I have not yet properly implemented.

The exact format for how I'm going to pick register locations, what a register ID actually means etc is subject to change, but currently `r2` is `40g`, `r3` is `50g`, etc.

As well as registers with `[r1]` to `[r97]`, you can use `[bstack]`, which will leave the value on the top of the bstack or read it from the top of the bstack. Again important to note that you should not leak any more values onto the bstack at the end beyond the number of `bstack` values in the listed outputs of the asm.

##### Register example
```c
int main(void) {
 int a = 10;
 __asm__(
  // read value from register 34, add 5, and write back to register 34
  "34g 5+ 34p"
  "befunge93!"
  : ["r34" (a)] // (outputs) copy register 34 into a
  : ["r34"  (a)] // (inputs) copy a into register 34
 );
 return a; // Returns 15
}
```
->
```
19+00g1-1p 00g1-1g34p 34g 5+ 34p 34g00g1-1p 00g1-1g20p00g1-00p10g1-:2g\1-:2g\10p^
                      befunge93!
```
Just to make it super clear exactly what is going on, here's the generated IR, with a copy to r2 before, and a copy from r2 after.
```
One(Copy, Immediate(10), Psuedo("a.1"))
One(Copy, Psuedo("a.1"), Register(34))
InlineBefunge(["34g 5+ 40p", "befunge93!"])
One(Copy, Register(34), Psuedo("a.1"))
Return(Psuedo("a.1"))
```

##### `bstack` example
This does the same as the example above, but writes and reads from the bstack instead of using a register.
```c
int main(void) {
 int a = 10;
 __asm__("5+": ["bstack" (a)]: ["bstack" (a)]);
 return a;
}
```
-> `19+00g1-1p 00g1-1g 5+ 00g1-1p 00g1-1g20p00g1-00p10g1-:2g\1-:2g\10p^`

##### Printing
```c
int print_int(int a, int b) {
  __asm__("." : : ["bstack" (a)]);
  return 0;
}
```
-> `00g1-1g . 020p00g1-00p10g1-:2g\1-:2g\10p`

## Generic Examples

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
  int x = 0xA;
  return fib(x);
}

int fib(int a) {
  if (a == 0 || a == 1) {
    return 1;
  }
  return fib(a - 1) + fib(a - 2);
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
CondBranch(NonZero, "skip.3", Stack(2))
Two(Equal, Stack(1), Immediate(1), Stack(3))
CondBranch(NonZero, "skip.3", Stack(3))
One(Copy, Immediate(0), Stack(4))
AlwaysBranch("end.4")
Label("skip.3")
One(Copy, Immediate(1), Stack(4))
Label("end.4")
CondBranch(Zero, "else.12", Stack(4))
Return(Immediate(1))
Label("else.12")
Two(Sub, Stack(1), Immediate(1), Stack(5))
Call("fib", [Stack(5)])
One(Copy, Register(0), Stack(6))
Two(Sub, Stack(1), Immediate(2), Stack(7))
Call("fib", [Stack(7)])
One(Copy, Register(0), Stack(8))
Two(Add, Stack(6), Stack(8), Stack(9))
Return(Stack(9))
```
Which then becomes this befunge
```b93
v!R#######
v#########            STACK -> ################################################...
v#########       CALL STACK -> ################################################...
v#########    STATIC MEMORY -> ################################################...
v#########        // IN FUTURE, malloc() mem could be a 4th mem location here
v#########
v#########
v#########
v#########
v#########
>"!"00p 010g2-2pv
v               <
0
1
>v
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
        >:#^_$                                                                                                                                                                                                                                                                                 v                                                                         
        ^-1<                                                                                                                                                                                                                                                                                                                                                             
        >:#^_$                                                                                                                                                                                                  v                                                                                                                                                        
^                                                                                                                                         <                                                                    <                                                                              <                                                                        < 
        ^-1<                                                                                                                                                                                                                                                                                                                                                                         
 v   _$ >:#^_$00g1+1g0-!00g2+1p 00g2+1g#v_ 00g1+1g1-!00g3+1p 00g3+1g#v_ 000g4+1p v > 100g4+1p > 00g4+1g!#v_ 120p00g9-00p10g1-:2g\1-:2g\10p^ > 00g1+1g1-00g5+1p 00g5+1g00g19++1p0200g9+00p210g2p110g1+2p10g2+10p^> 20g00g6+1p 00g1+1g2-00g7+1p 00g7+1g00g19++1p0200g9+00p210g2p210g1+2p10g2+10p^> 20g00g8+1p 00g6+1g00g8+1g+00g9+1p 00g9+1g20p00g9-00p10g1-:2g\1-:2g\10p^ 
                                                                                 >            ^                                                                                                                                                                                                                                                                          
                                                                                                         >                                  ^                                                                                                                                                                                                                            
                                        >                            >             ^                                                                                                                                                                                                                                                                                     
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

### Printing
```c
void print_int(int a) {
  __asm__("." : : [bstack] "" (a));
  return;
}

void print_char(int a) {
  __asm__("," : : [bstack] "" (a));
  return;
}
```
