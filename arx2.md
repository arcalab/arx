# ARx2

New ideas for a global language.

Flow:

 1. Full ARx2
 2. Plain ARx2
 3. Typed
 4. Just Automata + data
 

## 1. Full ARx

### Enums
```
enum Bool = True | False; // native
enum Unit = U; // native
enum Actions = Jump | Left | Right | Stop;
```
Operations: 
```
a > b; // == != <= >=
a && b; // || !. =>
```

### Data types
```
data List<type> = Nil | Cons(type,List<type>); // not in prelude
data Int = (native);
```
Operations:
```
isCons(a); getCons0(a); Cons(a,Nil);
a+b; // - * / %
```

### Automata (old sb)
```
aut name(in1:type,in2:type): type = {
  clock ...;
  init m1:type=...; m2:type=...;
  return out1:type;
  [rule1] from(in1,...), m1==in2, notAt(m3) if secure.level -->
          out1:=m1, m2':=m2+1,t:=0,to(m3);
  [rule2] from(m3),.... 
}
```
(optional types, rule names, ...)
(bad rules do not change the state - undesirable loops?)

### Primitive Streams (can be seen as automata, but can be later optimised)
```
once<exp1,exp2,...> // return "exp_i" at a time and stop
val<exp1,exp2,...> // return "exp_i" at a time and repeat
const x:Int = 42; // macro, expandable statically
```

### Architecture (composition of aut)
```
aut1(in1) -> out1,out2;
aut2(ou1,in2) -> out3;
aut3(aut1(in));
myName@aut1(in3);
//// syntactic sugar
a -()-> b   ; // fifo1
a -(42)-> b ; // fifo1full
a -{}-> b   ; // var(b)
a -{43}-> b ; // var<43>
a -+-> b,c  ; // xor
a -x-> b,c  ; // a->b;a->c;
a --- b     ; // drain(a,b) 
a,b -> c    ; a->c; b->c

def arch1(in1:type,in2:type): AType = {
  aut -> p1,p2;
  aut;
  return p1; // order is not relevant
}
```
Note: FIX composition to avoid combining interleavings (very compact composition with fifos (harder export to mCRL2?))

### Verification
```
checkCTL A[] myName.m1<=50;
```

### Other ideas / extensions
```
configure {
  myConstant: int = 10;
  oneOf {
    secure {level:Int;},
    open
  }
}

product P1 {
  secure {level = 5}
}

protocol ABC {
  a->b:x; (b->a:ok + b->a:no)
  +
  ....
}

test {
  aut1(in) -> out1,out2
  checkCTL aut1.m1>2 --> out2<1 
}

mytask@task(W a!,10 b?, TO c!)

hybrid name(p) = {
  init v:=2; // initial velocity (fixed)
  while true do {
    if v<=10
    then p'=v,v'=5  for 1
    else p'=v,v'=-2 for 1
  }
  return p;
}

(auth![0..1] -> auth?[1..5])
(a or b send-to c or d or e: a->ab;b->ab;val<U>->ab-+->c,d,e)
```

## 2. Plain ARx2

Enums and data types remain. Constants are expanded. Type checking comes after.

### Automata
```
aut name<m1:type,m2:type>(in1,in2)={
  clock ...
  init ...
  return ...
  [rule1] get(in1...), m1==in2, und(m3), ASK(m1,m2,in2) -->
          out1:=m1, m2':=m2+1, t:=0, m3=U;
  ...
}
```

### Architecture (without synt sugar)
```
aut1(in1) -> out1,out2;
aut2(ou1,in2) -> out3;
aut1(in)-> #v1,#v2 ; aut3(#v1,#v2);
myName@aut1(in3);
//// syntactic sugar
fifo(a)      -> b ;
fifofull<42> -> b ;
var(a)       -> b   ;
varFull<42>(a) -> b ; 
xor(a)       -> b,c  ; // xor
a->b;a->c;             // a -x-> b,c 
drain(a,b);            // a --- b
```

...

### Updated composition of Automata

If 3 pairs of agents communicate in parallel, all combinations of communications together are taken into account. We want to keep them apart: if 2 rules (or guarded commands) can be fired at the same time and are not (locally) conflicting, then this behaviour is implicit.

#### Conflicts

Define: 

 - __Local Conflict__ between 2 rules: if they share inputs, outputs, or variables.
 - __Global Conflict__ between 2 rules: if they produce a ill-formed rule, defined below.
 
#### Memory vs Streams/ports

 Need to be more clear distinguishing between memories and ports.
  
  - If `b` is a port, `c:=b,b:=a` means that the value written from `a` and to `c` is the same.
  - If `m` is a memory, `c:=m,m:=a` means that the value read by `c` is the old value of `m`, and `m` gets a new value from `a`.
  
Suggestion: write `m':=a` to mean "the new value of `m` comes from `a`". Ports cannot have a next value (have no memory).

#### Well-formed rule

Two rules from different automata cannot be composed if they are badly formed.
A rule `get(g),ask(a),und(u),pred -> w:=r` is well formed if:

 - Disjoint sets `g,a,u`
 - all variables `pred,r` are in `g,a,w`;
 - all variables `pred,w,r` are not in `u`;
 - variable `m` and `m'` are different (e.g., `und(m)->m':=5` and `und(m')->m:=5` are ok, but `und(m)->m:=5` is not.)
 - `w` cannot have repeated variables (no multiple assignments)
 

#### Combine rules (from different or same automata)

Merge all `g,a,e,pred,w,r`, remove all `w` from `g` and `a` (mixed ports from guards - mixed ports are still marked as input ports), and check if the result is well-formed.

#### Safe hiding

Hiding is an operation that:

 - removes ports from `w` that are not needed, replacing by their definition;
 - keeps all inputs and outputs and memories;
 - SHOULD throw an error if data loops are found in the process (not yet);
 - removes rules with mixed ports mention in `g,a`, and drop mixed ports from inputs.

Hiding should NOT affect the behaviour (only simplify rules based on what can be composed). This is not always clear (e.g., if a port exists to guarantee 2 rules are exclusive, removing it can allow them to fire simultaneously.)

#### Go alone

A rule can go alone if it has no outputs from the other automata.

#### Go together

Assume disjoint memories.
Two rules from 2 automata `r1,r2` can go together if:
 
 - at least one cannot go alone
 - composed rule is well-formed
 - considering inputs of `A1`,
   + inputs of `r1` have all outputs of `r2`
   + in+out of `r1` have all inputs  of `r2`
 - vice-versa

Two sets of rules from 2 automata `rs1,rs2` can go together if:

 - all `rsi` are locally conflict-free (disjoint)
 - composition is well-formed
 - minimal (cannot go together by dropping any rule that can go alone)




## Tools of-the-shelf

- Build composed automata (guarded commands)
  + Display its automata
- Display each primitive automata
- Display architecture
- Provide stats (nr states, edges, ...)

## More complex tools

- to Uppaal (compose complex sync parts first)
  + verify Uppaal properties (check blocks)
- to mCRL2
  + verify hierarchical properties
  + actions can be ports firing or rules triggering
- simulate
  + ignore time initially
  + side-by-side:
    * list of rules that can be applied (with names)
    * architecture (with minimal state, or nr of defined vars)
    * individual automata (with current state, or list of defined vars)
    * sequence diagram!
    * maximum info with mouse over
      - connecting source code, list of rules, automata, architecture, etc.
    * plots of hybrid systems, if they exist

Editor improvements:

- save/load:
  + program;
  + Examples (library with imports - modules!)
- Custom layout:
  + View-settings button, saying which widgets to include in each column, and order
  + Options with "perspectives", with pre-defined set of widgets
  + When saving a program include a annotated block (not displayed in the editor) with layout information.
- Improve connection between widgets
    * widgets can be active or not
    * widgets have dependencies and an output
    * widgets have an increasing version number
    * Global and Local reloads:
      - Global updates the global version and triggers ALL active widgets to reload,
      - Local updates the global version and triggers the current widget to reload,
      - When a widget reloads, if the current version is lower than global, updates its global version and THEN asks dependencies to update; otherwise does nothing.
      - Refresh is NOT reload: refresh only updates the layout (if active), and reload triggers updates from dependencies.

Also nice to have:

- ARx4fun:
  + import set of modules, have basic code
  + ask to complete an automata or an architecture
  + verify property, and give trace with error until success. 
  

ReoLive Reborn!

- Fork ReoLive
- Rebrand it - ARx? ArcaTools?
- Start small and simple
- Add better support for core widgets as in Caos
- Add support for server-client as in ReoLive (try to simplify reuse)

  
