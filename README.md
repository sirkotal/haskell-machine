# Haskell Machine

## PFL Practical Work - Haskell Machine Instructions + Compiler

### Developed by:

- João Pedro Rodrigues Coutinho, up202108787 (50%)
- Miguel Jorge Medeiros Garrido, up202108889 (50%)

## Project Description

### Part 1

The first part of the project consisted in implementing a low-level machine with configurations of the form (*c*, *e*, *s*) where *c* is a list of instructions (or code) to be executed, *e* is the evaluation stack, and *s* is the storage.
The evaluation stack is used to evaluate arithmetic (composed of integer numbers only, which can be positive or negative) and boolean expressions.

Therefore, we implemented the following machine instructions: ```Push Integer, Add, Mult, Sub, Tru, Fals, Equ, Le, And, Neg, Fetch String, Store String, Noop, Branch Code Code, Loop Code Code```.

The first thing we developed were data types for the stack (```Stack```) and its elements (```SVal```), which could either be an integer (```Integer Integer```) or one of two boolean constants (```Tt``` or ```Ff```). We then created a ```State``` data type to represent the storage of the machine - a list of ```(String, SVal)``` pairs. 

```haskell
data SVal = Integer Integer
          | Tt
          | Ff deriving Show

data Stack = Stack [SVal] deriving Show

type Var = String
type Val = SVal
data State = State [(Var, Val)] deriving Show
```

After this, we implemented the first function - ```push```, that takes both a stack value (```SVal```) and a stack as arguments and returns an updated stack which has first argument (the value) at its top.

```haskell
push :: SVal -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)
```

Since the ```push``` function accepts both integers and boolean constants as arguments, it actually helped us implementing three machine instructions in just one function: ```Push Integer```, ```Tru``` and ```Fals```.

To assist in the implementation of further machine instructions, we also implemented some auxiliary functions to help managing the evaluation stack: 

- ```pop``` → pops the top of the stack
- ```top``` → shows the element at the top of the stack
- ```isEmpty``` → checks if the stack is empty

```haskell
pop :: Stack -> Stack
pop (Stack (_:xs)) = Stack xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> SVal
top (Stack (x:_)) = x
top _ = error "Stack.top: empty stack"

isEmpty :: Stack -> Bool
isEmpty (Stack [])= True
isEmpty (Stack _) = False
```

The next step was to implement all remaining instructions that relied solely on the evaluation stack (```Add, Mult, Sub, Equ, Le, And, Neg```), alongside the ```createEmptyStack``` and ```stack2Str``` functions.
We created seven different functions (one for each of the instructions listed above) that take a stack as an argument and return an updated stack, based on their respective operations.

## Conclusions



## Bibliography

- <https://zvon.org/other/haskell/Outputglobal/index.html>
- <https://www.educative.io/>
- <https://stackoverflow.com/>
- <https://www.tutorialspoint.com/haskell/index.htm>
- <https://hackage.haskell.org/>
- <https://hoogle.haskell.org/>
- <https://typeclasses.com/>
- <https://en.wikibooks.org/wiki/Haskell>
- <https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html>
