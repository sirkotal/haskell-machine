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
We created seven different functions (one for each of the instructions listed above) that take a stack as an argument and return an updated stack, based on their respective operations. We also developed a ```valToString``` auxiliary function that returns a stack value in string format.
Afterwards, we implemented the ```createEmptyStack``` and the ```stack2Str``` functions; the former creates an empty stack and the latter is a recursive function that translates the contents of the stack into a string.
```stack2Str``` handles three different situations:

1. ***The stack is empty*** → The function returns an empty string, indicating that the stack has no elements left.
2. ***The stack has one element*** → If the stack only has one element left (becomes empty after popping an element), the function converts the value of the top element to a string.
3. ***Any other case*** → The function converts the value of the top element to a string and appends a comma to it; it is then recursively called to process the rest of the stack elements. The string returned by ```stack2Str``` is the concatenation of the string representation of the top element the stack, a comma, and the string representation of the rest of the elements in the stack.

```haskell
valToString :: SVal -> String
valToString (Integer x) = show x
valToString Tt = "True"
valToString Ff = "False"

createEmptyStack :: Stack
createEmptyStack = Stack []

stack2Str :: Stack -> String
stack2Str s = if isEmpty s
                then ""
              else if isEmpty (pop s)
                then valToString (top s)
              else
                valToString (top s) ++ "," ++ stack2Str (pop s)
```

We then implemented the ```Fetch String``` and ```Store String``` instructions via the ```fetch``` and ```store``` functions - both of them take a string, a stack and a state as arguments, but while ```fetch``` returns an updated stack, ```store``` updates both the stack and the state, returning them in a pair.
The ```fetch``` function is responsible for fetching a specific variable's value from the storage (state) and pushing it to the stack; if the variable isn't present in the storage, a runtime error message is displayed.
Meanwhile, the ```store``` function is responsible for pairing a variable with the value at the top of the stack and storing it in the storage; if a variable-value pair already exists, it merely updates the pair's second element.

The next step was to implement the ```createEmptyState``` and ```state2Str``` functions; the former creates an empty state and the latter translates the contents of the state into an alphabetically ordered string. To help with implementing ```state2Str```, we created the ```pairToStr``` function, which converts a ```(Var, Val)``` pair into a string by concatenating the variable with ```=``` and the value.
The ```state2Str``` function itself firstly sorts the state by the first element of each pair (the variable's name) and then proceeds to apply the ```pairToStr``` function to every pair from the sorted list. The last step of the function is to combine the string representations of the sorted pairs into a single string, separated by commas.

```haskell
createEmptyState :: State
createEmptyState = State []

pairToStr :: (Var, Val) -> String
pairToStr (var, val) = var ++ "=" ++ valToString val

state2Str :: State -> String
state2Str (State sta) = let sorted = sortBy (comparing fst) sta in 
                        intercalate "," (map pairToStr sorted)
```

Finally, we created an auxiliary function ```execute```, which receives an instruction (```Inst```), a stack and a state as arguments and returns a ```(Stack, State)``` pair, to help us implement the ```run``` function. We took advantage of the implementation of ```execute``` to implement the ```Noop``` instruction, since it is only a dummy meant to return the input stack and store.

Then, we implemented the ```run``` function, which receives a ```(Code, Stack, State)``` tuple (where ```type Code = [Inst]```) as an argument and returns a tuple of the same structure; it is responsible for running the machine (interpreting the code received and executing it).
We also took advantage of the function's implementation to implement the ```Branch Code Code``` and ```Loop Code Code``` instructions directly into the definition of ```run```.

```haskell
execute :: Inst -> Stack -> State -> (Stack, State)
execute (Push n) stk sta = (push (Integer n) stk, sta)
execute Add stk sta = (add stk, sta) 
execute Mult stk sta = (mul stk, sta)
execute Sub stk sta = (sub stk, sta) 
execute Tru stk sta = (push Tt stk, sta) 
execute Fals stk sta = (push Ff stk, sta) 
execute Equ stk sta = (eq stk, sta) 
execute Le stk sta = (le stk, sta) 
execute And stk sta = (Main.and stk, sta)
execute Neg stk sta = (neg stk, sta) 
execute (Fetch s) stk sta = ((fetch s sta stk), sta) 
execute (Store s) stk sta = store s sta stk
execute Noop stk sta = (stk, sta) 

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stk, sta) = ([], stk, sta)
run (inst : rest, stk, sta) = case inst of
                                Branch c1 c2 -> if (valToString (top stk)) == "True"
                                                    then run (c1 ++ rest, (pop stk), sta)
                                                else if (valToString (top stk)) == "False"
                                                    then run (c2 ++ rest, (pop stk), sta)
                                                else
                                                    run (rest, stk, sta)
                                Loop c1 c2 -> run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ rest, stk, sta)
                                _ -> let (new_stk, new_sta) = execute inst stk sta in run (rest, new_stk, new_sta)
```

### Part 2

The second part of the project consisted in defining a translation (a compiler) from a small imperative programming language (with arithmetic and boolean expressions, statements consisting of assignments of the form ```x := a```, sequences of statements ```(instr1 ; instr2)```, ```if...then...else``` statements and ```while``` loops) into lists of instructions in the previous machine.

The first step was to define three data types in Haskell to represent the expressions and statements of this imperative language: ```Aexp``` (arithmetic expressions), ```Bexp``` (boolean expressions) and ```Stm``` (statements).

```haskell
data Aexp = Num Integer    
            | Var String          
            | Sum Aexp Aexp         
            | Subt Aexp Aexp           
            | Mul Aexp Aexp deriving Show

data Bexp = BoolVal Bool          
            | Equal Aexp Aexp 
            | EqualBool Bexp Bexp   
            | LeEq Aexp Aexp        
            | LogAnd Bexp Bexp 
            | Not Bexp deriving Show

data Stm = Assign String Aexp          
            | Seq Stm Stm                
            | If Bexp Stm Stm            
            | While Bexp Stm deriving Show

type Program = [Stm]
```

We then implemented the ```compile``` function - a compiler from a program in this small imperative language into a list of machine instructions (as defined in Part 1 of the project). Alongside the main compiler function, we were also required to develop two additional auxiliary functions which compile arithmetic and boolean expressions: ```compA``` and 
```compB```, respectively.

To facilitate the realization of the intended code and manage potential recursion, we opted to construct multiple versions of the different statements that could be present in the ```Program``` input.

```haskell
compile :: Program -> Code
compile [] = []
compile ((Assign var expr):stmts) = compA expr ++ [Store var] ++ compile stmts
compile ((If cond thenBody elseBody):stmts) = compB cond ++ [Branch (compile [thenBody]) (compile [elseBody])] ++ compile stmts
compile ((While cond thenBody):stmts) = [Loop (compB cond) (compile [thenBody])] ++ compile stmts
```

Finally, the last thing we had to implement was a parser function (```parser```) that transforms an imperative program (represented as a string) into its corresponding representation in the ```Stm``` data type (a list of ```Stm``` statements).

-> Description of the parser implementation + code <-

## Conclusions

We managed to successfully implement the low-level machine and the compiler in GHCi ≥9.0.0.

One of the main issues we faced while developing this project was the amount of time it took to implement the parser, since we had no previous contact with this type of program in any class.
At times, it was also hard to understand what was asked of us in the project's specification.

All in all, however, this project allowed us to consolidate our Haskell knowledge.

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
- <https://youtu.be/dDtZLm7HIJs>
