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
