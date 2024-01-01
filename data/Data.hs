-- PFL 2023/24 - Haskell practical assignment quickstart
import Data.Maybe (fromJust)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Map (insert, fromList, toList)
import Control.Exception (Exception, throwIO)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data SVal = Integer Integer
          | Tt
          | Ff deriving Show

-- Converts a stack value into a string.
valToString :: SVal -> String
valToString (Integer x) = show x
valToString Tt = "True"
valToString Ff = "False"

data Stack = Stack [SVal] deriving Show

type Var = String
type Val = SVal
data State = State [(Var, Val)] deriving Show

-- Pushes a value into the stack; returns the updated stack.
push :: SVal -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)

-- Pops the value on the top of the stack; returns the updated stack.
pop :: Stack -> Stack
pop (Stack (_:xs)) = Stack xs
pop _ = error "Stack.pop: empty stack"

-- Returns the value on the top of the stack.
top :: Stack -> SVal
top (Stack (x:_)) = x
top _ = error "Stack.top: empty stack"

-- Performs an addition operation between the two values at the top of the stack; both values must be integers. It returns the updated stack.
add :: Stack -> Stack
add (Stack (Integer x : Integer y : xs)) = push (Integer (x + y)) (pop (pop (Stack (Integer x : Integer y : xs))))
add _ = error "Stack.add: need two integers at the top of the stack"

-- Performs a subtraction operation between the two values at the top of the stack; both values must be integers. It returns the updated stack.
sub :: Stack -> Stack
sub (Stack (Integer x : Integer y : xs)) = push (Integer (x - y)) (pop (pop (Stack (Integer x : Integer y : xs))))
sub _ = error "Stack.sub: need two integers at the top of the stack"

-- Performs a multiplication operation between the two values at the top of the stack; both values must be integers. It returns the updated stack.
mul :: Stack -> Stack
mul (Stack (Integer x : Integer y : xs)) = push (Integer (x * y)) (pop (pop (Stack (Integer x : Integer y : xs))))
mul _ = error "Stack.mul: need two integers at the top of the stack"

-- Performs a logical AND operation between the two values at the top of the stack; both values must be booleans. It returns the updated stack.
and :: Stack -> Stack
and (Stack (x : y : xs)) = case (valToString x, valToString y) of
                                    ("True", "True") -> push Tt (pop (pop (Stack (x : y : xs))))
                                    ("True", "False") -> push Ff (pop (pop (Stack (x : y : xs))))
                                    ("False", "True") -> push Ff (pop (pop (Stack (x : y : xs))))
                                    ("False", "False") -> push Ff (pop (pop (Stack (x : y : xs))))
                                    _ -> error "Run-time error"   -- Stack.and: need two booleans at the top of the stack

-- Performs a logical negation operation on the value at the top of the stack; the value must be a boolean. It returns the updated stack.
neg :: Stack -> Stack
neg (Stack (x : xs)) = case valToString x of
                                    ("True") -> push Ff (pop (Stack (x : xs)))
                                    ("False") -> push Tt (pop (Stack (x : xs)))
                                    _ -> error "Stack.neg: need a boolean at the top of the stack"

-- Performs a comparison between the two values at the top of the stack to check if they are the same; both values must be of the same data type. It returns the updated stack.
eq :: Stack -> Stack
eq (Stack (x : y : xs)) = case (x, y) of
                            (Integer x, Integer y) -> if (x == y)
                                                        then push Tt (pop (pop (Stack (Integer x : Integer y : xs))))
                                                      else 
                                                        push Ff (pop (pop (Stack (Integer x : Integer y : xs))))
                            (Tt, Tt) -> push Tt (pop (pop (Stack (x : y : xs))))
                            (Ff, Ff) -> push Tt (pop (pop (Stack (x : y : xs))))
                            (Tt, Ff) -> push Ff (pop (pop (Stack (x : y : xs))))
                            (Ff, Tt) -> push Ff (pop (pop (Stack (x : y : xs))))
                            _ -> error "Stack.le: need two values of the same type at the top of the stack"

-- Performs a comparison between the two values at the top of the stack to check if the first one (the topmost) is less or equal in value to the second one; both values must be integers. It returns the updated stack.
le :: Stack -> Stack
le (Stack (Integer x : Integer y : xs)) = case x <= y of
                                      True -> push Tt (pop (pop (Stack (Integer x : Integer y : xs))))
                                      False -> push Ff (pop (pop (Stack (Integer x : Integer y : xs))))
le _ = error "Stack.le: need two integers at the top of the stack"

-- Checks if the stack is empty.
isEmpty :: Stack -> Bool
isEmpty (Stack [])= True
isEmpty (Stack _) = False

-- Creates an empty stack.
createEmptyStack :: Stack
createEmptyStack = Stack []

-- Converts the stack into a string.
stack2Str :: Stack -> String
stack2Str s = if isEmpty s
                then ""
              else if isEmpty (pop s)              {- if the stack only has 1 element left -}
                then valToString (top s)
              else
                valToString (top s) ++ "," ++ stack2Str (pop s)

-- Creates an empty state.
createEmptyState :: State
createEmptyState = State []

-- Fetches a specific variable's value from the state and pushes it into the stack; returns the updated stack.
fetch :: String -> State -> Stack -> Stack
fetch str (State sta) stk = case lookup str sta of
                                      Nothing -> error "Run-time error"  -- Variable was not found in storage
                                      Just val -> push val stk

-- Stores a pair made of the variable passed as an argument and the value at the top of the stack into the state; returns a pair with the stack and the state - both updated.
store :: String -> State -> Stack -> (Stack, State)
store str (State sta) stk = if isEmpty stk
                                then error "Stack is empty"
                            else
                                (pop stk, State (toList (insert str (top stk) (fromList sta))))

-- Converts the state's variable-value pair into a string.
pairToStr :: (Var, Val) -> String
pairToStr (var, val) = var ++ "=" ++ valToString val

-- Converts the machine's state into a string.
state2Str :: State -> String
state2Str (State sta) = let sorted = sortBy (comparing fst) sta in 
                        intercalate "," (map pairToStr sorted)

-- Executes a specific instruction using the specified stack and state; returns the updated stack and state in the form of a pair.
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

-- Runs the list of instructions on the specified stack and state; returns a tuple consisting of the code list, a stack and a state.
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

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Aexp = Num Integer    
            | Var String          
            | Sum Aexp Aexp         
            | Subt Aexp Aexp           
            | Mul Aexp Aexp deriving Show

data Bexp = BoolVal Bool          
            | Equal Exp Exp    
            | LeEq Aexp Aexp        
            | LogAnd Bexp Bexp 
            | Not Bexp deriving Show

data Exp = A Aexp
           | B Bexp deriving Show

data Stm = Assign String Aexp          
            | Seq Stm Stm                
            | If Bexp Stm Stm            
            | While Bexp Stm deriving Show

type Program = [Stm]

compVal :: Exp -> Code
compVal (A n) = compA n
compVal (B n) = compB n

compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var x) = [Fetch x]
compA (Sum x y) = compA x ++ compA y ++ [Add]  
compA (Subt x y) = compA x ++ compA y ++ [Sub] 
compA (Mul x y) = compA x ++ compA y ++ [Mult] 

compB :: Bexp -> Code   
compB (BoolVal True) = [Tru] 
compB (BoolVal False) = [Fals] 
compB (Equal x y) = compVal x ++ compVal y ++ [Equ]
compB (LeEq x y) = compA x ++ compA y ++ [Le]
compB (LogAnd x y) = compB x ++ compB y ++ [And]
compB (Not v) = compB v ++ [Neg]

compile :: Program -> Code
compile [] = []
compile (Assign var expr : stmts) = compA expr ++ [Store var] ++ compile stmts 
-- compile (Seq s1 s2 : stmts) = compile s ++ compile stmts
-- compile (If a b c : stmts) = compB a ++ [Branch (compile [b]) (compile [c])] ++ compile stmts
-- compile (While a b : stmts) = [Loop (compB a) (compile [b])] ++ compile stmts

data Token = TWhile 
             | TAssign 
             | TSemicolon
             | TLPar
             | TRPar
             | TPlus
             | TMinus
             | TMult
             | TComp
             | TBoolComp deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('w':'h':'i':'l':'e':xs) = TWhile : lexer xs
lexer (':':'=':xs) = TAssign : lexer xs
lexer (';':xs) = TSemicolon : lexer xs
lexer ('(':xs) = TLPar : lexer xs
lexer (')':xs) = TRPar : lexer xs
lexer ('+':xs) = TPlus : lexer xs
lexer ('-':xs) = TMinus : lexer xs
lexer ('*':xs) = TMult : lexer xs
lexer ('=':xs) = TMinus : lexer xs
lexer ('=':'=':xs) = TMult : lexer xs

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")