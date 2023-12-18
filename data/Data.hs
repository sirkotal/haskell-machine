-- PFL 2023/24 - Haskell practical assignment quickstart
import Data.Maybe (fromJust)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Stack = Stack [Int] deriving Show

type Var = String
type Val = Int
data State = State [(Var, Val)] deriving Show

machineInstruction :: Inst -> Stack -> State -> Stack
machineInstruction (Push n) (Stack stk) sta = Stack (n:stk)

machineInstruction Add (Stack (x:y:stk)) sta = Stack ((x + y) : xs)
machineInstruction Sub (Stack (x:y:stk)) sta = Stack ((x - y) : xs)
machineInstruction Mult (Stack (x:y:stk)) sta = Stack ((x * y) : xs)

push :: Int -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)

pop :: Stack -> Stack
pop (Stack (_:xs)) = Stack xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> Int
top (Stack (x:_)) = x
top _ = error "Stack.top: empty stack"

isEmpty :: Stack -> Bool
isEmpty (Stack [])= True
isEmpty (Stack _) = False

createEmptyStack :: Stack
createEmptyStack = Stack []

stack2Str :: Stack -> String
stack2Str s = if isEmpty s
                then ""
              else if isEmpty (pop s)
                then show (top s)
              else
                show (top s) ++ "," ++ stack2Str (pop s)

createEmptyState :: State
createEmptyState = State []

fetch :: String -> State -> Stack -> Stack
fetch str (State sta) stk = case lookup str sta of
                                      Nothing -> error "Variable was not found in storage"
                                      Just val -> push val stk

store :: String -> State -> Stack -> State
store str (State sta) stk = if isEmpty stk
                                then error "Stack is empty"
                            else
                                State ((str, (top stk)) : sta)

-- state2Str :: State -> String
state2Str = undefined -- TODO

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

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

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")~

-- main = print(fetch "x" (State [("x",3)]) (Stack [1,2]))
main = print(store "x" (State [("y", 4)]) (Stack [1,2]))