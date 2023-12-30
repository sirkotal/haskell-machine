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

valToString :: SVal -> String
valToString (Integer x) = show x
valToString Tt = "True"
valToString Ff = "False"

data Stack = Stack [SVal] deriving Show

type Var = String
type Val = SVal
data State = State [(Var, Val)] deriving Show

push :: SVal -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)

pop :: Stack -> Stack
pop (Stack (_:xs)) = Stack xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> SVal
top (Stack (x:_)) = x
top _ = error "Stack.top: empty stack"

add :: Stack -> Stack
add (Stack (Integer x : Integer y : xs)) = push (Integer (x + y)) (pop (pop (Stack (Integer x : Integer y : xs))))
add _ = error "Stack.add: need two integers at the top of the stack"

sub :: Stack -> Stack
sub (Stack (Integer x : Integer y : xs)) = push (Integer (x - y)) (pop (pop (Stack (Integer x : Integer y : xs))))
sub _ = error "Stack.sub: need two integers at the top of the stack"

mul :: Stack -> Stack
mul (Stack (Integer x : Integer y : xs)) = push (Integer (x * y)) (pop (pop (Stack (Integer x : Integer y : xs))))
mul _ = error "Stack.mul: need two integers at the top of the stack"

and :: Stack -> Stack
and (Stack (x : y : xs)) = case (valToString x, valToString y) of
                                    ("True", "True") -> push Tt (pop (pop (Stack (x : y : xs))))
                                    ("True", "False") -> push Ff (pop (pop (Stack (x : y : xs))))
                                    ("False", "True") -> push Ff (pop (pop (Stack (x : y : xs))))
                                    ("False", "False") -> push Ff (pop (pop (Stack (x : y : xs))))
                                    _ -> error "Run-time error"   -- Stack.and: need two booleans at the top of the stack

neg :: Stack -> Stack
neg (Stack (x : xs)) = case valToString x of
                                    ("True") -> push Ff (pop (Stack (x : xs)))
                                    ("False") -> push Tt (pop (Stack (x : xs)))
                                    _ -> error "Stack.neg: need a boolean at the top of the stack"

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

le :: Stack -> Stack
le (Stack (Integer x : Integer y : xs)) = case x <= y of
                                      True -> push Tt (pop (pop (Stack (Integer x : Integer y : xs))))
                                      False -> push Ff (pop (pop (Stack (Integer x : Integer y : xs))))
le _ = error "Stack.le: need two integers at the top of the stack"

isEmpty :: Stack -> Bool
isEmpty (Stack [])= True
isEmpty (Stack _) = False

createEmptyStack :: Stack
createEmptyStack = Stack []

stack2Str :: Stack -> String
stack2Str s = if isEmpty s
                then ""
              else if isEmpty (pop s)
                then valToString (top s)
              else
                valToString (top s) ++ "," ++ stack2Str (pop s)

createEmptyState :: State
createEmptyState = State []

fetch :: String -> State -> Stack -> Stack
fetch str (State sta) stk = case lookup str sta of
                                      Nothing -> error "Run-time error"  -- Variable was not found in storage
                                      Just val -> push val stk

store :: String -> State -> Stack -> (Stack, State)
store str (State sta) stk = if isEmpty stk
                                then error "Stack is empty"
                            else
                                (pop stk, State (toList (insert str (top stk) (fromList sta))))

pairToStr :: (Var, Val) -> String
pairToStr (var, val) = var ++ "=" ++ valToString val

state2Str :: State -> String
state2Str (State sta) = let sorted = sortBy (comparing fst) sta in 
                        intercalate "," (map pairToStr sorted)

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
compA (Sum x y) = compA y ++ compA x ++ [Add]  
compA (Subt x y) = compA y ++ compA x ++ [Sub] 
compA (Mul x y) = compA y ++ compA x ++ [Mult] 

compB :: Bexp -> Code   
compB (BoolVal True) = [Tru] 
compB (BoolVal False) = [Fals] 
compB (Equal x y) = compVal x ++ compVal y ++ [Equ]
compB (LeEq x y) = compA x ++ compA y ++ [Le]
compB (LogAnd x y) = compB x ++ compB y ++ [And]
compB (Not v) = compB v ++ [Neg]

compile :: Program -> Code
compile [] = []
compile ((Assign var expr):stmts) = compA expr ++ [Store var] ++ compile stmts
compile ((Seq stm1 stm2):stmts) = compile [stm1] ++ compile [stm2] ++ compile stmts
compile ((If cond thenBody elseBody):stmts) = compB cond ++ [Branch (compile [thenBody]) (compile [elseBody])] ++ compile stmts
compile ((While cond thenBody):stmts) = [Loop (compB cond) (compile [thenBody])] ++ compile stmts

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
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")~



-- main = print(push (Integer 3) (pop (Stack [(Integer 1),(Integer 2)])))
-- main = print(top (Stack [(Integer 1),(Integer 2)]))
-- main = print(push Tt (createEmptyStack))
-- main = print(isEmpty (createEmptyStack))
-- main = print(stack2Str (Stack [(Integer 1),(Integer 2)]))
-- main = print(stack2Str (push Tt (push (Integer 4) createEmptyStack)))

-- main = print(sub (push (Integer 5) (push (Integer 4) createEmptyStack)))
-- main = print(Main.and (push Tt (push Ff createEmptyStack)))
-- main = print(neg (push Ff (push Ff createEmptyStack)))

-- main = print(eq (push Tt (push (Integer 4) createEmptyStack)))
-- main = print(le (push (Integer 5) (push (Integer 4) createEmptyStack)))

-- main = print(fetch "x" (State [("x",(Integer 3))]) (Stack [(Integer 1),(Integer 2)]))
-- main = print(store "x" (State [("y", (Integer 4))]) (Stack [(Integer 1),(Integer 2)]))
-- main = print(state2Str (State [("x",(Integer 3)), ("y", (Integer 4)), ("z", Tt)]))

-- main = print(run ([Push 10, Push 4, Push 3, Sub, Mult], createEmptyStack, createEmptyState))

-- main = print (testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
-- main = print(testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True"))
-- main = print(testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False"))
-- main = print(testAssembler [Push (-20),Tru,Fals] == ("False,True,-20",""))
-- main = print(testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20",""))
-- main = print(testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20",""))
-- main = print(testAssembler [Push (-20),Push (-21), Le] == ("True",""))
-- main = print(testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4"))
-- main = print(testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] )

-- main = print(testAssembler [Push 1,Push 2,And])
-- main = print(testAssembler [Tru,Tru,Store "y", Fetch "x",Tru])

--main = print(run ((compile [Assign "x" (Sum (Num 2) (Subt (Num 2) (Mul (Num 2) (Num 2))))]), createEmptyStack, createEmptyState))