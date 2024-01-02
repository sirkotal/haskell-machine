-- PFL 2023/24 - Haskell practical assignment quickstart
import Data.Maybe (fromJust)
import Data.List (intercalate, sortBy, break)
import Data.Ord (comparing)
import Data.Map (insert, fromList, toList)
import Data.Char (digitToInt, isDigit, isAlpha)
import Text.Read (reads)
import Control.Exception (Exception, throwIO)
import Text.Parsec hiding (parse, State)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

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

-- Part 2

-- Arithmetic Expressions
data Aexp = Num Integer    
            | Var String          
            | Sum Aexp Aexp         
            | Subt Aexp Aexp           
            | Mul Aexp Aexp deriving Show

-- Boolean Expressions
data Bexp = BoolVal Bool          
            | Equal Aexp Aexp 
            | EqualBool Bexp Bexp   
            | LeEq Aexp Aexp        
            | LogAnd Bexp Bexp 
            | Not Bexp deriving Show

-- Statements
data Stm = Assign String Aexp          
            | Seq Stm Stm                
            | If Bexp Stm Stm            
            | While Bexp Stm deriving Show

type Program = [Stm]

-- Converts Arithmetic Expressions to Code
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var x) = [Fetch x]
compA (Sum x y) = compA y ++ compA x ++ [Add]  
compA (Subt x y) = compA y ++ compA x ++ [Sub] 
compA (Mul x y) = compA y ++ compA x ++ [Mult] 

-- Converts Boolean Expressions to Code
compB :: Bexp -> Code   
compB (BoolVal True) = [Tru] 
compB (BoolVal False) = [Fals] 
compB (Equal x y) = compA y ++ compA x ++ [Equ]
compB (EqualBool x y) = compB y ++ compB x ++ [Equ]
compB (LeEq x y) = compA y ++ compA x ++ [Le]
compB (LogAnd x y) = compB y ++ compB x ++ [And]
compB (Not v) = compB v ++ [Neg]

-- Compiles the program parsed
compile :: Program -> Code
compile [] = []
compile ((Assign var expr):stmts) = compA expr ++ [Store var] ++ compile stmts
compile ((Seq stm1 stm2):stmts) = compile [stm1] ++ compile [stm2] ++ compile stmts
compile ((If cond thenBody elseBody):stmts) = compB cond ++ [Branch (compile [thenBody]) (compile [elseBody])] ++ compile stmts
compile ((While cond thenBody):stmts) = [Loop (compB cond) (compile [thenBody])] ++ compile stmts

-- Parses the string inputed
parse :: String -> [Stm]
parse input = case parseHelper input of
    Left _         -> []
    Right statements -> statements

-- Retrieves the content of parsec
parseHelper :: String -> Either ParseError [Stm]
parseHelper = P.parse sequenceParser ""

-- Parses Assigns Statements
assignParser :: Parser Stm
assignParser = do
        spaces
        var <- many1 letter <* spaces
        string ":=" <* spaces
        value <- aexpParser <* char ';' <* spaces
        return $ Assign var value

-- Parses Seq Statements
seqParser :: Parser Stm
seqParser = do
    spaces
    (char '(') <* spaces
    c1 <- stmParser <* spaces
    remaining <- optionMaybe (spaces *> stmParser <* spaces)
    (char ')') <* spaces
    optional (char ';')
    case remaining of
        Just c2 -> return $ Seq c1 c2
        Nothing -> return c1

-- Parses If Statements
ifParser :: Parser Stm
ifParser = do
    spaces
    string "if" <* spaces
    optional (char '(') <* spaces
    cond <- bexpParser <* spaces
    optional (char ')') <* spaces
    string "then" <* spaces
    thenBody <- try seqParser <|> stmParser <* spaces
    string "else" <* spaces
    elseBody <- try seqParser <|> stmParser
    spaces
    return $ If cond thenBody elseBody

-- Parses While Statements
whileParser :: Parser Stm
whileParser = do
  spaces
  string "while" <* spaces
  optional (char '(') <* spaces
  cond <- bexpParser <* spaces
  optional (char ')') <* spaces
  string "do" <* spaces
  body <- try seqParser <|> stmParser
  spaces
  return $ While cond body

-- Parses a Statement
stmParser :: Parser Stm
stmParser = try ifParser <|> try whileParser <|> assignParser

-- Parses Multiple Statements
sequenceParser :: Parser [Stm]
sequenceParser = spaces *> many (try ifParser <|> try assignParser <|> whileParser <* optional (char ';'))

-- Parses Arithmetic Expressions
aexpParser :: Parser Aexp
aexpParser = spaces *> chainl1 term addOp

term :: Parser Aexp
term = chainl1 factor mulOp

factor :: Parser Aexp
factor = try (Num <$> (read <$> many1 digit)) <* spaces
      <|> try (Var <$> many1 letter) <*spaces
      <|> parens aexpParser <* spaces

addOp :: Parser (Aexp -> Aexp -> Aexp)
addOp = do
  spaces
  op <- try (char '+') <|> char '-'
  spaces
  return $ if op == '+' then Sum else Subt

mulOp :: Parser (Aexp -> Aexp -> Aexp)
mulOp = do
  spaces
  char '*'
  spaces
  return Mul

parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* spaces <* char ')'

-- Parses Boolean Expressions
bexpParser :: Parser Bexp
bexpParser = try logAndParser <|> try equalBoolParser <|> try equalParser <|> try boolValParser <|> try leEqParser <|> notParser

bexpSimpleParser :: Parser Bexp
bexpSimpleParser = try equalParser <|> try boolValParser <|> try leEqParser <|> notParser

bexpSimpleParserB :: Parser Bexp
bexpSimpleParserB = try equalBoolParser <|> try equalParser <|> try boolValParser <|> try leEqParser <|> notParser

boolValParser :: Parser Bexp
boolValParser = do
  spaces
  value <- (try (string "True" >> return True) <|> (string "False" >> return False))
  spaces
  return (BoolVal value)

equalBoolParser :: Parser Bexp
equalBoolParser = do
  spaces
  x <- bexpSimpleParser
  spaces
  string "="
  spaces
  y <- bexpSimpleParser
  return (EqualBool x y)

equalParser :: Parser Bexp
equalParser = do
  spaces
  optional (string "(")
  spaces
  x <- aexpParser
  spaces
  string "=="
  spaces
  y <- aexpParser
  spaces
  optional (string ")")
  spaces
  return (Equal x y)

leEqParser :: Parser Bexp
leEqParser = do
  spaces
  optional (string "(")
  spaces
  x <- aexpParser
  spaces
  string "<="
  spaces
  y <- aexpParser
  spaces
  optional (string ")")
  spaces
  return (LeEq x y)

logAndParser :: Parser Bexp
logAndParser = do
  spaces
  optional (string "(")
  spaces
  x <- bexpSimpleParserB
  spaces
  string "and"
  spaces
  y <- bexpSimpleParserB
  spaces
  optional (string ")")
  spaces
  return (LogAnd x y)

notParser :: Parser Bexp
notParser = do
  spaces
  string "not"
  spaces
  optional (string "(")
  spaces
  v <- bexpSimpleParser
  spaces
  optional (string ")")
  spaces
  return (Not v)

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


