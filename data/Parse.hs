import Text.Parsec
import Text.Parsec.String (Parser)

-- PFL 2023/24 - Haskell practical assignment quickstart
import Data.Maybe (fromJust)
import Data.List (intercalate, sortBy, break)
import Data.Ord (comparing)
import Data.Map (insert, fromList, toList)
import Data.Char (digitToInt, isDigit, isAlpha)
import Text.Read (reads)
import Control.Exception (Exception, throwIO)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

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
compB (Equal x y) = compA x ++ compA y ++ [Equ]
compB (EqualBool x y) = compB x ++ compB y ++ [Equ]
compB (LeEq x y) = compA x ++ compA y ++ [Le]
compB (LogAnd x y) = compB x ++ compB y ++ [And]
compB (Not v) = compB v ++ [Neg]

compile :: Program -> Code
compile [] = []
compile ((Assign var expr):stmts) = compA expr ++ [Store var] ++ compile stmts
compile ((Seq stm1 stm2):stmts) = compile [stm1] ++ compile [stm2] ++ compile stmts
compile ((If cond thenBody elseBody):stmts) = compB cond ++ [Branch (compile [thenBody]) (compile [elseBody])] ++ compile stmts
compile ((While cond thenBody):stmts) = [Loop (compB cond) (compile [thenBody])] ++ compile stmts


assignParser :: Parser Stm
assignParser = do
        optional (char '(')
        var <- many1 letter <* spaces
        string ":=" <* spaces
        value <- aexpParser <* char ';' <* spaces
        optional (char ')')
        return $ Assign var value

--seqParser :: Parser Stm
--seqParser = do
--  stm1 <- anyStmParser <* spaces
--  string ";" <* spaces
--  stm2 <- anyStmParser
--  return $ Seq stm1 stm2

ifParser :: Parser Stm
ifParser = do
    spaces
    optional (char '(') <* spaces
    string "if" <* spaces
    optional (char '(') <* spaces
    cond <- bexpParser <* spaces
    optional (char ')') <* spaces
    string "then" <* spaces
    optional (char '(') <* spaces
    thenBody <- stmParser <* spaces
    optional (char ')') <* spaces
    optional (char '(') <* spaces
    string "else" <* spaces
    elseBody <- stmParser
    optional (char ')') <* spaces
    optional (char ')') <* spaces
    spaces
    return $ If cond thenBody elseBody

whileParser :: Parser Stm
whileParser = do
  spaces
  optional (char '(') <* spaces
  string "while" <* spaces
  optional (char '(') <* spaces
  cond <- bexpParser <* spaces
  optional (char ')') <* spaces
  string "do" <* spaces
  optional (char '(') <* spaces
  body <- stmParser
  optional (char ')') <* spaces
  optional (char ')') <* spaces
  spaces
  return $ While cond body

boolParser :: Parser Bexp
boolParser = do
  value <- string "true" <|> string "false"
  return $ BoolVal (value == "true")

stmParser :: Parser Stm
stmParser = try ifParser <|> try whileParser <|> assignParser

sequenceParser :: Parser [Stm]
sequenceParser = spaces *> many (try ifParser <|> try assignParser <|> whileParser <* optional (char ';'))

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

bexpParser :: Parser Bexp
bexpParser = try equalParser <|> try boolValParser <|> try leEqParser <|> notParser

-- try logAndParser <|> try equalBoolParser <|>

boolValParser :: Parser Bexp
boolValParser = do
  value <- (try (string "true" >> return True) <|> (string "false" >> return False))
  return (BoolVal value)

equalBoolParser :: Parser Bexp
equalBoolParser = do
  spaces
  x <- bexpParser
  spaces
  string "="
  spaces
  y <- bexpParser
  return (EqualBool x y)

equalParser :: Parser Bexp
equalParser = do
  spaces
  x <- aexpParser
  spaces
  string "=="
  spaces
  y <- aexpParser
  return (Equal x y)

leEqParser :: Parser Bexp
leEqParser = do
  spaces
  x <- aexpParser
  spaces
  string "<="
  spaces
  y <- aexpParser
  return (LeEq x y)

logAndParser :: Parser Bexp
logAndParser = do
  spaces
  x <- bexpParser
  spaces
  string "and"
  spaces
  y <- bexpParser
  return (LogAnd x y)

notParser :: Parser Bexp
notParser = do
  spaces
  string "not"
  spaces
  string "("
  spaces
  v <- bexpParser
  spaces
  string ")"
  spaces
  return (Not v)

testParser :: String -> Either ParseError Stm
testParser = parse ifParser ""

-- Test cases
--test1 :: Either ParseError Stm
--test1 = sequenceParser "if true then x:=1; else x:=2;"

--main :: IO ()
--main = do
  --print test1

testSequenceParser :: String -> Either ParseError [Stm]
testSequenceParser = parse sequenceParser ""

test :: String -> [Stm]
test input = case testSequenceParser input of
    Left _         -> []  -- Handle parsing error as needed
    Right statements -> statements

main :: IO ()
main = do
  let result = test "while x <= 10 do x := x + 1"
  print result
