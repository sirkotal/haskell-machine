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


assignParser :: Parser Stm
assignParser = do
            var <- many1 letter <* spaces
            string ":=" <* spaces
            value <- manyTill anyChar (char ';')
            return $ Assign var (parseAexp (value))

--seqParser :: Parser Stm
--seqParser = do
--  stm1 <- anyStmParser <* spaces
--  string ";" <* spaces
--  stm2 <- anyStmParser
--  return $ Seq stm1 stm2

ifParser :: Parser Stm
ifParser = do
    spaces
    string "if" <* spaces
    cond <- boolParser <* spaces
    string "then" <* spaces
    thenBody <- assignParser <* spaces
    string "else" <* spaces
    elseBody <- assignParser
    return $ If cond thenBody elseBody

whileParser :: Parser Stm
whileParser = do
  spaces
  string "while" <* spaces
  cond <- boolParser <* spaces
  string "do" <* spaces
  body <- assignParser
  return $ While cond body

boolParser :: Parser Bexp
boolParser = do
  value <- string "true" <|> string "false"
  return $ BoolVal (value == "true")

allParser :: Parser Stm
allParser = try assignParser <|> try ifParser <|> whileParser

sequenceParser :: Parser [Stm]
sequenceParser = many (try ifParser <|> assignParser <|> whileParser <* optional (char ';')) <* spaces

isNumber :: String -> Bool
isNumber input = case reads input :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

splitAround :: Eq a => a -> [a] -> Maybe ([a], [a])
splitAround x xs = case break (== x) xs of
    (_, [])     -> Nothing   -- x not found
    (before, after) -> Just (before, tail after)

parseAexp :: String -> Aexp
parseAexp input = case splitAround '+' input of
                      Just (before, after) -> 
                          Sum (parseAexp (filter (/= ' ') before)) (parseAexp (filter (/= ' ') after))
                      Nothing ->
                          case splitAround '-' input of
                              Just (before, after) ->
                                  Subt (parseAexp (filter (/= ' ') before)) (parseAexp (filter (/= ' ') after))
                              Nothing ->
                                  case splitAround '*' input of
                                      Just (before, after) ->
                                          Mul (parseAexp (filter (/= ' ') before)) (parseAexp (filter (/= ' ') after))
                                      Nothing ->
                                          if (isNumber input)
                                            then Num (toInteger (read input))
                                          else
                                            Var input

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

main :: IO ()
main = do
  let result = testSequenceParser "x:=1; if true then x:=1; else x:=2; while true do y:=3;"
  print result
