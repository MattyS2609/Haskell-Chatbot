module LGPT.Calculator (parseExpr,eval, Expr,parseVariable) where
{-
This modules handles all requests related to the calculations.
-}

import Text.Megaparsec
    ( (<|>),
      empty,
      anySingle,
      parse,
      many,
      someTill,
      MonadParsec(lookAhead, notFollowedBy, try) )
import Text.Megaparsec.Char ( space, space1, string, letterChar )
import Text.Megaparsec.Char.Lexer ( decimal )
import qualified Text.Megaparsec.Char.Lexer as L
import LGPT.Numbers ( parseLonghand ) 
import Control.Monad.State ( MonadState(get) )
import LGPT.Helpers (Memory, Parser)
import qualified Data.Map as M
import Control.Monad.Except
    ( runExceptT, MonadError(throwError) )
import Data.Foldable (Foldable(foldl'))

data SubExpr = Add Item -- a representation of a partial operator to be applied to an intitial value
    | Subtract Item
    | Multiply Item
    deriving (Eq, Ord, Show)

data Expr = Expr Item [SubExpr]  -- an intial value followed by a list of partial operators to be applied in turn to this value from left to right
    deriving (Eq, Ord, Show)

data Item = Number Int | Variable String -- either a number or a variable name that stores a number in the map
    deriving (Eq, Ord, Show)

number :: Parser Item -- parses a number in words into an Int
number = do
    value <- parseLonghand -- consumes and converts the number in words in input into an Int
    notFollowedBy letterChar -- checks the number just consumed was actually a number and not a variable name that contained a number name at the start, for example the word tent, required so that the number parser fails if the word being consumed contains a number at the start of the word so that the variable parser is used
    sc -- consumes trailing whitespace
    pure $ Number value

sc :: Parser ()
sc = L.space space1 empty empty -- used to consume arbitrary amounts of spaces

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc -- a function that takes in a parser and returns the parser that also absorbs all white space after it


eval :: (MonadState Memory m) =>  Expr -> m (Either String Int)
eval (Expr initialItem subExprs) = runExceptT $ foldl' evalSub initialNum subExprs -- evalutates the full expression by folding the partial operations from left to right into the intial number, runExceptT is used to handle any errors from variables being used that don't exist, foldl' is used to strictly evaluate at each step to save memory
    where initialNum = itemToNumber initialItem -- converts the initialItem into a number


evalSub :: (MonadState Memory m, MonadError String m) => m Int -> SubExpr -> m Int -- evaluates each sub expression
evalSub acc (Add item) = (+) <$> acc <*> itemToNumber item -- adds the item to the accumulator
evalSub acc (Subtract item) = (-) <$> acc <*> itemToNumber item -- subtracts the item from the accumulator
evalSub acc (Multiply item) = (*) <$> acc <*> itemToNumber item -- multiplies the item and the accumulator


itemToNumber :: (MonadState Memory m, MonadError String m) => Item -> m Int -- converts an item into a number by either returning the number stored or fetching the value stored in the variable and returning that
itemToNumber (Number num) = pure num -- returns the number stored in the item if the item is already a number
itemToNumber (Variable variable) = do -- returns the number stored in the variable
    memory <- get -- fetches the map of variables from state
    case M.lookup variable memory of -- finds the corresponding value in the map for the variable specified, explicit pattern matching is used as subsequent functions are long and using functions either and maybe makes it more confusing
        Just thing -> case parse parseLonghand "<stdin>" thing of -- converts the value stored in the variable to an int
            Left _ -> throwError ("Value stored in " ++ variable ++ " is not a valid number therefore cannot be used in an expression") -- if the variable doesn't store a valid number
            Right num -> pure num -- returns the value stored in the variable as an int 
        Nothing -> throwError err -- returns an error if the variable stored in the item doesn't exist
    where 
        err = case variable of 
            "that" -> "I haven't evaluated anything yet." -- if no previous evaluations have occured so that is not stored yet
            _ -> "Variable " ++ variable ++ " doesn't exist so cant be used in expression" -- if the variable used is not assigned yet



parseExpr :: Parser Expr -- use of lexer to remove need to manually handle white space
parseExpr = do
    sc -- consumes previous white space
    initialNum <- lexeme parseItem -- parses the intial number to fold over
    expr <- lexeme parseSubExpr -- parses all subsequent sub expressions to be applied to the intial number
    pure (Expr initialNum expr)
    

parseSubExpr :: Parser [SubExpr]
parseSubExpr = many $ do -- repeatedly parses sub expressions till all sub expressions are absorbed
    op <- parseOp  -- parses the opertor in the sub expression
    num <- lexeme parseItem -- parses the number or variable in the sub expression
    pure (op num)
    
parseOp :: Parser (Item -> SubExpr)
parseOp = lexeme (parseAdd' <|> parseSub' <|> parseMult') -- parses one of the three opertors
    where
    parseAdd' = string "plus" >> pure Add -- parses the plus operator
    parseSub' = string "minus" >> pure Subtract -- parses the minus operator
    parseMult' = string "times" >> pure Multiply -- parses the times operator

parseItem :: Parser Item 
parseItem = try number <|> parseVariable -- parses either a number or a variable

parseVariable :: Parser Item 
parseVariable = do
    variable <- someTill anySingle (lookAhead $ sc >> (string "plus" <|> string "minus" <|> string "times" <|> string "?")) -- absorbs all characters for the variable until either an operator showing the end of the variable or a ? marking the end of the expression, someTill is used as variables have to be atleast 1 character
    pure $ Variable variable





    

    

