module LGPT.TUI (runREPL, Memory) where 

-- https://williamyaoh.com/posts/2023-06-10-monad-transformers-101.html blog post I used to help solidify my understanding of the difference between mtl and transformers as well as the use of the lecture on the two.

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The 
code that is included here is a good starting point, but you don't need to 
keep it if you don't want to.
-}
import Control.Monad ( forever )
import Text.Megaparsec
    ( (<|>),
      anySingle,
      parse,
      someTill,
      MonadParsec(takeWhile1P, eof, takeWhileP) )
import Text.Megaparsec.Char ( char, string )
import Text.Megaparsec.Char.Lexer ( decimal )
import LGPT.Helpers (Parser, prompt, runStart, Memory)
import LGPT.Numbers (parseLonghand, printLonghand)
import Data.Time
    ( Day,
      LocalTime(localDay),
      ZonedTime(zonedTimeToLocalTime),
      diffDays,
      nominalDay,
      addLocalTime,
      getZonedTime )
import Data.Time.Format
    ( defaultTimeLocale, formatTime, parseTimeM )
import Data.Char ( isDigit )
import LGPT.Calculator ( Expr, eval, parseExpr ) 
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
    ( MonadIO(..),
      StateT,
      modify,
      evalStateT,
      MonadState(get) )
import Control.Monad.Except ( runExceptT )

import LGPT.Football (fetchNextFixture, formatFixture, fetchTeamPosition, formatStanding)




--------------------------------------------------------------------------------
{- | Our program. It runs a loop which:
      1. Reads a line of input
      2. Parses it into a structured Request
      3. Does something based on that request (normally printing something out).
-}
runREPL' :: StateT Memory IO ()
runREPL' = forever $ do
  output prompt 
  req <- liftIO getLine
  respondTo (readRequest req)

runREPL :: IO () 
runREPL = evalStateT runREPL' M.empty -- runs runREPL' with state so that the program can remember things 


--------------------------------------------------------------------------------
-- Parsing and responding to requests:


-- | Our request type, the result of parsing a string.
data Request = Unknown | Hello | Date DateRequest | Calculator Expr | Store String Expr | Remember String String | Tell String | Football FootballRequest
  deriving (Eq, Ord, Show)

data DateRequest = Today | Tomorrow | LongAgo Day  -- types of date requests the user can do
  deriving (Eq, Ord, Show)

data FootballRequest = NextFixture String | TeamPosition String -- types of football requests the user can do
  deriving (Eq, Ord, Show)



{- | Read a request. 

    This runs the parse function from Megaparsec, and
    converts any failed parses into an Unknown request.
-}
readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left  err -> Unknown
  Right req -> req


parseRequest :: Parser Request -- top level parser that trys each of the different parsers
parseRequest = do
  req <- parseDateRequests <|> parseHelloRequest <|> parseExpressionRequest <|> parseStoreRequest <|> parseRememberRequest <|> parseTellRequest <|> parseFootballRequest 
  eof -- used so that inputs with trailing garbage after a valid request are not accepted
  pure req


parseHelloRequest :: Parser Request -- parser for hello requests
parseHelloRequest = do
  string "Hello"
  pure Hello


parseDateRequests :: Parser Request -- parser that splits up the date requests into 3 seperate requests
parseDateRequests = do
  request <- parseDateToday <|> parseDateTomorrow <|> parseDateLongAgo 
  pure (Date request)

parseDateToday :: Parser DateRequest -- parser that handles What day is it?
parseDateToday = do
  string "What day is it?"
  pure Today

parseDateTomorrow :: Parser DateRequest -- parser that handles What day is it tomorrow?
parseDateTomorrow = do
  string "What day is it tomorrow?"
  pure Tomorrow

parseDateLongAgo :: Parser DateRequest -- parser that handles How long ago was yyyy-mm-dd?
parseDateLongAgo = do
  string "How long ago was "
  date <- parseDate -- parses the date provided by user 
  char '?'
  pure $ LongAgo date

parseDate :: Parser Day -- parser that takes in a date, this parser not only checks the format of the date but also whether it is a valid date aswell
parseDate = do
  rawDate <- takeWhile1P (Just "Date") (\c -> isDigit c || c == '-') -- takes in the raw date for further parsing
  let parsedDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" rawDate -- parses the date using Data.Time inbuilt parser, this was used as it already fully validates the date for you
  maybe (fail "Invalid Date") pure parsedDate -- extacts the value out of the maybe date and lifts it and fails if the date extracted was invalid previously, this will give the unknown message as required with invalid inputs

parseExpressionRequest :: Parser Request -- parser that handles expressions
parseExpressionRequest = do 
  string "What is "
  expression <- parseExpr -- imported from Calculator which was adapted from work in labs
  char '?'
  pure (Calculator expression)

parseStoreRequest :: Parser Request -- parser that handles assigning expressions to variables
parseStoreRequest = do 
  string "Let "
  variable <- someTill anySingle (string " equal ") -- parser that takes in all characters until the phrase " equal " which signals the end of [variable] then also parses the " equal "
  expression <- parseExpr
  char '.'
  pure (Store variable expression)

parseRememberRequest :: Parser Request -- parser that handles Remember that [name] is [thing].
parseRememberRequest = do 
  string "Remember that "
  name <- someTill anySingle (string " is ") -- parser that takes in all characters until the phrase " is " which signals the end of [name] then also parses the " is "
  thing <- takeWhileP (Just "[thing]") (/=  '.') -- parser that takes in all character until the delimeter '.' which signal the end of [thing], takeWhileP is used as it is more efficient and can be used here as the delimeter is only 1 character, also I believe [thing] can be nothing so is allowed by parser
  char '.'
  pure (Remember name thing)

parseTellRequest :: Parser Request -- parser that handles Tell me about [name].
parseTellRequest = do 
  string "Tell me about "
  name <- takeWhile1P (Just "[name]") (/=  '.') -- parser that takes in all character until the delimeter '.' which signal the end of [name], takeWhile1P is used as it is more efficient and can be used here as the delimeter is only 1 character
  char '.'
  pure (Tell name)

parseFootballRequest :: Parser Request 
parseFootballRequest = do 
  request <- parseNextFixtureRequest <|> parseTeamPositionRequest
  pure (Football request)

parseNextFixtureRequest :: Parser FootballRequest -- parser that handles Who is [team] playing next?
parseNextFixtureRequest = do 
  string "Who is "
  team <- someTill anySingle (string " playing next?") -- parses a team name by taking all characters till the string " playing next?" which indicates the end of the team name, this is done instead of manually writing out all the team names as it makes the parser more flexible and allows functionality for new teams to be easily added without changing parser
  pure (NextFixture team)

parseTeamPositionRequest :: Parser FootballRequest 
parseTeamPositionRequest = do 
  string "What position are "
  team <- someTill anySingle (string " in the Premier League?") -- parses a team name by taking all characters till the string " in the Premier League?" which indicates the end of the team name, this is done instead of manually writing out all the team names as it makes the parser more flexible and allows functionality for new teams to be easily added without changing parser
  pure (TeamPosition team)


-- | Respond to a request. This is where the behaviours of λGPT will go
respondTo :: (MonadState Memory m, MonadIO m) => Request -> m ()
respondTo Unknown = output "I don't understand that."
respondTo Hello = output "Hi there!"

respondTo (Date Today) = do 
  todaysTime <- getToday -- gets the current local time, needed to account for british summer time
  let today = localDay todaysTime -- converts the local time into a date
  let dayToday = dateToDayOfWeek today  -- converts the date to the day of the week it will be on that date (today)
  output ("Today is " ++ dayToday ++ ".")

respondTo (Date Tomorrow) = do
  today <- getToday -- gets the current local time, needed to account for british summer time
  let tomorrow = localDay $ addLocalTime nominalDay today  -- add 1 day to today to get tomorrow
  let dayTomorrow = dateToDayOfWeek tomorrow -- converts the date to the day of the week it will be on that date (tomorrow)
  output ("Tomorrow is " ++ dayTomorrow ++ ".")

respondTo (Date (LongAgo inputDate)) = do
  todaysTime <- getToday -- gets the current local time, needed to account for british summer time
  let today = localDay todaysTime -- converts the local time into a date
  let daysAgo = diffDays today inputDate -- finds the difference in days between the two dates
  output (show inputDate ++ " was " ++ show daysAgo ++ dayOrDays daysAgo ++ "ago.")
  where 
    dayOrDays daysAgo 
      | daysAgo == 1 = " day " -- this is done to keep the chat bot grammatically correct so it doesn't say 1 days ago as this doesn't make sense
      | otherwise = " days "

respondTo (Calculator expression) = evalAndStore "that" (\answer -> "The answer is " ++ answer ++ ".") expression  -- evaluates an expression outputs it and stores its result in the map under "that"

respondTo (Store variable expression) = evalAndStore variable (\answer -> "Ok, " ++ variable ++ " equals " ++ answer ++ ".") expression -- evaluates an expression outputs it and stores its result in the map under the variable name specified

respondTo (Remember name thing) = do 
  modify (M.insert name thing) -- stores name and thing pair in a map which is stored as the state
  output "Okay."

respondTo (Tell name) = do 
  memory <- get -- fetches the current state which is the map of all names and things
  maybe 
    (output ("Sorry, I don't know anything about " ++ name ++ ".")) -- if the name has not been added to the map yet
    (\thing -> output ("Sure - " ++ name ++ " is " ++ thing ++ "." )) -- if the thing can be found then output it
    (M.lookup name memory)  -- looks for the corresponing thing to the name provided

respondTo (Football (NextFixture team)) = do 
  fixture <- liftIO $ runExceptT $ fetchNextFixture team -- gets the next fixture of the team specified or throws an error if it fails to do so
  either output (output . formatFixture) fixture -- outputs either an error message or the fixture 

respondTo (Football (TeamPosition team)) = do 
  position <- liftIO $ runExceptT $ fetchTeamPosition team -- get the position in the league of the team specified or throws an error if it fails to do so
  either output (output . formatStanding) position -- outputs either an error message or the position


evalAndStore :: (MonadState Memory m, MonadIO m) => String -> (String -> String) -> Expr -> m () -- evaluates an expression and outputs the result as specified and stores the result in the map under the variable specified
evalAndStore variable message expression = do 
    answer <- eval expression -- evaluates the expression
    either output storeAndPrint answer -- either outputs the error (if that has not been found yet or the variable given is invalid) or outputs the evaluated result of the expression
  where 
    storeAndPrint value = do 
      let answerWords = printLonghand value -- converts the answer in numbers to words
      modify (M.insert variable answerWords) -- stores the variable in map
      output (message answerWords) -- outputs the answer with the message specified

getToday :: (MonadIO m) => m LocalTime
getToday = zonedTimeToLocalTime <$> liftIO getZonedTime -- using zonedTime to account for british summer time so date is correct when it is around midnight 

dateToDayOfWeek :: Day -> String
dateToDayOfWeek = formatTime defaultTimeLocale "%A" -- takes in a date and returns the day of the week this date fell/falls on

output :: (MonadIO m) => String -> m ()
output = liftIO . putStrLn -- removes manual lifting when outputting every time

