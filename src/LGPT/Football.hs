{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module LGPT.Football where 
-- This module handles all things related to football requests which are part of my optional section

-- https://bitsbybrad.com/2021-02-09-json-api-haskell/ is a blog post used to form my understanding of API requests in Haskell however no code was copied and the blog was only used to understand how to make an API request

import GHC.Generics ( Generic )
import Prelude hiding (id) -- done to avoid clash between team id and id function from prelude
import Data.Aeson ( FromJSON )
import Data.Text ( unpack, Text ) 
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpJSON, Response)
import Network.HTTP.Client.Conduit (Request, parseRequest)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Data.Time (formatTime, defaultTimeLocale, parseTimeM, UTCTime)
import Data.List (find)
import Control.Monad.Except ( MonadIO(..), MonadError(throwError) )
import System.Environment (lookupEnv)



data Team = Team  -- record that stores team information
    { id :: Int
    , name :: Text 
    } deriving (Show, Eq, Generic)

instance FromJSON Team -- automatically generates an instance of FromJSON for Team

data Fixture = Fixture -- record that stores fixture information
    { utcDate :: Text 
    , homeTeam :: Team
    , awayTeam :: Team
    } deriving (Show, Eq, Generic)

instance FromJSON Fixture -- automatically generates an instance of FromJSON for Fixture

newtype FixtureResponse = FixtureResponse -- record that stores list of fixtures from a fixture query
    { matches :: [Fixture] } deriving (Show, Eq, Generic)

instance FromJSON FixtureResponse -- automatically generates an instance of FromJSON for FixtureResponse

data Standing = Standing -- record that stores standing information
    { position :: Int
    , team :: Team  
    , points :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON Standing -- automatically generates an instance of FromJSON for Standing

newtype Table = Table 
    { table :: [Standing] } deriving (Show, Eq, Generic) -- record that stores a single table as a list of standings

instance FromJSON Table -- automatically generates an instance of FromJSON for Table

newtype StandingsResponse = StandingsResponse -- record that stores list of tables generated from Standings query
    { standings :: [Table] } deriving (Show, Eq, Generic) 
    
instance FromJSON StandingsResponse -- automatically generates an instance of FromJSON for StandingsResponse


getApiKey :: (MonadIO m, MonadError String m) => m String 
getApiKey = do 
    maybeKey <- liftIO $ lookupEnv "API_KEY" -- reads the key loaded from .env 
    maybe (throwError "API_KEY cannot be found, please check the .env file") pure maybeKey -- returns the key or throws an error if it cannot be found

baseURL :: String 
baseURL = "https://api.football-data.org/v4/" -- base url to make api requests from



teamIDs :: Map String Int 
teamIDs = M.fromList -- map of team names to ids so a user can specify a team by their name when making a request
    [ ("Chelsea", 61)
    , ("Arsenal", 57)
    , ("Aston Villa", 58)
    , ("Bournemouth", 1044)
    , ("Brentford", 402)
    , ("Brighton & Hove Albion", 397)
    , ("Burnley", 328)
    , ("Crystal Palace", 354)
    , ("Everton", 62)
    , ("Fulham", 63)
    , ("Leeds United", 341)
    , ("Liverpool", 64)
    , ("Manchester City", 65)
    , ("Manchester United", 66)
    , ("Newcastle", 67)
    , ("Nottingham Forest", 351)
    , ("Sunderland", 71)
    , ("Tottenham", 73)
    , ("West ham", 563)
    , ("Wolves", 76)
    ]

fetchFromAPI :: (MonadIO m, MonadError String m, FromJSON a) => String -> m (Response a)
fetchFromAPI end = do -- takes in an API request as a string and makes the request and returns the response
    request <- liftIO $ parseRequest (baseURL ++ end) -- takes the string request and parses it into a Request object that can be used to make a HTTP request
    apiKey <- getApiKey -- gets the API key from the environment variable which was loaded from the .env file
    let requestWithKey = addRequestHeader "X-Auth-Token" (BS.pack apiKey) request -- adds api key to the header of the request
    httpJSON requestWithKey -- executes the api request

findTeamID :: (MonadError String m) => String -> m Int
findTeamID teamName  = -- finds a team ID given the team name
    maybe  
        (throwError ("The team " ++ teamName ++ " is not in the premier league.")) -- if no id can be found
        pure -- returns the team ID
        (M.lookup teamName teamIDs) -- looks up the id of the team specified

fetchNextFixture :: (MonadIO m, MonadError String m) => String -> m Fixture
fetchNextFixture teamName =  do -- builds an api request to fetch a specicified teams next fixture
    teamID <- findTeamID teamName
    response <- fetchFromAPI ("teams/" ++ show teamID ++ "/matches?status=SCHEDULED&limit=1&competitions=PL") -- executes the API request to find a teams next game. /matches finds all matches of a team then SCHEDULED filters to only show upcoming games then limit = 1 shows only the first upcoming and closest game
    let matches' = matches $ getResponseBody response -- extracts the matches from the Response
    case matches' of 
        [] -> throwError ("No upcoming fixtures found for " ++ teamName ++ ".") -- if the team has no more matches left in the season
        (f:_) -> pure f -- gets the closest match

fetchTeamPosition :: (MonadIO m, MonadError String m) => String -> m Standing
fetchTeamPosition teamName = do -- builds api request to fetch a specicified teams position in the league
    teamID <- findTeamID teamName
    response <- fetchFromAPI "competitions/PL/standings" -- executes the API request to find a teams next game. competitions specifies competition information then PL specifies information about the Premier League and then standings specifies the standings table
    let tables = standings $ getResponseBody response -- extracts 3 prem tables (total, home, away) from the request
    case tables of -- extracts the total premier league table
        [] -> throwError "No Premier League table could be found for the current season" -- if the current Premier League table could not be found in the API
        (t:_) -> do -- gets the first table stored (total)
            let premTable = table t -- extracts the standings from the table
            let teamPositionM = find (\position -> id (team position) == teamID ) premTable -- finds the standing of the team specified
            maybe (throwError ("Couldn't find " ++ teamName ++ " in the premier league table")) pure teamPositionM -- extracts the team from the whole table and throws an error if it could not be found

formatStanding :: Standing -> String 
formatStanding standing = -- formats a standing so it can be displayed nicely to a user
    let team' = unpack $ name $ team standing -- extracts the team from the standing
        position' = position standing -- extracts the position from the standing
        points' = points standing -- extracts the points from the standing
        banter = case team' of -- adds a joke if the team is Tottenham (Big six who?)
            "Tottenham Hotspur FC" -> "Looks like they will be enjoying Stoke away next season at this rate!"
            _ -> ""
    in team' ++ " are in " ++ toOrdinal position' ++ " position with " ++ show points' ++ " points. " ++ banter -- returns the standing in a formatted string

formatFixture :: Fixture -> String 
formatFixture fixture = -- formats a fixture so it can be displayed nicely to a user
    let home = unpack $ name $ homeTeam fixture -- extracts the home team from the fixture
        away = unpack $ name $ awayTeam fixture -- extracts the away team from the fixture
        date = formatUtcDate (unpack $ utcDate fixture) -- extracts the date and time of the fixture
    in home ++ " play " ++ away ++ " on " ++ date -- returns the fixture in a formatted string

formatUtcDate :: String -> String 
formatUtcDate rawDate = 
    maybe 
        rawDate -- if the raw date parse fails just return the unformatted raw date
        (formatTime defaultTimeLocale "%A %d %B %Y, %H:%M UTC") -- returns the date formatted with the day of the week, day of month, month, year and time
        (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" rawDate :: Maybe UTCTime)  -- parses the date extracted from the JSON object into a Maybe UTCTime

toOrdinal :: Int -> String 
toOrdinal num = show num ++ suffix -- converts an integer number into an ordinal number represented by a string
    where 
        suffix = case num `mod` 100 of 
            11 -> "th"
            12 -> "th"
            13 -> "th"
            _ -> case num `mod` 10 of 
                1 -> "st"
                2 -> "nd"
                3 -> "rd"
                _ -> "th"

