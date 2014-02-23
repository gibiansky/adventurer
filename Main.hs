{-# LANGUAGE OverloadedStrings #-}
-- The OverloadedStrings compiler extension allows your string literals to be
-- converted to types other than String. It defaults to String if necessary,
-- but if your data type implements the IsString typeclass and the fromString
-- constructor method, then you can write string literals for your data type.

module Main where

-- Snap Imports.
import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

-- Non-Snap Imports.
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString.Lazy hiding (drop, filter, readFile, zipWith, map, putStrLn, zip, find, split)
import System.Environment
import Data.Maybe (fromJust)
import Data.Aeson
import Data.List (find)
import Data.String.Utils (replace, split)

-- My imports.
import Game.Types
import Game.Parser
import Game.Actions

-- Qualified imports.
import qualified Data.Map as Map
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Chars

data UserId = UserId String
            deriving (Eq, Ord)

type EpisodeName = String

data ServerState = ServerState {
                   games :: Map.Map (UserId, EpisodeName) Game  
                 }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["parse", filename] -> do
      parsed <- parseFile filename
      case parsed of
        Left (err, line, col) -> do
          putStrLn $ "Error on line " ++ show line ++ ", column " ++ show col ++ "."
          putStrLn err
        Right _ -> putStrLn "Success."
    _ -> do
      -- Create a mutable variable where we store all game state.
      -- An MVar is a mutable variable which you can access inside the IO monad.
      st <- newMVar $ ServerState Map.empty

      -- Serve the site.
      quickHttpServe $ site st

logString :: String -> String -> IO ()
logString fname str = do
  contents <- readFile fname
  let newContents = contents ++ str
  Prelude.writeFile fname newContents

site :: MVar ServerState -> Snap ()
site st =
  -- Order of routes:
  --   First, try the homepage.
  --   Second, serve all static files.
  --   Finally, serve dynamic routes.
  mainSite <|> playGameSite <|> staticDir <|> otherRoutes

  where
    -- Serve up the index.html file as the homepage.
    mainSite = ifTop $ serveFile "html/index.html"

    -- Serve static files from /static
    staticDir = dir "static" $ serveDirectory "static"

    -- Serve the page creator
    playGameSite = route [(toStrict "play/:episode", serveGame st)]

    serveGame :: MVar ServerState -> Snap ()
    serveGame state = do
      episode <- Chars.unpack . fromJust <$> getParam "episode"
      fileData <- liftIO $ readFile "html/play.html"
      writeBS $ Chars.pack $ replace "{EPISODE}" episode fileData

    -- Use dynamic routes to interact with the server via JSON.
    -- The 'route' function takes an association list of bytestrings and handlers.
    otherRoutes = route $ Map.assocs $ Map.map useRoute routes

    -- Construct a Snap handler from a simpler function, which takes
    -- the request body as input and produces some output, possibly altering
    -- the global state.
    useRoute :: (ByteString -> State Game (ByteString, String)) -> Snap ()
    useRoute rt = do
      -- Read the request body (up to a maximum request size).
      input <- readRequestBody 10000

      episode <- fmap (Chars.unpack . fromJust) $ getParam "episode"
      userId <- fmap (Chars.unpack . fromJust) $ getParam "id"

      let key = (UserId userId, episode)
      (response, log) <- liftIO $ modifyMVar st $ \(ServerState state) -> 
         case Map.lookup key state of
           Nothing -> do
             contents <- readFile $ "episodes/" ++ episode ++ ".adv"
             let Right gameEp = parseString contents
                 game = gameFromEpisode gameEp
                 (out, (newGame, log)) = runState (rt input) game
             return (ServerState $ Map.insert key newGame state, (out, log))
             
           Just game -> 
             let (out, (newGame, log)) = runState (rt input) game in
               return (ServerState $ Map.insert key newGame state, (out, log))

      id <- getParam "id"
      logString id log

      -- Write the response back to the client.
      writeLBS response

    gameFromEpisode :: Episode -> Game
    gameFromEpisode episode = 
      let pregame = Game {
            history = [],
            currentRoom = findRoom episode "init",
            episode = episode,
            lastId = 0,
            gameState = initState episode
          } in
        run (Command 0 "start" Nothing) pregame
      where
        findRoom :: Episode -> String -> Location
        findRoom episode name = fromJust $ find ((== name) . envName . unLoc) (rooms episode)

-- JSON endpoints. Each of the values in the map is a function that takes an
-- input bytestring and produces an output bytestring, potentially modifying
-- the game state while at it.
routes :: Map.Map Strict.ByteString (ByteString -> State Game (ByteString, String))
routes = Map.fromList [
    (toStrict "run/:episode/:id", runCommand),
    (toStrict "history/:episode/:id", getHistory)
  ]
