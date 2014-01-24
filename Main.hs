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

type ServerState = Map.Map String Game  

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["parse", filename] -> do
      parsed <- parseFile filename
      case parsed of
        Left (line, col) -> 
          putStrLn $ "Error on line " ++ show line ++ ", column " ++ show col ++ "."
        Right _ -> putStrLn "Success."
    _ -> do
      -- Create a mutable variable where we store all game state.
      -- An MVar is a mutable variable which you can access inside the IO monad.
      st <- newMVar Map.empty

      -- Serve the site.
      quickHttpServe $ site st

site :: MVar ServerState -> Snap ()
site st =
  -- Order of routes:
  --   First, try the homepage.
  --   Second, serve all static files.
  --   Finally, serve dynamic routes.
  mainSite <|> pageCreatorSite <|> adventureJs <|> staticDir <|> createGameRoute <|> otherRoutes

  where
    -- Serve up the index.html file as the homepage.
    mainSite = ifTop $ serveFile "html/index.html"

    -- Serve static files from /static
    staticDir = dir "static" $ serveDirectory "static"
    
    adventureJs = route [(toStrict "static/js/adventure.js", serveJs)]

    serveJs :: Snap ()
    serveJs = do
      req <- getRequest
      let referer = Chars.unpack $ fromJust $ getHeader "Referer" req :: String
      fileData <- liftIO $ readFile "static/js/adventure.js"

      let name = split "/" (drop 8 referer) !! 1
          newData = replace "{name}" name fileData
      writeBS $ Chars.pack newData
    

    -- Serve the page creator
    pageCreatorSite = route [(toStrict ":name/play", serveGame st)]

    serveGame :: MVar ServerState -> Snap ()
    serveGame state = do
      name <- fmap (Chars.unpack . fromJust) $ getParam "name"
      game <- liftIO $ fmap fromJust $ withMVar state $ \stateval -> return $ Map.lookup name stateval
      fileData <- liftIO $ readFile "html/play.html"
      let props = gameProperties game
          newData = replace "#{name}"        (propertyName props) . 
                    replace "#{background}"  (propertyBackground props) . 
                    replace "#{font-color}"  ("color: " ++ propertyFontColor props) . 
                    replace "#{font-family}" ("font-family: " ++ propertyFontFamily props) $ fileData
      writeBS $ Chars.pack newData

    createGameRoute = route [(toStrict ":name/create", createGame st)]

    createGame :: MVar ServerState -> Snap ()
    createGame state = do
      input <- readRequestBody 1000000
      name <- fmap (Chars.unpack . fromJust) $ getParam "name"
      let Just creation = decode input
      out <- liftIO $ modifyMVar state $ \st -> 
        case parseString (gameCode creation) of
          Left (line, col) -> return (st, CreationError line col)
          Right episode -> return (Map.insert name (createGameFromEpisode creation episode) st, CreationSuccess name)
      writeLBS $ encode out

    createGameFromEpisode :: GameCreation -> Episode -> Game
    createGameFromEpisode creation episode = 
      let pregame = Game {
            history = [],
            currentRoom = findRoom episode "init",
            episode = episode,
            lastId = 0,
            gameState = initState episode,
            gameProperties = GameProperties {
              propertyName = gameName creation,
              propertyBackground = backgroundColor creation,
              propertyFontColor = fontColor creation,
              propertyFontFamily = fontFamily creation
            }
          } in
        run (Command 0 "start" Nothing) pregame
      where
        findRoom :: Episode -> String -> Location
        findRoom episode name = fromJust $ find ((== name) . envName . unLoc) (rooms episode)

    -- Use dynamic routes to interact with the server via JSON.
    -- The 'route' function takes an association list of bytestrings and handlers.
    otherRoutes = route $ Map.assocs $ Map.map useRoute routes

    -- Construct a Snap handler from a simpler function, which takes
    -- the request body as input and produces some output, possibly altering
    -- the global state.
    useRoute :: (ByteString -> State Game ByteString) -> Snap ()
    useRoute rt = do
      -- Read the request body (up to a maximum request size).
      input <- readRequestBody 10000

      -- Modify the state and get the response. modifyMVar modifies the value
      -- of an MVar, while also producing a output value. Due to the weird
      -- type, we need the output of runState to be swapped and stuck in a
      -- monad in order to use it with modifyMVar.
      name <- fmap (Chars.unpack . fromJust) $ getParam "name"

      response <- liftIO $ modifyMVar st $ \state -> 
         case Map.lookup name state of
           Nothing -> error $ "Could not find name " ++ name
           Just game -> 
             let (out, newGame) = runState (rt input) game in
               return (Map.insert name newGame state, out)

      -- Write the response back to the client.
      writeLBS response

-- JSON endpoints. Each of the values in the map is a function that takes an
-- input bytestring and produces an output bytestring, potentially modifying
-- the game state while at it.
routes :: Map.Map Strict.ByteString (ByteString -> State Game ByteString)
routes = Map.fromList [
    (toStrict ":name/run", runCommand),
    (toStrict ":name/history", getHistory)
  ]
