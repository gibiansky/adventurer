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
import Data.ByteString.Lazy hiding (filter, readFile, zipWith, map, putStrLn, zip)
import Data.String.Utils
import Data.Tuple
import System.Directory

-- My imports.
import Game.Types
import Game.Parser
import Game.Actions

-- Qualified imports.
import qualified Data.Map as Map
import qualified Data.ByteString as Unlazy

-- | The directory with the game specification files.
gameDirectory :: FilePath
gameDirectory = "rooms"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["parse", filename] -> parseFile filename
    ["serve"] -> do
      -- Create a mutable variable where we store all game state.
      -- An MVar is a mutable variable which you can access inside the IO monad.
      st <- newMVar initGame

      -- Serve the site.
      quickHttpServe $ site st

site :: MVar Game -> Snap ()
site st =
  -- Order of routes:
  --   First, try the homepage.
  --   Second, serve all static files.
  --   Finally, serve dynamic routes.
  mainSite <|> staticDir <|> otherRoutes

  where
    -- Serve up the index.html file as the homepage.
    mainSite = ifTop $ serveFile "html/index.html"

    -- Serve static files from /static
    staticDir = dir "static" $ serveDirectory "static"

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
      response <- liftIO $ modifyMVar st $ return . swap . runState (rt input)

      -- Write the response back to the client.
      writeLBS response

-- JSON endpoints. Each of the values in the map is a function that takes an
-- input bytestring and produces an output bytestring, potentially modifying
-- the game state while at it.
routes :: Map.Map Unlazy.ByteString (ByteString -> State Game ByteString)
routes = Map.fromList [
    (toStrict "run", runCommand),
    (toStrict "history", getHistory),
    (toStrict "items", getItems)
  ]
