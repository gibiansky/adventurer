{-# LANGUAGE OverloadedStrings #-}
-- We need a separate Types module because circular imports are forbidden
-- in Haskell. Since pretty much all our modules need these types to be
-- available, we have no choice but to separate them out into a small module
-- separate from all the rest of the modules.

module Game.Types where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Debug.Trace
import Data.String.Utils (startswith)

import qualified Data.Map as Map

-- | All state and data necessary for the game. 
data Game = Game {
  history :: [Command],                   -- ^ A list of past commands and their responses.
  currentRoom :: Location,                -- ^ The current room the player is in.
  episode :: Episode,                     -- ^ The episode this game is for.
  lastId :: CommandId,                    -- ^ The last id assigned to a command.
  gameState :: GameState,
  gameProperties :: GameProperties
  } deriving Show

type GameState = Map.Map String (Either Int String)

data Episode = Episode {
  initState :: GameState,
  environments :: [Environment],
  rooms :: [Location]
  } deriving Show

-- | A command, before or after being interpreted.
-- | The command has a negative id and a Nothing response when it is 
-- | sent from the user to the server, and has those fields filled in
-- | when it is sent back.
data Command = Command {
    commandId :: CommandId,            -- ^ The unique id of the command.
    commandString :: String,           -- ^ The command string issued by the user.
    commandResponse :: CommandResponse -- ^ The response or Nothing if it is unevaluated.
  } deriving Show

-- | The response to a command is Just the output to show to the user
-- | or Nothing if the command has not yet been evaluated.
type CommandResponse = Maybe String

-- Allow reading and writing commands as JSON.
-- Implementing these two typeclasses allows us to use the 'encode' and
-- 'decode' functions to convert between Commands and ByteStrings.
instance ToJSON Command where
  toJSON (Command i cmd Nothing) = toJSON $ Command i cmd $ Just "No response."
  toJSON (Command i cmd (Just response)) = object ["id" .= i, "command" .= cmd, "response" .= response]
instance FromJSON Command where
  parseJSON (Object v) = Command <$> return (-1) <*> v .: "command" <*> return Nothing

  -- Commands must be objects, as they are toplevel entities.
  -- (That is, as opposed to "strings" or ints like 3.)
  parseJSON _ = mzero

-- | An action is a unit of things happening in the server.
-- | A response to a users input involves running many actions which
-- | can output text to the user or change the game state.
data Action 
  = Print InterpolatedString                    -- ^ Just output text to the user.
  | MoveToRoom String                           -- ^ Move to a new room, running the relevant exit and enter actions.
  | IfExpr Expression [Action] [Action]         -- ^ If statement. 
  | Assign VariableName Expression              -- ^ Assign a value to a variable in the global state.
  | Trigger String                              -- ^ Trigger a command as if the user entered it.
  deriving Show

-- Aliases to make type signatures cleaner and easier to read.
type CommandId = Int  

newtype Location = Location {unLoc :: Environment } deriving Show
data Environment = Environment {
  envName :: EnvironmentName,
  envParent :: Maybe EnvironmentName,
  envObjs :: [Obj],
  envCommands :: [CommandPattern],
  envSynonyms :: [Synonym]
  } deriving Show

type LocationName = String 
type ObjectName = String 
type EnvironmentName = String 
type VariableName = String 
data CommandPattern = Pattern [String] [Action]
                    deriving Show
data Obj = Obj ObjectName InterpolatedString
         deriving Show
data InterpolatedString = Interpolate [Either String Expression]
                          deriving Show
data Synonym = Synonym String [String]  deriving Show

data Expression = Var VariableName
                | Add Expression Expression
                | Sub Expression Expression
                | And Expression Expression
                | Or Expression Expression
                | Not Expression
                | Equal Expression Expression
                | GreaterThan Expression Expression
                | GreaterThanEq Expression Expression
                | LessThan Expression Expression
                | LessThanEq Expression Expression
                | StringVal String
                | IntVal Int  
                deriving Show

data GameCreation = CreateGame {
  gameName :: String,
  backgroundColor :: String,
  fontColor :: String,
  fontFamily :: String,
  gameCode :: String
  } deriving Show

data GameProperties = GameProperties {
  propertyName :: String,
  propertyBackground :: String,
  propertyFontColor :: String,
  propertyFontFamily :: String
  } deriving Show

data CreationResult = CreationError Int Int | CreationSuccess String

instance FromJSON GameCreation where
  parseJSON (Object v) = do
    name <- v .: "name"
    background <-  readBg
    fcolor <-  v .: "font-color"
    ffamily <-  v .: "font-family"
    code <-  v .: "code"
    return $ CreateGame name background fcolor ffamily code

    where
    readBg = do
      bgString <- v .: "background"
      return $ if startswith "data" (bgString :: String)
         then "background: url(" ++ bgString ++ ")"
         else  "background-image: none; background-color: " ++ bgString

  parseJSON _ = fail "Expecing an object."
               



instance ToJSON CreationResult where
  toJSON (CreationError line col) = object ["result" .= errstr, "line" .= line, "column" .= col]
  toJSON (CreationSuccess name) = object ["result" .= successstr, "name" .= name]

errstr :: String
errstr = "error"

successstr :: String
successstr = "success"
