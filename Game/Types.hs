{-# LANGUAGE OverloadedStrings #-}
-- We need a separate Types module because circular imports are forbidden
-- in Haskell. Since pretty much all our modules need these types to be
-- available, we have no choice but to separate them out into a small module
-- separate from all the rest of the modules.

module Game.Types where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson

import qualified Data.Map as Map

-- | All state and data necessary for the game. 
data Game = Game {
  history :: [Command],                   -- ^ A list of past commands and their responses.
  commandCounts :: Map.Map PowerName Int, -- ^ Number of times that each power name has been used in this room.
  currentRoom :: Room,                    -- ^ The current room the player is in.
  rooms :: Map.Map RoomName Room,         -- ^ The list of rooms (keyed by their names).
  powers :: [PowerName],                  -- ^ The list of powers the players can use.
  lastId :: CommandId,                    -- ^ The last id assigned to a command.
  items :: [ItemName]                     -- ^ All items that the player has in their inventory.
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

-- A room. Rooms represent distinct areas of the game, where 
-- powers take on unique meanings. The meaning of a power depends on
-- the definition of that power in the current room.  
data Room = Room {
  enterActions :: [Action],   -- ^ Actions to take upon entering a room.
  exitActions :: [Action],    -- ^ Actions to take upon exiting a room.
  powerDefinitions :: [Power] -- ^ Definitions of powers in this room.
  } deriving Show

-- | An action is a unit of things happening in the server.
-- | A response to a users input involves running many actions which
-- | can output text to the user or change the game state.
data Action 
  = Print String                                -- ^ Just output text to the user.
  | GainPower PowerName String                  -- ^ Gain use of a power and notify the user about it.
  | LosePower PowerName String                  -- ^ Lose access to a power and notify the user about it.
  | ChooseByCount PowerName [String]            -- ^ Output something to the user. Cycle through different 
                                                -- ^ options, depending on how many times a given power has been used.
  | MoveToRoom String                           -- ^ Move to a new room, running the relevant exit and enter actions.
  | GainItem ItemName String                    -- ^ Gain an item and notify the user.
  | LoseItem ItemName String                    -- ^ Lose an item and notify the user.
  | IfPosessingItem ItemName [Action] [Action]  -- ^ Choose between a set of actions depending on whether an item is in inventory.
  | PowerTrigger String                         -- ^ Artificially trigger actions associated with a certain power.  
  deriving Show

-- | A power triggered by the user typing the power string.
data Power = Power {
    powerName :: PowerName,     -- ^ The name of the power (the first word).
    powerArgs :: [PowerArg],    -- ^ The arguments to the power (all but the first word).
    powerActions :: [Action]    -- ^ The actions to run upon encountering this power used.
  } deriving Show

-- Define power equality as equality in the name and arguments.
instance Eq Power where
  Power powname1 powargs1 _ == Power powname2 powargs2 _ = 
    (powargs1 == powargs2) && powname1 == powname2

-- Aliases to make type signatures cleaner and easier to read.
type CommandId = Int  
type ItemName = String
type EventName = String
type RoomName = String
type PowerName = String
type PowerArg = String
