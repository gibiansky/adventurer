{-# LANGUAGE OverloadedStrings #-}

-- | This module implements all the actual gameplay mechanics.
-- | The Action data type encodes all possible things that can
-- | happen in the game, and this module is responsible for updating
-- | game state and running actions to generate output.

module Game.Actions (
  runCommand, -- Keep a limited external interface.
  getHistory, -- Only include things that the web server actually needs.
  getItems,
  initGame
  ) where

import Control.Monad (foldM)
import Control.Monad.State (State, modify, gets)
import Control.Monad.Writer (runWriter, Writer, tell)
import Data.Aeson (encode, decode)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.String.Utils (replace)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy

import Game.Types

--- External interface ---
--------------------------

-- | Given a JSON-encoded command from the client, parse the command,
-- | run all actions that are triggered, update the game state, and
-- | return the response as a ByteString.  
runCommand :: Lazy.ByteString -> State Game Lazy.ByteString
runCommand commandStr = 
  -- Decode the command from the client.
  let Just command = decode commandStr in
      do
        -- Run the actual command.
        -- The 'modify' function invokes its argument (run command) on the
        -- current value of the state, and then updates the state.
        modify $ run command

        -- Get the last command in the history, encode it as JSON, and return.
        gets (encode . last . history)

-- | Ignoring the input, return the command history encoded as JSON.
getHistory :: Lazy.ByteString -> State Game Lazy.ByteString
getHistory _ = do
  -- Get all history from the game state.
  commands <- gets history

  -- Encode it as a JSON bytestring.
  return $ encode commands

-- | Ignoring the input, return the set of items as JSON.
getItems :: Lazy.ByteString -> State Game Lazy.ByteString
getItems _ = do
  -- Get all items.
  myItems <- gets items

  -- Encode them as JSON and return.
  return $ encode myItems

-- | Given the map of room names to rooms, create a new game. 
initGame :: Map.Map RoomName Room -> Game
initGame roomlist =
  -- Start in the room named 'init'.
  let room = fromJust $ Map.lookup "init" roomlist
      -- Create an empty game.
      game = Game {
        history = [],
        commandCounts = Map.empty,
        currentRoom = room,
        rooms = roomlist,
        lastId = 1,
        powers = [],
        items = []
      }
      -- Run the the enter actions and generate output.
      (initializedGame, initOut) = runWriter $ foldM runAction game $ enterActions room in

    -- Add a single command to the history which has the output the enter actions generated.
    initializedGame { history = [Command 0 "start" $ Just initOut]}

--- Module-private functions ---
--------------------------------

-- | Given a command and game, run the command and return the modified game.
-- | Although it may seem that this produces no output, the output is stored in the
-- | response to the command, which is now in the history of the new game.
run :: Command -> Game -> Game
run Command { commandString = cmdstr } game = 
  -- Try to find a power matching this command in the power definitions of the current room.
  case find (powerMatches cmdstr) (powerDefinitions $ currentRoom game) :: Maybe Power of
       -- If we don't find anything, return an error.
       Nothing -> 
         let powerName = head $ words cmdstr
             hasPowerWithName = powerName `elem` powers game
             errResponse = if hasPowerWithName then noSuchCommand else noSuchPower
             errcmd = Command (lastId game) cmdstr errResponse in
           -- Add the error to the history and increment the last ID.
           game {history = history game ++ [errcmd], lastId = 1 + lastId game}

       -- If we manage to find the power, run the triggered actions.
       Just pow -> 
         -- Run the actions triggered.
         -- runAction will run a single action, and we use the monadic fold (foldM) to 
         -- run all of the actions while keeping the state (the game) in the background.
         -- The state is the result of the fold, since runAction takes a game and outputs a game.
         --
         -- We use a writer to keep track of action output, which is why we need the monadic fold.
         -- See 'runAction' for more detail.
         let (game', cmdOutput) = runWriter $ foldM runAction game $ powerActions pow
             -- Create the new command with the command output and a new ID.
             newCommand = Command (lastId game) cmdstr $ Just cmdOutput

             -- Update the counts for this power in the command counts.
             newCounts = setOrModify 1 (+1) (powerName pow) $ commandCounts game'  in

           -- Create the new game with updated history, command counts, and last used ID.
           game' {history = history game ++ [newCommand], lastId = 1 + lastId game, commandCounts = newCounts}

-- | Run an action on a game and return the new game.
-- | Keep track of the output of the action using the Writer monad.
-- |
-- | The writer monad accumulates values using 'mappend' in the background,
-- | and you can give it a new value using 'tell'. Thus, each action is responsible
-- | for just 'tell'-ing its command output, and later it is all joined together.
runAction :: Game -> Action -> Writer String Game

-- Just output the given string.
-- Return the game unchanged.
runAction game (Print string) = do
  tell string
  return game

-- Just output the given string, resplacing underscores with the power name.
-- Add the power name to the list of usable powers.
runAction game (GainPower name dispstring) = do
  tell $ replace "_" name dispstring
  return $ game {powers = name : powers game}

-- Just output the given string, resplacing underscores with the power name.
-- Remove the power name from the list of usable powers.
runAction game (LosePower name dispstring) = do
  tell $ replace "_" name dispstring
  return $ game {powers = filter (/= name) $ powers game}

-- Output the nth string (looping around if n is greater than the number of options),
-- where n is the number of times that the power with the given name was used.
-- Return the game unchanged.
runAction game (ChooseByCount name strlist) = do
  let ct = Map.findWithDefault 0 name $ commandCounts game
  tell $ cycle strlist !! ct
  return game

-- Move to a new room.
-- The changes to the game are as follows:
--   1. Run the exit actions of the current room.
--   2. Change to the new room.
--   3. Run the enter actions of the new room.
runAction game (MoveToRoom name) = do
  -- Run the exit actions.
  afterExitGame <- foldM runAction game (exitActions $ currentRoom game)
  case Map.lookup name $ rooms game of
    Nothing -> error $ concat ["No room named ", name, " in room list!"]
    Just room -> do
      -- Move to the next room.
      let nextRoomGame = afterExitGame { currentRoom = room, commandCounts = Map.empty }

      -- Run the new room enter actions.
      foldM runAction nextRoomGame $ enterActions room

-- Print the display string.
-- Add the item to the inventory of the game.
runAction game (GainItem itemName dispstring) = do
  tell dispstring
  return game {items = itemName : items game}

-- Print the display string.
-- remove the item from the inventory of the game.
runAction game (LoseItem itemName dispstring) = do
  tell dispstring
  return game {items = filter (/= itemName) $ items game}

-- Check if the item is in the inventory.
-- If it is, run the first set of actions.
-- If it isn't, run the second set of items.
runAction game (IfPosessingItem itemName thenActs elseActs) =
  foldM runAction game (if itemName `elem` items game then thenActs else elseActs)

-- Trigger a power artificially.
-- Run all actions that this power would trigger.
runAction game (PowerTrigger cmdstr) =
  let pow = fromJust $ find (powerMatches cmdstr) (powerDefinitions $ currentRoom game) in
    foldM runAction game (powerActions pow)


--- Utilities ---
-----------------

-- | The command response when a matching power isn't found.
noSuchCommand :: CommandResponse
noSuchCommand = Just "error: no matching command found. enqueue headpat."

-- | The command response when the power is invalid.
noSuchPower :: CommandResponse
noSuchPower = Just "error: command doesn't exist. enqueue headpat."

-- | Set or modify a value in a hash map.
-- | If the value exists, modify it using the modifier function.
-- | If there is no such key, add the value.
setOrModify :: Ord k => a -> (a -> a) -> k -> Map.Map k a -> Map.Map k a
setOrModify val modifier key hashmap =
  case Map.lookup key hashmap of
       Nothing -> Map.insert key val hashmap
       Just oldval -> Map.insert key (modifier oldval) hashmap

-- | Check whether the command string matches the given power.
powerMatches :: String -> Power -> Bool
powerMatches str Power { powerName = name, powerArgs = args } =
  -- This power is a match if its name and arguments match the words in the string. 
  words str == name : args
