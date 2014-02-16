{-# LANGUAGE OverloadedStrings #-}

-- | This module implements all the actual gameplay mechanics.
-- | The Action data type encodes all possible things that can
-- | happen in the game, and this module is responsible for updating
-- | game state and running actions to generate output.

module Game.Actions (
  runCommand, -- Keep a limited external interface.
  getHistory, -- Only include things that the web server actually needs.
  run
  ) where

import Control.Monad (foldM, join)
import Control.Applicative
import Control.Monad.State (State, modify, gets)
import Control.Monad.Writer (runWriter, Writer, tell)
import Data.Aeson (encode, decode)
import Data.List (find, foldl', isInfixOf)
import Data.List.Utils (replace, startswith)
import Data.Maybe (fromJust, fromMaybe)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy

import Debug.Trace

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

--- Module-private functions ---
--------------------------------

-- | Given a command and game, run the command and return the modified game.
-- | Although it may seem that this produces no output, the output is stored in the
-- | response to the command, which is now in the history of the new game.
run :: Command -> Game -> Game
run Command { commandString = cmdstr } game = 
  -- Try to find a power matching this command in the power definitions of the current room.
  case findMatchingCommand cmdstr game of
       -- If we don't find anything, return an error.
       Nothing -> let errcmd = Command (lastId game) cmdstr noSuchCommand in
           -- Add the error to the history and increment the last ID.
           game {history = history game ++ [errcmd], lastId = 1 + lastId game}

       -- If we manage to find the power, run the triggered actions.
       Just (Pattern _ actions) -> 
         -- Run the actions triggered.
         -- runAction will run a single action, and we use the monadic fold (foldM) to 
         -- run all of the actions while keeping the state (the game) in the background.
         -- The state is the result of the fold, since runAction takes a game and outputs a game.
         --
         -- We use a writer to keep track of action output, which is why we need the monadic fold.
         -- See 'runAction' for more detail.
         let (game', cmdOutput) = runWriter $ foldM runAction game actions
             -- Create the new command with the command output and a new ID.
             newCommand = Command (lastId game) cmdstr $ Just cmdOutput in

           -- Create the new game with updated history, command counts, and last used ID.
           game' {history = history game ++ [newCommand], lastId = 1 + lastId game}

-- Convert an evaluated expression into a 1 or 0
getBool :: Either Int String -> Bool
getBool expr = 
  case expr of
    Left i -> i /= 0
    Right s -> s /= ""

getBools ::  Game -> Expression -> Expression -> (Bool, Bool)
getBools game e1 e2 =
  let evaledExp1 = evaluateExpression game e1
      evaledExp2 = evaluateExpression game e2
      truthVal1 = getBool evaledExp1
      truthVal2 = getBool evaledExp2 in
    (truthVal1, truthVal2)

-- | Operations on expressions
evaluateExpression :: Game -> Expression -> Either Int String
evaluateExpression game (Var varName) = fromMaybe err (Map.lookup varName $ gameState game)
  where
    err = error $ concat ["No state variable named ", varName, "! You be cray."]
 
evaluateExpression game (Add exp1 exp2) = 
  let evaledExp1 = evaluateExpression game exp1
      evaledExp2 = evaluateExpression game exp2 in
    case (evaledExp1, evaledExp2) of
      (Left i1, Left i2) -> Left $ i1 + i2  
      (Right s1, Right s2) -> Right $ s1 ++ s2
      (Left i1, Right s2) -> Right $ show i1 ++ s2
      (Right s1, Left i2) -> Right $ s1 ++ show i2
     
evaluateExpression game (Sub exp1 exp2) = 
  let evaledExp1 = evaluateExpression game exp1
      evaledExp2 = evaluateExpression game exp2 in
    case (evaledExp1, evaledExp2) of
      (Left i1, Left i2) -> Left $ i1 - i2  
      _ -> error "Trying to subtract string"

evaluateExpression game (And exp1 exp2) = 
    Left $ if uncurry (&&) $ getBools game exp1 exp2 then 1 else 0

evaluateExpression game (Or exp1 exp2) = 
    Left $ if uncurry (||) $ getBools game exp1 exp2 then 1 else 0

evaluateExpression game (Not exp) = 
  let evaledExp = evaluateExpression game exp 
      truthiness = getBool evaledExp in
    Left $ if truthiness then 0 else 1

evaluateExpression game (Equal exp1 exp2) = 
  let evaledExp1 = evaluateExpression game exp1
      evaledExp2 = evaluateExpression game exp2 in
    case (evaledExp1, evaledExp2) of
      (Left i1, Left i2) -> Left $ if i1 == i2 then 1 else 0
      (Right s1, Right s2) -> Left $ if s1 == s2 then 1 else 0
      _ -> Left 0

evaluateExpression game (GreaterThan exp1 exp2) =
  let evaledExp1 = evaluateExpression game exp1
      evaledExp2 = evaluateExpression game exp2 in
    case (evaledExp1, evaledExp2) of
      (Left i1, Left i2) -> Left $ if i1 > i2 then 1 else 0
      (Right s1, Right s2) -> Left $ if s1 > s2 then 1 else 0
      _ -> Left 0

evaluateExpression game (LessThan exp1 exp2) =
  let evaledExp1 = evaluateExpression game exp1
      evaledExp2 = evaluateExpression game exp2 in
    case (evaledExp1, evaledExp2) of
      (Left i1, Left i2) -> Left $ if i1 < i2 then 1 else 0
      (Right s1, Right s2) -> Left $ if s1 < s2 then 1 else 0
      _ -> Left 0

evaluateExpression game (GreaterThanEq exp1 exp2) = evaluateExpression game $
  Or (GreaterThan exp1 exp2) (Equal exp1 exp2)

evaluateExpression game (LessThanEq exp1 exp2) = evaluateExpression game $
  Or (LessThan exp1 exp2) (Equal exp1 exp2)

evaluateExpression _ (StringVal string) = Right string

evaluateExpression _ (IntVal int) = Left int

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
  tell $ evalInterpolation game string
  return game

runAction game (MoveToRoom name) = return game { currentRoom = findRoom game name }
  where
    findRoom :: Game -> String -> Location
    findRoom game name = fromJust $ find ((== name) . envName . unLoc) (rooms $ episode game)

runAction game (IfExpr expr thens elses) = foldM runAction game $
  if getBool $ evaluateExpression game expr
  then thens
  else elses

runAction game (Assign varname expr) =
  let result = evaluateExpression game expr in
    return game { gameState = assign (gameState game) varname result }
  where
    assign :: GameState -> String -> Either Int String -> GameState
    assign state var value = Map.insert var value state

runAction game (Trigger cmdstr) =
  case findMatchingCommand cmdstr game of
       Nothing -> error $ "Broken trigger: " ++ cmdstr
       Just (Pattern _ actions) -> foldM runAction game actions

evalInterpolation :: Game -> InterpolatedString -> String
evalInterpolation game (Interpolate interp) = foldl (joinInterp game) "" interp
  where
    joinInterp :: Game -> String -> Either String Expression -> String
    joinInterp _ str (Left nextStr) = str ++ nextStr
    joinInterp game str (Right expr) = str ++ evalStr (evaluateExpression game expr)
      where
        evalStr (Left int) = show int
        evalStr (Right str) = str

--- Utilities ---
-----------------

-- | The command response when a matching power isn't found.
noSuchCommand :: CommandResponse
noSuchCommand = Just "error: command unavailable."

findMatchingCommand :: String -> Game -> Maybe CommandPattern
findMatchingCommand cmdstr game = case trace ("syn " ++ synonymedStr ++ " from " ++ cmdstr) $ words synonymedStr of
  "look":objwords -> case objResult objwords of
    Nothing -> trace ("Failing to find obj " ++ show objwords) cmdResult
    Just (Obj name description) -> Just $ Pattern ("look":name) [Print description]
  _ -> cmdResult
  where
    env = unLoc $ currentRoom game
    syns = findSynonyms game env
    synonymedStr = applySynonyms syns cmdstr
    cmdResult = findMatchingCommand' synonymedStr game env
    objResult objwords = findMatchingObject objwords game $ unLoc $ currentRoom game

findMatchingCommand' :: String -> Game -> Environment -> Maybe CommandPattern
findMatchingCommand' cmdstr game loc = 
  find (cmdMatches cmdstr game loc) cmds <|>
    join (findMatchingCommand' cmdstr game <$> parentEnv)
  where
    cmds = envCommands loc
    parentEnv = flip findEnvWithName game <$> envParent loc

findMatchingObject objwords game loc = 
  find (objMatches objwords loc) objs <|>
  join (findMatchingObject objwords game <$> parentEnv)
  where
    objs = envObjs loc
    parentEnv = flip findEnvWithName game <$> envParent loc

objMatches :: [String] -> Environment -> Obj  -> Bool
objMatches objwords env (Obj objname _) = wordsMatch objwords objname

cmdMatches :: String -> Game -> Environment -> CommandPattern -> Bool
cmdMatches str game env (Pattern pat _) = wordsMatch (words str) pat


-- Returns the synonyms with the childrens synonyms first.
findSynonyms :: Game -> Environment -> [Synonym]
findSynonyms game env = parentSyns ++ envSynonyms env
  where parentSyns =
          case flip findEnvWithName game <$> envParent env of
            Nothing -> []
            Just env' -> findSynonyms game env'

wordsMatch :: [String] -> [String] -> Bool
wordsMatch _ ("*":_) = True
wordsMatch (x:xs) (y:ys) = x == y && wordsMatch xs ys
wordsMatch [] [] = True
wordsMatch _ _ = False

applySynonyms :: [Synonym] -> String -> String
applySynonyms syns str =
  let applicable = filter isApplicable syns
      remaining = filter (not . isApplicable) syns in
    case applicable of 
      [] -> str
      syns' -> applySynonyms remaining $ foldl' doApply str syns'

  where
    isApplicable (Synonym to from) = 
      let infixMatch = any (`isInfixOf` str) $ map (' ':) from
          prefixMatch = any (`startswith` str) $ map (++ " ") from in
        infixMatch || prefixMatch
    doApply string (Synonym to from) =
      let postSpace = (++ " ")
          preSpace = (' ':)
          replacer s rep = replace (preSpace rep) (preSpace to) . replace (postSpace rep) (postSpace to) $ s in
        foldl' replacer str from

findEnvWithName :: EnvironmentName -> Game -> Environment
findEnvWithName name game = fromJust $ find ((== name) . envName) $ environments $ episode game
