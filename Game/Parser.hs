-- | Parse room files, which are written in a custom language for adventure gaming.
module Game.Parser (
  parseFile, -- ^ The only exposed function is one which takes file contents and returns a parsed episode
  parseString
  ) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>), (<*>))
import Data.Char (isDigit)

import Data.String.Utils (replace)
import qualified Data.Map as Map

import Debug.Trace

import Game.Types

parseFile :: String -> IO String
parseFile filename = do
  contents <- readFile filename
  case parse parseEpisode filename (removeComments contents) of
    Left err -> return $ show err
    Right game -> return $ show game

parseString :: String -> Either (Int, Int) Episode
parseString contents =
  case parse parseEpisode "<interactive>" (removeComments contents) of
    Left err -> Left (sourceLine pos, sourceColumn pos)
      where pos = errorPos err
    Right episode -> Right episode

removeComments :: String -> String
removeComments = unlines . filter (not . isComment) . lines
  where 
    isComment line = case dropWhile (== ' ') line of
      '#':_ -> True
      _ -> False

data Decl = SynDecl Synonym
          | EnvDecl Environment
          | LocDecl Location 
          | ObjDecl Obj
          | ComDecl CommandPattern

decl2Env :: Decl -> Environment
decl2Env (EnvDecl e) = e

decl2Loc :: Decl -> Location
decl2Loc (LocDecl e) = e

decl2Syn :: Decl -> Synonym
decl2Syn (SynDecl e) = e

decl2Obj :: Decl -> Obj
decl2Obj (ObjDecl e) = e

decl2Com :: Decl -> CommandPattern
decl2Com (ComDecl e) = e

isEnv :: Decl -> Bool
isEnv (EnvDecl _) = True
isEnv _ = False

isLoc :: Decl -> Bool
isLoc (LocDecl _) = True
isLoc _ = False

isSyn :: Decl -> Bool
isSyn (SynDecl _) = True
isSyn _ = False

isObj :: Decl -> Bool
isObj (ObjDecl _) = True
isObj _ = False

isCom :: Decl -> Bool
isCom (ComDecl _) = True
isCom _ = False

-- | Parse an entire game.
parseEpisode :: Parser Episode
parseEpisode = do
  stateDecl <- stateParser
  items <- many toplevelItem

  -- Done after decls
  whitespace
  eof

  let envs = decl2Env <$> filter isEnv items
      locs = decl2Loc <$> filter isLoc items
      syns = decl2Syn <$> filter isSyn items
  return Episode {
              initState = stateDecl,
              synonyms = syns,
              environments = envs,
              rooms = locs
  }


toplevelItem :: Parser Decl 
toplevelItem = do
  declType <- parseDeclType ["location", "environment", "synonym"]
  name <- case declType of
    "synonym" -> parseStr
    _ -> many (noneOf " ;{")
  whitespace
  case declType of
    "location" -> LocDecl <$> braced (parseLocation name)
    "environment" -> EnvDecl <$> braced (parseEnvironment name)
    "synonym" -> SynDecl <$> parseSynonym name

parseSynonym :: String -> Parser Synonym
parseSynonym name = do
  synlist <- many parseStr
  semicolon
  return $ Synonym name synlist

parseStr :: Parser String
parseStr = do
  char '"'
  contents <- many (noneOf "\"")
  char '"'
  whitespace
  return contents

semicolon :: Parser ()
semicolon = void $ whitespace >> char ';' >> whitespace

parseLocation :: String -> Parser Location
parseLocation name = do
  items <- many locItem

  let objs = decl2Obj <$> filter isObj items
      coms = decl2Com <$> filter isCom items
  return Location {
              locationParent = Nothing,
              locationObjs = objs,
              locationCommands = coms,
              locationName = name
  }

parseEnvironment :: String -> Parser Environment
parseEnvironment name = do
  items <- many locItem

  let objs = decl2Obj <$> filter isObj items
      coms = decl2Com <$> filter isCom items
  return Environment {
              envParent = Nothing,
              envObjs = objs,
              envCommands = coms,
              envName = name
  }

parseDeclType :: [String] -> Parser String 
parseDeclType options = do
    whitespace 
    result <- choice $ map string options
    whitespace
    return result

locItem :: Parser Decl
locItem = do
  declType <- parseDeclType ["object", "command"]
  name <- many (noneOf " ;{")
  whitespace
  case declType of
    "object" -> ObjDecl <$> braced (parseObject name)
    "command" -> do
      pat <- parseCommandPat
      ComDecl <$> braced (parseCommand pat name)

parseObject :: String -> Parser Obj
parseObject name = do
  interpolated <- parseInterpolated
  return $ Obj name interpolated

parseCommandPat :: Parser [String]
parseCommandPat = many word
  where
  word = do
    contents <- many1 (noneOf " {")
    whitespace
    return contents

parseCommand :: [String] -> String -> Parser CommandPattern
parseCommand pat name = do
  actions <- many1 parseAction
  return $ Pattern (name:pat) actions

parseInterpolated :: Parser InterpolatedString
parseInterpolated = Interpolate <$> many (choice [strParser, exprParser])
  where
    strParser = Left <$> many1 (noneOf "`}")
    exprParser = do
      char '`'
      exp <- parseExpr
      char '`'
      return $ Right exp

parseExpr :: Parser Expression
parseExpr = do
  whitespace
  word <- parseWord
  if word `elem` functions
     then parseFun word
     else return $ if isValue word then makeValue word else Var word
  where
    parseWord = many1 (noneOf " `();")
    functions = ["add", "sub", "and", "or", "not", "eq"]

isValue :: String -> Bool
isValue (first:_) = isDigit first || first == '"'

makeValue :: String -> Expression
makeValue str = 
  case (head str, last str) of
    ('"', '"') -> StringVal $ init $ tail str 
    ('"', _) -> error "Unterminated quote in expression"
    (_, '"') -> error "Unstarted quote in expression"
    (_, _) -> IntVal $ read str

parseFun :: String -> Parser Expression
parseFun "add" = Add <$> parseArg <*> parseArg
parseFun "sub" = Sub <$> parseArg <*> parseArg
parseFun "and" = And <$> parseArg <*> parseArg
parseFun "or" = Or <$> parseArg <*> parseArg
parseFun "not" = Not <$> parseArg
parseFun "eq" = Equal <$> parseArg <*> parseArg
parseFun fun = error $ "Unknown function " ++ fun

parseArg :: Parser Expression
parseArg = do
  whitespace
  nextChar <- lookAhead anyChar
  case nextChar of
    '(' -> do
       char '('
       e <- parseExpr
       char ')'
       return e
    _ -> parseExpr


-- | Parse another parser surrounded by braces and whitespace.
braced :: Parser a -> Parser a
braced parser = do
  -- Parse opening brace and whitespace.
  whitespace
  char '{'
  whitespace

  -- Parse the actual values.
  val <- parser

  -- Parse closing brace and whitespace.
  whitespace
  char '}'
  whitespace
  
  return val

-- | Parse one action.
parseAction :: Parser Action
parseAction = whitespace >> choice (map try actionParsers) -- Choose one of the possible actions.
  where
    -- All possible actions.
    actionParsers = [respondParser, moveToParser,ifParser, assignParser]

-- | Parse a room switching action.
-- | These look like this:
-- |   move-to room-name;
moveToParser :: Parser Action
moveToParser = do
  whitespace
  string "move-to"
  whitespace
  room <- many (noneOf " ;")
  semicolon
  return $ MoveToRoom room

-- | Parse a string output action.
-- | These look like this:
-- |   respond { Out string }
respondParser :: Parser Action
respondParser = do
  whitespace
  string "respond"
  val <- braced parseInterpolated
  return $ Print $ processPrinted val

processPrinted :: InterpolatedString -> InterpolatedString
processPrinted (Interpolate pieces) = Interpolate $ map fixStrings pieces
  where
  fixStrings (Left str) = Left $ replace ('\n' : replicate maxNumSpaces ' ') "" str
  fixStrings x = x

  maxNumSpaces :: Int
  maxNumSpaces = minimum $ map numLeadingSpace $ lines $ joinPieces $ filter isStr pieces

  isStr (Left _) = True
  isStr _ = False

  joinPieces [] = ""
  joinPieces (Left str : rest) = str ++ joinPieces rest

  numLeadingSpace :: String -> Int
  numLeadingSpace str = length $ takeWhile (== ' ') str

ifParser :: Parser Action
ifParser = do
  whitespace
  string "if"
  whitespace
  exp <- between (char '`') (char '`') parseExpr

  thenActs <- braced $ many $ try parseAction
  whitespace >> string "else" >> whitespace
  elseActs <- braced $ many $ try parseAction
  return $ IfExpr exp thenActs elseActs

assignParser :: Parser Action
assignParser = do
  whitespace
  string "set"
  whitespace
  varname <- many (noneOf " ;")
  whitespace
  nextChar <- lookAhead anyChar
  exp <- if nextChar == '`'
        then between (char '`') (char '`') parseExpr
        else if isDigit nextChar || nextChar == '"'
             then do 
                word <- many1 (noneOf " ;")
                return $ makeValue word
             else error "Expecting values or backticks."
  semicolon

  return $ Assign varname exp

stateParser :: Parser GameState
stateParser = do
  whitespace
  string "state"
  whitespace
  braced parseAllVariables

parseAllVariables :: Parser GameState
parseAllVariables = do
  vars <- many parseVar :: Parser [(String, Either Int String)]
  return $ foldl (flip $ uncurry Map.insert) Map.empty vars

parseVar :: Parser (String, Either Int String)
parseVar = do
  whitespace
  string "variable"
  whitespace
  name <- many $ noneOf " "
  whitespace
  val <- parseVal
  semicolon
  return (name, val)

parseVal :: Parser (Either Int String)
parseVal = do
  value <- many $ noneOf " ;"
  return $ case (head value, last value) of
    ('"', '"') -> Right $ init $ tail value 
    ('"', _) -> error "Unterminated quote in default value"
    (_, '"') -> error "Unstarted quote in default value"
    (_, _) -> Left $ read value


-- | Parse whitespace, returning nothing.
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t\n"
