-- | Parse room files, which are written in a custom language for adventure gaming.
module Game.Parser (
  parseFile,
  parseString
  ) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>), (<*>))
import Data.Char (isDigit)

import Data.String.Utils (replace)
import qualified Data.Map as Map

import Debug.Trace

import Game.Types

data Decl = SynDecl { decl2Syn :: Synonym }
          | EnvDecl { decl2Env :: Environment }
          | LocDecl { decl2Loc :: Location }
          | ObjDecl { decl2Obj :: Obj }
          | ComDecl { decl2Com :: CommandPattern }


-- | Given a file, parse it, and either display the parsed result or an
-- error. Useful for debugging only.
parseFile filename = do
  contents <- readFile filename
  return $ parseString contents

-- | Given a data string, parse it, and return the parsed episode.
-- If there is an error, return the line and column number.
parseString :: String -> Either (String, Int, Int) Episode
parseString contents =
  case parse parseEpisode "<interactive>" (removeComments contents) of
    Left err -> Left (msgs, sourceLine pos, sourceColumn pos)
      where pos = errorPos err
            msgs = unlines $ tail $ lines $ show err
    Right episode -> Right episode

removeComments :: String -> String
removeComments = unlines . map removeComment . lines
  where 
    removeComment ('\\':'#':xs) = '#':removeComment xs
    removeComment ('#':_) = ""
    removeComment (x:xs) = x:removeComment xs
    removeComment [] = []

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
              environments = envs,
              rooms = locs
  }


toplevelItem :: Parser Decl 
toplevelItem = do
  declType <- parseDeclType ["location", "environment"]
  name <- many (noneOf " ;{")
  whitespace
  let parseParent = option Nothing $ do
        par <- many $ noneOf " ;{"
        return $ if null par
                 then Nothing
                 else Just par

  case declType of
    "location" -> do
      parent <- parseParent
      LocDecl <$> braced (parseLocation name parent)
    "environment" -> do
      parent <- parseParent
      EnvDecl <$> braced (parseEnvironment name parent)


parseStr :: Parser String
parseStr = do
  char '"'
  contents <- many (noneOf "\"")
  char '"'
  whitespace
  return contents

semicolon :: Parser ()
semicolon = void $ whitespace >> char ';' >> whitespace

parseLocation :: String -> Maybe EnvironmentName -> Parser Location
parseLocation name parent = Location <$> parseEnvironment name parent

parseEnvironment :: String -> Maybe EnvironmentName -> Parser Environment
parseEnvironment name parent = do
  items <- many envItem

  let objs = decl2Obj <$> filter isObj items
      coms = decl2Com <$> filter isCom items
      syns = decl2Syn <$> filter isSyn items
  return Environment {
              envParent = parent,
              envObjs = objs,
              envCommands = coms,
              envName = name,
              envSynonyms = syns
  }

parseDeclType :: [String] -> Parser String 
parseDeclType options = do
    whitespace 
    result <- choice $ map string options
    whitespace
    return result

envItem :: Parser Decl
envItem = do
  declType <- parseDeclType ["object", "command", "synonym"]
  whitespace
  case declType of
    "object" -> ObjDecl <$> parseObject
    "synonym" -> SynDecl <$> parseSynonym
    "command" -> ComDecl <$> parseCommand

parseSynonym :: Parser Synonym
parseSynonym = do
  name <- parseStr
  synlist <- many parseStr <?> "list of string literals"
  semicolon <?> "semicolon"
  return $ Synonym name synlist

parseObject :: Parser Obj
parseObject = do
  nameWords <- parseWords
  braced $ do
    interpolated <- parseInterpolated
    return $ Obj nameWords interpolated

parseWords :: Parser [String]
parseWords = many word
  where
  word = do
    contents <- many1 (noneOf " {")
    whitespace
    return contents

parseCommand :: Parser CommandPattern
parseCommand = do
  name:pat <- parseWords
  braced $ do
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
  foldl1 (<|>) $ map try $ 
     map parseFun functions ++ [parseExprVal]
  where
    functions = ["add", "sub", "and", "or", "not", "eq",
                 "neq", "gt", "gte", "lt", "lte"]

parseExprVal :: Parser Expression
parseExprVal = do
  val <- parseVal
  return $ case val of
    Left int -> IntVal int
    Right str -> StringVal str

parseFun :: String -> Parser Expression
parseFun word = do
  string word
  go word
  where
    go "add" = Add <$> parseArg <*> parseArg
    go "sub" = Sub <$> parseArg <*> parseArg
    go "and" = And <$> parseArg <*> parseArg
    go "or" = Or <$> parseArg <*> parseArg
    go "not" = Not <$> parseArg
    go "eq" = Equal <$> parseArg <*> parseArg
    go "neq" = Not <$> (Equal <$> parseArg <*> parseArg)
    go "gt" = GreaterThan <$> parseArg <*> parseArg
    go "gte" = GreaterThanEq <$> parseArg <*> parseArg
    go "lt" = LessThan <$> parseArg <*> parseArg
    go "lte" = LessThanEq <$> parseArg <*> parseArg
    go fun = error $ "Unknown function " ++ fun

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
    actionParsers = [respondParser, moveToParser, ifParser, assignParser, triggerParser]

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

-- | Parse a trigger action.
-- | These look like this:
-- |   trigger word1 word2 ... wordn;
triggerParser :: Parser Action
triggerParser = do
  whitespace
  string "trigger"
  whitespace
  cmd <- many (noneOf ";")
  semicolon
  return $ Trigger cmd

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
  elseActs <- option [] $ do
    whitespace >> string "else" >> whitespace
    braced $ many $ try parseAction
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
             then parseExprVal
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
  vars <- many parseVar
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
parseVal = parseString <|> parseInt
  where
    parseInt = try $ do
      value <- many $ noneOf " ;"
      return $ Left $ read value
    parseString = try $ do
      char '"'
      str <- many $ noneOf "\""
      char '"'
      return $ Right str

-- | Parse whitespace, returning nothing.
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t\n"
