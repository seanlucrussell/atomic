{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative (Alternative (empty), liftA2)
import Control.Effect.Fail (Fail, Has, fail)
import Control.Monad (ap)
import Data.Char (isHexDigit, ord)
import qualified Data.Functor
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))
import Numeric (readHex)
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)

{-

Here is the data type we would like to parse into, ideally

data Instruction
  = UseFrom (NonEmpty Name) Term Instruction
  | Assign (NonEmpty Name) Term Instruction
  | Reveal Term

data Term
  = Apply (NonEmpty Term)
  | Record [MapEntry]
  | RecordAccess Term (NonEmpty Name)
  | Reference Name
  | GlobalReference Int
  | Lambda (NonEmpty Name) Term
  | Import Term
  | StringLiteral String
  | NatLiteral Int
  | GetChar
  | PutChar
  | Exit

data MapEntry
  = MirrorNameWithLabel Name
  | LabelToTerm Name Term

data Name = Name String

-}

-- Data Types
type Program = [Instruction]

data Term
  = Lambda Name Term
  | App Term Term
  | Ref Name
  | GlobalRef Int
  | MapLiteral [MapEntry]
  | MapAccess Term Name
  | GetChar
  | PutChar
  | Exit
  | StringLiteral String
  | NatLiteral Int
  | ChrLiteral Char
  | Import Term
  deriving (Show)

newtype Name = Name String deriving (Show, Eq, Ord)

instance IsString Term where
  fromString :: String -> Term
  fromString = Ref . Name

instance IsString Name where
  fromString :: String -> Name
  fromString = Name

hexParser :: Parser Int
hexParser = do
  hexChars <- many1 (satisfy isHexDigit)
  case readHex hexChars of
    [(n, "")] -> return n
    _ -> fail "Invalid hex format"

inlineWhitespace :: Parser Char
inlineWhitespace = try (commentParser >> optional newline >> char ' ')

lineSpacing :: Parser ()
lineSpacing = skipMany inlineWhitespace

spaceSeparator :: Parser ()
spaceSeparator = skipMany1 inlineWhitespace

name :: Parser Name
name = Name <$> many1 letter

tryAll :: [Parser a] -> Parser a
tryAll = foldl (<|>) (fail "Nothing worked") . fmap try

termParser :: Parser Term
termParser = appParser

natParser :: Parser Term
natParser = NatLiteral . read <$> many1 digit

chrParser :: Parser Term
chrParser = ChrLiteral <$> between (char '\'') (char '\'') anyChar

strParser :: Parser Term
strParser = StringLiteral <$> between (char '"') (char '"') (many (noneOf "\""))

lambdaParser :: Parser Term
lambdaParser = strSymbol "<" *> liftA2 manyLambdas (sepEndBy1 name lineSpacing) (strSymbol "|" *> termParser <* strSymbol ">")

manyLambdas :: [Name] -> Term -> Term
manyLambdas s t = foldr Lambda t s

groupParser :: Parser Term
groupParser = between (strSymbol "(") (strSymbol ")") termParser

getChar :: Parser Term
getChar = string "GETCHAR" >> return GetChar

putChar :: Parser Term
putChar = string "PUTCHAR" >> return PutChar

exit :: Parser Term
exit = string "EXIT" >> return Exit

importParser :: Parser Term
importParser = string "@IMPORT" >> lineSpacing >> Import <$> termParser

baseTokens :: Parser Term
baseTokens =
  tryAll
    [ Parser.getChar,
      Parser.putChar,
      importParser,
      exit,
      globalRefParser,
      refParser,
      natParser,
      strParser,
      chrParser
    ]

appParser :: Parser Term
appParser = do
  fn : args <-
    sepEndBy1
      ( tryAll
          [ lambdaParser,
            mapAccessParser,
            mapParser,
            groupParser,
            baseTokens
          ]
      )
      lineSpacing
  return (foldl App fn args)

refParser :: Parser Term
refParser = Ref <$> name

globalRefParser :: Parser Term
globalRefParser = GlobalRef <$> (char '&' *> hexParser)

data MapEntry = MapEntry Name Term deriving (Show)

labelTermParser :: Parser MapEntry
labelTermParser = MapEntry <$> name <* strSymbol ":" <*> termParser

mirrorNameParser :: Parser MapEntry
mirrorNameParser = do
  n <- name
  pure (MapEntry n (Ref n))

mapEntry :: Parser MapEntry
mapEntry = try labelTermParser <|> try mirrorNameParser

symbol :: Parser a -> Parser a
symbol = between lineSpacing lineSpacing

strSymbol :: String -> Parser String
strSymbol = symbol . string

mapParser :: Parser Term
mapParser = MapLiteral <$> (strSymbol "{" *> sepBy mapEntry (try $ strSymbol ",") <* strSymbol "}")

mapAccessParser :: Parser Term
mapAccessParser = do
  t <- tryAll [groupParser, mapParser, globalRefParser, refParser]
  char '.'
  accesses <- sepBy name (char '.')
  return (foldr (flip MapAccess) t accesses)

assignmentParser :: Parser Instruction
assignmentParser = do
  n : args <- sepEndBy1 name lineSpacing
  strSymbol "="
  term <- symbol termParser
  sepBy endOfLineParser newline
  Assign n (manyLambdas args term) <$> lineParser

commentParser :: Parser ()
commentParser = optional (string "--" >> skipMany (noneOf "\n"))

useFromParser :: Parser Instruction
useFromParser = do
  string "use"
  spaceSeparator
  names <- sepBy1 name (try $ strSymbol ",")
  spaceSeparator
  string "from"
  spaceSeparator
  term <- symbol termParser
  sepBy endOfLineParser newline
  UseFrom names term <$> lineParser

revealParser :: Parser Instruction
revealParser = string "reveal" >> spaceSeparator >> Reveal <$> termParser <* sepBy endOfLineParser newline <* eof

endOfLineParser :: Parser ()
endOfLineParser = lineSpacing >> commentParser

data Instruction = UseFrom [Name] Term Instruction | Assign Name Term Instruction | Reveal Term deriving (Show)

lineParser :: Parser Instruction
lineParser = try assignmentParser <|> try useFromParser <|> try revealParser

parseProgram :: (Has Fail sig m, MonadFail m) => FilePath -> String -> m Instruction
parseProgram = atomParse (skipMany (endOfLineParser >> newline) >> lineParser)

atomParse :: (Has Fail sig m, MonadFail m) => Parser a -> FilePath -> String -> m a
atomParse parser path = either (fail . show) pure . parse parser path
