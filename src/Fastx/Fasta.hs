module Fastx.Fasta (
    parseOne,
    parseMany,
    parseFile,
    Sequence (..),
) where

import Data.Char
import Data.Functor
import Text.Parsec
import Text.Parsec.String

data Sequence = Sequence {description :: String, seq :: String} deriving (Show, Eq)

nucleicAcid :: Parser Char
nucleicAcid = oneOf "ACGTUIRYKMSWBDHVN-"

aminoAcid :: Parser Char
aminoAcid = oneOf "ABCDEFGHIJKLMNOPQRSTUVWYZX*-"

parseLine :: Parser String
parseLine = newline >> many (letter <|> oneOf "*-")

-- TODO: support for comments?
parseSequence :: Parser Sequence
parseSequence = do
    char '>'
    description <- many $ noneOf "\n"
    seq <- many parseLine
    optional newline
    return $ Sequence description (map toUpper $ concat seq)

parseOne :: String -> Either ParseError Sequence
parseOne = parse parseSequence ""

parseMany :: String -> Either ParseError [Sequence]
parseMany = parse (many parseSequence) ""

parseFile :: FilePath -> IO (Either ParseError [Sequence])
parseFile = parseFromFile $ many parseSequence
