module Fastx.Fasta (
    parseOne,
    parseMany,
    parseFile,
    Sequence (..),
) where

import Data.Functor
import Text.ParserCombinators.Parsec

data Sequence = Sequence {description :: String, sqn :: String} deriving (Show, Eq)

nucleicAcid :: Parser Char
nucleicAcid = oneOf "ACGTUIRYKMSWBDHVN-"

aminoAcid :: Parser Char
aminoAcid = oneOf "ABCDEFGHIJKLMNOPQRSTUVWYZX*-"

-- TODO: map toUpper
parseLine :: Parser String
parseLine = newline >> many (nucleicAcid <|> aminoAcid)

parseSequence :: Parser Sequence
parseSequence = do
    char '>'
    description <- many $ noneOf "\n"
    sqn <- many parseLine
    optional newline
    return $ Sequence description (concat sqn)

parseOne :: String -> Either ParseError Sequence
parseOne = parse parseSequence ""

parseMany :: String -> Either ParseError [Sequence]
parseMany = parse (many parseSequence) ""

parseFile :: FilePath -> IO (Either ParseError [Sequence])
parseFile = parseFromFile $ many parseSequence
