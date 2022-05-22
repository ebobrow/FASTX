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
parseLine = do
    line <- many (nucleicAcid <|> aminoAcid)
    newline
    return line

parseSequence :: Parser Sequence
parseSequence = do
    char '>'
    description <- many $ noneOf "\n"
    char '\n'
    sqn <- many parseLine
    return $ Sequence description (concat sqn)

parseOne :: String -> Either ParseError Sequence
parseOne = parse parseSequence ""

parseMany :: String -> Either ParseError [Sequence]
parseMany = parse (many parseSequence) ""

parseFile :: FilePath -> IO (Either ParseError [Sequence])
parseFile = parseFromFile $ many parseSequence
