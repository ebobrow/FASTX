module Fastx.Fastq (
    Sequence (..),
    parseOne,
    parseMany,
    parseFile,
) where

import Data.Char
import Text.Parsec
import Text.Parsec.String

data Sequence = Sequence {seqname :: String, seq :: String, quality :: String} deriving (Eq, Show)

parseLine :: Parser Char -> Parser String
parseLine p = newline >> many p

qualityValue :: Parser Char
qualityValue = oneOf "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

parseSequence :: Parser Sequence
parseSequence = do
    char '@'
    -- seqname <- many $ alphaNum <|> oneOf "_.:-"
    seqname <- many $ noneOf "\n"
    seq <- many $ parseLine $ letter <|> oneOf ".~"
    char '+'
    optional $ string seqname
    newline
    -- TODO: multi-line quality thing
    -- qual <- manyTill (parseLine qualityValue) (eof <|> lookAhead $ char '@')
    qual <- many qualityValue
    if length qual /= length (concat seq)
        then fail "Length of quality string doesn't match length of sequence"
        else optional newline >> optional newline
    return $ Sequence seqname (map toUpper $ concat seq) qual

parseOne :: String -> Either ParseError Sequence
parseOne = parse parseSequence ""

parseMany :: String -> Either ParseError [Sequence]
parseMany = parse (many parseSequence) ""

parseFile :: FilePath -> IO (Either ParseError [Sequence])
parseFile = parseFromFile $ many parseSequence
