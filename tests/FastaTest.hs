module Main where

import Fastx.Fasta
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain parseTests

oneSeq :: String
oneSeq =
    ">Description\n\
    \AbC\n\
    \DEf"

manySeqs :: String
manySeqs =
    ">D1\n\
    \AGCT\n\n\
    \>D2\n\
    \HE\n\
    \LLO\n"

parseTests :: TestTree
parseTests =
    testGroup
        "FASTA Parsing Tests"
        [ testCase "Read One" $
            parseOne oneSeq @?= Right (Sequence "Description" "ABCDEF")
        , testCase "Read Many" $
            parseMany manySeqs @?= Right [Sequence "D1" "AGCT", Sequence "D2" "HELLO"]
        , testCase "From File" $
            parseFile
                "tests/test.fasta"
                >>= (@=? Right [Sequence "Amino acids" "ABCDEF", Sequence "Nucleic acids" "ACGT"])
        ]
