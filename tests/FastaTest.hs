module Main where

import qualified Fastx.Fasta as A
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

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

tests :: TestTree
tests =
    testGroup
        "FASTA Parsing Tests"
        [ testCase "Read One" $
            A.parseOne oneSeq @?= Right (A.Sequence "Description" "ABCDEF")
        , testCase "Read Many" $
            A.parseMany manySeqs @?= Right [A.Sequence "D1" "AGCT", A.Sequence "D2" "HELLO"]
        , testCase "From File" $
            A.parseFile
                "tests/test.fasta"
                >>= (@=? Right [A.Sequence "Amino acids" "ABCDEF", A.Sequence "Nucleic acids" "ACGT"])
        , testCase "Sequence to String" $
            A.write (A.Sequence "Description" "ABCDEF") @?= ">Description\nABCDEF\n"
        ]
