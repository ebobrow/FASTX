module Main where

import Fastx.Convert
import qualified Fastx.Fasta as A
import qualified Fastx.Fastq as Q
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

fastqSqns :: String
fastqSqns =
    "@hi\n\
    \ACGT\n\
    \+\n\
    \;;;;\n\
    \\n\
    \@hi2\n\
    \AAAA\n\
    \+\n\
    \1111\n\
    \\n\
    \@hi3\n\
    \GTGT\n\
    \+hi3\n\
    \1234"

fastaSqns :: String
fastaSqns =
    ">hi\n\
    \ACGT\n\n\
    \>hi2\n\
    \AAAA\n\n\
    \>hi3\n\
    \1234\n"

tests :: TestTree
tests =
    testGroup
        "FASTQ to FASTA Conversion Tests"
        [ testCase "Convert One" $
            fastqToFasta (Q.Sequence "name" "ABC" "!!!") @?= A.Sequence "name" "ABC"
            -- TODO: This won't work ("resource busy")
            -- , testCase "Convert File" $
            --     do
            --         (file, handle) <- openTempFile "/tmp" "sqns.fastq"
            --         hPutStr handle fastqSqns
            --         convertFile file
            --         res <- hGetContents handle
            --         hClose handle
            --         removeFile file
            --         res @?= fastaSqns
        ]
