module Main where

import Data.Either
import qualified Fastx.Fastq as Q
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

oneSeq :: String
oneSeq =
    "@name\n\
    \AAAAAAAC\n\
    \+\n\
    \;;;;;;;;"

manySeqs :: String
manySeqs =
    "@EAS54_6_R1_2_1_413_324\n\
    \CCCTTCTTGTCTTCAGCGTTTCTCC\n\
    \+\n\
    \;;3;;;;;;;;;;;;7;;;;;;;88\n\
    \@EAS54_6_R1_2_1_540_792\n\
    \TTGGCAGGCCAAGGCCGATGGATCA\n\
    \+\n\
    \;;;;;;;;;;;7;;;;;-;;;3;83\n\
    \@EAS54_6_R1_2_1_443_348\n\
    \GTTGCTTCTGGCGTGGGTGGGGGGG\n\
    \+EAS54_6_R1_2_1_443_348\n\
    \;;;;;;;;;;;9;7;;.7;393333"

badLenQual :: String
badLenQual =
    "@badlen\n\
    \ACGT\n\
    \+\n\
    \123"

badRepeatSeqname :: String
badRepeatSeqname =
    "@originalname\n\
    \TGATTACTGTAATAAG\n\
    \+invalidname\n\
    \1132345234523455"

tests :: TestTree
tests =
    testGroup
        "FASTQ Parsing Tests"
        [ testCase "Read One" $
            Q.parseOne oneSeq @?= Right (Q.Sequence "name" "AAAAAAAC" ";;;;;;;;")
        , testCase "Read Many" $
            Q.parseMany manySeqs
                @?= Right
                    [ Q.Sequence "EAS54_6_R1_2_1_413_324" "CCCTTCTTGTCTTCAGCGTTTCTCC" ";;3;;;;;;;;;;;;7;;;;;;;88"
                    , Q.Sequence "EAS54_6_R1_2_1_540_792" "TTGGCAGGCCAAGGCCGATGGATCA" ";;;;;;;;;;;7;;;;;-;;;3;83"
                    , Q.Sequence "EAS54_6_R1_2_1_443_348" "GTTGCTTCTGGCGTGGGTGGGGGGG" ";;;;;;;;;;;9;7;;.7;393333"
                    ]
        , testCase "Read File" $
            Q.parseFile "tests/test.fastq"
                >>= ( @?=
                        Right
                            [ Q.Sequence "hi" "ACGT" ";;;;"
                            , Q.Sequence "hi2" "AAAA" "1111"
                            , Q.Sequence "hi3" "GTGT" "1234"
                            ]
                    )
        , testCase "Invalid Quality Length" $ assertBool "" $ isLeft $ Q.parseOne badLenQual
        , testCase "Invalid Repeat Seqname" $ assertBool "" $ isLeft $ Q.parseOne badRepeatSeqname
        , testCase "Sequence to String" $ Q.write (Q.Sequence "name" "AAAAAAAC" ";;;;;;;;") @?= (oneSeq ++ "\n")
        ]
