module Main where

import Fastx.Fasta
import System.Exit (exitFailure)

main :: IO ()
main = do
    someFunc
    putStrLn "just so I remember how to fail"
    exitFailure
