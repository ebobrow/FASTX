module Fastx.Convert (
    fastqToFasta,
    convertFile,
) where

import qualified Fastx.Fasta as A
import qualified Fastx.Fastq as Q
import System.Directory
import System.IO

fastqToFasta :: Q.Sequence -> A.Sequence
fastqToFasta (Q.Sequence seqname seq _) = A.Sequence seqname seq

convertFile :: FilePath -> IO ()
convertFile filename = do
    fastqSeqs <- Q.parseFile filename
    case fastqSeqs of
        Left e -> return () -- TODO: Better error handling
        Right seqs -> do
            let fastaSeqs = map fastqToFasta seqs
            handle <- openFile filename WriteMode
            mapM_ (hPutStr handle . (++ "\n") . A.write) fastaSeqs
            renameFile filename (convertFilename filename)
            hClose handle

-- TODO: actually make sure this is in the expected format?
convertFilename :: FilePath -> FilePath
convertFilename = (++ "a") . init
