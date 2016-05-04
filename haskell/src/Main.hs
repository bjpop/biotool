{-
 - TODO: 
 -     handle fasta file from stdin
 -     ignore sequences with len < minlen
-}

{-# LANGUAGE RecordWildCards #-}
module Main where

import Bio.Sequence.Fasta
   ( readFasta, seqlength, Sequence )
import Options.Applicative 
   ( Parser, option, auto, long, metavar, help, value
   , some, argument, str, info, execParser, switch
   , fullDesc, (<*>), (<$>), (<>), (<|>), helper, progDesc )
import Data.List
    ( foldl', intersperse )
import System.IO
    ( hPutStrLn, stderr)

defaultMinLen :: Integer
defaultMinLen = 0

data Options = Options
    { minlen :: Integer
    , version :: Bool
    , verbose :: Bool
    , fastaFiles :: [FilePath]
    }
    deriving (Eq, Ord, Show)

defineOptions :: Parser Options
defineOptions = Options
    <$> option auto 
            (long "minlen"
            <> metavar "N"
            <> help ("Minimum length sequence to include in stats (default=" ++ show defaultMinLen ++ ")")
            <> value defaultMinLen)
    <*> switch (long "version" <> help "Print version and exit")
    <*> switch (long "verbose" <> help "Print more stuff about what's happening")
    <*> some (argument str (metavar "FASTA_FILE [FASTA_FILE ...]"))

main :: IO ()
main = do
    options <- execParser optionParser 
    processFastaFiles $ fastaFiles options
    where
    optionParser =
        info (helper <*> defineOptions)
             (fullDesc <> progDesc "Print fasta stats")

header :: String
header = "FILENAME\tTOTAL\tNUMSEQ\tMIN\tAVG\tMAX"

processFastaFiles :: [FilePath] -> IO ()
processFastaFiles files = putStrLn header >> mapM_ processFile files

processFile :: FilePath -> IO ()
processFile filePath = do
    sequences <- readFasta filePath
    let stats = foldl' updateStats initStats sequences
    case prettyOutput filePath stats of
        Left err -> hPutStrLn stderr err
        Right msg -> putStrLn msg

prettyOutput :: FilePath -> Stats -> Either String String
prettyOutput filePath stats@(Stats {..})
    | numSequences <= 0 = Left $ "Skipping " ++ filePath ++ " - doesn't seem to be FASTA?"
    | otherwise = Right $ concat $ intersperse "\t" (filePath : numbers)
  where
    average = round (fromIntegral numBases /
                     fromIntegral numSequences)
    numbers = [ show numSequences
              , show numBases
              , maybe "NaN" show minSequenceLength
              , if numSequences==0 then "NaN" else show average
              , maybe "NaN" show maxSequenceLength]

data Stats =
    Stats
    { numSequences :: !Integer
    , numBases :: !Integer
    , minSequenceLength :: Maybe Integer
    , maxSequenceLength :: Maybe Integer
    }
    deriving (Eq, Ord, Show)

initStats :: Stats
initStats = Stats
    { numSequences = 0
    , numBases = 0
    , minSequenceLength = Nothing
    , maxSequenceLength = Nothing
    }

updateStats :: Stats -> Sequence -> Stats
updateStats stats@(Stats {..}) sequence =
    stats { numSequences = numSequences + 1
          , numBases = numBases + thisLength
          , minSequenceLength = newLength min minSequenceLength
          , maxSequenceLength = newLength max maxSequenceLength
          }
    where
    thisLength = fromIntegral $ seqlength sequence
    newLength f old = f thisLength <$> old  <|> Just thisLength
