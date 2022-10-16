module Main where

import Control.Monad (when)
import Data.Char (isLetter)
import Data.List (group, sort, sortBy)
import Data.Ord (Down (Down), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import System.Environment

type Entry = (Text, Int)

type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    cleanWord = T.dropAround (not . isLetter)
    buildEntry xs@(x : _) = (x, length xs)

allWords :: Vocabulary -> [Text]
allWords = map fst

allWordsReport :: Vocabulary -> Text
allWordsReport vocab =
  fmt $ nameF "All words" $ unlinesF (allWords vocab)

wordsCount :: Vocabulary -> (Int, Int)
wordsCount = foldr sumAndCount (0, 0)
  where
    sumAndCount :: Entry -> (Int, Int) -> (Int, Int)
    sumAndCount (_, count) (total, unique) = (total + count, unique + 1)

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab =
  fmt $
    "Total number of words: " +| total
      |+ "\nnumber of unique words: " +| unique
      |+ "\n"
  where
    (total, unique) = wordsCount vocab

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy (comparing $ Down . snd)

frequentWordsReport :: Vocabulary -> Int -> Text
frequentWordsReport vocab num =
  fmt $
    nameF "Frequent words" $
      blockListF' "" fmtEntry reportData
  where
    reportData = take num $ wordsByFrequency vocab
    fmtEntry (t, n) = "" +| t |+ ": " +| n |+ ""

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words:"
  TIO.putStrLn $ T.unlines $ map fst vocab

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname withAllWords n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  when withAllWords $ TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-a", fname, num] -> processTextFile fname True (read num)
    [fname, num] -> processTextFile fname False (read num)
    _ -> putStrLn "Usage: ch01 [-a] filename freq_words_count"
