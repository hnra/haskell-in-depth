module Main where

import Fmt
import Radar
import System.Environment (getArgs)

instance Buildable Direction where
  build :: Direction -> Builder
  build North = "N"
  build East = "E"
  build South = "S"
  build West = "W"

instance Buildable Turn where
  build :: Turn -> Builder
  build TNone = "--"
  build TLeft = "<-"
  build TRight = "->"
  build TAround = "||"

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname = do
  f <- readFile fname
  let turns = map read $ lines f
      finalDir = rotateMany dir turns
      dirs = rotateManySteps dir turns
  fmtLn $ "Final direction: " +|| finalDir ||+ ""
  fmt $ nameF "Intermediate directions" $ unwordsF dirs

orientFromFile :: FilePath -> IO ()
orientFromFile fname = do
  f <- readFile fname
  let dirs = map read $ lines f
      turns = orientMany dirs
  fmt $ nameF "All turns" $ unwordsF turns

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r", fname, dir] -> rotateFromFile (read dir) fname
    ["-o", fname] -> orientFromFile fname
    _usage ->
      putStrLn $
        "Usage: ch02 -o filename\n"
          ++ "       ch02 -r filename direction"
