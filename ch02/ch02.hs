{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Fmt
import System.Environment (getArgs)

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

data Direction = North | East | South | West
  deriving (Eq, Show, Bounded, Enum, CyclicEnum, Read)

instance Buildable Direction where
  build :: Direction -> Builder
  build North = "N"
  build East = "E"
  build South = "S"
  build West = "W"

data Turn = TNone | TLeft | TRight | TAround
  deriving (Eq, Show, Bounded, Enum, Read)

instance Buildable Turn where
  build :: Turn -> Builder
  build TNone = "--"
  build TLeft = "<-"
  build TRight = "->"
  build TAround = "||"

instance Semigroup Turn where
  (<>) :: Turn -> Turn -> Turn
  TNone <> t = t
  TLeft <> TLeft = TAround
  TLeft <> TAround = TRight
  TLeft <> TRight = TNone
  TRight <> TRight = TAround
  TRight <> TAround = TLeft
  TAround <> TAround = TNone
  t1 <> t2 = t2 <> t1

instance Monoid Turn where
  mempty :: Turn
  mempty = TNone

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir turns = rotate (mconcat turns) dir

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

orientMany :: [Direction] -> [Turn]
orientMany ds@(_ : _ : _) = zipWith orient ds (tail ds)
orientMany _ = []

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
