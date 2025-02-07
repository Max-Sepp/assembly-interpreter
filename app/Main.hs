module Main where

import Data.Char (isSpace)
import Data.Array (Array, listArray, assocs, Ix)
import System.Environment (getArgs)
import Control.Monad.State (execStateT)
import Data.Int (Int32)
import Data.List (dropWhileEnd, intercalate, transpose)
import Data.List.Split (splitOn)

import Parsing
import Interpreting

toInt32List :: [String] -> [Int32]
toInt32List = map read

numberRegisters :: Integral a => a
numberRegisters = 16

initialRegisters :: [String] -> [Int32]
initialRegisters []       = replicate numberRegisters 0
initialRegisters (rstr:_) = rs ++ replicate (numberRegisters - length rs) 0
  where
    rs = toInt32List (splitOn "," rstr)

initialState :: [Int32] -> [Instruction] -> IState
initialState rs is =
  IState { program = listArray (0, fromIntegral (length is - 1)) is
         , registers    = listArray (0, numberRegisters - 1) rs
         , pointer      = 0
         , cycleCount   = 0 }

showArrayTable :: forall i e . (Show e, Show i, Ix i) => Array i e -> String
showArrayTable xs = intercalate "\n" (map concat (transpose chunks))
  where
    chunks = map getChunk (assocs xs) ++ [["+", "|", "-", "|", "+"]]
    getChunk :: (i, e) -> [String]
    getChunk (x, y) =
      [ bar, x' ++ replicate (n - lx) ' '
      , bar, y' ++ replicate (n - ly) ' '
      , bar                               ]
      where
        x'  = "| " ++ show x ++ " "
        y'  = "| " ++ show y ++ " "
        lx  = length x'
        ly  = length y'
        n   = max lx ly
        bar = '+' : replicate (n - 1) '-'


main :: IO ()
main = do
  (a:as) <- getArgs
  c <- dropWhileEnd isSpace <$> readFile a

  is <- case runP instructions c of
    Right is -> pure is
    Left x -> ioError $ userError ("parse error at: \"" ++ x ++ "\"")

  st <- case execStateT interpret (initialState (initialRegisters as) is) of
    Right st -> pure st
    Left x -> ioError $ userError ("interpretation error at: \"" ++ x ++ "\"")

  putStrLn $ "cycles: " ++ show (cycleCount st)
  putStrLn "registers:"
  putStrLn $ showArrayTable (registers st)