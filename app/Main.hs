module Main where

import Data.Char (isSpace)
import Data.Array (Array, bounds, listArray, elems)
import System.Environment (getArgs)
import Control.Monad.State (execStateT)
import Data.Int (Int32)
import Data.List (dropWhileEnd, intercalate)
import Data.List.Split (splitOn)

import Parsing
import Interpreting

toInt32List :: [String] -> [Int32]
toInt32List = map read

numberRegisters :: Int
numberRegisters = 16

getInitialRegisters :: [String] -> [Int32]
getInitialRegisters []       = replicate numberRegisters 0
getInitialRegisters (rstr:_) = rs ++ replicate (numberRegisters - length rs) 0
  where
    rs = toInt32List (splitOn "," rstr)

initialState :: [Int32] -> [Instruction] -> IState
initialState rs is =
  IState { program = listArray (0, fromIntegral (length is - 1)) is
         , registers    = listArray (0, 15) rs
         , pointer      = 0
         , cycleCount   = 0 }

showArrayTable :: (Num a, Enum a, Show a, Show b, Integral a) => 
                  Array a b -> String
showArrayTable rs =
  row ++ "\n"
  ++ indexRow ++ "\n"
  ++ row ++ "\n"
  ++ registerRow ++ "\n"
  ++ row ++ "\n"
  where
    (minIndex, maxIndex) = bounds rs

    elementWidth =
        maximum (
            map (length . show) [minIndex..maxIndex]
            ++ map (length . show) (elems rs)
        )

    numElements = fromIntegral (maxIndex - minIndex + 1)

    row = 
      "+-" ++
      intercalate "-+-" (replicate numElements (replicate elementWidth '-'))
      ++ "-+"

    pad :: String -> String
    pad cs = replicate (elementWidth - length cs) ' ' ++ cs

    indexStringList = map (pad . show) [minIndex..maxIndex]

    indexRow = "| " ++ intercalate " | " indexStringList ++ " |"

    registerStringList = map (pad . show) (elems rs)

    registerRow = "| " ++ intercalate " | " registerStringList ++ " |"

main :: IO ()
main = do
  (a:rest) <- getArgs
  c <- dropWhileEnd isSpace <$> readFile a

  is <- case runP instructions c of
    Right is -> pure is
    Left x -> ioError $ userError ("parse error at: \"" ++ x ++ "\"")

  let initialRegisters = getInitialRegisters rest

  st <- case execStateT interpret (initialState initialRegisters is) of
    Right st -> pure st
    Left x -> ioError $ userError ("interpretation error at: \"" ++ x ++ "\"")

  putStrLn $ "cycles: " ++ show (cycleCount st)
  putStrLn "registers:"
  putStr $ showArrayTable (registers st)