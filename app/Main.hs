module Main where

import Data.Char (isSpace)
import Data.Bits ((.|.), (.&.), shiftL, shiftR, )
import Data.Array (Array, bounds, (!), indices, (//), listArray, elems)
import System.Environment (getArgs)
import Control.Monad.State (StateT, modify, gets, when, unless, execStateT, lift)
import Data.Int (Int32)
import Data.List (dropWhileEnd, intercalate)
import Data.List.Split (splitOn)

import Parsing

-- Interpreter

data IState = IState
  { program :: Array Int32 Instruction
  , registers :: Array Int32 Int32
  , pointer :: Int32
  , cycleCount :: Int32 }
type Interpreter a = StateT IState (Either String) a

ierror :: String -> Interpreter a
ierror = lift . Left

increment :: Interpreter ()
increment = modify (\s -> s { pointer = pointer s + 1 })

jump :: Int32 -> Interpreter ()
jump p = modify (\s -> s { pointer = p })

nextInstruction :: Interpreter Instruction
nextInstruction = do
  ixs <- gets (indices . program)
  p <- gets pointer
  is <- gets program
  if p `elem` ixs
    then pure (is ! p)
    else ierror "invalid pointer position"

isFinished :: Interpreter Bool
isFinished = do
  (_, u) <- gets (bounds . program)
  p <- gets pointer
  pure (u == p - 1)

readRegister :: Int32 -> Interpreter Int32
readRegister ri = do
  rs <- gets registers
  if ri `elem` indices rs
    then pure (rs ! ri)
    else ierror "invalid register"

writeRegister :: Int32 -> Int32 -> Interpreter ()
writeRegister ri v = do
  rs <- gets registers
  if ri `elem` indices rs
    then modify (\s -> s { registers = rs // [(ri, v)] })
    else ierror "invalid register"

countCycles :: Int32 -> Interpreter ()
countCycles n = modify (\s -> s { cycleCount = cycleCount s + n })

safeAdd :: Int32 -> Int32 -> Interpreter Int32
safeAdd x y
  | y > 0 && s < x || y < 0 && s > x = ierror "Integer overflow occurred"
  | otherwise = pure s
  where s = x + y

safeMul :: Int32 -> Int32 -> Interpreter Int32
safeMul x y
  | res `div` y /= x = ierror "Integer overflow occurred"
  | otherwise = pure res
  where
    res = x * y

runInstruction :: Instruction -> Interpreter ()
runInstruction (Add d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  s <- safeAdd x0 x1
  writeRegister d s
  countCycles 1
runInstruction (Sub d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  s <- safeAdd x0 (-x1)
  writeRegister d s
  countCycles 1
runInstruction (Mul d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  r <- safeMul x0 x1
  writeRegister d r
  countCycles 3
runInstruction (Shl d a) = do
  x <- readRegister d
  y <- readRegister a
  writeRegister d (shiftL x (fromIntegral y))
  countCycles 1
runInstruction (Shr d a) = do
  x <- readRegister d
  y <- readRegister a
  writeRegister d (shiftR x (fromIntegral y))
  countCycles 1
runInstruction (Bor d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 .|. x1)
  countCycles 1
runInstruction (Band d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 .&. x1)
  countCycles 1
runInstruction (Li d i) = do
  writeRegister d i
  countCycles 1
runInstruction (Jz a i) = do
  p <- gets pointer
  x <- readRegister a
  when (x == 0) (jump (p + i - 1))
  countCycles 3
runInstruction (Jp a i) = do
  p <- gets pointer
  x <- readRegister a
  when (x > 0) (jump (p + i - 1))
  countCycles 3

interpret :: Interpreter ()
interpret = do
  i <- nextInstruction
  runInstruction i
  increment
  isFinished >>= flip unless interpret

-- main

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