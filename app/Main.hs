{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.State (get, put, evalStateT, gets, modify, when, unless, execStateT, StateT)
import Data.Char (isDigit)
import Data.Bits ((.|.), (.&.))
import Data.Array (Array, bounds, (!), indices, (//), listArray, assocs)
import System.Environment (getArgs)

type Register = Int

data Instruction
  = Add Register Register Register
  | Sub Register Register Register
  | Mul Register Register Register
  | Shl Register Register
  | Shr Register Register
  | Bor Register Register Register
  | Band Register Register Register
  | Li Register Int
  | Jz Register Int
  | Jp Register Int
  deriving Show

-- Parsing

type Parser a = StateT [String] Maybe a

consume :: Parser (Maybe String)
consume = do
  xs <- get
  case xs of
    []    -> pure Nothing
    (x:xs') -> put xs' >> pure (Just x)

expectNumber :: Parser Int
expectNumber = do
  Just cs@(c:cs') <- consume
  if all isDigit cs || c == '-' && all isDigit cs'
    then pure (read cs)
    else fail "expected digit"

expect2Numbers :: Parser (Int, Int)
expect2Numbers = (,) <$> expectNumber <*> expectNumber

expect3Numbers :: Parser (Int, Int, Int)
expect3Numbers = (,,) <$> expectNumber <*> expectNumber <*> expectNumber

parse :: Parser [Instruction]
parse = do
  consume >>= \case
    Just "add" -> do
      (d, a, b) <- expect3Numbers
      (Add d a b :) <$> parse
    Just "sub" -> do
      (d, a, b) <- expect3Numbers
      (Sub d a b :) <$> parse
    Just "mul" -> do
      (d, a, b) <- expect3Numbers
      (Mul d a b :) <$> parse
    Just "shl" -> do
      (d, a) <- expect2Numbers
      (Shl d a :) <$> parse
    Just "shr" -> do
      (d, a) <- expect2Numbers
      (Shr d a :) <$> parse
    Just "bor" -> do
      (d, a, b) <- expect3Numbers
      (Bor d a b :) <$> parse
    Just "band" -> do
      (d, a, b) <- expect3Numbers
      (Band d a b :) <$> parse
    Just "li" -> do
      (d, i) <- expect2Numbers
      (Li d i :) <$> parse
    Just "jz" -> do
      (d, i) <- expect2Numbers
      (Jz d i :) <$> parse
    Just "jp" -> do
      (d, i) <- expect2Numbers
      (Jp d i :) <$> parse
    Nothing -> pure []
    _ -> fail "invalid command"

getInstructions :: String -> Maybe [Instruction]
getInstructions code = evalStateT parse (words code)

-- Interpreter

data IState = IState
  { instructions :: Array Int Instruction
  , registers :: Array Int Int
  , pointer :: Int }
type Interpreter a = StateT IState Maybe a

increment :: Interpreter ()
increment = modify (\s -> s { pointer = pointer s + 1 })

jump :: Int -> Interpreter ()
jump p = do
  modify (\s -> s { pointer = p })

nextInstruction :: Interpreter Instruction
nextInstruction = do
  ixs <- gets (indices . instructions)
  p <- gets pointer
  is <- gets instructions
  if p `elem` ixs
    then pure (is ! p)
    else fail "invalid pointer position"

isFinished :: Interpreter Bool
isFinished = do
  (_, u) <- gets (bounds . instructions)
  p <- gets pointer
  pure (u == p - 1)

readRegister :: Int -> Interpreter Int
readRegister ri = do
  rs <- gets registers
  if ri `elem` indices rs
    then pure (rs ! ri)
    else fail "invalid register"

writeRegister :: Int -> Int -> Interpreter ()
writeRegister ri v = do
  rs <- gets registers
  if ri `elem` indices rs
    then modify (\s -> s { registers = rs // [(ri, v)] })
    else fail "invalid register"

runInstruction :: Instruction -> Interpreter ()
runInstruction (Add d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 + x1)
runInstruction (Sub d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 - x1)
runInstruction (Mul d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 * x1)
runInstruction (Shl d a) = do
  x <- readRegister a
  writeRegister d (2 * x)
runInstruction (Shr d a) = do
  x <- readRegister a
  writeRegister d (div x 2)
runInstruction (Bor d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 .|. x1)
runInstruction (Band d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 .&. x1)
runInstruction (Li d i) = do
  writeRegister d i
runInstruction (Jz a i) = do
  p <- gets pointer
  x <- readRegister a
  when (x == 0) (jump (p + i - 1))
runInstruction (Jp a i) = do
  p <- gets pointer
  x <- readRegister a
  when (x > 0) (jump (p + i - 1))

interpret :: Interpreter ()
interpret = do
  i <- nextInstruction
  runInstruction i
  increment
  isFinished >>= flip unless interpret

-- main

run :: String -> Maybe [(Int, Int)]
run s = do
  is <- getInstructions s
  let state = IState { instructions = listArray (0, length is - 1) is
                     , registers    = listArray (0, 15) (replicate 16 0)
                     , pointer      = 0
                     }
  state' <- execStateT interpret state
  pure (assocs (registers state'))


main :: IO ()
main = do
  (a:_) <- getArgs
  readFile a >>= print . run
