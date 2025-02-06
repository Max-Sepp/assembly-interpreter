module Main where

import Data.Char (isSpace)
import Data.Bits ((.|.), (.&.), shiftL, shiftR, )
import Data.Array (Array, bounds, (!), indices, (//), listArray, assocs)
import System.Environment (getArgs)
import Control.Applicative (liftA2, Alternative (empty, (<|>)), asum, some, many)
import Control.Monad.State (StateT, modify, gets, when, unless, execStateT)
import Data.Functor (($>))
import Data.Int (Int32)
import Data.List (dropWhileEnd)

type Register = Int32

data Instruction
  = Add Register Register Register
  | Sub Register Register Register
  | Mul Register Register Register
  | Shl Register Register
  | Shr Register Register
  | Bor Register Register Register
  | Band Register Register Register
  | Li Register Int32
  | Jz Register Int32
  | Jp Register Int32
  deriving Show

-- Parsing

newtype P a = P (forall r. String -> (a -> String -> r)
                                  -> (a -> r)
                                  -> (String -> r) 
                                  -> r 
                                  -> r)

runP :: P a -> String -> Either String a
runP (P p) str = p str
  (\x _ -> Right x)
  Right
  (Left . takeWhile (/='\n'))
  (Left "unknown error")

satisfy :: (Char -> Bool) -> P Char
satisfy f = P $ \inp cok _ _ eerr ->
  case inp of
    c:cs | f c -> cok c cs
    _          -> eerr

atomic :: P a -> P a
atomic (P x) = P $ \inp cok eok _ eerr ->
  x inp cok eok (const eerr) eerr

instance Functor P where
  fmap :: (a -> b) -> P a -> P b
  fmap f (P p) = P $ \inp cok eok cerr eerr ->
    p inp (cok . f) (eok . f) cerr eerr

instance Applicative P where
  pure :: a -> P a
  pure x = P $ \_ _ eok _ _ -> eok x

  liftA2 :: (a -> b -> c) -> P a -> P b -> P c
  liftA2 f (P px) (P py) = P $ \inp cok eok cerr eerr ->
    let cok' x inp' = py inp' (cok . f x) (\y -> cok (f x y) inp') cerr (cerr inp')
        eok' x      = py inp (cok . f x) (eok . f x) cerr eerr
    in px inp cok' eok' cerr eerr

instance Alternative P where
  empty :: P a
  empty = P $ \_ _ _ _ eerr -> eerr

  (<|>) :: P a -> P a -> P a
  P px <|> P py = P $ \inp cok eok cerr eerr ->
    px inp cok eok cerr (py inp cok eok cerr eerr) 

eof :: P ()
eof = P $ \inp _ eok _ eerr ->
  if null inp
    then eok ()
    else eerr

(<:>) :: P a -> P [a] -> P [a]
px <:> py = (:) <$> px <*> py

(<|+>) :: P [a] -> P a -> P [a]
px <|+> py = ((++) <$> px <*> ((: []) <$> py)) <|> px

char :: Char -> P Char
char c = satisfy (== c)

space :: P ()
space = some (satisfy isSpace) $> ()

string :: String -> P String
string = traverse char

oneOf :: [Char] -> P Char
oneOf = asum . map char

digit :: P Char
digit = oneOf ['0'..'9']

number :: P Int32
number = read <$> (some digit <|> char '-' <:> some digit)

instruction :: P Instruction
instruction = asum $ map atomic
  [ a3 "add" Add, a3 "sub" Sub, a3 "mul" Mul, a3 "bor" Bor, a3 "band" Band
  , a2 "shl" Shl, a2 "shr" Shr, a2 "li"   Li, a2 "jz"   Jz, a2 "jp"     Jp ]
  where a3 i c = string i *> space *>
                  (c <$> number <* space <*> number <* space <*> number)
        a2 i c = string i *> space *>
                  (c <$> number <* space <*> number)

instructions :: P [Instruction]
instructions =
  (many (atomic (instruction <* space)) <|+> instruction) <* eof

-- Interpreter

data IState = IState
  { program :: Array Int32 Instruction
  , registers :: Array Int32 Int32
  , pointer :: Int32
  , cycleCount :: Int32 }
type Interpreter a = StateT IState Maybe a

increment :: Interpreter ()
increment = modify (\s -> s { pointer = pointer s + 1 })

jump :: Int32 -> Interpreter ()
jump p = do
  modify (\s -> s { pointer = p })

nextInstruction :: Interpreter Instruction
nextInstruction = do
  ixs <- gets (indices . program)
  p <- gets pointer
  is <- gets program
  if p `elem` ixs
    then pure (is ! p)
    else fail "invalid pointer position"

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
    else fail "invalid register"

writeRegister :: Int32 -> Int32 -> Interpreter ()
writeRegister ri v = do
  rs <- gets registers
  if ri `elem` indices rs
    then modify (\s -> s { registers = rs // [(ri, v)] })
    else fail "invalid register"

countCycles :: Int32 -> Interpreter ()
countCycles n = modify (\s -> s { cycleCount = cycleCount s + n })


runInstruction :: Instruction -> Interpreter ()
runInstruction (Add d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 + x1)
  countCycles 1
runInstruction (Sub d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 - x1)
  countCycles 1
runInstruction (Mul d a b) = do
  x0 <- readRegister a
  x1 <- readRegister b
  writeRegister d (x0 * x1)
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

initialState :: [Instruction] -> IState
initialState is =
  IState { program = listArray (0, fromIntegral (length is - 1)) is
         , registers    = listArray (0, 15) (replicate 16 0)
         , pointer      = 0
         , cycleCount   = 0 }


main :: IO ()
main = do
  (a:_) <- getArgs
  c <- dropWhileEnd isSpace <$> readFile a

  is <- case runP instructions c of
    Right is -> return is 
    Left x -> ioError $ userError ("parse error at: \"" ++ x ++ "\"")
  
  st <- case execStateT interpret (initialState is) of
    Just st -> return st
    Nothing -> ioError $ userError "interpretation error"

  putStrLn $ "cycles: " ++ show (cycleCount st)
  putStrLn "registers:"
  print (assocs (registers st))