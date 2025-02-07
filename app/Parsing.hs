module Parsing
  ( Register, Instruction(..)
  , instructions, runP
  ) where

import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Int (Int32)
import Control.Applicative (liftA2, Alternative, (<|>), empty, many, some, asum)

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
  if all isSpace inp
    then eok ()
    else eerr

whitespace :: P ()
whitespace = many (satisfy isSpace) $> ()

fully :: P a -> P a
fully p = whitespace *> p <* eof

lexeme :: P a -> P a
lexeme p = p <* whitespace

(<:>) :: P a -> P [a] -> P [a]
px <:> py = (:) <$> px <*> py

char :: Char -> P Char
char c = satisfy (== c)

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
  where a2 i c = lexeme (string i) *> 
                   (c <$> lexeme number <*> lexeme number)
        a3 i c = lexeme (string i) *>
                   (c <$> lexeme number <*> lexeme number <*> lexeme number)

instructions :: P [Instruction]
instructions = fully (many (atomic (lexeme instruction)))