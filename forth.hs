{-# LANGUAGE OverloadedStrings #-}

-- Inspired by
-- https://exercism.org/tracks/haskell/exercises/forth

module Forth (
    ForthError(..),
    ForthState,
    evalText,
    toList,
    emptyState
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import Text.Read
import Data.Either

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState [Int]

instance Show ForthState where
  show (ForthState stack) = "ForthState " ++ show stack

emptyState :: ForthState
emptyState = ForthState []

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText t state = runParsed state $ parseWords t

run :: String -> Either ForthError ForthState
run str = evalText (T.pack str) emptyState

-- like foldl but applies a lists of functions that return Eithers to acc,
-- probably a better way to do this
runParsed :: a -> [a -> Either b a] -> Either b a
runParsed acc [] = Right $ acc
runParsed acc (fn:fns) =
    case (fn acc) of
        Right v  -> runParsed v fns
        Left err -> Left err

-- Interpret Forth code text to list of functions
parseWords :: Text -> [ForthState -> Either ForthError ForthState]
parseWords = map (parseWord) . T.words

-- Translate single word to a Forth instruction
parseWord :: Text -> ForthState -> Either ForthError ForthState
parseWord str =
    case str of
        "/"    -> forthSafeDiv
        "+"    -> forthOp (+)
        "-"    -> forthOp (-)
        "*"    -> forthOp (*)
        "DUP"  -> forthDup
        "DROP" -> forthDrop
        "SWAP" -> forthSwap
        "OVER" -> forthOver
        _      -> case decimal str of -- if not function: try to parse as int
                    Right v  -> forthAppend $ fst v
                    Left err ->  \_ -> Left $ UnknownWord $ str

-- Pop a, b from stack, push a/b to top. If b is zero return DivisionByZero
forthSafeDiv :: ForthState -> Either ForthError ForthState
forthSafeDiv (ForthState (x1:x2:xs)) =
    case x2 of
        0 -> Left $ DivisionByZero
        _ -> Right $ ForthState (div x1 x2:xs)

-- Pop a, b from stack, apply `op` to them and push the result on top.
forthOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
forthOp op (ForthState (x1:x2:xs)) = Right $ ForthState (op x1 x2:xs)
forthOp op _ = Left InvalidWord

-- Duplicate the stack head
forthDup :: ForthState -> Either ForthError ForthState
forthDup (ForthState (x:xs)) = Right $ ForthState (x:x:xs)
forthDup _ = Left InvalidWord

-- Discard the stack head
forthDrop :: ForthState -> Either ForthError ForthState
forthDrop (ForthState (_:xs)) = Right $ ForthState xs
forthDrop _ = Left StackUnderflow

-- Swap the top two elements of the stack.
forthSwap :: ForthState -> Either ForthError ForthState
forthSwap (ForthState (xa:xb:xs)) = Right $ ForthState (xb:xa:xs)
forthSwap _ = Left InvalidWord

-- Copy second item to top
forthOver :: ForthState -> Either ForthError ForthState
forthOver (ForthState (x1:x2:xs)) = Right $ ForthState (x2:x1:x2:xs)
forthOver _ = Left InvalidWord

-- Add v to the top of the stack
forthAppend :: Int -> ForthState -> Either ForthError ForthState
forthAppend v (ForthState (x:xs)) = Right $ ForthState (v:x:xs)
forthAppend v _ = Right $ ForthState [v]

toList :: ForthState -> [Int]
toList (ForthState stack) = stack

