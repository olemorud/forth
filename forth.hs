{-# LANGUAGE OverloadedStrings #-}

-- This is a WIP solution to
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
import Data.Either
import Data.Maybe

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

-- Your evaluator has to support the following words:
--   +, -, *, / (integer arithmetic)
--   DUP, DROP, SWAP, OVER (stack manipulation)
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text =
    case text of
        "/"       -> forthOp div
        "+"       -> forthOp (+)
        "-"       -> forthOp (-)
        "*"       -> forthOp (*)
        "DUP"     -> forthDup
        "DROP"    -> forthDrop
        "SWAP"    -> forthSwap
        "OVER"    -> forthOver
        otherwise -> appendAsNum
    where
    appendAsNum =
        case decimal text of
            Right v -> forthAppend $ fst v
            Left _ -> \_ -> Left $ UnknownWord text

-- Pop a, b from stack, apply `op` to them and push the result on top.
forthOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
forthOp op (ForthState (x1:x2:xs)) = Right $ ForthState (op x1 x2:xs)
forthOp op _ = Left StackUnderflow

-- Duplicate the stack head
forthDup :: ForthState -> Either ForthError ForthState
forthDup (ForthState (x:xs)) = Right $ ForthState (x:x:xs)
forthDup _ = Left StackUnderflow

-- Discard the stack head
forthDrop :: ForthState -> Either ForthError ForthState
forthDrop (ForthState (_:xs)) = Right $ ForthState xs
forthDrop _ = Left StackUnderflow

-- Swap the top two elements of the stack.
forthSwap :: ForthState -> Either ForthError ForthState
forthSwap (ForthState (xa:xb:xs)) = Right $ ForthState (xb:xa:xs)
forthSwap _ = Left StackUnderflow

-- Copy second item to top
forthOver :: ForthState -> Either ForthError ForthState
forthOver (ForthState (x1:x2:xs)) = Right $ ForthState (x2:x1:x2:xs)
forthOver _ = Left StackUnderflow

-- Add v to the top of the stack
forthAppend :: Int -> ForthState -> Either ForthError ForthState
forthAppend v (ForthState (x:xs)) = Right $ ForthState (v:x:xs)

toList :: ForthState -> [Int]
toList (ForthState stack) = stack

