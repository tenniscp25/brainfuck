{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types where

import Control.Lens (makeLenses)
import Prelude hiding (Op)

data Op
  = OpNoop
  | OpDec
  | OpInc
  | OpLeft
  | OpRight
  | OpPrint
  | OpRead
  | OpLoop [Op]
  deriving stock (Show)

data EvalState = EvalState
  { _stInput :: [Int],
    _stOutput :: Seq Int,
    _stMem :: [Int],
    _stPtr :: Int
  }
  deriving stock (Show)

makeLenses ''EvalState
