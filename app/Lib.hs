{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}

module Lib
  ( Direction (Left, Right),
  )
where

import GHC.Generics
import Optics.Core
import qualified Data.HashMap.Strict as Map
import qualified GHC.Records

data Alphabet a = Alphabet a | R

data Tape

-- https://discourse.haskell.org/t/how-do-you-avoid-the-wunused-top-binds-with-makelenses/8615/4

data Point = Point
  { x :: Int,
    y :: Int
  }
  deriving (Generic)

fieldAccess :: GHC.Records.HasField "x" r a => r -> a
fieldAccess p = p.x

fieldUpdate p = p & #x .~ 3

data State

data Direction = Left | Right
  deriving (Show)

-- data Symbol
type Symbol = Char
