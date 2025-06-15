module State where

import Data.Hashable as Hashable
import Data.HashMap.Strict as HashMap

data State symbol = State
  { write :: symbol,
    next :: symbol -> Maybe (State symbol),
    direction :: Direction
  }

toFunction :: (Hashable symbol) => [(symbol, State symbol)] -> symbol -> Maybe (State symbol)
toFunction m s = HashMap.lookup s $ HashMap.fromList m
