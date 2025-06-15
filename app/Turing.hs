{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Turing (log, logn, exampleLoop, emptyTape, exampleToggle1, exampleState1) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right, head, log, read)
import Data.Aeson
import GHC.Generics

-- https://en.wikipedia.org/wiki/Turing_machine#Formal_definition
-- https://cs.stackexchange.com/questions/45589/why-is-the-tape-not-part-of-the-definition-of-a-turing-machine
data MachineModel symbol = Machine
  { tape :: TapeModel symbol,
    input :: HashSet symbol,
    initialState :: State symbol,
    finalStates :: HashSet (State symbol)
    -- transition :: symbol -> State symbol -> Maybe (State symbol)
  }

data TapeModel symbol = TapeModel
  { alphabet :: HashSet symbol,
    blank :: symbol
  }

data State symbol = State
  { write :: symbol,
    next :: symbol -> Maybe (State symbol),
    direction :: Direction
  }

-- TODO

data StateIndirect symbol = StateIndirect
  { write :: symbol,
    next :: [(symbol, Int)],
    direction :: Direction
  }

exampleIndirect = HashMap.fromList [
    (0 :: Int, 0 :: Int)
  ]

exampleState1 :: State Char
exampleState1 =
  State
    { write = '1',
      next = stateFromList [('0', exampleState2)],
      direction = Right
    }

exampleState2 :: State Char
exampleState2 =
  State
    { write = '0',
      next = stateFromList [('0', exampleState1)],
      direction = Left
    }

exampleLoop :: State Char
exampleLoop =
  State
    { write = '1',
      next = stateFromList [('0', exampleLoop)],
      direction = Right
    }

exampleToggle1 :: State Char
exampleToggle1 =
  State
    { write = '1',
      next = stateFromList [('0', exampleToggle10)],
      direction = Right
    }

exampleToggle10 :: State Char
exampleToggle10 =
  State
    { write = '0',
      next = stateFromList [('0', exampleToggle0)],
      direction = Left
    }

exampleToggle0 :: State Char
exampleToggle0 =
  State
    { write = '0',
      next = stateFromList [('1', exampleToggle00)],
      direction = Right
    }

exampleToggle00 :: State Char
exampleToggle00 =
  State
    { write = '0',
      next = stateFromList [('0', exampleToggle1)],
      direction = Left
    }

interpret1 :: (Hashable symbol) => symbol -> State symbol -> Tape symbol -> Maybe (State symbol, Tape symbol)
interpret1 blank state tape = case maybeNext of
  Nothing -> Nothing
  Just next ->
    Just
      ( next,
        writeMove $
          WriteMove
            { blank,
              tape,
              write = state.write,
              direction = state.direction
            }
      )
  where
    current = read blank tape
    maybeNext = state.next current

interprets :: (Hashable symbol) => symbol -> State symbol -> Tape symbol -> [Tape symbol]
interprets blank state tape = case next of
  Nothing -> [tape]
  Just (state2, tape2) -> tape : interprets blank state2 tape2
  where
    next = interpret1 blank state tape

interpretn :: (Hashable symbol) => Int -> symbol -> State symbol -> Tape symbol -> [Tape symbol]
interpretn n b s t = take n $ interprets b s t

log :: Char -> State Char -> Tape Char -> String
log c s t = concatMap ((++ "\n") . show) all'
  where
    all' = interprets c s t

logn :: Int -> Char -> State Char -> Tape Char -> String
logn n c s t = concatMap ((++ "\n") . show) $ take n $ interprets c s t

interpret :: (Hashable symbol) => symbol -> State symbol -> Tape symbol -> Tape symbol
interpret blank state tape = case maybeNext of
  Nothing -> tape
  Just next ->
    interpret blank next $
      writeMove $
        WriteMove
          { blank,
            tape,
            write = state.write,
            direction = state.direction
          }
  where
    current = read blank tape
    maybeNext = state.next current

--
-- Direction
--

data Direction = Left | Right

--
-- Tape
--

-- | A tape, infinite in both directions. A write is done to the right.
data Tape symbol = Tape
  { left :: [symbol],
    right :: [symbol]
  }

write :: symbol -> Tape symbol -> Tape symbol
write s t@Tape {right = []} = t {right = [s]}
write s t@Tape {right = _ : rest} = t {right = s : rest}

read :: symbol -> Tape symbol -> symbol
read blank Tape {right = []} = blank
read _ Tape {right = x : _} = x

-- | Move machine's head in a 'Direction' with a blank default symbol in case there's nothing there.
move :: symbol -> Direction -> Tape symbol -> Tape symbol
move _ Left tape@Tape {left = x : xs} = Tape {left = xs, right = x : tape.right}
move blank Left head = Tape {left = [], right = blank : head.right}
move _ Right tape@Tape {right = x : xs} = Tape {right = xs, left = x : tape.left}
move blank Right head = Tape {right = [], left = blank : head.left}

data WriteMove symbol = WriteMove
  { blank :: symbol,
    write :: symbol,
    direction :: Direction,
    tape :: Tape symbol
  }

writeMove :: WriteMove symbol -> Tape symbol
writeMove WriteMove {blank, write = symbol, direction, tape} = move blank direction $ write symbol tape

--
-- Convenience
--

emptyTape :: Tape a
emptyTape = Tape {left = [], right = []}

mov :: Direction -> Tape Char -> Tape Char
mov = move '0'

--
-- Show tape
--

instance Show (Tape Char) where show tape = map (fromMaybe '|') $ toRight tape

toRight :: Tape symbol -> [Maybe symbol]
toRight tape = toRight' tape.left $ Nothing : map Just tape.right
  where
    toRight' [] r = r
    toRight' (x : xs) r = toRight' xs $ Just x : r

--
-- Functionalization
--

stateFromList :: (Hashable symbol) => [(symbol, State symbol)] -> symbol -> Maybe (State symbol)
stateFromList m s = HashMap.lookup s $ HashMap.fromList m
