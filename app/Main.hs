module Main where

import qualified Turing

main :: IO ()
main = do
  putStr $ Turing.log '0' Turing.exampleState1 Turing.emptyTape
  putStr $ Turing.logn 5 '0' Turing.exampleLoop Turing.emptyTape
  putStr $ Turing.logn 10 '0' Turing.exampleLoop Turing.emptyTape
  putStr $ drop 5 $ Turing.logn 8 '0' Turing.exampleToggle1 Turing.emptyTape
