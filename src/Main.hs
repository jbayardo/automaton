module Main where

import DFA
import NDFA
import NDFAL
import Syntax
import RegEx

-- Accepts strings that end with 'ing'
ing = DFA.fromTable t "Start" "Failure" ["Success"]
  where
    t = automaton [
      "Start" `with` [
        'i' `into` "Saw 'i'",
        'n' `into` "Start",
        'g' `into` "Start"
      ],
      "Saw 'i'" `with` [
        'i' `into` "Saw 'i'",
        'n' `into` "Saw 'in'",
        'g' `into` "Start"
      ],
      "Saw 'in'" `with` [
        'i' `into` "Start",
        'n' `into` "Start",
        'g' `into` "Success"
      ]]

-- Accepts strings that have at least one occurence of 1 within the last 3
-- characters
aaa :: NDFA String Char
aaa = NDFA.fromTable t "Start" ["C3"]
  where
    t = automaton [
      "Start" `with` [
        '1' `into'` ["Start", "C1", "C2", "C3"],
        '0' `into'` ["Start"]
      ],
      "C1" `with` [
        '1' `into'` ["C2"],
        '0' `into'` ["C2"]
      ],
      "C2" `with` [
        '1' `into'` ["C3"],
        '0' `into'` ["C3"]
      ]]

bbb :: NDFAL String Char
bbb = NDFAL.fromTable t "Start" ["Success"]
  where
    t = automaton [
      "Start" `with` [
        (lambda :: Lam Char) `into''` ["StartA", "StartB"]
      ],
      "StartA" `with` [
        'H' `into''` ["StartA-1"]
      ],
      "StartA-1" `with` [
        'e' `into''` ["StartA-2"]
      ],
      "StartA-2" `with` [
        'l' `into''` ["StartA-3"]
      ],
      "StartA-3" `with` [
        'l' `into''` ["StartA-4"]
      ],
      "StartA-4" `with` [
        'o' `into''` ["Success"]
      ],
      "StartB" `with` [
        'W' `into''` ["StartB", "StartB-2"]
      ],
      "StartB-2" `with` [
        (lambda :: Lam Char) `into''` ["Success"]
      ]]

main :: IO ()
main = do
  putStrLn "hello world"
