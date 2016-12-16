module Lib
    ( Field(..)
    , Board(..)
    ) where

import Data.List

data Field = Marked | Unmarked | Open Int | Mine
  deriving (Show, Eq)

type Board = [[Field]]

data GameState = GameState { board :: Board
                           , mines :: [Coordinates]
                           , state :: State
                           }
type Coordinates = (Int, Int)
data State = Won | Lost | Undecided

-- data Action = Toggle Coordinates | openField Coordinates

(!!!) :: Board -> Coordinates -> Field
(!!!) [fields] (x, y) = [fields] !! x !! y

updateBoard :: Board -> Coordinates -> Field -> Board
updateBoard [fields] (x, y) field = take x [fields] ++ [updatedRow] ++ drop (x+1) [fields]
            where row = [fields] !! x
                  updatedRow = replaceField row y field

replaceField :: [Field] -> Int -> Field -> [Field]
replaceField fields i element = take i fields ++ [element] ++ drop (i+1) fields

  --- Kopiere alle Reihen bis Reihe x
  --- Kopiere alle Elemente bis Element y
  --- Füge das neue Feld an Stelle von Element y ein
  --- Kopiere den Rest
