module Lib
    ( Field(..)
    , Board(..)
    , (!!!)
    ) where

import Data.List

data Field = Marked | Unmarked | Open Int | Mine
  deriving (Show)

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
