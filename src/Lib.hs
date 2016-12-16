module Lib
    ( Field(..)
    , Board(..)
    , GameState(..)
    , State(..)
    , minesAround
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

neighbours :: GameState -> Coordinates -> [Coordinates]
neighbours state (xCor,yCor)
                         | xCor < m && yCor < m && xCor >= 0 && yCor >= 0 = removeItem (xCor, yCor) [ (x,y) | x <- [xCor-1..xCor+1], y <- [yCor-1..yCor+1], x >= 0, x < m, y >= 0, y < m]
                         | otherwise = [(x,y) | x <- [1..m], y <- [1..m]]
           where matrix = board state
                 m = length matrix

removeItem :: Coordinates -> [Coordinates] -> [Coordinates]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

minesAround :: Coordinates -> GameState -> Int
minesAround (x,y) state = length $ intersect nbs mineField
            where nbs = neighbours state (x,y)
                  mineField = mines state
