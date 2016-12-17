module Lib
    ( Field(..)
    , Board(..)
    , GameState(..)
    , State(..)
    , updateCells
    , updateSingleCell
    ) where

import Data.List

data Field = Marked | Unmarked | Open Int | Mine
  deriving (Show, Eq)

type Board = [[Field]]

data GameState = GameState { board :: Board
                           , mines :: [Coordinates]
                           , state :: State
                           } deriving (Show)
type Coordinates = (Int, Int)
data State = Won | Lost | Undecided deriving (Show)

-- data Action = Toggle Coordinates | openField Coordinates

(!!!) :: Board -> Coordinates -> Field
(!!!) [fields] (x, y) = [fields] !! x !! y

updateBoard :: Board -> Coordinates -> Field -> Board
updateBoard board (x, y) field = take x board ++ [updatedRow] ++ drop (x+1) board
            where row = board !! x
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

updateSingleCell :: GameState -> Coordinates -> GameState -- Works just fine
updateSingleCell state coordinates = newState
              where oldBoard = board state
                    around = minesAround coordinates state
                    field = if isFieldOnMine coordinates state == True then Mine else Open around
                    newBoard = updateBoard oldBoard coordinates field
                    minesOnBoard = mines state
                    newState = GameState newBoard minesOnBoard Undecided

isFieldOnMine :: Coordinates-> GameState -> Bool
isFieldOnMine field state = field `elem` listOfMines
            where listOfMines = mines state

updateCells :: GameState -> Coordinates -> GameState -- seems to make problems...
updateCells state coordinate = newState
            where listOfNeighbours = neighbours state coordinate
                  newState = foldl updateSingleCell state (listOfNeighbours ++ [coordinate])
