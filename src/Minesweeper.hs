module Minesweeper where

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

data Action = Toggle Coordinates | OpenField Coordinates

(!!!) :: Board -> Coordinates -> Field
board !!! (x, y) = board !! x !! y

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

updateSingleCell :: GameState -> Bool -> Coordinates -> GameState
updateSingleCell state showMine (x,y) = newState
              where oldBoard = board state
                    around = minesAround (x,y) state
                    oldField = oldBoard !!! (x,y)
                    field = if isFieldOnMine (x,y) state == True
                              then if showMine == True
                                    then Mine
                                    else Unmarked
                              else Open around
                    newBoard = updateBoard oldBoard (x,y) field
                    minesOnBoard = mines state
                    newState = GameState newBoard minesOnBoard Undecided

isFieldOnMine :: Coordinates-> GameState -> Bool
isFieldOnMine field state = field `elem` listOfMines
            where listOfMines = mines state

updateCells :: GameState -> Coordinates -> GameState
updateCells gs (x,y) =
  let coords = (x,y):(neighbours gs (x,y))
  in foldl (\st a -> updateSingleCell st False a) gs coords

showMines :: GameState -> GameState
showMines gs =
  let coords = mines gs
  in foldl (\st a -> updateSingleCell st True a) gs coords

playStep :: Action -> GameState -> GameState
playStep a gamestate@GameState{state = Lost} = gamestate
playStep a gamestate@GameState{state = Won} = gamestate
playStep (Toggle (x,y)) gamestate = newState
        where oldField = (board gamestate) !!! (x,y)
              newField = case oldField of
                          Marked -> Unmarked
                          Unmarked -> Marked
                          other -> other
              minesOnBoard = mines gamestate
              oldState = state gamestate
              oldBoard = board gamestate
              newBoard = updateBoard oldBoard (x,y) newField
              newState = GameState newBoard minesOnBoard oldState
playStep (OpenField (x,y)) gamestate = if (x,y) `elem` (mines gamestate)
                                       then (showMines gamestate){state=Lost}
                                       else setState $ updateCells gamestate (x,y)

setState gamestate = if all open [ (x,y,col) | (row, y) <- zip (board gamestate) [0..]
                                             , (col, x) <- zip row [0..]]
                     then gamestate {state = Won}
                     else gamestate
                     where open (r,c,Open i) = True
                           open (r,c,Mine) = True -- Mine
                           open (r,c,_) = (r,c) `elem` (mines gamestate)
