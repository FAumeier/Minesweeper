module Lib
    ( someFunc
    , Field(..)
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Data Types

data Field = Field 
                    { x :: Int
                    , y :: Int
                    , hasMine :: Bool
                    } deriving (Show)

data Board = Board [Field] [Field] deriving (Show) 

-- Methods

createFields :: Int -> [Field]
createFields m = [ Field x y False | x <- [1..m], y <- [1..m]]

printRow :: Board -> Int -> [Field]
printRow board i = putStrLn row where row =  