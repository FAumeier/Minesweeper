module Main where
-- first party imports
import System.Environment
import Data.List
import System.Random
-- import System.Random
-- third party imports
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Minesweeper
-- end of imports

-- size of window in pixel (wsizex,wsizey)
wsizex :: Int
wsizex = 600
wsizey :: Int
wsizey = 600
-- converting them to float
fwsizex = fromIntegral wsizex
fwsizey = fromIntegral wsizey

start :: Int -> GameState
start size = newState
    where
      state = Undecided
      bd = replicate size $ replicate size Unmarked
      ms = generateCoordinates size
      newState = GameState bd ms state

generateCoordinates :: Int -> [Coordinates]
generateCoordinates size =
        let liste = zip (randomRs (0,size-1)  (mkStdGen 2)) (randomRs (0,size-1)  (mkStdGen 5))
        in randomizeCoordinates 15 liste []

randomizeCoordinates :: Int -> [(Int, Int)] -> [(Int, Int)] -> [Coordinates]
randomizeCoordinates 0 _ newCoors = newCoors
randomizeCoordinates i (coor:coors) newCoors
  | coor `elem` newCoors = randomizeCoordinates i coors newCoors
  | otherwise = randomizeCoordinates (i-1) coors (coor:newCoors)

main =
  do
      let size = 8 -- size of the board
      let gamestate = start size
      play (InWindow "Minesweeper 0.1" (wsizex,wsizey) (10,10)) white 1 gamestate getPicture handleEvent (\float world -> world)

-- handleEvent handles mouse events
handleEvent (EventKey (MouseButton button) Down _ (x,y)) gs =
 let -- converting mouse coordinates (x,y) to board coordinates (xnew,ynew):
     ylen  = fromIntegral $ length $ (board gs)!!0
     xlen  = fromIntegral $ length $ (board gs)
     ynew  = truncate (xlen * ((x+(fwsizex/2)))/fwsizex)
     xnew  = truncate (ylen * (abs ((((y+(fwsizey/2)))/fwsizey) -1)))
 in
   -- which button was pressed
   case button of
       LeftButton ->  playStep (OpenField (xnew,ynew)) gs
       RightButton -> playStep (Toggle (xnew,ynew)) gs
       _           -> gs
handleEvent (EventKey (Char 'q') Down _ _) gs = error "Stop!"
-- handle all over events (they do nothing)
handleEvent event state = state

-- generate picture for the gamestate
getPicture :: GameState -> Picture
getPicture gs = Pictures $ field:message
  where
    -- calculate size of board from gamestate
   xlen  = fromIntegral $ length $ (board gs) !! 0
   ylen  = fromIntegral $ length $ (board gs)
   -- the board
   field = getField xlen ylen gs
   -- a message if won or lost
   message = case state gs of
                 Lost -> [Translate ((-1)*fwsizex/2) ((-1)*fwsizey/4) $ Color (light $ light red) (Scale (fwsizex) (fwsizey/2)  (Polygon [(0,0),(1,0),(1,1),(0,1),(0,0)]))
                         ,Translate ((-1)*fwsizex/2.1) ((-1)*fwsizey/16) $ Text "Verloren!"]
                 Won  -> [Translate ((-1)*fwsizex/2) ((-1)*fwsizey/4) $ Color (light $ light green) (Scale (fwsizex) (fwsizey/2)  (Polygon [(0,0),(1,0),(1,1),(0,1),(0,0)]))
                         ,Translate ((-1)*fwsizex/2.1) ((-1)*fwsizey/16) $ Text "Gewonnen!"]
                 other -> []


getField xlen ylen gs = Translate ((-1)*fwsizex/2) (fwsizey/2) $  Scale (scaleField xlen ylen) (scaleField xlen ylen) $ Pictures [Translate (j-1) (-1*i) $ toCell e | (i,zs) <- boardToCoordinates board gs, (j,e) <- zs]

boardToCoordinates bd gs = zip [1..] (map (zip [1..]) (board gs))

scaleField xlen ylen = min (fwsizex/xlen) (fwsizey/ylen)

toCell :: Field -> Picture
toCell field = case field of
  Marked   -> getFlag
  Unmarked -> getHiddenbox
  (Open i) -> minesInProximity i
  Mine     -> getBomb
--  otherwise -> getHiddenbox


getBomb = Pictures [box red [(0,0),(1,0),(1,1),(0,1),(0,0)],Translate (0.5) (0.5) $ Color black (circleSolid 0.3)]

getFlag = Pictures [box (greyN 0.7)  [(0,0),(1,0),(1,1),(0,1),(0,0)],
                 (Pictures [box (light $ yellow) [(0.7,0.1),(0.7,0.85),(0.85,0.85),(0.85,0.1),(0.7,0.1)],
                            box (dark $ red)     [(0.2,0.6),(0.8,0.6),(0.8,0.8),(0.2,0.8),(0.2,0.6)]])]

getHiddenbox = box (greyN 0.5) [(0,0),(1,0),(1,1),(0,1),(0,0)]

minesInProximity i  =
  if i > 2 then Pictures [box orange [(0,0),(1,0),(1,1),(0,1),(0,0)],Translate 0.2 (0.1) $ Scale 0.007 0.007 (Text (show i))]
  else if i > 0
    then Pictures [box yellow [(0,0),(1,0),(1,1),(0,1),(0,0)],Translate 0.2 (0.1) $ Scale 0.007 0.007 (Text (show i))]
  else Pictures [box green [(0,0),(1,0),(1,1),(0,1),(0,0)],Translate 0.2 (0.1) $ Scale 0.007 0.007 (Text (show i))]

-- generate colorful boxes
box col xs = Pictures [Color col (Polygon xs), Color black (Line xs)]
