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

-- generate the initial gamestate
start init1 init2 size percentage_mines =   GameState {board = bd, mines = ms, state=Undecided}
  where
    -- initialize board
    bd     = replicate size (replicate size Unmarked)
    -- randomly distribute mines
    ms     = takeNub (round $ (fromIntegral $ size*size)*percentage_mines/100) $ zip (randomRs (0,size-1)  (mkStdGen init1)) (randomRs (0,size-1) (mkStdGen init2))
    -- helper
    takeNub i xs =  go i xs []
    go 0 _ ys = ys
    go i (x:xs) ys
      | x `elem` ys  = go i     xs ys
      | otherwise    = go (i-1) xs (x:ys)

main =
  do
      let size = 8 -- size of the board
      let percentage_mines = 5
      -- two pseudo random numbers
      init1 <- randomIO
      init2 <- randomIO
      let gamestate = (start init1 init2 size percentage_mines)
      play (InWindow "Minesweeper 0.1" (wsizex,wsizey) (10,10)) white 1 gamestate toPicture handleEvent (\float world -> world)

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
toPicture :: GameState -> Picture
toPicture gs = Pictures $ field:message
  where
   -- the board
   field =  Translate ((-1)*fwsizex/2) (fwsizey/2) $  Scale scale scale $ Pictures [Translate (j-1) (-1*i) $ toCell e | (i,zs) <- toTup board , (j,e) <- zs]
   -- a message if won or lost
   message = case state gs of
                 Lost -> [Translate ((-1)*fwsizex/2) ((-1)*fwsizey/4) $ Color (light $ light red) (Scale (fwsizex) (fwsizey/2)  (Polygon [(0,0),(1,0),(1,1),(0,1),(0,0)]))
                         ,Translate ((-1)*fwsizex/2.1) ((-1)*fwsizey/16) $ Text "You lost!"]
                 Won  -> [Translate ((-1)*fwsizex/2) ((-1)*fwsizey/4) $ Color (light $ light green) (Scale (fwsizex) (fwsizey/2)  (Polygon [(0,0),(1,0),(1,1),(0,1),(0,0)]))
                         ,Translate ((-1)*fwsizex/2.1) ((-1)*fwsizey/16) $ Text "You won!"]
                 other -> []
   -- Board to coordinates
   toTup matrix = zip [1..] (map (zip [1..]) (board gs))
   -- calculate size of board from gamestate
   xlen  = fromIntegral $ length $ (board gs)!!0
   ylen  = fromIntegral $ length $ (board gs)
   -- scaling of a field
   scale = min (fwsizex/xlen) (fwsizey/ylen)
   -- picture of a field
   toCell Marked   = flag
   toCell Unmarked = hidden
   toCell (Open i) = num i
   toCell Mine     = bomb
   -- Mine
   bomb = Pictures [box red [(0,0),(1,0),(1,1),(0,1),(0,0)],Translate (0.5) (0.5) $ Color black (circleSolid 0.3)]
   -- Marks
   flag = Pictures [box (greyN 0.7)  [(0,0),(1,0),(1,1),(0,1),(0,0)],
                    (Pictures [box (light $ yellow) [(0.7,0.1),(0.7,0.85),(0.85,0.85),(0.85,0.1),(0.7,0.1)],
                               box (dark $ red)     [(0.2,0.6),(0.8,0.6),(0.8,0.8),(0.2,0.8),(0.2,0.6)]])]
   -- hidden
   hidden = box  (greyN 0.5) [(0,0),(1,0),(1,1),(0,1),(0,0)]
   -- field open, display number of mines
   num i  = Pictures [box yellow [(0,0),(1,0),(1,1),(0,1),(0,0)],Translate 0.2 (0.1) $ Scale 0.007 0.007 (Text (show i))]
   -- generate colorful boxes
   box col xs = Pictures [Color col (Polygon xs), Color black (Line xs)]
