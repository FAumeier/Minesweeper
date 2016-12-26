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

-- Startzustand erzeugen      
start init1 init2 size percentage_mines =   GameState {board = bd, mines = ms, state=Undecided} 
  where 
    -- Matrix: Alles unmarkiert
    bd     = replicate size (replicate size Unmarked)
    -- Minen zu"fallig erzeugen
    ms     = takeNub (round $ (fromIntegral $ size*size)*percentage_mines/100) $ zip (randomRs (0,size-1)  (mkStdGen init1)) (randomRs (0,size-1) (mkStdGen init2))
    -- Hilfsfunktionen
    takeNub i xs =  go i xs []
    go 0 _ ys = ys
    go i (x:xs) ys 
      | x `elem` ys  = go i     xs ys
      | otherwise    = go (i-1) xs (x:ys)

main =
  do
      -- Groesse des Spielfelds: erstes Argument oder default = 10
      let size = 8
      -- Prozentualer Anteil an Minen: zweites Argument oder default = 40%
      let percentage_mines = 35
      -- Startzustand erzeugen
      -- zwei Pseudo-Zufallszahlen
      init1 <- randomIO 
      init2 <- randomIO
      let gamestate = (start init1 init2 size percentage_mines)
      -- play aus der Gloss-Bibliothek 
      play (InWindow "Minesweeper 0.1" (wsizex,wsizey) (10,10)) white 1 gamestate toPicture handleEvent (\float world -> world)
            
-- handleEvent behandelt die Maus-Events, rechter Button zum Markieren / Entmarkieren, linker Button um Feld aufzudecken      
handleEvent (EventKey (MouseButton button) Down _ (x,y)) gs = 
 let -- Mauskoordinaten (x,y) in Spielfeldkoordinaten (xnew,ynew) umrechnen:
     xlen  = fromIntegral $ length $ (board gs)!!0
     ylen  = fromIntegral $ length $ (board gs)
     xnew  = truncate (xlen * ((x+(fwsizex/2)))/fwsizex)
     ynew  = truncate (ylen * (abs ((((y+(fwsizey/2)))/fwsizey) -1)))
 in 
   -- welcher Button wurde gedrückt
   case button of 
       LeftButton ->  playStep (OpenField (xnew,ynew)) gs
       RightButton -> playStep (Toggle (xnew,ynew)) gs 
       _           -> gs
handleEvent (EventKey (Char 'q') Down _ _) gs = error "Stop!"
-- alle anderen Events ver"andern den Zustand nicht:       
handleEvent event state = state

-- toPicture: Bild f"ur den GameState erzeugen
toPicture :: GameState -> Picture
toPicture gs = Pictures $ field:message                 
  where
   -- das Spielfeld
   field =  Translate ((-1)*fwsizex/2) (fwsizey/2) $  Scale scale scale $ Pictures [Translate (j-1) (-1*i) $ toCell e | (i,zs) <- toTup board , (j,e) <- zs]
   -- eine Nachricht, fall gewonnen oder verloren wurde:
   message = case state gs of
                 Lost -> [Translate ((-1)*fwsizex/2) ((-1)*fwsizey/4) $ Color (light $ light red) (Scale (fwsizex) (fwsizey/2)  (Polygon [(0,0),(1,0),(1,1),(0,1),(0,0)]))
                         ,Translate ((-1)*fwsizex/2.1) ((-1)*fwsizey/16) $ Text "You lost!"]
                 Won  -> [Translate ((-1)*fwsizex/2) ((-1)*fwsizey/4) $ Color (light $ light green) (Scale (fwsizex) (fwsizey/2)  (Polygon [(0,0),(1,0),(1,1),(0,1),(0,0)]))
                         ,Translate ((-1)*fwsizex/2.1) ((-1)*fwsizey/16) $ Text "You won!"]
                 other -> []
   -- Matrix in Koordinaten umrechnen                 
   toTup matrix = zip [1..] (map (zip [1..]) (board gs))
   -- Gr"osse der Matrix aus dem GameState bestimmen
   xlen  = fromIntegral $ length $ (board gs)!!0
   ylen  = fromIntegral $ length $ (board gs)
   -- Skalierung eines Felds: Fenstergr"osse / Anzahl Felder
   scale = min (fwsizex/xlen) (fwsizey/ylen)
   -- Bild eines Felds erzeugen, abh"anging vom Zustand des Felds
   toCell Marked   = flag   
   toCell Unmarked = hidden
   toCell (Open i) = num i
   toCell Mine     = bomb
   -- Mine
   bomb = Pictures [box red [(0,0),(1,0),(1,1),(0,1),(0,0)],Translate (0.5) (0.5) $ Color black (circleSolid 0.3)]
   -- Markierung
   flag = Pictures [box (greyN 0.7)  [(0,0),(1,0),(1,1),(0,1),(0,0)],
                    (Pictures [box (light $ yellow) [(0.7,0.1),(0.7,0.85),(0.85,0.85),(0.85,0.1),(0.7,0.1)], 
                               box (dark $ red)     [(0.2,0.6),(0.8,0.6),(0.8,0.8),(0.2,0.8),(0.2,0.6)]])]
   -- Noch nicht aufgedeckt
   hidden = box  (greyN 0.5) [(0,0),(1,0),(1,1),(0,1),(0,0)]
   -- Aufgedeckt: Zahl anzeigen
   num i  = Pictures [box yellow [(0,0),(1,0),(1,1),(0,1),(0,0)],Translate 0.2 (0.1) $ Scale 0.007 0.007 (Text (show i))]
   -- Farbige Box erzeugen
   box col xs = Pictures [Color col (Polygon xs), Color black (Line xs)]