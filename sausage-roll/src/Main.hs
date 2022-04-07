import System.Environment (getArgs)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Data.List
import Data.Function (on)


--      Datatypes
type Coord = (Int, Int)
--direction E S W N
type Direction = Coord --MAYBE WRONG TODO
type Player = [Coord]
type Status = Int
data Alive = InProgress | Completed deriving (Show, Eq)
data Sausage = Sausage  { coords :: [Coord]
                        , statusBoven :: (Status, Status)
                        , statusOnder :: (Status, Status)
                        } deriving (Show)
data World = World { grass :: [Coord]
                   , water :: [Coord]
                   , grills :: [Coord]
                   , player :: [Coord]
                   , sausages :: [Sausage]
                   , direction :: Coord
                   , status :: Alive
                   } deriving (Show)

--      Env Constants
scale' = 60
scale'' = scale' `div` 5 * 4
startWorld = World [] [] [] [] [] (0,0) InProgress
tileList = ['+', '~', '#']
bodyList = ['N', 'E', 'S', 'W']
dirList = [(0,-1),(1,0),(0,1),(-1,0)]
sausList = [['e', 's'],['2','3'],['b','c']]
dxdyList = [(1,0),(0,1),(0,-1),(-1,0)]
sausListstr = [['e','s','n','w'],['2','3','1','4'],['b','c','a','d']]


--      Display
-- | Schetst de window
window :: Display
window = FullScreen
height = 10
width  = 10

-- | De achtergrondkleur in RGBA waarden tussen 0 en 255.
backgroundcolor :: Color
backgroundcolor = white

-- | De voorgrondkleur in RGBA waarden tussen 0 en 255.
foregroundcolor :: Color
foregroundcolor = white

-- | Zet de fps van het spel.
fps :: Int
fps = 30

-- | De box waarin het tetrisveld zich bevindt.
box :: Picture
box =  Color foregroundcolor (rectangleSolid scaleW scaleH)
       where scaleW = fromIntegral (scale' * width)
             scaleH = fromIntegral (scale' * height)

-- Een Picture die een enkel vakje op het scherm voorstelt.
square :: Picture
square  = rectangleSolid (fromIntegral scale') (fromIntegral scale')
square' = rectangleSolid (fromIntegral scale'') (fromIntegral scale'')

grilled :: Color
grilled = makeColorI 150 90 1 255
burnt :: Color
burnt = makeColorI 63 40 6 255
body :: Color
body = greyN 0.34
body' = greyN 0.33
raw :: Color
raw = makeColorI 206 123 195 255
orange' :: Color
orange' = makeColorI 220 150 5 255
green'  = makeColorI 72 160 20 255

-- | Kleurt een vierkant.
colSq :: Color -> Picture
colSq c = Color c square
colSq' c = Color c square'

-- | Zet een coord om naar een afbeelding op de juiste positie.
coordToSq :: Color -> Coord -> Picture
coordToSq c (x,y) | c `elem` [raw,grilled,burnt, body']  = translate dx dy (colSq' c)
                  | otherwise = translate dx dy (colSq c)
                    where dx = fromIntegral ((x - (width `div` 2))* scale' + ds)
                          dy = fromIntegral (((height `div` 2)- y)* scale' + ds)
                          ds = scale' `div` 2

-- | Zet een wereld om naar een afbeelding.
worldToPicture :: World -> Picture
worldToPicture (World grs wtr grl plyr saus _ _) =
  pictures $ map (coordToSq green') grs ++
             map (coordToSq azure) wtr ++
             map (coordToSq orange') grl ++
             [coordToSq body (head plyr)] ++
             [coordToSq body' (last plyr)] ++
             map (coordToSq raw) (filterStatus 0) ++
             map (coordToSq grilled) (filterStatus 1) ++
             map (coordToSq burnt) (filterStatus 2)
              where filterStatus i = map fst $ filter (\(co,s) -> s == i) coSt
                    coSt = concatMap (\(Sausage [c0,c1] (s0,s1) _) -> [(c0,s0),(c1,s1)]) saus


--      Functies
-- | Verwerkt invoer tot een wereld.
parser :: Coord -> String -> World -> World
parser _ "" w = w
parser co@(x,y) (c:s) w@(World grs wtr grl pl sa di _)
  -- Tiles
  | c == '+'  = parser co s $ w {grass = co:grs}
  | c == '~'  = parser co s $ w {water = co:wtr}
  | c == '#'  = parser co s $ w {grills = co:grl}
  -- Entities
  | c `elem` bodyList         = parser (x+1,y) s $ addPlayer w co c
  | c `elem` head sausList    = parser (x+1,y) s $ addSaucer w co c 0
  | c `elem` (sausList !! 1)  = parser (x+1,y) s $ addSaucer w co c 1
  | c `elem` last sausList    = parser (x+1,y) s $ addSaucer w co c 2
  | c == ' '  = parser (x+1,y) s w
  | c `elem` restList = parser (x+1,y) s w
  -- Newlines
  | c == '\n' = parser (0,y+1) s w
  | otherwise = parser co s w
    where restList = ['n','w','x','1','4','a','d']


-- | Add a player to the world.
addPlayer :: World -> Coord -> Char -> World
addPlayer (World grs wtr grl _ sa _ st) (x,y) c
  = World grs wtr grl pl sa dir st
    where dir@(dx, dy) = dirList !! (\(Just n) -> n) (c `elemIndex` bodyList)
          pl = [(x,y), (x+dx, y+dy)]

-- | Add a new sausage to the world.
addSaucer :: World -> Coord -> Char -> Int -> World
addSaucer w (x,y) c n = w {sausages = newSa n}
                        where (dx, dy)  = dxdyList !! (\(Just n) -> n) (c `elemIndex` (sausList !! n))
                              newSa n   = sausages w ++ [Sausage [(x, y), (x+dx, y+dy)] (n,n) (n,n)]

-- | Move the player in a certain direction (Or rotate, and push & Burn everything)
move :: World -> Direction -> World
move w@(World grs _ grl pl sa di _) dir@(dx,dy)
  | (di == dir || di == (-dx,-dy)) && validMove = if inFront 0
                             then w {player = newPl 0, sausages = checkedSa 0 dir }
                             else w {player = newPl 0}
                             -- ^ Aligned, er moet niet geroteerd worden
  | di /= dir && validRot = if inFront 1 && not upperBlock
                            then w {player = newPl 1, direction = dir, sausages = checkedSa 1 ((\(x,y) -> (-x,-y)) di)}
                            else w {player = newPl 1, direction = dir, sausages = checkedSa 1 dir }

                    -- ^ Niet aligned, moet roteren.
  | otherwise = w   -- ^ invalid move (op water/grill).
    where inFront i = any (`elem` concatMap coords sa) (newPl i)
          newPl i | i == 0     = map addto pl
                  | otherwise  = [head pl, addto $ head pl]
          addto (x, y)  = (x+dx, y+dy)
          mult f (x,y)  = (f x, f y)
          newSa i       = push grl sa [] (newPl i++[addto (pl !! 1)])
          validMove     = head (newPl 0) `elem` grs
          validRot      = not ( fst (addto di) == 0 || snd (addto di) == 0)
          upperBlock    = addto (pl !! 1) `elem` concatMap coords sa
          checkedSa i d = checkSausGone (grs++grl) [] (newSa i d)

-- | Function that proccesses the pushing of sausages & grilling.
push :: [Coord] -> [Sausage] -> [Sausage] -> [Coord] -> Direction -> [Sausage]
push grl s ds [] _ = s++ds
push grl saus movedSaus (co:s) (dx, dy) = push grl non (grilledMoved++movedSaus) (s++newCo) (dx, dy)
    where non   = [s | s <- saus,  co `notElem` coords s]
          movedRoll = [s {coords = map addto $ coords s, statusBoven = stO, statusOnder = stB}
                      | s <- saus, stO <- [statusOnder s], stB <- [statusBoven s], co `elem` coords s && tBRolled s]
          movedNoRoll = [s {coords = map addto $ coords s}  | s <- saus, co `elem` coords s && not (tBRolled s)]
          grilledMoved = checkSaus grl (movedRoll++movedNoRoll)
          addto (x,y)  = (x+dx, y+dy)
          newCo = concatMap coords (movedRoll ++ movedNoRoll)
          tBRolled (Sausage [(x1,y1),(x2,y2)] _ _)  = (x2-x1)+dx /= 0 && (y2-y1)+dy /= 0

-- | Checks the sausages for grilling & sinking.
checkSaus :: [Coord] -> [Sausage] -> [Sausage]
checkSaus grl sa = non++grilled
                    where non     = [Sausage [co j !! i | j <- [0,1]] (stB !! i) (stO !! i)
                                     | i <- [0..length sa -1], not (coIsGrill 0 !! i || coIsGrill 1 !! i)]
                          grilled = [Sausage [co j !! i | j <- [0,1]] (stB !! i) (new_stO i)
                                     | i <- [0..length sa -1], coIsGrill 0 !! i || coIsGrill 1 !! i ]
                          stB = map statusBoven sa
                          stO = map statusOnder sa
                          co j = map ((!! j) . coords) sa
                          coIsGrill i = map ((`elem` grl) . (!! i) . coords) sa
                          new_stO i = (if coIsGrill 0 !! i && stO0 i < 2 then stO0 i + 1 else stO0 i
                                      ,if coIsGrill 1 !! i && stO1 i < 2 then stO1 i + 1 else stO1 i)
                          stO0 i = fst $ stO !! i
                          stO1 i = snd $ stO !! i

checkSausGone :: [Coord] -> [Sausage] -> [Sausage] -> [Sausage]
checkSausGone _ ns [] = ns
checkSausGone co ns (sa:s) | any (`elem` co) (coords sa) = checkSausGone co (sa:ns) s
                           | otherwise = checkSausGone co ns s


--      Process
main :: IO ()
main = do (boardFile:steps) <- getArgs
          boardString       <- readFile boardFile
          let succes    = status lastWorld == Completed
              lastWorld = checkStatus $ last worldList
              worldList = scanl move (parser (0,0) boardString startWorld) (map strToDir steps)
          if null steps
            then play window white 30 (parser (0,0) boardString startWorld)
                      worldToPicture processInput processTime
            else if succes
                 then putStr (( unlines . map worldToString $ worldList)++"Opgelost!\n\n")
                 else putStr . unlines . map worldToString $ worldList



-- | Functie die richtingen inleest
strToDir :: String -> Direction
strToDir (c:_) = dirList !! (\(Just n) -> n) (c `elemIndex` bodyList)

-- | Hulpfunctie voor de proccesInput.
isKey k1 (EventKey k2 Down  _ _ )  = k1 == k2
isKey _  _  = False
-- | Functie die de input verwerkt.
processInput :: Event -> World -> World
processInput _ w@(World _ _ _ _ _ _ Completed)  = w
processInput key w@(World _ _ _ _ _ _ InProgress)
    | isKey (Char 'q') key || isKey (SpecialKey KeyLeft) key  = checkStatus $ move w (-1, 0)
    | isKey (Char 'd') key || isKey (SpecialKey KeyRight) key = checkStatus $ move w ( 1, 0)
    | isKey (Char 'z') key || isKey (SpecialKey KeyUp) key    = checkStatus $ move w ( 0,-1)
    | isKey (Char 's') key || isKey (SpecialKey KeyDown) key  = checkStatus $ move w ( 0, 1)
    | otherwise = w

-- | Functie die de status van de wereld updatet na elke move.
checkStatus :: World -> World
checkStatus w | all (==1) statusList = w {status = Completed}
              | otherwise = w
                where statusListTUP = map statusOnder (sausages w) ++ map statusBoven (sausages w)
                      statusList = [ n | (x,y) <- statusListTUP, n <- [x,y] ]

-- | Zeer interessante functie die de tijd verwerkt, deze is zo interessant
-- omdat er helemaal niets moet gebeuren als je het spel gewoon laat aanstaan.
processTime :: Float -> World -> World
processTime _ w = w

-- | Zet de wereld om in een String.
worldToString :: World -> String
worldToString (World grs wtr grl plyr saus dir st)
  | st == Completed = "Opgelost!"
  | otherwise       = addlines ((rev' . sortBy (compare `on` fst) . rev') coList) ""
    where coT i = map (\co -> (co, tileList !! i))
          coBody 0 co = (co, bodyList !! (\(Just n) -> n) (dir `elemIndex` dirList))
          coBody i co = (co, 'x')
          coSaus = concatMap (\(Sausage [co0, co1] (stB0, stB1) _) -> [(co0,str' co0 co1 stB0),(co1,str' co1 co0 stB1)] )
          str' co0 co1 stB = (sausListstr !! stB) !! j co0 co1
          j co0 co1 = (\(Just n) -> n) $ dxdy co0 co1 `elemIndex` dxdyList
          dxdy (x1,y1) (x2,y2) = (x2 - x1, y2 - y1)
          coList = coTileList ++ coEntityList ++ filteredCoList
          coTileList = coT 0 grs ++ coT 1 wtr ++ coT 2 grl
          coEntityList = [coBody i (plyr !! i) | i <- [0,1]] ++ coSaus saus
          completeCoList = [((x,y),' ') | x <- [0..maximum (map (fst . fst) coTileList)], y <- [0..maximum (map (snd . fst) coTileList)]]
          filteredCoList = filter (\(co,c) -> co `notElem` map fst coEntityList) completeCoList
          rev' = map (\((x,y),c) -> ((y,x),c))

-- | Hulpfunctie voor worldToString die "\n" toevoegt waar nodig.
addlines :: [(Coord, Char)] -> String -> String
addlines [(_,c)] s = s++[c]++"\n"
addlines tot@( ((x,y),c) : co'@((x',y'),c') : cocs) str
  | length tot == 1 = str++[c]++"\n"
  | y' == y         = addlines (co':cocs) (str++[c])
  | otherwise       = addlines (co':cocs) (str++[c]++"\n")
