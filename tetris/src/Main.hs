
-- standard library imports
import Data.List

-- third party imports
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color

-- local application imports
import Debug.Trace (traceShowId)
import System.Random
{-|

  We stellen een vallende tetromino voor door een lijst van blokken.

  - `size` is de breedte van de tetromino, a.k.a. het vierkantje waarbinnen
    hij draait. Zie downstaand voorbeeld.
  - `position` is de huidige positie van de tetromino, de linkeruphoek van
    het vierkantje gedefinieerd door size.
  - `blocks` zijn de 4 blokken van deze tetromino, met positie relatief ten
    opzicht van de positie van de tetromino.

       0 1 2 3 4 x         0 1 2 3 4 x    Wie zien hier hoe de i-tetromino
    o-------------->    o-------------->  gedraaid wordt naar right. De
    |                   |                 min-karakters teken je niet, maar
   0|  - - - -         0|  - - x -        zij geven het vierkant bepaald
   1|  x x x x         1|  - - x -        door `size` aan. De x-karakters
   2|  - - - -         2|  - - x -        stellen de blokken voor. We zien
   3|  - - - -         3|  - - x -        hoe het vierkant (size) en de
   4|                  4|                 positie down de rotatie niet
  y |                 y |                 veranderen.
    v                   v

       0 1 2 3 4 x         0 1 2 3 4 x    Wie zien hier hoe de s-tetromino
    o-------------->    o-------------->  gedraaid wordt naar right.
    |                   |                 Opnieuw gaan `size` en `position`
   0|  - x x           0|  - x -          niet veranderen, enkel de
   1|  x x -           1|  - x x          relatieve positie van de blokjes.
   2|  - - -           2|  - - x
   3|                  3|
  y |                 y |
    v                   v

-}
-- | Een enkele blok met een (x,y) positie.
newtype Block = Block (Int, Int) deriving (Eq, Show)
show' :: Block -> (Int, Int)
show' (Block (x,y)) = (x,y)

-- | Het bord met de vaste (niet-vallende) vierkantjes.
type Board = [Block]

data Status = OK | KO deriving (Show)

type Coordinate = (Int, Int)
type Direction = (Int, Int)
-- | De verschillende richtingen worden hier gedefinieerd
left :: Direction
left = (-1,0)

right :: Direction
right = (1,0)

down :: Direction
down = (0,1)

rotation :: Direction
rotation = (0, 0)

-- | Tetris bestaat uit vaste en vallende blokjes.
data Tetris = Tetris { board :: Board
                     , tetromino :: Tetromino
                     , direction :: Direction
                     , status :: Status
                     , rndmgen :: StdGen
                     , time :: Float
                     } deriving (Show)


data Tetromino = Tetromino
    { size     :: Int         -- ^ omvattend vierkant
    , position :: (Int, Int)  -- ^ positie linkeruphoek vierkant
    , blocks   :: [Block]     -- ^ blokken relatief t.o.v. position
    } deriving (Eq, Show)

-- | Beschikbare tetronimo's
tetrominoes :: [Tetromino]
tetrominoes = [i,o,t,j,l,s,z]

i, o, t, j, l, s, z :: Tetromino
i = Tetromino 4 (halfw,0) [ Block (0,1), Block (1,1), Block (2,1), Block (3,1) ]
o = Tetromino 2 (halfw,0) [ Block (0,0), Block (1,0), Block (0,1), Block (1,1) ]
t = Tetromino 3 (halfw,0) [ Block (1,0), Block (0,1), Block (1,1), Block (2,1) ]
j = Tetromino 3 (halfw,0) [ Block (0,0), Block (0,1), Block (1,1), Block (2,1) ]
l = Tetromino 3 (halfw,0) [ Block (2,0), Block (0,1), Block (1,1), Block (2,1) ]
s = Tetromino 3 (halfw,0) [ Block (1,0), Block (2,0), Block (0,1), Block (1,1) ]
z = Tetromino 3 (halfw,0) [ Block (0,0), Block (1,0), Block (1,1), Block (2,1) ]
halfw = fromIntegral (width `div` 2 - 1)

-- | Constanten om de breedte/hoogte en seed in te stellen.
width :: Int
width   = 10
height :: Int
height  = 20
scale' :: Int
scale'  = 20

seed :: Int
seed    = 159

-- | Schetst de window
window :: Display
window = InWindow "UGent Functioneel Programmeren Opdracht 2"
         ((width+2) * scale', (height+2) * scale')
         (0, 0)
window' = FullScreen

-- | De achtergrondkleur in RGBA waarden tussen 0 en 255.
backgroundcolor :: Color
backgroundcolor = makeColorI 0 0 0 0

-- | De voorgrondkleur in RGBA waarden tussen 0 en 255.
foregroundcolor :: Color
foregroundcolor = makeColorI 190 175 0 0

-- | Zet de fps van het spel.
fps :: Int
fps = 30

-- | De box waarin het tetrisveld zich bevindt.
box :: Picture
box =  Color foregroundcolor (rectangleSolid scaleW scaleH)
       where scaleW = fromIntegral (scale' * width)
             scaleH = fromIntegral (scale' * height)

-- | Het beginbord, met een rand van vaste blokjes aan de downkant en beide zijkanten.
boardZero = [Block (x, height+1) | x <- [0..width]] ++ [Block (x, y) | x <- [-1, width], y <- [0..height+1]]
beginTetr = tetrominoes !! (seed `mod` length tetrominoes)

-- | The starting world.
startTetris = Tetris boardZero beginTetr down OK (mkStdGen seed) 0


-----------------------------------------------------------------------
-----------------------------------------------------------------------


-- Een Picture die een enkel vakje op het scherm voorstelt.
square :: Picture
square  = rectangleSolid (fromIntegral scale') (fromIntegral scale')

greysquare :: Picture
greysquare  = Color (greyN 0.3) square

whitesquare :: Picture
whitesquare = Color white square

sQuare :: Int -> Picture
sQuare 0 = whitesquare
sQuare 1 = greysquare
sQuare 2 = square

-- | Beweeg het vierkantje naar de juiste positie
setsquare :: Int -> Coordinate -> Picture
setsquare clr (x, y) = translate dx dy (sQuare clr)
                      where dx = fromIntegral ((x - (width `div` 2)) *scale' + ds)
                            dy = fromIntegral (((height `div` 2)- y) *scale' + ds)
                            ds = scale' `div` 2

-- | Zet een lijst van blokken om naar een lijst van pictures
blocksToFigures :: Int -> [Block] -> [Picture]
blocksToFigures clr = map (setsquare clr . show')


displayTetris :: Tetris -> Picture
displayTetris (Tetris b (Tetromino s o blks) _ _ _ _)
      = pictures ( (blocksToFigures 1 b) ++ (blocksToFigures 0 blks') )
        where blks' = map (Block . (\(x,y) -> (x + fst o, y + snd o)) . show') blks

-------------------------------------------------------------------------------

-- | Hulpfuncties die de co teruggeven.
co' o = map ((\(x,y) -> (x + fst o, y + snd o)) . show')
co d  = map ((\(x,y) -> (x - fst d, y - snd d)) . show')

-- | 'solid?' kijkt wanneer een Tetromino een deel wordt van de vaste blokjes,
-- als er dus een blokje van het bord onder een blokje staat van de tetro.
solid :: Tetromino -> Board -> Bool
solid (Tetromino s o blks) b = or [a == b | a <- co (0,1) b, b <- co' o blks]

sides :: Tetromino -> Direction -> Board -> Bool
sides (Tetromino s o blks) d b = or [a == b | a <- co d b, b <- co' o blks]

-- | Checkt wanneer het bord komt waar een tetro zou moeten spawnen, wat het spel dus KO'd
tooHigh :: Board -> Bool
tooHigh b = or [ y == 0 && x >= 0 && x < width | (x, y) <- map show' b]

-- | validMove checkt als de tetro in het bord past (of eiglijk juist niet).
validMove :: Board -> Tetromino -> Bool
validMove b (Tetromino s o blks) = and [ i /= j | i <- map show' b, j <- co' o blks]

-- | Beweegt de tetro in een bepaalde richting.
moveTetro :: Board -> Tetromino -> Direction -> Tetromino
moveTetro b t@(Tetromino s o blks) d
    | d == rotation && validMove b rot_t  = rot_t
    | validMove b new_t = new_t
    | otherwise         = t
                         where new_t = Tetromino s ((fst o) + (fst d), (snd o) + (snd d)) blks
                               rot_t = rotate' t

-- | rotate' geeft het blokje (in hetzelfde vierkant) terug, maar gedraaid.
rotate' :: Tetromino -> Tetromino
rotate' (Tetromino s o blks) = Tetromino s o newBlks
                      where newBlks = [Block (y, s-1 - x) | (x,y) <- map show' blks]

-- | Kiest welke tetromino de volgende zal zijn, en vernieuwd de stdGen.
nextTetr :: Tetris -> Tetris
nextTetr (Tetris b t d s g ti)  = Tetris b (tetrominoes !! (new_i `mod` length tetrominoes)) d s new_g ti
                                    where (new_i, new_g) = next g

-- | Checkt en verwijdert volle lijnen uit het bord.
lineSweeper :: Board -> Int -> Board
lineSweeper b i | i == (height+1) = b
                | fullLine      = lineSweeper new_b (i+1)
                | otherwise     = lineSweeper b (i+1)
                where fullLine  = length (filter_y b (==) i) == (width + 2)
                      new_b     = underi ++ abovei
                      underi    = filter_y b (>) i ++ filter_y (filter_x b (==) (-1) ++ filter_x b (==) width) (<=) i
                      abovei    = map (Block . (\(x,y) -> (x,y+1)) . show') (filter_y (filter_x (filter_x b (>) 0) (<) width) (<) i)
                      filter_y b f i = map Block (filter (\(x,y) -> y `f` i) (map show' b))
                      filter_x b f i = map Block (filter (\(x,y) -> x `f` i) (map show' b))

-- | Functie die ervoor zorgt dat de tetromino vast gezet wordt, en checkt als er een volle lijn gemaakt is.
solidify :: Tetris -> Tetris
solidify (Tetris b t@(Tetromino _ o blks) d s g ti) = nextTetr (Tetris new_b t d s g ti)
                                                      where new_b = lineSweeper (b ++ map Block (co' o blks)) 0

-- | Deze functie geeft een tetriswereld terug maar checkt als:
-- | de tetris niet moet vastgezet worden, of als het spel KO'd,
-- | en beweegt de wereld dan naar beneden.
processTetris :: Tetris -> Tetris
processTetris ttr@(Tetris brd t d st g ti)
  | tooHigh brd = ttr {status = KO}
  | solid t brd = solidify ttr
  | sides t d brd = ttr {direction = down}
  | otherwise   = ttr {tetromino = moveTetro brd t d}

-- | Functie die telkens een stap uitvoert.
step :: Float -> Tetris -> Tetris
step delta ttr@(Tetris _ _ _ KO _ _ ) = ttr
step delta ttr@(Tetris _ _ _ OK _ ti) | new_ti < 0.3 = ttr {time = new_ti}
                                      | otherwise  = processTetris (ttr {time = 0.0, direction = down})
                                        where new_ti = ti + delta


isKey k1 (EventKey k2 Down  _ _ )  = k1 == k2
isKey _  _  = False
-- | Functie die de input verwerkt.
processInput :: Event -> Tetris -> Tetris
processInput key ttr@(Tetris _ _ _ KO g _)
    | isKey (SpecialKey KeyEnter) key = startTetris {rndmgen = snd (next g)}
    | otherwise = ttr
processInput key ttr@(Tetris b t d OK g ti)
    | isKey (SpecialKey KeyLeft) key  = processTetris (Tetris b t left OK g 0)         -- ^ Beweeg volgende stap naar left
    | isKey (SpecialKey KeyRight) key = processTetris (Tetris b t right OK g 0)        -- ^ Beweeg volgende stap naar right
    | isKey (Char 'r') key || isKey (SpecialKey KeyUp) key  = processTetris (Tetris b t rotation OK g 0)
    | isKey (SpecialKey KeyDown) key  = processTetris (Tetris b t down OK g 0)    -- ^ Beweeg volgende stap niet, roteer
    | otherwise                       = ttr         -- ^ Beweeg volgende stap naar beneden

-------------------------------------------------------------------------------

-- | Tetris!
main :: IO ()
main =  play window         backgroundcolor
             fps            startTetris
             displayTetris  processInput
             step
