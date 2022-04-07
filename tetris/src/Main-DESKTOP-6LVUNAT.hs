
-- standard library imports
import Data.List

-- third party imports
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color

-- local application imports
import System.Random (RandomR, mkStdGen, StdGen)

-- | Een enkele blok met een (x,y) positie.
data Block = Block (Int, Int) deriving (Eq, Show)

{-|

  We stellen een vallende tetromino voor door een lijst van blokken.

  - `size` is de breedte van de tetromino, a.k.a. het vierkantje waarbinnen
    hij draait. Zie onderstaand voorbeeld.
  - `position` is de huidige positie van de tetromino, de linkerbovenhoek van
    het vierkantje gedefinieerd door size.
  - `blocks` zijn de 4 blokken van deze tetromino, met positie relatief ten
    opzicht van de positie van de tetromino.

       0 1 2 3 4 x         0 1 2 3 4 x    Wie zien hier hoe de i-tetromino
    o-------------->    o-------------->  gedraaid wordt naar rechts. De
    |                   |                 min-karakters teken je niet, maar
   0|  - - - -         0|  - - x -        zij geven het vierkant bepaald
   1|  x x x x         1|  - - x -        door `size` aan. De x-karakters
   2|  - - - -         2|  - - x -        stellen de blokken voor. We zien
   3|  - - - -         3|  - - x -        hoe het vierkant (size) en de
   4|                  4|                 positie onder de rotatie niet
  y |                 y |                 veranderen.
    v                   v

       0 1 2 3 4 x         0 1 2 3 4 x    Wie zien hier hoe de s-tetromino
    o-------------->    o-------------->  gedraaid wordt naar rechts.
    |                   |                 Opnieuw gaan `size` en `position`
   0|  - x x           0|  - x -          niet veranderen, enkel de
   1|  x x -           1|  - x x          relatieve positie van de blokjes.
   2|  - - -           2|  - - x
   3|                  3|
  y |                 y |
    v                   v

-}
data Tetromino = Tetromino
    { size     :: Int         -- ^ omvattend vierkant
    , position :: (Int, Int)  -- ^ positie linkerbovenhoek vierkant
    , blocks   :: [Block]     -- ^ blokken relatief t.o.v. position
    } deriving (Eq, Show)

-- | Het bord met de vaste (niet-vallende) vierkantjes.
data Board = Board [Block] deriving (Eq, Show)

-- | Tetris bestaat uit vaste en vallende blokjes.
data Tetris = Tetris Board Tetromino deriving (Eq, Show)

-- | Beschikbare tetronimo's
tetrominoes :: [Tetromino]
tetrominoes = [i,o,t,j,l,s,z]

i, o, t, j, l, s, z :: Tetromino
i = Tetromino 4 (0,0) [ Block (0,1), Block (1,1), Block (2,1), Block (3,1) ]
o = Tetromino 2 (0,0) [ Block (0,0), Block (1,0), Block (0,1), Block (1,1) ]
t = Tetromino 3 (0,0) [ Block (1,0), Block (0,1), Block (1,1), Block (2,1) ]
j = Tetromino 3 (0,0) [ Block (0,0), Block (0,1), Block (1,1), Block (2,1) ]
l = Tetromino 3 (0,0) [ Block (2,0), Block (0,1), Block (1,1), Block (2,1) ]
s = Tetromino 3 (0,0) [ Block (1,0), Block (2,0), Block (0,1), Block (1,1) ]
z = Tetromino 3 (0,0) [ Block (0,0), Block (1,0), Block (1,1), Block (2,1) ]

-- | Constanten om de breedte/hoogte en seed in te stellen.
breedte :: Int
breedte = 20
hoogte :: Int
hoogte  = 50
schaal :: Int
schaal = 10

seed :: Int
seed    = 42

-- | Het beginbord, met een rand van vaste blokjes aan de onderkant en beide zijkanten.
boardZero = Board ([Block (x, (hoogte+1)) | x <- [1..breedte]] ++ [Block (x, y) | x <- [-1, breedte], y <- [0..hoogte+1]])
beginTetr =
-- | The new world.
data World  = TetrisWorld Board Tetromino StdGen
              deriving (Show)
beginworld = TetrisWorld boardZero beginTetr (mkStdGen seed)

-- | 'outOfBounds' die checkt als een blok botst met de zijkanten van het spelbord.
outOfBounds :: Tetromino -> Bool
outOfBounds t = leftClear && rightClear
          --moet telkens de uiterste blok (of toch tenminste alle x waarden)
          --controleren en checken als ze niet negatief zijn of groter dan de breedte
              where leftClear t  = not ( any (map ()))
                    rightClear t =

-- | 'solid?' kijkt wanneer een Tetromino een deel wordt van de vaste blokjes.
solid? :: Tetromino -> Board -> Bool
solid? = undefined

-- | 'oneDown' laat de tetromino zakken. (Tijdens het spelverloop moet
--    hiervoor worden gecheckt als de tetromino niet raakt aan de vaste blokken)
oneDown :: Tetromino -> Tetromino
oneDown (Tetromino s (x,y) listbl) = Tetromino s (x, y+1) listbl

-- | 'rotate' geeft het blokje (in hetzelfde vierkant) terug, maar gedraaid.
rotate :: Tetromino -> Tetromino
rotate t s p = undefined

-- | Kiest welke tetromino de volgende zal zijn, en vernieuwd de stdGen.
nextTetr :: World -> World
nextTetr (TetrisWorld b [] std) = TetrisWorld b (tetrominoes !! tetrIndex) gen
                                  where (tetrIndex, gen) = randomR (0,7) std

-- |

-- | Zorgt dat een lijst van blokken omgezet wordt naar een lijst met de figuren.
coordsToFigure :: [Block] -> [Picture]
coordsToFigure blks = map coordToFigure blks

-- | Tetris!
main :: IO ()
main = return ()
