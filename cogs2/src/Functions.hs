module Functions where

import Datatypes
import Data.Maybe     (fromJust)
import Data.List      (find, elemIndex, nub)
import Constants

--        SETUP

-- | Add direction to all the sources/sinks. (priority is NWSE and sourc > sink).
addDirs :: Int -> World -> World
addDirs n wo@(World (w,h) so si _ _ _ _)
  | n == 0    = wo { sources = new si so }
  | otherwise = wo { sinks = new so si}
    where
      new l = updateLinks (w,h) l []

-- | Update Sources OF sinks.
updateLinks :: Coord -> [Link] -> [Link] -> [Link] -> [Link]
updateLinks _ _ nS [] = nS
updateLinks z@(w,h) l nS (s:ss) = updateLinks z l (s {dir = d (pos s)}:nS) ss
  where valid c di = Source c di `notElem` (nS++l) && Sink c di `notElem` (nS++l)
        d c@(x,y) | y == 0 && valid c north = north
                  | x == w-1 && valid c east = east
                  | y == h-1 && valid c south = south
                  | otherwise = west

-- | Add a hole to the world. (Coord with no tile)
makeHole :: World -> World
makeHole wo@(World (w,h) _ _ ft lt _ _) = wo {hole = head rest}
  where full = [ (x,y) | x <- [0..w-1], y <- [0..h-1]]
        tiles = map coo ft++map coo lt
        rest = filter (`notElem` tiles) full

-- | A proper setup of a world.
setup :: World -> World
setup = (addSi . addSo) . makeHole
        where addSo = addDirs 0
              addSi = addDirs 1


--        PROCESS

-- | Moves the tile that has been clicked. Assumed that movement is possible
move :: Coord -> World -> World
move co ww | validpos co && notFixed = ww { hole = co , looseT = nt:filt_tls}
           | otherwise = ww
  where match (Tile c _) = co == c
        filt_tls = filter (not . match) $ looseT ww
        nt = (fromJust $ find match (looseT ww)) {coo = hole ww}
        -- nt = (\(Tile _ d) -> Tile (hole ww) d) (fromJust (find match $ looseT ww)):filt_tls
        validpos (x,y) = x >= 0 && y >= 0 && x < w && y < h
        notFixed = co `notElem` map coo (fixedT ww)
        (w,h) = dms ww


--        CHECKSTATUS

-- | Checks if no leak && if connected to a closed network with #sinks == #sources
checkLink :: World -> Coord -> Bool
checkLink w c = lengthCheck && notEmpty
                 where  lengthCheck = length connSi <= length connSo
                        notEmpty    = not $ null connSi
                        connxs = connections w c
                        connSo = filter (`elem` sources w) $ map (uncurry Source) connxs
                        connSi = filter (`elem` sources w) $ map (uncurry Sink) connxs

checkStatus :: World -> World
checkStatus w@(World _ so si _ _ _ _)
  | lengthCheck && notEmpty = w {status = Completed}
  | otherwise   = w {status = InProgress}
    where conxs = nub . concatMap (connections w . pos)
          lengthCheck = length leakedSi <= length leakedSo
          notEmpty    = all (not . null) $ map (connections w . pos) si
          leakedSo = filter (`elem` so) (map (uncurry Source) $ conxs si)
          leakedSi = filter (`elem` si) (map (uncurry Sink) $ conxs si)

-- | Functie die checkt welke tiles er verbonden zijn met een bepaalde tile.
connections :: World -> Coord -> [(Coord, Direction)]
connections w c = connections' w [c] [c] []

connections' :: World -> [Coord] -> [Coord] -> [(Coord, Direction)] -> [(Coord, Direction)]
connections' _ [] _ cd = cd
connections' wo@(World (w,h) _ _ ft lt hole _) (co:s) ac cd
  | co == hole || hole `elem` nCo || length nT /= length fnC = []
  | otherwise = connections' wo (s++fnC) (ac++fnC) (cd++fncd)
    where
      tileFromCo co = (ft++lt) !! fromJust (co `elemIndex` map coo (ft++lt))
      thisT = tileFromCo co
      nCo = map (addCo (coo thisT)) $ dirs thisT
      insideBox (x,y) = x >= 0 && x < w && y >= 0 && y < h
      nT = map tileFromCo $ filter (`notElem` ac) $ filter insideBox nCo
      fnC = map coo $ filter (\ (Tile coo dirs) -> co `elem` map (addCo coo) dirs) nT
      fncd = map (\co' -> (co ,  subCo co' co)) $ filter (`notElem` map fst cd) $ filter (not . insideBox) nCo


--          OTHER FUNCTIONS

position :: Coord -> (Float,Float) -> Coord
position (width, height) (x,y) = (dx, dy)
            where dx = floor ((x  + (fromIntegral width * fromIntegral scale' / 2)) / fromIntegral scale')
                  dy = round (((fromIntegral height + 1) * fromIntegral scale' / 2 - y) / fromIntegral scale')

neighbour :: Coord -> Coord -> Bool
neighbour (a,b) (c,d) = (c-a)*(d-b) == 0 && ( abs (c-a) == 1 || abs (d-b) == 1 )

addCo :: Coord -> Coord -> Coord
addCo (a,b) (c,d) = (a+c, b+d)

subCo :: Coord -> Coord -> Coord
subCo (a,b) (c,d) = (a-c, b-d)

pack :: Eq a => [a] -> [[a]]
pack = foldr join []
 where join l []                       = [[l]]
       join l ([]:xss)                 = [l]:xss
       join l ((x:xs):xss) | l == x    = (l:x:xs):xss
                           | otherwise = [l]:(x:xs):xss
