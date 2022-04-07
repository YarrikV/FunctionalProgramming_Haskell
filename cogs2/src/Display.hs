module Display where

import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (fromJust)
import Control.Monad (liftM, fmap)
import Datatypes
import Constants
import Functions (checkLink, addCo)

--      Display
-- | Schetst de window.
window :: Display
window = FullScreen

-- | De achtergrondkleur.
backgroundcolor :: Color
backgroundcolor = white

-- | De voorgrondkleur.
foregroundcolor :: Color
foregroundcolor = white

-- | De box waarin het tetrisveld zich bevindt.
box :: (Int, Int) -> Picture
box (width, height) =  Color foregroundcolor (rectangleSolid scaleW scaleH)
                       where scaleW = fromIntegral (scale' * width)
                             scaleH = fromIntegral (scale' * height)

-- | Zet een coord om naar een afbeelding op de juiste positie.
transToCoord :: Coord -> Coord -> Picture -> Picture
transToCoord (width,height) (x,y) =
  translate dx dy
  where dx = fromIntegral ((x - (width `div` 2))* scale' + ds)
        dy = fromIntegral (((height `div` 2)- y)* scale' + ds)
        ds = scale' `div` 2

tileToPict :: [([Direction], Picture)] -> [(String, Picture)] -> Coord -> String -> Tile -> Picture
tileToPict f b dms s (Tile co' dirs) = pictures $ map (transToCoord dms co') [pictback, pictfront]
  where pictfront = fromJust $ lookup dirs f
        pictback  = fromJust $ lookup s b

 -- translate dx dy
 --  where dx = fromIntegral ((x - (width `div` 2))* scale' + ds)
 --        dy = fromIntegral (((height `div` 2)- y)* scale' + ds)
 --        ds = scale' `div` 2


-- | Turns a link into the appropriate picture.
linkToPict :: [(String, Picture)] -> World -> Int -> Link -> Picture
linkToPict ps w n ss = transToCoord (dms w) (addCo (pos ss) (dir ss)) $ rotate angle pict
  where pict = fromJust $ lookup s1 ps
        angle | dir ss == east  = 180
              | dir ss == south = 270
              | dir ss == west  = 0
              | otherwise       = 90
        s1 | n == 0    = "pump"
           | otherwise = "balloon" ++ s2
        s2 | status w == Completed || checkLink w (pos ss) = "full"
           | otherwise = "empty"

-- | Zet een wereld om naar een afbeelding.
worldToPicture :: [(String, Picture)] -> [([Direction],Picture)] -> World -> Picture
worldToPicture op tp w@(World dms so si ft lt hole _) =
  pictures ( map (tileToPict tp op dms "fixed") ft ++
             map (tileToPict tp op dms "loose") lt ++
             map (linkToPict op w 0) so ++
             map (linkToPict op w 1) si ++
             [transToCoord dms hole (fromJust $ lookup "empty" op)] )
