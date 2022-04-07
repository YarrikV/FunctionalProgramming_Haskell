module Main where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap   (loadBMP)
import System.Environment (getArgs)
import Control.Monad      (when)
import Data.List
import Parser
import Datatypes
import Display
import Constants
import Functions


--      Process

main :: IO ()
main = do
  args <- getArgs
  if length args > 1
    then do
      let (opt:worldFile:_) = args
      when (opt /= "--test") (putStrLn $ "Unidentified option: "++opt++", didn't you mean: --test? \nRunning --test instead:")
      worldString <- readFile (args !! 1)
      let world = fst . head $ apply parseWorld worldString
          updatedWorld = checkStatus . setup $ world
      if status updatedWorld == Completed
        then putStrLn "SOLVED"
        else putStrLn "UNSOLVED"
    else
      if length args == 1
        then do
          worldString <- readFile $ head args
          let world = fst . head $ apply parseWorld worldString
              updatedWorld = checkStatus . setup $ world
          tilePs <- mapM loadBMP tileFilePaths
          otherPs <- mapM loadBMP otherFilePaths
          let tileDP = zip tileDirs tilePs
              otherSP = zip otherTypes otherPs
              render = worldToPicture otherSP tileDP
          play window backgroundcolor fps updatedWorld
               render processInput processTime
        else
          putStrLn "Not enough arguments given with \'cogs\', expected 1 (boardfile) or more (--test option and directory of boardfiles)."

-- | Hulpfunctie voor de proccesInput.
isKey k1 (EventKey k2 Down  _ _ )  = k1 == k2
isKey _  _  = False
-- | Functie die de input verwerkt.
processInput :: Event -> World -> World
processInput _ w@(World _ _ _ _ _ _ Completed) = w
processInput key@(EventKey _ _ _ co) w@(World d _ _ _ _ coHole InProgress)
    | isKey (MouseButton LeftButton) key && neighbour poskey coHole = checkStatus $ move poskey w
    | isKey (Char 'q') key || isKey (SpecialKey KeyLeft) key  = checkStatus $ move (addCo coHole ( 1, 0)) w
    | isKey (Char 'd') key || isKey (SpecialKey KeyRight) key = checkStatus $ move (addCo coHole (-1, 0)) w
    | isKey (Char 'z') key || isKey (SpecialKey KeyUp) key    = checkStatus $ move (addCo coHole ( 0, 1)) w
    | isKey (Char 's') key || isKey (SpecialKey KeyDown) key  = checkStatus $ move (addCo coHole ( 0,-1)) w
    | otherwise = w
      where poskey = position d co
processInput _ w = w


-- | Nothing should happen with the world if time passes.
processTime :: Float -> World -> World
processTime _ w = w
