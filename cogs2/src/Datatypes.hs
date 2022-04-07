module Datatypes where

data World = World { dms :: (Int, Int)
                   , sources :: [Link]
                   , sinks :: [Link]
                   , fixedT :: [Tile]
                   , looseT :: [Tile]
                   , hole :: Coord
                   , status :: Status
                   } deriving (Show)

data Link = Source  { pos :: Coord
                    , dir :: Direction}
          | Sink    { pos :: Coord
                    , dir :: Direction}
                    deriving (Show, Eq)

data Status = InProgress | Completed deriving (Show, Eq)
data Tile = Tile  { coo :: Coord
                  , dirs :: [Direction]
                  } deriving (Show)
type Coord = (Int, Int)
type Direction = Coord
-- | All directions:
north :: Direction
north = ( 0,-1)
east :: Direction
east  = ( 1, 0)
south :: Direction
south = ( 0, 1)
west :: Direction
west  = (-1, 0)
