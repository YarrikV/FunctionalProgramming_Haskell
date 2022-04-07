module Constants where

import Datatypes

-- | Scale of the pictures.
scale' :: Int
scale' = 50

-- | Refresh rate (per second).
fps :: Int
fps = 60

-- | FilePaths & Lists to do your lookup in.
tileDirs = [[east,west], [north,south], [south,west], [north, west], [north, east], [east, south], [east, south, west], [north, south, west], [north, east, west], [north, east, south], [north, east, south, west],[]]
tileTypes = ["I1","I2","L1","L2","L3","L4","T1","T2","T3","T4","X","O"]
otherTypes = ["fixed","loose","empty","pump","balloonempty","balloonfull"]
tileFilePaths = map ((++ ".bmp") . ("./bmp/" ++)) tileTypes
otherFilePaths = map ((++ ".bmp") . ("./bmp/" ++)) otherTypes
