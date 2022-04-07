module Parser where

import Data.Char
import Data.Maybe           (fromJust)
import Data.List            ((\\), sortBy)
import Data.Function        (on)
import Control.Monad
import Control.Applicative
import Datatypes
import Debug.Trace
import Functions             (pack)

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap f m = Parser $ \s -> [(f x,t) | (x,t) <- apply m s]

instance Applicative Parser where
  pure x  = Parser $ \s -> [(x,s)]
  (<*>) = ap

instance Monad Parser where
  return x = Parser $ \s -> [(x,s)]
  m >>= k = Parser $ \s -> [ (y,u) | (x,t) <- apply m s, (y,u) <- apply (k x) t]

instance Alternative Parser where
  empty     = Parser $ const []
  m <|> n   = Parser $ \s -> let l = apply m s
                             in  if null l
                                 then apply n s
                                 else l

-- | Match 0 or more occurrences.
star :: Parser a -> Parser [a]
star p = plus p <|> return []

-- | Match 1 or more occurrences.
plus :: Parser a -> Parser [a]
plus p = do x <- p
            xs <- star p
            return (x:xs)

apply :: Parser a -> String -> [(a,String)]
apply (Parser m) = m

parse :: Parser a -> String -> a
parse m s = one [ x | (x,t) <- apply m s, t==""]
  where
    one []        = error "no parse"
    one [x]       = x
    one xs | length xs > 1 = error "ambiguous parse"

parsechar' :: Parser Char
parsechar' = Parser f
  where
    f [] = []
    f (c:s) = [(c,s)]

parseSpot :: (Char -> Bool) -> Parser Char
parseSpot p = do  c <- parsechar'
                  guard $ p c
                  return c

parseChar :: Char -> Parser Char
parseChar c = parseSpot (== c)

parseCharNL :: Char -> Parser String
parseCharNL c = do
  parsews
  parseChar c
  parsews

parseDigit :: Parser Char
parseDigit = do c <- parsechar'
                guard $ isDigit c
                return c

parseString :: String -> Parser String
parseString s = do
  parseHorws
  mapM_ parseChar s
  parseHorws

parsews :: Parser String
parsews = star $ parseChar '\t' <|> parseChar ' ' <|> parseChar '\n' <|> parseChar '\r'

parseHorws :: Parser String
parseHorws = star $ parseChar '\t' <|> parseChar ' '

parseNumber :: Parser String
parseNumber = plus parseDigit

parseWidth :: Parser Int
parseWidth = do
  parseString "width:"
  w <- parseNumber
  parseHorws
  parseChar '\n' <|> parseChar '\r'
  return $ read w

parseHeight :: Parser Int
parseHeight = do
  parseString "height:"
  h <- parseNumber
  parseHorws
  parseChar '\n' <|> parseChar '\r'
  return $ read h

--There has to be at least one pump and at least one balloon.
parseLinks ::  (Coord -> Direction -> Link) -> Parser [Link]
parseLinks d = do
  parseCharNL '{'
  cs <- star parseCoord
  parseCharNL '}'
  let links = map (`d` (0,0)) cs -- ^ dit werkt niet met undefined, ik krijg een speciale error. nl. Err.hs:79;14 Exception: prelude.undefined
  -- ^dit is getest geweest wanneer alle tests alle
  return links

parseCoord :: Parser Coord
parseCoord = do
  parseChar '('
  parseHorws
  y <- parseNumber
  parseChar ','
  parseHorws
  x <- parseNumber
  parseChar ')'
  star $ parseCharNL ','
  return (read x,read y)

parsePos :: Parser Coord
parsePos = do
  parseString "pos:"
  parseCoord


listOfDirs = [("east", east), ("west",west), ("north", north), ("south",south)]
parseDir :: Parser [Direction]
parseDir = do
  s <- plus $ parseSpot isAlpha
  parsews
  let d | s == "default" = [north, south, east, west]
        | otherwise = map fromJust [lookup s listOfDirs]
  return d

parseStatus :: Parser Int
parseStatus = do
  s <- plus $ parseSpot isAlpha
  let n | s ==  "open"  = 1
        | s == "closed" = 0
        | otherwise     = undefined --TODO ERROR
  return n

parseGate :: Parser (Int, [Direction])
parseGate = do
  d <- parseDir
  parseString ":"
  s <- parseStatus
  parsews
  star $ parseCharNL ','
  return (s,d)

parseGates :: Parser [Direction]
parseGates = do
  parsews
  dirs <- star parseGate
  let in_ = concatMap snd $ filter (\(s,_) -> s == 1) dirs
      out = concatMap snd $ filter (\(s,_) -> s == 0) dirs
  return (in_ \\ out)

parseFixed :: Parser Int
parseFixed = do
  parseString "fixed:"
  a <- plus $ parseSpot isAlpha
  let fxd | a == "yes" = 0
          | a == "no"  = 1
          | otherwise  = undefined --TODO ERROR
  star $ parseCharNL ','
  return fxd

-- | Parsed een tile, de Int zal het verschil zijn tussen een fixed en een non-fixed tile.
parseTile :: Parser (Int,Tile)
parseTile = do
  parseCharNL '{'
  co <- parsePos
  fxd <- parseFixed
  dirs <- parseGates
  parseCharNL '}'
  star $ parseCharNL ','
  let sorted []  = []
      sorted [a] = [a]
      sorted (a:b:c) | a == north || b == west = a : sorted (b:c)
                     | b == north || a == west = b : sorted (a:c)
                     | a == east = a : sorted (b:c)
                     | otherwise = b : sorted (a:c)
  return (fxd, Tile co (sorted dirs))

parseTiles :: Parser ([Tile],[Tile])
parseTiles = do
  parseString "tiles:"
  parseCharNL '{'
  tiles <- star parseTile
  parseCharNL '}'
  let sorted :: [(Int, Tile)] -> [Tile] -> [Tile] -> ([Tile], [Tile])
      sorted [] a b = (a,b)
      sorted ((n,t):nts) a b | n == 0 = sorted nts (t:a) b
                             | n == 1 = sorted nts a (t:b)
  return $ sorted tiles [] []

parseWorld :: Parser World
parseWorld = do
  w <- parseWidth
  h <- parseHeight
  parseString "sources:"
  sources <- parseLinks Source
  parsews
  parseString "sinks:"
  sinks <- parseLinks Sink
  parsews
  tiles <- parseTiles
  let ft = fst tiles
      lt = snd tiles
      rightAmountOfTiles = length (ft++lt) == w * h - 1
      noWrongLinks = all (not . threeMany) (pack $filter corners sortedLinks) && all (not . twoMany) (pack $ filter sides sortedLinks)
      sides (x,y) = (x `elem` [0,w] && y `elem` [1..h-1]) || (x `elem` [1..w-1] && y `elem` [0,h])
      corners (x,y) = x `elem` [0,w] && y `elem` [0,h]
      twoMany l = length l > 1
      threeMany l = length l > 2
      sortedLinks = sortBy (compare `on` fst) $ sortBy (compare `on` snd) (map pos (sources++sinks))
  if rightAmountOfTiles
  then
    if noWrongLinks
    then
      return $ World (w,h) sources sinks ft lt (0,0) InProgress
    else
      undefined --TODO ERROR: Inapropriate Links.
  else
    undefined -- TODO ERROR: Wrong number of Tiles.
