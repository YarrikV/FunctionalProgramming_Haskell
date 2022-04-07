module ParserT where

import Data.Char
import Data.Maybe           (fromJust)
import Data.List            ((\\))
import Parser               (Parser(..), apply, star, plus)
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class ()
import Datatypes
import Debug.Trace

newtype EitherP a = EitherP { runEitherP :: Parser (Either [ParseErr] a) }

data ParseErr = CharERR Char Char
              | StringERR String
              | NumberERR
              | VSpaceERR
              | TooManyLinks
              | InvalidAmountofTiles
              | EndOfFile

instance Show ParseErr where
  show (CharERR c d) = "Expected character: "++show c++". But got "++show d++" instead."
  show (StringERR s) = "Can't find: "++show s++"."
  show NumberERR  = "Can't find a number."
  show VSpaceERR  = "Expected \'\\n\'"
  show TooManyLinksERR = "Invalid world: Too many Sinks/Sources at the same spot."
  show InvalidAmountofTiles = "Invalid amount of Tiles (There can only be 1 hole)."
  show EndOfFile = "Unexpected End of file."

instance Functor EitherP where
  fmap = liftM

instance Applicative EitherP where
  pure  = return
  (<*>)   = ap

instance Monad EitherP where
  return  = pure
  x >>= f = EitherP $ runEitherP x
                           >>=
                           either (return . Left)
                                  (runEitherP . f)

instance Alternative EitherP where
empty = Left []
m <|> n = EitherP $ Parser $ \s -> let [(ea,t)] = apply m s
                                   in case ea of
                                    Left e1 -> let [(eb, u)] = apply n s
                                       in case eb of
                                         Left e2 -> return (Left (e1++e2),s)
                                         b -> return (Right b,u)
                                    a -> return (Right a,t)

etoken :: EitherP Char
etoken = EitherP $ Parser f
  where
    f [] =  [(Left [EndOfFile],"")]
    f (c:s) = [(Right c, s)]

ethrow :: ParseErr -> EitherP a
ethrow e = EitherP $ Parser $ \s -> [(Left [e], s)]

eaddErr :: ParseErr -> [ParseErr] -> EitherP a
eaddErr e es = EitherP $ Parser $ \s -> [(Left (p:ps),s)]

eFindErr :: EitherP a -> ([ParseErr] -> EitherP a) -> EitherP a
eFindErr ep f = EitherP $ \s -> do (a,s) <- runEitherP ep s
                                   case a of
                                     Right a -> return (a,s)
                                     Left e -> runEitherP (f e) s

espot :: (Char -> Bool) -> (Char -> ParseErr)-> EitherP Char
espot p e = do
  ea <- etoken
  if p ea then
    return ea
  else
    ethrow $ e ea

eparseChar :: Char -> EitherP Char
eparseChar c = espot (== c) (CharERR c)

eParseString :: String -> EitherP String
eParseString s = do
  y <- mapM parseChar s
  eFindErr y (eaddErr (StringERR s))

-- | Match 0 or more occurrences.
estar :: EitherP a -> EitherP [a]
estar = plus (runEitherP p) ParserT.<|> runEitherP []

-- | Match 1 or more occurrences.
eplus :: EitherP a -> EitherP [a]
eplus p = do x <- p
             xs <- estar p
             return (x:xs)

eparsews :: Parser String
eparsews = estar $ eparseChar '\t' <|> eparseChar ' ' <|> eparseChar '\n' <|> eparseChar '\r'

eparseHorws :: Parser String
eparseHorws = estar $ eparseChar '\t' <|> eparseC har ' '

eparseDigit :: EitherP Char
eparseDigit = espot isDigit

eparseNumber' :: EitherP String
eparseNumber' = do
  n <- eplus eparseDigit
  return $ read n
eparseNumber = eFindErr eparseNumber' (eaddErr NumberERR)

eparseWidth :: EitherP Int
eparseWidth = do
  eparseString "width:"
  w <- eparseNumber
  eparseHorws
  eparseChar '\n' EitherP.<|> eparseChar '\r'
  return w

eparseHeight :: EitherP Int
eparseHeight = do
  eparseString "height:"
  h <- eparseNumber
  parseHorws
  parseChar '\n' <|> parseChar '\r'
  return h

--There has to be at least one pump and at least one balloon.
eparseLinks ::  (Coord -> Direction -> Link) -> Parser [Link]
eparseLinks d = do
  eparseCharNL '{'
  cs <- many eparseCoord
  eparseCharNL '}'
  let links = map (`d` (0,0)) cs
  return links

eparseCoord :: Parser Coord
eparseCoord = do
  eparseChar '('
  eparseHorws
  y <- eparseNumber
  eparseChar ','
  eparseHorws
  x <- eparseNumber
  eparseChar ')'
  many $ eparseCharNL ','
  return (x, y)

eparsePos :: Parser Coord
eparsePos = do
  eparseString "pos:"
  eparseCoord


listOfDirs = [("east", east), ("west",west), ("north", north), ("south",south)]
eparseDir :: Parser [Direction]
eparseDir = do
  s <- many $ eparseSpot isAlpha
  eparsews
  let d | s == "default" = [north, south, east, west]
        | otherwise = map fromJust [lookup s listOfDirs]
  return d

eparseStatus :: Parser Int
eparseStatus = do
  s <- many $ eparseSpot isAlpha
  let n | s ==  "open"  = 1
        | s == "closed" = 0
        | otherwise     = undefined --TODO ERROR
  return n

eparseGate :: Parser (Int, [Direction])
eparseGate = do
  d <- eparseDir
  eparseString ":"
  s <- eparseStatus
  eparsews
  many $ eparseCharNL ','
  return (s,d)

eparseGates :: Parser [Direction]
eparseGates = do
  eparsews
  dirs <- many eparseGate
  let in_ = concatMap snd $ filter (\(s,_) -> s == 1) dirs
      out = concatMap snd $ filter (\(s,_) -> s == 0) dirs
  return (in_ \\ out)

eparseFixed :: Parser Int
eparseFixed = do
  eparseString "fixed:"
  a <- many $ eparseSpot isAlpha
  let fxd | a == "yes" = 0
          | a == "no"  = 1
          | otherwise  = undefined --TODO ERROR
  many $ eparseCharNL ','
  return fxd

-- | Parsed een tile, de Int zal het verschil zijn tussen een fixed en een non-fixed tile.
eparseTile :: Parser (Int,Tile)
eparseTile = do
  eparseCharNL '{'
  co <- eparsePos
  fxd <- eparseFixed
  dirs <- eparseGates
  eparseCharNL '}'
  many $ eparseCharNL ','
  return (fxd, Tile co dirs)

eparseTiles :: Parser ([Tile],[Tile])
eparseTiles = do
  eparseString "tiles:"
  eparseCharNL '{'
  tiles <- many eparseTile
  eparseCharNL '}'
  let sorted :: [(Int, Tile)] -> [Tile] -> [Tile] -> ([Tile], [Tile])
      sorted [] a b = (a,b)
      sorted ((n,t):nts) a b | n == 0 = sorted nts (t:a) b
                             | n == 1 = sorted nts a (t:b)
  return $ sorted tiles [] []

eparseWorld :: Parser World
eparseWorld = do
  w <- eparseWidth
  h <- eparseHeight
  eparseString "sources:"
  sources <- eparseLinks Source
  eparsews
  eparseString "sinks:"
  sinks <- eparseLinks Sink
  eparsews
  tiles <- eparseTiles
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
      ethrow TooManyLinks
  else
    ethrow InvalidAmountofTiles
