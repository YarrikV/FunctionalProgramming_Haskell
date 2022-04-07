{-
  _________              __
 /   _____/ ____ _____  |  | __ ____
 \_____  \ /    \\__  \ |  |/ // __ \
 /        \   |  \/ __ \|    <\  ___/
/_______  /___|  (____  /__|_ \\___  >
        \/     \/     \/     \/    \/

Opdracht 1: Functioneel Programmeren 2018-2019

-- Hint omzetten getallen:
-- https://wiki.haskell.org/Converting_numbers

-}

-- In deze opdacht maken we gebruik van de grafische
-- bibliotheek gloss. Om deze kunnen te gebruiken
-- moeten we deze eerst importeren (keyword import)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

-- Om de appel te kunnen plaatsten moeten
-- we ook gebruik maken van een random nummer
-- generator
import System.Random


-- We beginnen met het definiëren van enkele
-- type synoniemen. Dit wilt zeggen dat we
-- een bepaald type een nieuwe naam geven
-- In dit geval geven we het type (Int,Int)
-- de naam Coordinaat en Richting.
type Coordinaat = (Int,Int)
type Richting   = (Int,Int)


-- Daarna definiëren we de data om de slang voor te stellen
-- In deze opdracht moet je zelf nog geen data definities maken
-- maar je moet deze wel reeds kunnen lezen.
--
-- De slang zelf stellen we voor als een lijst van coordinaten
type Slang   =  [Coordinaat]

-- We moeten bijhouden of de slang levend of dood is
-- Hiervoor maken we het Status datatype
data Status     = Levend | Dood
                  deriving (Show)

-- Een appel is een coordinaat samen
-- met een random generator om de volgende
-- positie te genereren
type Appel = (Coordinaat,StdGen)

-- Tenslotte maken we een datatype Wereld dat
-- alle informatie bevat van het snake spel
-- 1) De slang
-- 2) De richting waarin de slang beweegt
-- 3) De tijd sinds de laatste update
-- 4) Is de slang levend of dood
-- 5) Plaats van de appel
-- Merk op dat hier twee definities voor "Wereld" gegeven worden
-- De definitie links geeft aan dat "Wereld" een nieuw type is
-- De definitie rechts geeft aan dat Wereld een constructor is
-- voor het type Wereld.
data Wereld     = Wereld Slang Richting Float Status Appel
                  deriving (Show)

{- Hieronder definiëren we een aantal constanten  -}

-- We stellen een richting voor door aan te duiden
-- hoeveel vakjes er in de x en y richting moet
-- bewogen worden (dx,dy).
links :: Richting
links = (-1,0)

rechts :: Richting
rechts = (1,0)

boven :: Richting
boven = (0,-1)

onder :: Richting
onder = (0,1)

-- Constanten voor de grootte van het bord
breedte :: Int
breedte = 64
hoogte  :: Int
hoogte  = 64
-- Geeft de schaal aan waarop elk vakje op het
-- scherm getekend zal worden 10x10
schaal  :: Int
schaal  = 10

-- Achtergrondkleur: R         G         B        A
nokia   = makeColor (20/255) (121/255) (100/255) 1

-- Begin positie van de slang
startSlang :: Slang
startSlang = [(breedte `div` 2, hoogte `div` 2)]

seed :: Int
seed = 42

-- Begin wereld
startWereld :: Wereld
startWereld = Wereld startSlang
                     rechts
                     0
                     Levend
                     ((breedte `div` 3,hoogte `div` 3),mkStdGen seed)

--f
-- Het gloss venster om het spel te tekenen
venster :: Display
venster = InWindow "UGent Functioneel Programmeren Opdracht 1"
                   (breedte * schaal, schaal * hoogte)
                   (0,0)

-- inBox is een functie die een coordinaat neemt
-- en een boolean teruggeeft die aangeeft of deze coordinaat
-- binnen het bord valt.
inBox :: Coordinaat -> Bool
inBox (x,y) = x > 0 && y > 0 && x <= breedte && y <= hoogte

-- Verleng een lijst gegeven de
-- richting waarin de slang moet bewegen
verleng :: Slang -> Richting -> Slang
verleng sn richting = ( fst (last sn) + fst richting, snd (last sn) + snd richting):sn

-- Beweeg de slang voorgesteld als een lijst van coordinaten
-- in de gegeven richting
beweeg :: Slang -> Richting -> Slang
beweeg sn richt = init (verleng sn richt)

-- Gegeven een lijst van coordinaten die de slang voorstelt
-- en een andere coordinaat geeft aan of deze coordinaant "botst"
-- met de slang
-- (* Moeilijkheid 1 *)
botsingP :: [Coordinaat] -> Coordinaat -> Bool
botsingP sn coord = elem coord sn

-- Een Picture die een enkel vakje op het scherm
-- voorstelt.
vakje :: Picture
vakje  = rectangleSolid (fromIntegral schaal) (fromIntegral schaal)

-- Elk vakje op het bord stellen we voor door een coordinaat
coordNaarFiguur :: Coordinaat -> Picture
coordNaarFiguur (x,y) = translate (fromIntegral x) (fromIntegral (-y)) (norm vakje)
                where norm vakje = translate 5 (-5) vakje


-- coordsNaarFiguur zet een lijst van coordinaten om
-- naar een lijst van figuren ("Pictures")
coordsNaarFiguur :: [Coordinaat] -> [Picture]
coordsNaarFiguur coords = map coordNaarFiguur coords

-- Nokia snake achtergrond
box =  Color nokia (rectangleSolid schaalb schaalh)
       where schaalb = fromIntegral (schaal * breedte)
             schaalh = fromIntegral (schaal * hoogte)

-- Zet een wereld om naar een figuur
maakFiguren :: Wereld -> Picture
maakFiguren (Wereld sn _ _ _ (coord, _)) = Pictures ((coordsNaarFiguur sn)++(coordsNaarFiguur [coord]))

-- Botsing zet een wereld om naar een nieuwe wereld
-- maar kijkt na of de slang niet met zichzelf botst
-- en binnen het speelveld blijft.
botsing :: Wereld -> Wereld
botsing wrld@(Wereld sn coord float status appel) | inBox hoofd && (if (length sn == 1) then True else not (hoofd `elem` (tail sn))) = wrld
                                                  | otherwise = Wereld sn coord float Dood appel
                                                              where hoofd = head sn

-- Genereer de volgende appel
volgendeAppel :: Appel -> Appel
volgendeAppel (coord, gen) = let rndm1 = randomR (1, breedte) gen
                                 rndm2 = randomR (1, hoogte) (snd rndm1)
                              in ((fst rndm1 , fst rndm2), snd rndm2)

--
-- Kijk na of de slang en de appel botsen met elkaar
-- als ze botsen genereer de volgende appel
botsAppel :: Slang -> Appel -> Appel
botsAppel sn appel@(coord, random) | botsingP sn coord = volgendeAppel appel
                                   | otherwise = (coord, random)

--
-- Neem 1 stap
-- Beweeg de slang bij elke oproep
-- en verleng de slang na 1 seconde.
stap ::  Float -> Wereld -> Wereld
stap delta (Wereld sn richting tijd x1 x2) | nieuwe_tijd < 1 = Wereld sn richting nieuwe_tijd x1 x2
                                           | otherwise = Wereld (beweeg sn richting) richting (nieuwe_tijd - 1)  x1 x2
                                            where nieuwe_tijd = delta + tijd
--
-- Hulpfunctie die nagaat of een Event
-- een bepaalde key down event is
isKey k1 (EventKey (SpecialKey k2)    Down  _ _ )  = k1 == k2
isKey _  _  = False

-- Vervolledig verwerk invoer zodat:
-- 1) de slang beweegt door op de pijltjes toetsen in te drukken
-- 2) dat het spel herbegint indien de speler dood is en op enter duwt.
verwerkInvoer :: Event -> Wereld -> Wereld
verwerkInvoor toets wrld@(Wereld sn richt x1 Dood x2)   | isKey KeyEnter toets = startWereld
                                                        | otherwise = wrld
verwerkInvoer toets wrld@(Wereld sn richt x1 status x2) | isKey KeyUp toets  = Wereld sn boven x1 status x2
                                                        | isKey KeyDown toets = Wereld sn onder x1 status x2
                                                        | isKey KeyLeft toets = Wereld sn links x1 status x2
                                                        | isKey KeyRight toets = Wereld sn rechts x1 status x2
										                | otherwise = wrld

--
-- Start van het spel
main :: IO ()
main =  play venster      white
             60           startWereld
             maakFiguren  verwerkInvoer
             stap
