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
nokia   = makeColor (200/255) (200/255) (20/255) 1

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

--
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
verleng sn richting = (fst (head sn) + fst richting, snd (head sn) + snd richting):sn

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
coordNaarFiguur (x,y) = translate (fromIntegral (((x - ((breedte+1) `div` 2))*schaal))) (fromIntegral ((((hoogte+1) `div` 2) - y)*schaal)) vakje

-- rescale n m s = (fromIntegral $ (n - m `div` 2) * (s * schaal) - s * (schaal `div` 2)
-- dx = rescale x breedte +1 ofzo


-- coordsNaarFiguur zet een lijst van coordinaten om
-- naar een lijst van figuren ("Pictures")
coordsNaarFiguur :: [Coordinaat] -> [Picture]
coordsNaarFiguur = map coordNaarFiguur

-- Nokia snake achtergrond
-- :t box => Picture
box =  Color nokia (rectangleSolid schaalb schaalh)
       where schaalb = fromIntegral (schaal * breedte)
             schaalh = fromIntegral (schaal * hoogte)

-- Zet een wereld om naar een figuur
maakFiguren :: Wereld -> Picture
maakFiguren (Wereld sn _ _ _ (co, _)) = Pictures (box:(coordsNaarFiguur (co:sn)))

-- Botsing zet een wereld om naar een nieuwe wereld
-- maar kijkt na of de slang niet met zichzelf botst
-- en binnen het speelveld blijft.
botsing :: Wereld -> Wereld
botsing wrld@(Wereld sn coord float status appel) | botsingP (tail sn) (head sn) || not (inBox (head sn)) = Wereld sn coord float Dood appel
                                                  | otherwise = wrld


-- Genereer de volgende appel
volgendeAppel :: Appel -> Appel
volgendeAppel (coord, gen) = let rndm1 = randomR (1, breedte) gen
                                 rndm2 = randomR (1, hoogte) (snd rndm1)
                              in ((fst rndm1 , fst rndm2), snd rndm2)
                              -- next stdgen -> (waarde, stdgen)

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
stap _ wrld@(Wereld _ _ _ Dood _) = wrld
stap delta (Wereld sn richt tijd stat apl) | new_t < 1  = botsing (Wereld (beweeg sn richt) richt new_t stat (botsAppel sn apl))
                                           | otherwise  =  botsing (Wereld (verleng sn richt) richt 0 stat apl)
                                                    where new_t = delta + tijd



-- Hulpfunctie die nagaat of een Event
-- een bepaalde key down event is
isKey k1 (EventKey (SpecialKey k2)    Down  _ _ )  = k1 == k2
isKey _  _  = False

-- Vervolledig verwerk invoer zodat:
-- 1) de slang beweegt door op de pijltjes toetsen in te drukken
-- 2) dat het spel herbegint indien de speler dood is en op enter duwt.
verwerkInvoer :: Event -> Wereld -> Wereld
verwerkInvoer toets wrld@(Wereld _ _ _ Dood _)  | isKey KeyEnter toets = startWereld
                                                | otherwise = wrld
verwerkInvoer toets wrld@(Wereld sn richt x1 stat x2)   | (richt /= onder) && isKey KeyUp toets  = Wereld sn boven x1 stat x2
                                                        | (richt /= boven) && isKey KeyDown toets = Wereld sn onder x1 stat x2
                                                        | (richt /= rechts) && isKey KeyLeft toets = Wereld sn links x1 stat x2
                                                        | (richt /= links) && isKey KeyRight toets = Wereld sn rechts x1 stat x2
                                                        | otherwise = wrld

--
-- Start van het spel
main :: IO ()
main =  play venster      (makeColorI 0 0 51 0)
             60          startWereld
             maakFiguren  verwerkInvoer
             stap
