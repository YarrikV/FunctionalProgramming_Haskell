{- 

  _________              __           
 /   _____/ ____ _____  |  | __ ____  
 \_____  \ /    \\__  \ |  |/ // __ \ 
 /        \   |  \/ __ \|    <\  ___/ 
/_______  /___|  (____  /__|_ \\___  >
        \/     \/     \/     \/    \/ 

Opdracht 1: Functioneel Programmeren 2018-2019

--
-- Hint omzetten getallen: 
-- https://wiki.haskell.org/Converting_numbers
--

-}

--
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
-- 

type Slang   =  [Coordinaat]

-- We moeten bijhouden of de slang levend of dood is 
-- Hiervoor maken we het Status datatype
data Status     = Levend | Dood
                  deriving (Show)

--
-- Een appel is een coordinaat samen 
-- met een random generator om de volgende
-- positie te genereren
--
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
--
data Wereld     = Wereld Slang Richting Float Status Appel 
                  deriving (Show)
				  

  {- Hieronder definiëren we een aantal constanten  -}

-- We stellen een richting voor door aan te duiden 
-- hoeveel vakjes er in de x en y richting moet 
-- bewogen worden (dx,dy).   
-- 
-- Hint: het assenstelsel van de gridwereld kan je
-- terugvinden in de opgave. 
--
-- (* Moeilijkheid 1 *)

links :: Richting
links = (-1,0)

rechts :: Richting 
rechts = (1,0)

boven :: Richting
boven = (1,0)

onder :: Richting
onder = (-1,0)


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
nokia   = makeColor (119/255) (121/255) (100/255) 1  

-- Begin positie van de slang
startSlang :: Slang 
startSlang = [(breedte `div` 2,hoogte `div` 2)]

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
--
venster :: Display
venster = InWindow "UGent Functioneel Programmeren Opdracht 1" 
                   (breedte * schaal, schaal * hoogte) 
                   (0,0)

-- inBox is een functie die een coordinaat neemt 
-- en een boolean teruggeeft die aangeeft of deze coordinaat 
-- binnen het bord valt 
-- (* Moeilijkheid 1 *)
inBox :: Coordinaat -> Bool
inBox (x,y) = (abs x <= breedte `div` 2 ) && (abs y <= hoogte `div` 2)

-- Verleng een lijst gegeven de 
-- richting waarin de slang moet bewegen  
-- (* Moeilijkheid 1 *)
verleng :: Slang -> Richting -> Slang 
verleng = Slang ++ [( fst end + fst richting, snd end + snd richting)]
					where end = last Snake