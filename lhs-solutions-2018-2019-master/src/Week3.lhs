
Oplossing Reeks 3 (Sudoku)
============================

> module Week3 where

Basis
-----

De eerste regels nemen we gewoon over uit de *boilerplate* code op Dodona. We definiëren een type `Board` om onze latere types wat leesbaarder te maken, en stellen een standaard grote van het veld in. Verder geven we een nuttige naam aan `0`, want `0` zal een lege cel voorstellen. Ik voeg nog een typedefinitie extra toe voor een rij, een kolom en een vierkantje.

> type Row    = [Int]  -- ^ Een rij in een sudoku.
> type Col    = [Int]  -- ^ Een kolom in een sudoku.
> type Square = [Int]  -- ^ Een vierkantje in een sudoku.
> type Board  = [Row]  -- ^ Een sudoku-bord.
> type X      = Int    -- ^ De x van een coördinaat.
> type Y      = Int    -- ^ De y van een coördinaat.

> empty, width, height :: Int
> empty       = 0  -- ^ Een leeg vakje.
> width       = 9  -- ^ De breedte van een sudoku.
> height      = 9  -- ^ De hoogte van een sudoku.

Vervolgens definiëren we twee eenvoudige functies die ons respectievelijk een lege rij en een leeg bord teruggeven.

> -- | Een lege rij.
> emptyRow :: Row
> emptyRow = replicate width empty

> -- | Een leeg bord.
> emptyBoard :: Board
> emptyBoard = replicate height emptyRow

Het volgende deel van de oplossing vraagt om enkele hulpfuncties te definiëren.

> -- | Neem een rij uit een sudoku.
> getRow :: Board -> Y -> Row
> getRow = (!!)

> -- | Neem een kolom uit een sudoku.
> getCol :: Board -> X -> Col
> getCol b x = map (!! x) b

> -- | Controleert of een gegeven getal voorkomt in een gegeven rij.
> contains :: Int -> [Int] -> Bool
> contains = elem

> -- | Bereken een deellijst.
> getSubList :: Int  -- ^ Start-index (inclusief).
>            -> Int  -- ^ Eind-index (inclusief).
>            -> [a]  -- ^ Volledige lijst.
>            -> [a]  -- ^ Deellijst.
> getSubList s e l = take (e - s + 1) $ drop s l

Merk op hoe ik hier zoveel mogelijk gebruik maak van de extra typedefinities. Deze kunnen de leesbaarheid erg verhogen.

Werken met vierkantjes
----------------------

De opgave vraagt ons om nu drie functies te definiëren die ons helpen controleren of een 3x3 vierkantje een gegeven nummer bevat. Hiervoor zullen we eerst de grenzen van ons vierkantje bepalen, gegeven een coördinaat in het 9x9 rooster van de sudoku.

Hieronder nog eens een complete sudoku met indices:

```
    0  1  2 | 3  4  5 | 6  7  8    
0 [[0, 0, 0,| 0, 0, 0,| 0, 0, 0],
1  [0, 0, 0,| 0, 0, 0,| 0, 0, 0],
2  [0, 0, 0,| 0, 0, 0,| 0, 0, 0],
    --------+---------+--------
3  [0, 0, 0,| 0, 0, 0,| 0, 0, 0],
4  [0, 0, 0,| 0, 0, 0,| 0, 0, 0],
5  [0, 0, 0,| 0, 0, 0,| 0, 0, 0],
    --------+---------+--------
6  [0, 0, 0,| 0, 0, 0,| 0, 0, 0],
7  [0, 0, 0,| 0, 0, 0,| 0, 0, 0],
8  [0, 0, 0,| 0, 0, 0,| 0, 0, 0]]
```

Nu kunnen we eenvoudig de `index` methode schrijven, die ons de grenzen van een vakje op 1 enkele as zal teruggeven.

> -- | De grenzen van het vierkantje waar gegeven getal in ligt.
> index :: Int -> (Int, Int)
> index i = let s = 3 * (i `div` 3) in (s, s + 2)

Om een deelvak op te vragen kunnen we nu index gebruiken, tesamen met `getSubList`, zoals gehint.

> -- | De lijst van getallen die in het vierkantje van gegeven coördinaat liggen.
> getSubSquare :: Board -> Int -> Int -> Square
> getSubSquare b x y = concatMap (getSubList xs xe) (getSubList ys ye b)
>   where (xs, xe) = index x
>         (ys, ye) = index y

In onze `where`-clause vragen we eerst de start- en eindcoördinaten op voor x en y. Vervolgens halen we de relevante rijen uit ons bord met `getSubList ys ye`. Dan halen we uit elk van deze rijen (`map`) de relevante kolommen op `getSubList xs xe`. De resultaten concateneren we gewoon.

> -- | Of in een rooster een getal op de gegeven coördinaten ligt.
> containedInSquare :: Board -> Int -> X -> Y -> Bool
> containedInSquare b n x y = contains n $ getSubSquare b x y


Afwerking
---------

> -- | Of in een rooster een getal op gegeven coördinaat past. Dit is het geval
> -- als de omvattende rij, kolom of vakje van die coördinaat dat getal nog niet
> -- bevatten.
> canPlaceNumber :: Board -> Int -> Int -> Int -> Bool
> canPlaceNumber board n x y = not $ inRow || inColumn || inSquare
>   where inRow    = contains n $ getRow board y
>         inColumn = contains n $ getCol board x
>         inSquare = containedInSquare board n x y

> -- | Vervangt het element op gegeven coördinaat door het gegeven element.
> replace :: a -> Int -> [a] -> [a]
> replace n x l = front ++ [n] ++ back
>   where (front, _:back) = splitAt x l

> -- | Vervangt het element op gegeven coördinaat door het gegeven element.
> update :: Int -> X -> Y -> Board -> Board
> update n x y b = replace (replace n x $ getRow b y) y b

In `update` nemen we de aan-te-passen rij uit ons bord, en vervangen in die rij de betreffende kolom. Dan plaatsen we het resultaat hiervan terug in ons bord.

De volgende twee functies, `findFirstEmpty` en `noneEmpty`, werken beide met de lege vakjes in de sudoku. We voegen een nieuwe hulpfunctie toe die deze zal oplijsten. We doen dit door eerst de rijen te nummeren (`zip`pen met `[0..]`) en vervolgens de kolommen te nummeren in een LC.

> -- | De coördinaten van de lege vakjes.
> empties :: Board -> [(X, Y)]
> empties b = [ (x, y)
>             | (y, r) <- zip [0..] b
>             , (x, c) <- zip [0..] r
>             , c == empty ]

`findFirstEmpty` en `noneEmpty` zijn nu triviaal te schrijven als respectievelijk het eerste element van `empties` en of de lijst `empties` niet ledig is.

> -- | De eerste (in leesvolgorde) lege coördinaat in een bord.
> findFirstEmpty :: Board -> (X, Y)
> findFirstEmpty = head . empties

> -- | Of dit bord geen lege vakjes meer bevat.
> noneEmpty :: Board -> Bool
> noneEmpty = not . null . empties

> -- | De lijst van getallen die past op een locatie op een bord.
> options :: Board -> X -> Y -> [Int]
> options b x y = [ n | n <- [1..9], canPlaceNumber b n x y ]

> -- | De lijst van mogelijke borden die 1 vakje verder ingevuld zijn.
> nextBoards :: Board -> [Board]
> nextBoards b = [ update n x y b  | n <- options b x y  ]
>   where (x, y) = findFirstEmpty b

We vragen de eerste locatie op die leeg is, en zullen op die plaats alle mogelijke opties zetten.

> -- | Los een sudoku-puzzel op.
> solve :: Board    -- ^ De focus, de huidige oplossing.
>       -> [Board]  -- ^ De borden die we nog verder kunnen uitwerken.
>       -> Board    -- ^ De opgeloste sudoku.
> solve focus options | noneEmpty focus           = focus
>                     | (null $ nextBoards focus) = solve (head options) (tail options)
>                     | otherwise                 = solve nextFocus (next ++ options)
>                         where (nextFocus:next) = nextBoards focus

We bekijken diverse opties:
- Als er geen lege vakjes op ons bord meer zijn, is de sudoku opgelost. We geven onze `focus` terug.
- Er zijn nog lege vakjes. Vinden we niets om hier in te vullen (`null $ nextBoards focus`) dan nemen we ons eerste alternatief en zoeken we daarmee verder.
- Er zijn nog lege vakjes en de huidige focus heeft nog potentieel. We gaan dieper in op de eerste kandidaat en plaatsen de rest op onze stack met "nog te bekijken" oplossingen.

Het kan ook iets korter door te focussen op de top van de stack.

> solve' :: Board -> Board
> solve' init = loop $ init:[]
>   where loop []     = undefined -- geen oplossingen
>         loop (x:xs) | noneEmpty x = x
>                     | otherwise   = loop $ nextBoards x ++ xs

Hier werken we gewoon steeds verder op de top van de stack, zodat we geen tweede argument nodig hebben. Als `nextBoards x` dan leeg is, zullen we automatisch verder werken op de rest van de stack.
