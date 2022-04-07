
Oplossingen Week 4
==================

Omdat we later de `not` functie zelf zullen definiëren, moeten we verhinderen dat deze automatisch uit de Prelude ingeladen wordt.

> module Week4 where
> import Prelude hiding (not)


Booleans in Haskell
-------------------

> data BoolT = TrueT | FalseT

> instance Eq BoolT where
>     TrueT  == TrueT  = True
>     FalseT == FalseT = True
>     _      == _      = False

> instance Show BoolT where
>     show TrueT  = "T"
>     show FalseT = "F"

> and, or :: BoolT -> BoolT -> BoolT
> and TrueT  TrueT  = TrueT
> and _      _      = FalseT
> or  FalseT FalseT = FalseT
> or  _      _      = TrueT

> not :: BoolT -> BoolT
> not TrueT  = FalseT
> not FalseT = TrueT


Rock Paper Scissors
-------------------

> data Move = Rock | Paper | Scissors
>           deriving (Show, Eq)

> data Result = Win | Lose | Draw
>             deriving (Show, Eq)

> outcome :: Move -> Move -> Result
> outcome Paper    Rock     = Win
> outcome Rock     Scissors = Win
> outcome Scissors Paper    = Win
> outcome x y | x == y      = Draw
>             | otherwise   = Lose


Bomen in Haskell
----------------

Onderstaande is een initiële versie van een `Tree` datatype.

< data Tree = Vertex Int Tree Tree
<           | Empty
<           deriving (Show, Eq)

We kunnnen echter enkel gehele getallen opslaan in deze boom, het is veel interessanter om een generiek datatype te schrijven. Dit noteren we liefst met record-syntax zodat we meteen accessor functies gratis krijgen.

> data Tree a = Empty
>             | Vertex { item :: a
>                      , left :: Tree a
>                      , right :: Tree a
>                      }
>             deriving (Eq, Show)

Een lege boom wordt eenvoudigweg door onze constructor gegeven.

> empty :: Tree a
> empty = Empty

In `insert` kunnen we goed gebruik maken van een handige feature van record syntax: het aanmaken van een record op basis van een bestaande.

> insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
> insert n Empty = Vertex n Empty Empty
> insert n v@(Vertex x l r)
>   | n < x     = v { left  = insert n l }
>   | n > x     = v { right = insert n r }
>   | otherwise = v

> search :: (Eq a, Ord a) => a -> Tree a -> Bool
> search _ Empty = False
> search n (Vertex x l r)
>   | n < x     = search n l
>   | n > x     = search n r
>   | otherwise = True

Een instantie van `Functor` vraagt om een implementatie van `fmap`. `fmap` zal een functie toepassen over een functor door die functie op elke waarde in de functor toe te passen.

> instance Functor Tree where
>     fmap _ Empty = Empty
>     fmap f (Vertex v l r) = Vertex (f v) (fmap f l) (fmap f r)

Tenslotte nog een cadeautje om jullie eigen code makkelijk te kunnen debuggen.

> pprint :: Show a => Tree a -> IO ()
> pprint = go "" "    " "    "
>   where go _ _ _ Empty = return ()
>         go n l r node = do
>             go (n ++ r) "|   " "    " $ right node
>             putStrLn $ n ++ "+-- " ++ show (item node)
>             go (n ++ l) "    " "|   " $ left node


Evaluator
---------

> data Exp = Const Int
>          | Add Exp Exp 
>          | Sub Exp Exp 
>          | Mul Exp Exp
>          deriving Show

> eval :: Exp -> Int
> eval (Const n) = n
> eval (Add x y) = eval x + eval y
> eval (Sub x y) = eval x - eval y
> eval (Mul x y) = eval x * eval y

> data Inst = IPush Int
>           | IAdd
>           | ISub
>           | IMul
>           deriving Show

> type Prog  = [Inst]
> type Stack = [Int]

We kunnen run recursief definiëren.

< run :: Stack -> Prog -> Stack
< run s [] = s
< run s (IPush n:prog) = run (n:s) prog
< run (x:y:s) (IAdd:prog) = run (x + y : s) prog
< -- ...

Of mooier met een fold.

> run :: Stack -> Prog -> Stack
> run = foldl step
>   where step s (IPush n) = n:s
>         step (x:y:s) IAdd = x + y : s
>         step (x:y:s) ISub = x - y : s
>         step (x:y:s) IMul = x * y : s

Bij compile moeten we opletten. De volgorde waarin we getallen op de stack plaatsen is relevant voor de uitkomst van ons verschil. In `run` plaatsen we `x` bovenop `y` op de stack als we `x - y` willen uitvoeren. We moeten dus bij het compileren van de overeenkomstige `Sub x y`  er voor zorgen dat `x` boven `y` op de stapel komt. Hieronder zie je hoe.

> compile :: Exp -> Prog
> compile (Const n) = [IPush n]
> compile (Add x y) = compile y ++ compile x ++ [IAdd]
> compile (Sub x y) = compile y ++ compile x ++ [ISub]
> compile (Mul x y) = compile y ++ compile x ++ [IMul]

