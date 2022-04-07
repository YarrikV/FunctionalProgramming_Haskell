
Oplossingen Week 6
==================

Monadische interpreter met errors
---------------------------------

We beginnen onze oplossing met dezelfde typedefinities als de basis-interpreter.

> type Name  = String

> data Term  = Var Name
>            | Con Int
>            | Add Term Term
>            | Lam Name Term
>            | App Term Term

> data Value = Wrong
>            | Num Int
>            | Fun (Value -> M Value)

> type Environment = [(Name,Value)]

We definiëren een nieuw datatype om onze fouten voor te stellen, en maken een typesynoniem `M` aan, zodat we onze functies zo weinig mogelijk moeten aanpassen.

> data E a = Success a | Error String

> type M a   =  E a

Op het toevoegen van de error-string na verandert er niets aan onderstaande definities.

> showval              :: Value -> String
> showval Wrong        = "<wrong>"
> showval (Num i)      = show i
> showval (Fun f)      = "<function>"

> lookUp               :: Name -> Environment -> M Value
> lookUp x []          = Error $ "Variable "
>                             ++ show x
>                             ++ " niet gevonden in de omgeving"
> lookUp x ((y,b):e)   = if x == y then unitM b else lookUp x e

> interp               :: Term -> Environment -> M Value
> interp (Var x) e     = lookUp x e
> interp (Con i) e     = unitM (Num i)
> interp (Add u v) e   = interp u e `bindM` (\a ->
>                        interp v e `bindM` (\b ->
>                        add a b))
> interp (Lam x v) e   = unitM (Fun (\a -> interp v ((x,a):e)))
> interp (App t u) e   = interp t e `bindM` (\f ->
>                        interp u e `bindM` (\a ->
>                        apply f a))

> add                  :: Value -> Value -> M Value
> add (Num i)  (Num j) = unitM (Num (i+j))
> add a b              = Error $ "Kan " ++ showval a
>                             ++ " niet optellen bij " ++ showval b

> apply                :: Value -> Value -> M Value
> apply (Fun k) a      = k a
> apply f a            = Error $ "Waarde die geen functie is "
>                             ++ "gebruikt op functie positie: "
>                             ++ showval f

> test                 :: Term -> String
> test t               = showM (interp t [])

Dan komt de definitie van onze nieuwe monad. We beginnen met `unitM`, ofwel het wrappen van een waarde met minimale wijziging. We kiezen hier voor een succesvolle berekening.

> unitM :: a -> M a
> unitM x = Success x

Om verdere berekeningen met een resultaat te maken, kijken we of het resultaat succesvol berekend werd. Zoja, rekenen we gewoon verder met het succesvolle resultaat, anders propageren we de fout.

> bindM :: M a -> (a -> M b) -> M b
> (Success x) `bindM` k = k x
> (Error   s) `bindM` k = Error s

Tenslotte voorzien we en functie om onze monadische waarde af te printen.

> showM :: M Value -> String
> showM (Success x) = showval x
> showM (Error   s) = "Error: " ++ s

