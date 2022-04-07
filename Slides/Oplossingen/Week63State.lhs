
Oplossingen Week 6
==================

Evaluatiestappen bijhouden
--------------------------

We beginnen opnieuw met dezelfde basisdefinities.

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

Ditmaal werken we met een State monad. We leggen echter het type van de staat vast als `Int`.

> type State = Int

Onze monad wordt dan de transformatie van een state naar een antwoord met een nieuwe state.

> type M a = State -> (a, State)

Aan de `showval`, `lookUp` en `interp` functies moeten we niets aanpassen.

> showval              :: Value -> String
> showval Wrong        = "<wrong>"
> showval (Num i)      = show i
> showval (Fun f)      = "<function>"

> lookUp               :: Name -> Environment -> M Value
> lookUp x []          = unitM Wrong
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

Het opheffen van een waarde tot onze monad doen we bij de state monad door deze waarde terug te geven zonder de state aan te passen.

> unitM :: a -> M a -- :: a -> State -> (a, State)
> unitM x s = (x, s)

Bij `bindM` willen we een functie die onze state wijzigt toepassen op het resultaat van een eerdere state-transformatie. Ik werk eerst het type van deze functie volledig uit.

> bindM -- :: M a -> (a -> M b) -> M b
>       -- M vervangen door zijn synoniem.
>       -- :: (State -> (a, State))
>       -- -> (a -> (State -> (b, State)))
>       -- -> (State -> (b, State))
>       -- haakjes weglaten waar mogelijk
>       :: (State -> (a, State))      -- m
>       -> (a -> State -> (b, State)) -- k
>       -> State                      -- s
>       -> (b, State)

Om de werking van `bindM` duidelijk te maken, schrijf ik hieronder even het type op een licht gewijzigde manier neer, tesamen met het type voor `flip (.)`.

> bindM' :: (State -> (a, State))
>        -> ((a, State) -> (b, State))
>        -> (State -> (b, State))

< flip (.) :: (x -> y)
<          -> (y -> z)
<          -> (x -> z)

We zien hoe `flip (.)` twee functies, `x -> y` en `y -> z`, combineert tot 1 functie `x -> z`. We weten al dat `(.)` staat voor functie-compositie. Bekijken we `bindM'` op dezelfde manier, zien we hoe twee functies, `State -> (a, State)` en `(a, State) -> (b, State)`, gecombineerd worden tot 1 functie `State -> (b, State)`. De `bindM` van de state-monad staat dan ook voor de functie-compositie van *stateful* operaties. Uit de definitie van `(.)` kunnen we nu ook vlot de definitie van `bindM'` afleiden.

< f . g = \x -> f (g x)
< -- of
< (flip (.)) g f x = f $ g x

> bindM' m k s = k $ m s

Om de echte `bindM` neer te schrijven, moeten we gewoon nog de twee resultaten van onze `m` opsplitsen naar twee aparte argumenten.

< bindM m k s = let (x, s') = m s
<                in k x s'

We kunnen die ook met behulp van `uncurry` schrijven. Tenslotte is `uncurry k :: (a, State) -> (b, State)`.

> bindM m k s = uncurry k $ m s

`showM` werd al voor ons geschreven. We zien hier hoe we onze state-transformer uitrekenen beginnende met state `0`, en uit het einde een waarde en een aantal evaluatie-stappen uitkomen.

> showM :: M Value -> String
> showM m = let (a, s1) = m 0
>            in "Waarde: " ++ showval a ++ "; " ++
>               "Evaluatie stappen: " ++ show s1

In `tickS` worden we gevraagd om het aantal evaluatiestappen met 1 te verhogen. Hierbij moeten we geen waarde teruggeven, dus ons returntype is unit.

> tickS :: M ()
> tickS s = ((), s + 1)

Vervolgens schrijven we `add` en `apply`. Hierbij moeten we ervoor zorgen dat de oude implementatie nog steeds gebruikt wordt, maar we ook 1 stap optellen bij onze state. We kunnen daar de `tickS` methode voor gebruiken. We combineren deze dan met de oorspronkelijke implementatie met behulp van `bindM`.

> add                  :: Value -> Value -> M Value
> add (Num i)  (Num j) = tickS `bindM` \_ -> unitM (Num $ i + j)
> add a b              = unitM Wrong

We kunnen hier ook `const` gebruiken.

> apply                :: Value -> Value -> M Value
> apply (Fun k) a      = tickS `bindM` const (k a)
> apply f a            = unitM Wrong

> test                 :: Term -> String
> test t               = showM (interp t [])

