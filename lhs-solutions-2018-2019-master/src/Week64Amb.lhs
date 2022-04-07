
Oplossingen Week 6
==================

> module Week64Amb where

Niet deterministische interpreter
---------------------------------

> type Name  = String

> data Term  = Var Name
>            | Con Int
>            | Add Term Term
>            | Lam Name Term
>            | App Term Term
>            -- Nieuwe taalconstructies
>            | Amb Term Term
>            | Fail

> data Value = Wrong
>            | Num Int
>            | Fun (Value -> M Value)

> type Environment = [(Name,Value)]

Om ervoor te zorgen dat we meerdere mogelijke oplossingen kunnen teruggeven, zullen we ons resultaat opslaan in een lijst. Zo krijgen we de lijst-monad.

> type M a = [a]

Als we nu een resultaat tot onze lijst-monad willen liften, kunnen we dit beschouwen als het enige mogelijke resultaat. Met andere woorden, het enige element in de resultatenlijst.

> unitM :: a -> M a
> unitM a = [a]

> bindM :: M a -> (a -> M b) -> M b
>       -- [a] -> (a -> [b]) -> [b]

`bindM` zal een functie met een onzeker resultaat toepassen op een onzekere waarde. Als we het type uitwerken van `bindM`, zien we best onze functie `k` toepassen op elke mogelijke waarde van `m` en alle resultaten samennemen. Dit klinkt en is ook gewoon `concatMap`.

> m `bindM` k = k `concatMap` m

Om het resultaat te tonen, zullen we gewoon elk van de mogelijke resultaten tonen in lijst-vorm.

> showM   :: M Value -> String
> showM m = show $ map showval m

Tenslotte krijgen we nog `plusL`. Deze zal twee resultatengroepen combineren, ofwel hun lijsten concateneren.

> plusL :: M Value -> M Value -> M Value
> m1 `plusL` m2 = m1 ++ m2

Opnieuw kunnen we `showval` en `lookUp` gewoon overnemen.

> showval              :: Value -> String
> showval Wrong        = "<wrong>"
> showval (Num i)      = show i
> showval (Fun f)      = "<function>"

> lookUp               :: Name -> Environment -> M Value
> lookUp x []          = unitM Wrong
> lookUp x ((y,b):e)   = if x == y then unitM b else lookUp x e

Aan `interp` voegen we stappen voor `Fail` en `Amb` toe. Gezien `Fail` geen antwoord teruggeeft, resulteert deze in een lege lijst. `Amb` zal de resultaten van beide expressies nemen en ze combineren.

> interp               :: Term -> Environment -> M Value
> interp Fail      e   = []
> interp (Amb u v ) e  = interp u e `plusL` interp v e
> interp (Var x) e     = lookUp x e
> interp (Con i) e     = unitM (Num i)
> interp (Add u v) e   = interp u e `bindM` (\a ->
>                        interp v e `bindM` (\b ->
>                        add a b))
> interp (Lam x v) e   = unitM (Fun (\a -> interp v ((x,a):e)))
> interp (App t u) e   = interp t e `bindM` (\f ->
>                        interp u e `bindM` (\a ->
>                        apply f a))

De andere functies kunnen we gewoon terug overnemen.

> add                  :: Value -> Value -> M Value
> add (Num i)  (Num j) = unitM (Num (i+j))
> add a b              = unitM Wrong

> apply                :: Value -> Value -> M Value
> apply (Fun k) a      = k a
> apply f a            = unitM Wrong

> test                 :: Term -> String
> test t               = showM (interp t [])

