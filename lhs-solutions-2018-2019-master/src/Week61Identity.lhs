
Oplossingen Week 6
==================

> module Week61Identity where

Monadische interpreter
----------------------

In deze opgave zullen we berekeningen in een lambda calculus voorstellen, en uitrekenen binnen de context van een monadische omgeving. Deze omgeving zal de koppeling tussen de namen van variabelen en hun waarde bijhouden. Namen van variabelen worden voorgesteld door `String`s.

> type Name  = String

Een term in onze programmeertaal stellen we voor door `Term`, en kan een variabele zijn, een constant getal, een operatie (het optellen van twee termen), een anonieme functie of de toepassing van een anonieme functie (resulterend uit gegeven term) op een tweede term.

> data Term  = Var Name
>            | Con Int
>            | Add Term Term
>            | Lam Name Term
>            | App Term Term

Het resultaat van een berekening kan 3 vormen aannemen. Als er iets mis gelopen is (optelling van twee termen die geen waarden zijn, het toepassen van een term die geen functie is, het opvragen van een onbestaande variabele, ...) is het resultaat `Wrong`. Andere mogelijke resultaten zijn een geheel getal of een functie.

> data Value = Wrong
>            | Num Int
>            | Fun (Value -> M Value)

Verder houden we een omgeving bij om onze variabele/waarde-mapping bij te houden.

> type Environment = [(Name, Value)]

Hieronder volgt eerst de rest van de gegeven code, te beginnen met een show-functie.

> showval (Wrong) = "<wrong>"
> showval (Num i) = show i
> showval (Fun f) = "<function>"

Met behulp van `lookUp` kunnen we een variabele opvragen uit de omgeving. Als de gevraagde variabele niet bestaat, geven we waarde `Wrong` terug.

> lookUp             :: Name -> Environment -> M Value
> lookUp x []        = unitM Wrong
> lookUp x ((y,b):e) = if x == y then unitM b else lookUp x e

`interp` zal een term uitrekenen binnen een omgeving, resulterende in een waarde. Een term die bestaat uit een variabele zullen we gewoon opvragen aan onze omgeving. Een constante term resulteerd in een numerieke waarde.

> interp           :: Term -> Environment -> M Value
> interp (Var x) e = lookUp x e
> interp (Con i) e = unitM (Num i)

Om twee termen op te tellen, zullen we eerst die twee termen uitrekenen. Vervolgens tellen we de resultaten op met `add`.

> interp (Add u v) e = interp u e `bindM` (\a ->
>                      interp v e `bindM` (\b ->
>                      add a b))

Het uitrekenen van een anonieme functie is complexer. We geven als waarde een nieuwe functie terug. Deze functie neemt 1 argument, dat we hieronder `a` noemen. Vervolgens rekenen we `v` (het functie-blok) uit in een omgeving waar we `x` (de naam van ons argument) mappen op de gegeven waarde `a`.

> interp (Lam x v) e = unitM (Fun (\a -> interp v ((x,a):e)))

Het toepassen van zo'n functie delegeren we naar `apply`. Eerst zullen we echter de functie die we aanroepen en zijn argument al interpreteren.

> interp (App t u) e = interp t e `bindM` (\f ->
>                      interp u e `bindM` (\a ->
>                      apply f a))

Vervolgens krijgen we hieronder de definities van `add` en `apply`. Deze zullen respectievelijk twee getallen optellen of een functie op een waarde toepassen. Als het type van één van de argumenten niet klopt, geven we de waarde `Wrong` terug. 

> add                 :: Value -> Value -> M Value
> add (Num i) (Num j) = unitM (Num (i+j))
> add a b             = unitM Wrong

> apply               :: Value -> Value -> M Value
> apply (Fun k) a     = k a
> apply f a           = unitM Wrong

Uiteindelijk krijgen we ook nog `test`, een hulpfunctie om een term uit te rekenen en om te zetten naar een String.

> test   :: Term -> String
> test t = showM (interp t [])

Nu kunnen we beginnen met de eigenlijke oplossing. Er werd gevraagd om een "identiteitsmonad" te implementeren. We herinneren onze de identiteitsfunctie en zijn type `id :: a -> a`. De identiteitsmonad lijkt hier erg op, die heeft namelijk *kind* `M :: * -> *`.

> type M a = a

Hierdoor wordt `unitM :: a -> M a` eigenlijk gewoon `unitM :: a -> a` ofwel zelf de identiteitsfunctie. Ook het type van `bindM` en `showM` wordt erg eenvoudig.

> unitM       :: a -> M a
> unitM x     = x
>
> bindM       :: M a -> (a -> M b) -> M b
>             --  a  -> (a ->  b)  ->  b
> m `bindM` k = k m
>
> showM       :: M Value -> String
> showM m     = showval m

