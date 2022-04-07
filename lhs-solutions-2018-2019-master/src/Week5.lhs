
Oplossingen Week 5
==================

We importeren bovenaan in ons bestand al `liftM` en `ap` uit `Control.Monad`. We zullen deze pas bij de "State Monad" oefening gebruiken.

> module Week5 where
> import Control.Monad(liftM,ap)


Error Monad
-----------

Hieronder definiëren we het `Ofwel` datatype, in de standaardbibliotheek beschikbaar onder de naam `Either`. We kunnen dit type gebruiken om 2 mogelijke types als return-type van een functie op te geven (een beetje zoals we bij een tuple altijd 2 types terug zullen geven). Dit wordt vaak gebruikt bij functies die fout kunnen lopen: we krijgen dan een `Links` type voor fouten en een `Rechts` type voor geslaagde calls.

> data Ofwel e a = Rechts a
>                | Links e
>                deriving (Show, Eq)

Eerst instantiëren we `Functor` voor ons nieuwe datatype. Merk op dat we hier `Ofwel e` schrijven, en niet `Ofwel e a`. Het "weggelaten" typeargument is het type dat we zullen "wrappen" met onze typeklasse. Als we het algemene en specifieke type van `fmap` bekijken voor `Ofwel e`, zien we dit terug:

< fmap :: Functor f => (a -> b) -> f a -> f b
< fmap :: (a -> b) -> Ofwel e a -> Ofwel e b

We zien dat we de gegeven functie zullen toepassen op de `Rechts` van `Ofwel`. De `Links` laten we met rust.

> instance Functor (Ofwel e) where
>     fmap f (Links x) = Links x
>     fmap f (Rechts x) = Rechts (f x)

Voor een `Applicative` instantie moeten we twee functies definiëren: `pure` en `<*>` (uitgesproken als "applied over"). Hierbij zal `pure` een waarde "liften" binnen ons datatype. `<*>` zal een gelifte functie en een gelifte waarde nemen en die functie proberen toepassen op die waarde. In het geval van `Applicative` kunnen we deze eenvoudig verkrijgen met behulp van *pattern matching*.

> instance Applicative (Ofwel e) where
>     pure                      = Rechts
>     (Links e)  <*> _          = Links e
>     (Rechts f) <*> (Links x)  = Links x
>     (Rechts f) <*> (Rechts x) = Rechts (f x)

Een `Monad` wordt gekenmerkt voor twee functies: `return` en `>>=` (uitgesproken als "bind"). Ook hier zal `return` een waarde liften tot onze monad. `>>=` zal een gelifte waarde een een monadische functie nemen, en die functie toepassen op de waarde binnen onze monad.

> instance Monad (Ofwel e) where
>     return = Rechts
>     (Links x)  >>= f = Links x
>     (Rechts x) >>= f = f x

Nu kunnen we onze monad gebruiken om te rekenen met "falende functies", zoals `myDiv`.

> myDiv :: Int -> Int -> Ofwel String Int
> myDiv x y | y == 0         = Links "can't divide by zero"
>           | x `mod` y /= 0 = Links "not exact"
>           | otherwise      = Rechts (x `div` y)

Dit kunnen we doen met twee notaties: de `do`-notatie en de `>>=` notatie.

< div_plus_20 :: Int -> Int -> Ofwel String Int
< div_plus_20 x y = myDiv x y >>= (\r ->
<                   return $ r + 20)
<          -- === = myDiv x y >>= return . (+20)

> div_plus_20 :: Int -> Int -> Ofwel String Int
> div_plus_20 x y = do r <- myDiv x y
>                      return $ r + 20

Hierbij is de `do`-notatie *syntactic sugar* voor de `>>=`-notatie en kan deze in sommige gevallen een stuk leesbaarder zijn. Bij de `do`-notatie is whitespace van belang! Je kan ook `{;}` gebruiken als je andere indentatie wilt, of soms (zelden) kan het ook handig zijn om de `do`-notatie op 1 lijn te schrijven.

< div_plus_20 :: Int -> Int -> Ofwel String Int
< div_plus_20 x y = do { r <- myDiv x y
<                      ; return $ r + 20
<                      }

< div_plus_20 :: Int -> Int -> Ofwel String Int
< div_plus_20 x y = do { r <- myDiv x y; return $ r + 20 }

Dit is meestal lelijk.


State Monad
-----------

De volgende monad die we bekijken is de `State` monad. Het `State` datatype wrapt een functie die een staat transformeert en een waarde teruggeeft.

> data State s a = State (s -> (a,s))

Om deze transformatie uit te voeren definiëren we de `run` functie.

> run :: State s a -> s -> (a, s)
> run (State f) s = f s

We willen meerdere van deze bewerkingen aan elkaar kunnen koppelen, om ze dan in hun geheel toe te passen op een zeker staat. Het resultaat wordt dan een paar van een uitkomst en een eind-staat. Hiervoor gebruiken we de `>>=` van een monad.

> instance Monad (State s)  where
>   return a = State $ \s -> (a,s)
>   m >>= k  = State $ \s -> let (x,s') = run m s
>                             in run (k x) s'

We zien hoe een nieuwe `State` maken, die onze twee bewerkingen aan elkaar koppelt: gegeven een beginstaat `s` zullen we eerst bewerking `m` erop uitvoeren. Vervolgens voeren we `k x`, een bewerking uit basis van het resultaat van `m`, uit op de resulterende staat. Merk op dat, dankzij *laziness*, deze bewerkingen nog niet *nu* uitgevoerd worden: ze worden aan elkaar gekoppeld voor later.

In het voorbeeld hieronder zullen we bijvoorbeeld de bewerking `plus1` definiëren. Deze bewerking zal niets teruggeven, maar de staat met 1 verhogen.

> plus1 :: State Int ()
> plus1 = State $ \s -> ((), s + 1)

Als we dan de bewerking `plus2` willen definiëren, kunnen we dat doen als `plus1 >> plus2` (`>>` is een variant op `>>=`, zie google). Nu krijgen we `run plus2 1 === ((), 3)`.

We kunnen de instantie voor `Functor` en `Applicative` als volgt schrijven.

< instance Functor (State s) where
<     fmap f m = State $ \s -> let (x, s') = run m s
<                               in (f x, s')

< instance Applicative (State s) where
<     pure    = State $ \s -> (a, s)
<     f <*> m = State $ \s -> let (x, s') = run m s
<                                 (g, s'') = run f s'
<                              in (g x, s'')

Echter, omdat we eerst een `Monad` instantie gemaakt hebben, kunnen we ook `liftM` en `ap` gebruiken, die een generieke implementatie voor deze functies aan de hand van hun monad definiëren.

> instance Functor (State s) where
>     fmap = liftM

> instance Applicative (State s) where
>     pure  = return
>     (<*>) = ap

Vervolgens definiëren we 3 functies die ons zullen helpen bij de verdere definities. `get` zal de huidige staat als return-type zetten, `put` zal een gegeven staat als nieuwe staat nemen en `modify` zal de huidige staat aanpassen gegeven een functie.

> get :: State s s
> get = State $ \s -> (s, s)

> put :: s -> State s ()
> put s = State $ \_ -> ((), s)

> modify :: (s -> s) -> State s ()
> modify f = State $ \s -> ((), f s)

Met deze definities kunnen we eenvoudig de basisbewerking op een stapel definiëren.

> type Stack = [Int]

> push :: Int -> State Stack ()
> push x = modify (x:)

> pop :: State Stack Int
> pop = do (s:ss) <- get
>          put ss
>          return s

> add, mult :: State Stack ()
> add  = do x <- pop
>           y <- pop
>           push $ x + y
> mult = do x <- pop
>           y <- pop
>           push $ x * y

Nu kunnen we op een duidelijk wijze rekenen met een stapel, bijvoorbeeld `run (push 2 >> push 3 >> mult >> pop) []` zal ons als resultaat `6` geven en als eindstaat `[]`.
