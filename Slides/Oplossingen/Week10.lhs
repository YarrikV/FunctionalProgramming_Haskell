Continuation Passing Style
==========================

We beginnen met een waarschijnlijk nog ongekende feature te introduceren. Met onderstaande syntax kunnen we speciale features van de compiler aanzetten. Het nut van deze feature wordt onderaan duidelijker. Om didactische redenen vermijden we het gebruik hiervan.

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

> module Week10 where
> import Prelude hiding (and)
> import Control.Monad.Cont
> import Control.Monad.State
> import Control.Applicative

Fibonacci
---------

In de opgave kregen we al de "normale" definitie van `add` en `fibonacci`.

> add :: Int -> Int -> Int
> add x y = x + y

> fibonacci :: Int -> Int
> fibonacci 0 = 0
> fibonacci 1 = 1
> fibonacci n = add (fibonacci $ n - 1) (fibonacci $ n - 2)

We proberen deze nu te schrijven in *continuation passing style*. In plaats van het originele resultaat zal de functie nu een functie teruggeven die dat originele resultaat aan zijn argument zal meegeven. Dat argument stelt de rest van de berekening voor. Onze functie krijgt dus een derde argument, een functie waarmee we verder doen nadat onze berekening afgelopen is.

Onze `add'` functie zal dus als extra argument een `Int -> r` nemen, want het originele resultaat was `Int`. Eenmaal we onze `Int` berekend hebben, geven we die mee aan dat extra argument, die extra berekeningen zal doen. Het resultaat van die extra berekeningen wordt het resultaat van onze `add'` functie, dus `r`.

< add' :: Int -> Int -> (Int -> r) -> r
< add' x y k = k $ add x y

Net zoals we `1 + 2` kunnen schrijven als `(+2) 1` (dus `(+2)` toegepast op `1`) kunnen we ook hier onze `k $ add x y` schrijven als `($ add x y) k`. Dit zorgt voor onderstaande elegante notatie voor *continuation passing style* functies.

> add' :: Int -> Int -> ((Int -> r) -> r)
> add' x y = ($ add x y)

We voorzien een extra hulpfunctie `decrement'`, waarmee we een getal decrementeren in CPS.

> decrement' :: Int -> ((Int -> r) -> r)
> decrement' n = ($ n - 1)

Tenslotte schrijven we de `fibonacci'` functie. De twee basisgevallen blijven vrijwel ongewijzigd. In het recursieve geval wordt nu duidelijk in welke volgorde we zullen werken. Onze variabele `n1`, die `n - 1` voorsteld, is afhankelijk van n, `n2` (`n - 2`) is afhankelijk van `n1`, ... We kunnen dan ook eenvoudig correct lijnen van volgorde wisselen, omdat de afhankelijkheid onmiddelijk duidelijk is.

> fibonacci' :: Int -> ((Int -> r) -> r)
> fibonacci' 0 = ($ 0)
> fibonacci' 1 = ($ 1)
> fibonacci' n = \k -> decrement' n $ \n1 ->
>                      decrement' n1 $ \n2 ->
>                      fibonacci' n1 $ \fn1 ->
>                      fibonacci' n2 $ \fn2 ->
>                      add' fn1 fn2 $ k

Ook hier kunnen we opnieuw het extra argument ongeschreven laten.

< fibonacci' n = decrement' n $ \n1 ->
<                decrement' n1 $ \n2 ->
<                fibonacci' n1 $ \fn1 ->
<                fibonacci' n2 $ \fn2 ->
<                add' fn1 fn2

Is het toeval dat dit ons erg doet denken aan een monadische berekening zonder `do`?


Pattern Matching through Continuations
--------------------------------------

Een andere manier om continuations te gebruiken, is om pattern matching te implementeren. Jullie hebben dit zonder het te weten eigenlijk al gebruikt toen we Church Booleans en Church Numbers schreven. Aan aan Church boolean gaven we 2 argumenten mee om zijn waarde te weten te komen. Als de boolean True was, werd zijn eerste argument teruggegeven, anders het tweede. Die argumenten zijn dus eigenlijk gewoon constante functies waarmee we "verder doen" met de berekening. Hieronder vind je de definitie van Church Booleans en dan analoog die voor expressies.

< data Bool = True  -- De functie die true teruggeeft
<           | False -- De functie die false teruggeeft

> type BoolC r = (r  -- De functie die we aanroepen indien "true"
>              -> r  -- De functie die we aanroepen indien "false"
>              -> r) -- Het resultaat van de continuatie (ofwel de eerste of tweede r).
 
> true  = \t _ -> t
> false = \_ f -> f
> and x y t f = y (x t f) f

< data Exp = Zero                 -- De functie die nul teruggeeft
<          | One                  -- De functie die één teruggeeft
<          | Const Int            -- Een functie die een getal neemt en dat getal als constante teruggeeft
<          | Plus (Exp r) (Exp r) -- Een functie die twee expressies neemt en hun som teruggeeft
<          | Mult (Exp r) (Exp r) -- Een functie die twee expressies neemt en hun vermenigvuldiging teruggeeft

In Continuation Style Programming wordt dit het onderstaande. Met op dat we hier werken met een `data` en een constructor om een oneindig (door de recursie) type te vermijden.

>                                 -- De functie die we aanroepen als ons resultaat...
> data Exp r = Exp (r                     -- ... een 0 is
>                -> r                     -- ... een 1 is
>                -> (Int -> r)            -- ... een constante is
>                -> (Exp r -> Exp r -> r) -- ... een optelling van twee expressies is
>                -> (Exp r -> Exp r -> r) -- ... een vermenigvuldiging van twee expressies is
>                -> r)                    -- het uiteindelijke resultaat

We krijgen onze constructoren net zoals bij true en false, maar dan met meer argumenten.

> zero, one :: Exp r
> zero = Exp $ \f _ _ _ _ -> f
> one  = Exp $ \_ f _ _ _ -> f

> const :: Int -> Exp r
> const x = Exp $ \_ _ f _ _ -> f x

> plus, mult :: Exp r -> Exp r -> Exp r
> plus x y = Exp $ \_ _ _ f _ -> f x y
> mult x y = Exp $ \_ _ _ _ f -> f x y

Als we dan onze expressie willen evalueren, moeten we gewoon nog de manier van berekenen meegeven aan elk van onze "patronen". Hieronder eerst de "gewone" *pattern matching* en vervolgens die in CPS.

< evaluate :: Exp Int -> Int
< evaluate Zero = 0
< evaluate One = 1
< evaluate (Const i) = i
< evaluate (Plus x y) = evaluate x + evaluate y
< evaluate (Mult x y) = evaluate x * evaluate y

> evaluate :: Exp Int -> Int
> evaluate (Exp e) = e 0 1 id (\x y -> evaluate x + evaluate y) (\x y -> evaluate x * evaluate y)

Exceptions in CPS
-----------------

Met behulp van CPS kunnen we de _control flow_ van een programma aansturen. We gebruiken dat om uitzonderingen te implementeren. Bekijk onderstaande functie.

> tryCont :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
> tryCont c h = callCC $ \ok -> do
>                   err <- callCC $ \notOk -> do
>                       x <- c notOk
>                       ok x
>                   h err

Als argumenten neemt ze 2 functies, `c` en `h`. `c` is een functie die we willen uitvoeren, maar die misschien uitzonderingen kan opgooien. `h` is de manier waarop we die uitzondering willen afhandelen. Aan `c` zullen we een functie meegeven die deze kan gebruiken om de fout op te gooien. Die functie heeft hetzelfde resultaat als `c`, namelijk `m a`. Op die manier kan onze `tryCont` altijd hetzelfde type teruggeven.

> data SqrtException = LessThanZero deriving (Show, Eq)

We schrijven nu een `sqrtIO` functie, die de vierkantswortel zal berekenen, en misschien een fout opgooien.

> sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
> sqrtIO throw = do lift $ putStr "Geeft een getal: "
>                   i <- lift readLn
>                   if i < 0
>                     then throw LessThanZero
>                     else lift $ print $ sqrt i

We schrijven eerst onze vraag uit, en interpreteren vervolgens een lijn invoer. (Mogelijks gebruik je hier `readLn :: IO Float` om aan te geven dat je een kommagetal wilt inlezen. Nu zal de compiler interfereren dat je een `Floating`-ding wilt omdat je het resultaat meegeeft aan `sqrt`.) We vergelijken ons getal met 0, en maken de juiste keuze.

In ons type zie je hoe we de ContT Monad Transformer gebruiken om continuations met IO te kunnen combineren.

> main :: IO ()
> main = runContT (tryCont sqrtIO $ lift . print) return

In de `main` functie roepen we nu onze `sqrtIO` aan. We gebruiken `tryCont` om een handler mee te geven. Deze zal de uitzondering gewoon afprinten. We voeren de opgebouwde continuatie uit met `runContT` en als *continuation function`, waar we mee verder gaan, geven we gewoon `return` mee.





Coroutines
----------

Coroutines zijn een paradigma om meerdere berekeningen "tegelijk" uit te voeren, zonder gebruik te maken van threads of multiprocessing. In werkelijkheid gebeurt er niets tegelijk, maar zullen de verschillende routines elkaar afwisselen. Met behulp van de primitieve `fork` kan een (co)routine zich "opsplitsen" in 2 coroutines. Met behulp van de primitieve `yield` kan een coroutine zijn controle afgeven aan een andere coroutine. Het gebruik van coroutines vind je bijvoorbeeld in Python's asyncio.

We schrijven een CoroutineT monad transformer. Deze neemt 3 argumenten: `r`, `m` en `a`. `r` is het resultaat-type van de *continuation function*. `m` is de monad die we omvatten. `a` is het argument-type van de *continuation function*. Ze bestaat uit een momenteel uitgevoerde coroutine (`ContT r (...) a`) en de gepauzeerde coroutines (opgeslagen in een `StateT [Coroutine r m ()] m`).

> newtype CoroutineT r m a = CoroutineT
>     { runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a
>     } deriving (Functor, Applicative, Monad, MonadCont, MonadIO)

We gebruiken hier de *GeneralizedNewtypeDeriving* om het voorbeeld kort te houden.

We schrijven eerst enkele hulpfuncties om de coroutines te manipuleren. `getCCs` zal onze lijst van gepauzeerde coroutines teruggeven uit de State monad. Omgekeerd zl `putCCs` een lijst van coroutines aplaatsen in de State monad.

> getCCs :: Monad m => CoroutineT r m [CoroutineT r m ()]
> getCCs = CoroutineT $ lift get

> putCCs :: Monad m => [CoroutineT r m ()] -> CoroutineT r m ()
> putCCs = CoroutineT . lift . put

Met `getCCs` en `putCCs` kunnen we nu `dequeue` en `queue` schrijven. Enerzijds zal `dequeue` de coroutine bovenaan de lijst (we beschouwen de lijst als een queue) zal nemen en uitvoeren. Anderzijds zal `queue` een gegeven coroutine achteraan in de queue plaatsen.

> dequeue :: Monad m => CoroutineT r m ()
> dequeue = do ccs <- getCCs
>              case ccs of []        -> return ()
>                          (cc:ccs') -> putCCs ccs' >> cc

> queue :: Monad m => CoroutineT r m () -> CoroutineT r m ()
> queue cc = do ccs <- getCCs
>               putCCs (ccs++[cc])

Hiermee schrijven we `yield`, die de controle afgeeft. Hiertoe plaatst `yield` de huidige coroutine (a.k.a. de huidige continuation) in de queue van gepauzeerde coroutines en voert de voorste coroutine uit die lijst uit.

> yield :: Monad m => CoroutineT r m ()
> yield = callCC $ \cc -> do queue $ cc ()
>                            dequeue

`fork` zal de controle opsplitsen. `fork` neemt als argument de "extra" coroutine die we willen uitvoeren. We plaatsen de huidige coroutine in de queue en geven controle aan de nieuwe coroutine. Als de nieuwe coroutine eindigt (eventueel na een hele hoop heen-en-weer gegooi van de controle) willen we nog een `dequeue` doen. Zo zijn we zeker dat de huidige coroutine (moest die nog niet geëindigd zijn) ook nog afgehandeld wordt. (Merk op: ik zeg huidige, maar dit kan evengoed een andere coroutine uit de lijst zijn. Het belangrijke hier is dat we 1 extra coroutine in de lijst plaatsen, en er dus ook 1tje zeker afhalen.)

> fork :: Monad m => CoroutineT r m () -> CoroutineT r m ()
> fork nc = callCC $ \cc -> do queue (cc ())
>                              nc
>                              dequeue

Jullie kregen nog `exhaust` cadeau. `exhaust` is een coroutine die steeds als laatste zal eindigen. Ze kijkt of er andere coroutines zijn, en zoja, zal controle daaraan geven. Door te wachten op het einde van `exhaust` kunnen we het einde van alle coroutines afwachten.

> exhaust :: Monad m => CoroutineT r m ()
> exhaust = do exhausted <- null <$> getCCs
>              if not exhausted then yield >> exhaust
>                               else return ()

`runCoroutineT` zal de coroutines uitvoeren in de basismonad. Gegeven een coroutine `c` voeren we eerst `c <* exhuast` uit. `<*` zal zijn rechterargument uitvoeren en indien dit succesvol gaat, zijn linkerargument teruggeven als resultaat. De andere drie functies halen de andere laagjes van onze coroutine monad.

> runCoroutineT :: Monad m => CoroutineT r m r -> m r
> runCoroutineT = flip evalStateT []
>               . flip runContT return
>               . runCoroutineT'
>               . (<* exhaust)

Tenslotte schrijven we een voorbeeldje, met een `printOne` die een gegeven getal eenmalig zal afprinten, en vervolgens controle afgeven. Het voorbeeld zal 3 maal "3" afprinten, 4 maal "4" en 2 maal "2". Deze zullen achter door elkaar heen verschijnen in de uitvoer.

> printOne :: Int -> CoroutineT r IO ()
> printOne n = do liftIO (print n)
>                 yield

> example :: IO ()
> example = runCoroutineT $ do fork $ replicateM_ 3 (printOne 3)
>                              fork $ replicateM_ 4 (printOne 4)
>                              replicateM_ 2 (printOne 2)
