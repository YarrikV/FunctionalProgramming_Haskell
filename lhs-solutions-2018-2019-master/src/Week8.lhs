
Oplossingen Week 8
==================

> module Week8 where
> import Control.Monad                  (liftM, ap, guard)
> import Control.Applicative            (Alternative(..))
> import Control.Monad.Trans.Class      (MonadTrans(lift))
> import Control.Monad.IO.Class         (liftIO)
> import Control.Monad.Trans.State.Lazy (State, StateT(..), get
>                                       , put, modify)


EitherIO Monad met do-notatie
-----------------------------

We definiéren onze `EitherIO` gecombineerde monad. Het is belangrijk om goed op het type te letten, vooral de verschillende lagen. We zullen bij monad transformers namelijk steeds de lagen in volgorde moeten verwijderen en terug toevoegen als onze berekening gedaan is.

> data EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }

We herinneren ons het type van `fmap` en werken het verder uit op ons specifiek geval.

< fmap :: Functor f => (a -> b) -> f          a -> f          b
< fmap ::              (a -> b) -> EitherIO e a -> EitherIO e b

Om onze "a -> b" te kunnen toepassen, moeten we dus iets van het type "a" hebben. We beginnen dus de verschillende lagen van ons tweede argument te halen. Met `runEitherIO :: EitherIO e a -> IO (Either e a)` kunnen we onze `EitherIO` laag verwijderen. Om de `IO` kwijt te raken gebruiken we de *do*-notatie in combinatie met een `<-`, waardoor we een `Either e a` krijgen. Nu komt het moment om ons te herinneren hoe `Either e` zelf een functor is, en we er dus `fmap` op kunnen toepassen.

Als we dat doen krijgen we een `Either e b`, die we terug wrappen in `IO` met `return` en in `EitherIO` met diens constructor.

> instance Functor (EitherIO e) where
>       fmap g x = EitherIO $ do either <- runEitherIO x
>                                return $ fmap g either

Voor `Applicative` passen we dezelfde strategie toe, maar zullen we beide argumenten uit hun laagjes moeten halen. Ook hier maken we er gebruik van dat `Either e` zelf *applicative* is. Onze `pure` zelf trouwens gewoon onze drie lagen wrappen: binnenin `Either`, dan `IO`, dan `EitherIO`.

> instance Applicative (EitherIO e) where
>      pure         = EitherIO . return . Right
>      meab <*> meb = EitherIO $ do eab <- runEitherIO meab
>                                   eb  <- runEitherIO meb
>                                   return $ eab <*> eb

Helaas kunnen we dezelfde strategie niet gebruiken voor `Monad`. Onze lagen verwijderen zal wel werken, maar om gebruik te maken van `instance Monad (Either e)` zouden we een functie `a -> Either e b` nodig hebben, en we hebben enkel een `a -> EitherIO e b`. Na het verwijderen van onze lagen zullen we dus zelf onze functie moeten toepassen met pattern matching.

< instance Monad (EitherIO e) where
<     return   = pure
<     m1 >>= f = EitherIO $ do
<                    e <- runEitherIO m1
<                    case e of
<                         (Left error) -> return (Left error)
<                         (Right val)  -> runEitherIO (f val)

Links geven we gewoon onze error door, weliswaar met onze `IO` laag reeds rond gewrapped. Rechts voeren we onze gekregen functie uit en unwrappen het resultaat, zodat we hetzelfde returntype hebben als links.

Merk op dat we dit ook mooi kunnen schrijven met de `either :: (a -> c) -> (b -> c) -> Either a b -> c` functie, die twee functies neemt, zodat we beide takken in eenzelfde type kunnen omzetten.

> instance Monad (EitherIO e) where
>     return   = pure
>     m1 >>= f = EitherIO $ do e <- runEitherIO m1
>                              either (return . Left)
>                                     (runEitherIO . f) e


Voor Functor en Applicative hadden we ook gewoon `liftM` en `ap` kunnen gebruiken. De import hiervoor is opnieuw bovenaan terug te vinden.

< instance Functor (EitherIO e) where
<     fmap = liftM

< instance Applicative (EitherIO e) where
<     pure  = return
<     (<*>) = ap


Van EitherIO tot EitherT m
--------------------------

We werken nu met een algemeen type `EitherT IO e a` in plaats van `EitherIO e a`.

> data EitherT m e a = EitherT { runEitherT :: m (Either e a) }

Door te vereisen in de instantie van functor, applicative en monad voor `EitherT m e` dat `m` zelf een monad is, kunnen we `liftM`, `ap`, `>>=` en de *do*-notatie gebruiken. De implementatie verandert daardoor vrijwel niet ten opzichte van de implementatie in de vorige opgave, en volgende code is dan ook de verwachte oplossing.

< instance Monad m => Functor (EitherT m e) where
<     fmap = liftM

< instance Monad m => Applicative (EitherT m e) where
<     pure  = return
<     (<*>) = ap

> instance Monad m => Monad (EitherT m e) where
>     return  = EitherT . return . Right
>     m >>= k = EitherT $ runEitherT m >>= either (return . Left)
>                                                 (runEitherT . k)

Je zou echter tot het besef kunnen komen, dat je voor `Functor (EitherT m e)` helemaal niet nodig hebt dat `m` een monad is: het is voldoende dat het een functor is. Hetzelfde geldt voor applicative. Hieronder staan deze minder eisende oplossingen. Deze worden echter een heel stukje (nodeloos) complexer.

> instance Functor f => Functor (EitherT f e) where
>     fmap g x = EitherT $ fmap g <$> runEitherT x

Laat ons voorzichtig de types doorlopen. Je hebt `runEitherT x :: f (Either e a)`. Omdat `f` applicatief is, kunnen we hier een functie van type `Either e a -> Either e b` op toepassen. We hebben een `g :: a -> b`. Nu is `Either e` zelf ook applicatief, dus kunnen we `fmap :: (a -> b) -> (Either e a -> Either e b)` (met extra haakjes) gebruiken om `g` te liften.

Nu kunnen we `fmap g :: Either e a -> Either e b` toepassen op `runEitherT x :: f (Either e a)` met nog een `fmap` of infix-synoniem `<$>`.

De `pure` van applicative blijft eenvoudig: we gebruiken gewoon `pure` in plaats van `return` om te wrappen in `f`. De `<*>` wordt aechter een verschrikking. Hieronder vind je hem terug, eerst opgesplitst met *let-in*.

< instance Applicative f => Applicative (EitherT f e) where
<     pure         = EitherT . pure . Right
<     meab <*> mea = EitherT $ let eab = runEitherT meab
<                               -- :: f (Either e (a -> b))
<                                  ea  = runEitherT mea
<                               -- :: f (Either e a)
<                                  me1 = (<*>) <$> eab
<                               -- :: f (Either e a -> Either e b)
<                               in me1 <*> ea

De opgesplitste vorm begint eenvoudig: we lopen `runEitherT` op `meab` en `mea` om die laag er al af te halen. Nu zouden we graag `eab` toepassen op `ea`, maar we kunnen niet verder unwrappen zoals we dat binnen de *do*-notatie kunnen. De oplossing: in plaats van onze argumenten verder te unwrappen, zullen we onze functies opliften.

`f` is applicatief (en dus een functor) waardoor we met  `<$> :: (a -> b) -> f a -> f b` onze waarde binnen `f` kunnen wijzigen. Met `<*> :: Either e (a -> b) -> Either e a -> Either e b` vormen we de functie binnenin `eab` om: `(<*>) <$> eab :: f (Either e a -> Either e b)`. Deze kunnen we met `<*> :: f (Either e a -> Either e b) -> f (Either e a) -> f (Either e b)` dan toepassen op `ea :: f ea :: f (Either e a)`.

Schrijven we dit kort zonder let, krijgen we onderstaande final form.

> instance Applicative f => Applicative (EitherT f e) where
>     pure         = EitherT . pure . pure
>     meab <*> mea = EitherT $ ((<*>) <$> runEitherT meab) <*> runEitherT mea


Falende stack-operaties
-----------------------

We herschrijven eerst de state-monad uit voorgaande lessen. Om compatibel met de volgende opgave te blijven zal ik dit hier niet doen, maar gewoon de state-monad importeren uit de standaardbibliotheek.

De MaybeT monad transformer lijkt erg op de EitherT monad transformer, dus bij de implementatie hieronder komt geen extra uitleg. Denk aan het pellen en terugleggen van onze lagen.

> newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

> instance Monad m => Functor (MaybeT m) where
>     fmap = liftM

> instance Monad m => Applicative (MaybeT m) where
>     pure  = return
>     (<*>) = ap

> instance Monad m => Monad (MaybeT m) where
>     return  = MaybeT . return . return
>     m >>= k = MaybeT $ runMaybeT m >>= maybe (return Nothing)
>                                              (runMaybeT . k)

Daarnaast instantiëren we ook nog alternative voor MaybeT. Alternative omvat `empty` en `<|>`. `(<|>)` is een associatieve binary operatie, die meestal zijn argumenten zal combineren. `empty` is de identiteit van `<|>`, m.a.w. `empty <|> a = a = a <|> empty`.

> instance Monad m => Alternative (MaybeT m) where
>     empty   = MaybeT (return Nothing)
>     x <|> y = MaybeT $ do x' <- runMaybeT x
>                           y' <- runMaybeT y
>                           return $ x' <|> y'
>                           -- Maybe is ook Alternative

Tenslotte schrijven we een MonadTrans instantie voor MaybeT, zodat we waarden van onze interne monad `m` kunnen liften tot `MaybeT m`.

< instance MonadTrans MaybeT where
<     lift m = MaybeT $ do m' <- m
<                          return (Just m')

ofwel (met `=<<` de ge`flip`te versie van `>>=`)

> instance MonadTrans MaybeT where
>     lift m = MaybeT $ return . Just =<< m

Tenslotte schrijven we ons `Stack` type waar we onze monads rond zullen gooien.

> type Stack = [Int]

De `push` functie zal een element bovenop de stack plaatsen. Deze operatie kan niet mislukken (we gaan er even vanuit dat we de geheugenlimiet van de computer niet overschreiden), dus is een `State` monad voldoende.

> push :: Int -> State Stack ()
> push x = modify (x:)

Ook `size` kan niet mislukken.

> size :: State Stack Int
> size = length <$> get

Onze `pop` functie kan echter wel mislukken, als de stack leeg is.  We `guard`en hiertegen door eerst de grootte met `0` te vergelijken. Om onze niet-MaybeT functie te kunnen gebruiken binnen de `MaybeT` monad, liften we ze.

> pop :: MaybeT (State Stack) Int
> pop = do s <- lift size
>          guard $ s > 0
>          (i:is) <- lift get
>          lift $ put is
>          return i

We werken deze functie nog eens uit voor de duidelijkheid.

< pop :: MaybeT (State Stack) Int
< pop = do s      <- lift size
<          _      <- guard (s > 0)
<          (i:is) <- lift get
<          _      <- lift (put is)
<          return i

< pop :: MaybeT (State Stack) Int
< pop = do lift size     >>= \s      -> $
<          guard (s > 0) >>= \_      -> $
<          lift get      >>= \(i:is) -> $
<          lift (put is) >>= \_      -> $
<          return i


> add :: MaybeT (State Stack) ()
> add = do a <- pop
>          b <- pop
>          lift $ push (a + b)

> mul :: MaybeT (State Stack) ()
> mul = do a <- pop
>          b <- pop
>          lift $ push (a * b)


IO vanuit de stack
------------------

Om te kunnen printen binnenin onze programma's, hebben we er `IO` nodig. We vervangen onze `StateT Stack Identity` (geschreven als `State Stack`) door een `StateT Stack IO`, en herschrijven onze stack-functies. Merk op dat we ditmaal ook `push` en `size` al in de `MaybeT` transformer plaatsen, om een andere strategie te demonstreren.

De basisfuncties blijven zeer gelijkaardig.

> pushIO :: Int -> MaybeT (StateT Stack IO) ()
> pushIO x = lift $ modify (x:)

> sizeIO :: MaybeT (StateT Stack IO) Int
> sizeIO = lift $ length <$> get

> popIO :: MaybeT (StateT Stack IO) Int
> popIO = do s <- sizeIO
>            guard $ s > 0
>            (i:is) <- lift get
>            lift (put is)
>            return i

> addIO :: MaybeT (StateT Stack IO) ()
> addIO = do a <- popIO
>            b <- popIO
>            pushIO (a + b)

> mulIO :: MaybeT (StateT Stack IO) ()
> mulIO = do a <- popIO
>            b <- popIO
>            pushIO (a * b)

En we krijgen een nieuwe functie `echo`, die de bovenste waarde van de stack zal printen.

> echo :: MaybeT (StateT Stack IO) ()
> echo = do i <- popIO
>           lift . liftIO . putStrLn . show $ i
>           pushIO i

We schrijven tenslotte nog een hulpfunctie om nu een hoop operaties uit te voeren op een lege stack.

> runMSIO :: MaybeT (StateT Stack IO) a -> IO (Maybe a)
> runMSIO program = do (x, s) <- runStateT (runMaybeT program) []
>                      return $ if null s then x else Nothing

We kunnen nu programma's uitvoeren met deze functie. Merk op dat we vereisen dat de stack leeg is op het einde van het programma om een geldige berekening te hebben.

    *Main> runMSIO (pushIO 3 >> pushIO 4 >> echo >> mulIO >> popIO)
    4
    Just 12

