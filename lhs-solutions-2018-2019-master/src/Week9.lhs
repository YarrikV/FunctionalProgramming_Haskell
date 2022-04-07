
Oplossingen Reeks 9
===================

> module Week9 where
> import Prelude hiding (read)
> import Control.Monad (when)
> import Data.Maybe (isJust)
> import Control.Concurrent (MVar, newEmptyMVar, putMVar,
>                            takeMVar, forkIO, threadDelay)
> import Control.Concurrent.STM (STM, TVar, newTVar, readTVar,
>                                writeTVar, retry, atomically)

Logging in Threads
------------------

We definiëren we nieuwe types: een logger en een logcommando. Een logger houdt gewoon een `MVar LogCommand` bij, waarmee we er boodschappen naar zullen versturen. Een `LogCommand` is ofwel een boodschap in de vorm van een `String`, ofwel een stop-commando.

Aan het stop-commando wordt een tweede `MVar` meegegeven, met een `()` als type. We kunnen dus geen effectieve waarden hierin plaatsen. Deze kan echter wel gebruikt worden om een signaal te sturen: één thread kan wachten op zo'n lege `MVar ()` tot een andere er iets in plaatst. We zullen deze `MVar` gebruiken om vanuit de log-thread te signaliseren aan de main-thread dat onze logger afgesloten is, zodat het sturen van een stop-commando een synchrone actie wordt.

> data Logger = Logger (MVar LogCommand)
> data LogCommand = Message String | Stop (MVar ())

Met `mkLogger` maken we een nieuwe logger aan. We maken een lege `MVar` en zullen die enerzijds teruggeven, anderzijds in meegeven aan een tweede (nieuwe) thread die de boodschappen zal uitschrijven.

> mkLogger :: IO Logger
> mkLogger = do m <- newEmptyMVar
>               let l = Logger m
>               _ <- forkIO (runLogger l)
>               return l

Als we de gegeven code van `logMessage` bekijken, zien we hoe we een nieuwe boodschap in onze `MVar` plaatsen. Het houdt dus steek dat we in onze andere thread (`runLogger`) zullen wachten tot we een boodschap uit onze `MVar` kunnen halen en die dan loggen.

> logMessage :: Logger -> String -> IO ()
> logMessage (Logger m) s = putMVar m (Message s)

In `runLogger` zullen we dus ons commando uit de `MVar` nemen (mogelijks blokkeren tot er iets in geplaatst wordt). Hiervoor gebruiken we `takeMVar`. We kijken wat voor commando we krijgen.

Is het een boodschap, kunnen we die gewoon printen met `putStrLn`, en zullen we onszelf recursief aanroepen om te blokkeren tot er een tweede boodschap komt. We stoppen hier ook eventueel een `threadDelay` in om het duidelijk te maken dat er iets gebeurt in de thread.

Is het een stop-commando, dan printen we ook een lijntje. Daarna zullen we signaliseren aan de main-thread dat we ons werk gedaan hebben door de meegegeven `f :: MVar ()` in te vullen.

We maken hier gebruik van de `>>` ("then") functie, een variant van `>>=` die het resultaat van de eerste stap negeert.

> -- Given a logger, read the logcommand and execute it.
> -- Message => print the message and "loop"
> -- Stop    => print "logger: stop" 
> runLogger :: Logger -> IO ()
> runLogger (Logger m) = do
>     message <- takeMVar m
>     case message of
>          (Message str) -> threadDelay (1000000 * length str)
>                        >> putStrLn str
>                        >> runLogger (Logger m)
>          (Stop f)      -> putStrLn "logger: stop"
>                        >> putMVar f ()

De stop-functie zal dus een nieuwe lege `MVar` aanmaken en deze meegeven via de logger-`MVar`. Daarna zal hij blokkeren met `takeMVar` tot er iets in die nieuwe `MVar` ingevuld wordt.

> logStop :: Logger -> IO ()
> logStop (Logger m) = do finished <- newEmptyMVar
>                         putMVar m (Stop finished)
>                         takeMVar finished

MVar's met STM
--------------

We proberen het gekende gedrag van `MVar`s te emuleren met behulp van `TVar`s. In een `TVar` moet altijd een waarde zitten, dus gebruiken we de waarde `Nothing` om lege `MVar` te simuleren.

> newtype STMMVAR a = STMMVAR (TVar (Maybe a))

Een nieuwe lege `STMMVAR` zal dus bestaan uit een nieuwe `TVar` met daarin een `Nothing`.

> newEmptySTMMVAR :: STM (STMMVAR a)
> newEmptySTMMVAR = STMMVAR <$> newTVar Nothing

Als we nu de waarde van een `STMMVAR` willen uitlezen, zijn er twee mogelijkheden. Er kan een `Nothing` of een `Just _` in zitten. Een `MVar` blokkeert bij het nemen van een lege waarde, dus dat willen wij ook doen. We zullen dat doen door opnieuw te proberen tot we een `Just _` waarde vinden.

Hiervoor gebruiken we de `STM` functie `retry`. We zouden ook `takeSTMMVAR` recursief kunnen aanroepen. Dat zorgt echter voor "actief wachten" (zonder tussenpauze de hele tijd opnieuw proberen), terwijl `retry` "passief wachten" zal voorzien. `retry` zal namelijk pas opnieuw proberen als de inhoud van één van de aangesproken `TVar`s wijzigt.

< takeSTMMVAR :: STMMVAR a -> STM a
< takeSTMMVAR (STMMVAR t) = do v <- readTVar t
<                              case v of
<                                  Nothing -> retry
<                                  Just v' -> return v'

Korter met `maybe`.

> takeSTMMVAR :: STMMVAR a -> STM a
> takeSTMMVAR (STMMVAR t) = readTVar t >>= maybe retry return

Voor `putSTMMVAR` doen we vrijwel hetzelfde, maar we zullen opnieuw proberen als er al een waarde in de `STMMVAR` zit en onze eigen waarde er in plaatsen anders.

> putSTMMVAR :: STMMVAR a -> a -> STM ()
> putSTMMVAR (STMMVAR t) a = do v <- readTVar t
>                               case v of
>                                 Nothing -> writeTVar t (Just a)
>                                         >> return ()
>                                 Just _  -> retry

Omdat één van onze takken kort is, en de andere vrij lang, kunnen we hier heel mooi `when` gebruiken. `when :: Bool -> m () -> m ()` zal een actie voorwaardelijk uitvoeren in een monad.

< putSTMMVAR :: STMMVAR a -> a -> STM ()
< putSTMMVAR (STMMVAR t) a = do v <- readTVar t
<                               when (isJust v) retry
<                               writeTVar t (Just a)
<                               return ()

De rest van de code om onze oplossing te testen was hieronder gegeven. De functie `atomically` geeft de grenzen aan van de code die we willen `retry`en als er iets mis loopt.

> write :: STMMVAR String -> STMMVAR String -> IO ()
> write ta tb = do threadDelay 1000000
>                  atomically $ do putSTMMVAR ta "Hello"
>                                  putSTMMVAR tb "World"
>                                  return ()

> read :: STMMVAR String -> STMMVAR String -> IO (String, String)
> read ta tb = atomically $ do b <- takeSTMMVAR tb
>                              a <- takeSTMMVAR ta
>                              return (a,b)

> main :: IO ()
> main = do ta <- atomically newEmptySTMMVAR
>           tb <- atomically newEmptySTMMVAR
>           _  <- forkIO $ write ta tb
>           x  <- read ta tb
>           putStrLn $ show x
