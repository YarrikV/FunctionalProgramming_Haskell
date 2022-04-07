
-- We beginnen onze bibliotheek met onze publieke interface te declareren.

module ReactiveMBot ( ReactiveMBot
                    , newMBot, runMBot
                    , distance, leftScanner, rightScanner
                    , left, forward, right, backward
                    , red, green, blue
                    , LED, leftLight, rightLight
                    ) where

-- We importeren onder andere onze oorspronkelijke MBot bibliotheek en het *reactive programming framework* dat we zullen gebruiken, [Reactive Banana](http://hackage.haskell.org/package/reactive-banana).

import qualified MBot as M
import           Reactive.Banana.Combinators (Event)
import qualified Reactive.Banana.Frameworks as RBF
import           Reactive.Banana.Frameworks (MomentIO)
import           Control.Event.Handler      as CEH
import           Control.Monad

-- Lees de documentatie van MBot en het Reactive Banana framework. Probeer daarmee onderstaande (reeds gedocumenteerde functies) te implementeren.

-- | A Wrapper around the MBot library for Reactive Programming
data ReactiveMBot = ReactiveMBot { device :: M.Device
                                 , adh :: AddHandler Float
                                 , fire :: Handler Float}

-- | Create a new MBot connect
newMBot :: IO ReactiveMBot
newMBot = do dev <- M.openMBot
             (adh, fire) <- CEH.newAddHandler
             return $ ReactiveMBot dev adh fire

-- | The distance to the closest object the MBot can see
distance :: ReactiveMBot -> MomentIO (Event Float)
distance b = RBF.fromAddHandler $ adh b


-- | Whether the left scanner on the MBot sees a black surface.
leftScanner :: ReactiveMBot -> MomentIO (Event Bool)
leftScanner = undefined

-- | Whether the right scanner on the MBot sees a black surface.
rightScanner :: ReactiveMBot -> MomentIO (Event Bool)
rightScanner = undefined

send :: M.Command -> ReactiveMBot -> IO ()
send com rbot = M.sendCommand (device rbot) com

-- | Tells the MBot to go forward.
forward :: ReactiveMBot -> IO ()
forward = send (M.setMotor 1 1)

-- | Tells the MBot to turn left.
left :: ReactiveMBot -> IO ()
left = send (M.setMotor 0 1)

-- | Tells the MBot to turn right.
right :: ReactiveMBot -> IO ()
right = send (M.setMotor 1 0)

-- | Tells the MBot to go backwards
backward :: ReactiveMBot -> IO ()
backward = send (M.setMotor (-1) (-1))

-- | A controllable LED on the MBot
type LED = Int

-- | The left LED on the MBot
leftLight = 1 :: LED

-- | The right LED on the MBot
rightLight = 2 :: LED

-- | Emits green light on the given LED.
green :: LED -> ReactiveMBot -> IO ()
green led rbot = undefined

-- | Emits blue light on the given LED.
blue :: LED -> ReactiveMBot -> IO ()
blue led rbot = undefined

-- | Emits red light on the given LED.
red :: LED -> ReactiveMBot -> IO ()
red led rbot = undefined

-- Tenslotte hebben we onze runMBot functie.
--Deze zal een gegeven netwerk compileren en uitvoeren.
-- Enerzijds worden de sensoren herhaaldelijk uitgelezen met de
--oorspronkelijke bibliotheek en naar de invoer van ons netwerk gestuurd.
-- Anderzijds activeren we het netwerk bijvooorbeeld elke 3000µs.

-- | Compile and run a ReactiveMBot network
runMBot :: MomentIO () -> ReactiveMBot -> IO ()
runMBot mom rbot = do network <- RBF.compile mom
                      forever $ do  f <- M.readUltraSonic (device rbot)
                                    fire rbot f
                                    RBF.actuate network
