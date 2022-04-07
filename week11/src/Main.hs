module Week11Opgave where

import ReactiveMBot
import Reactive.Banana.Frameworks (MomentIO, reactimate)

main = do mbot <- newMBot
          runMBot (avoidWall mbot) mbot

drive :: ReactiveMBot -> Float -> IO ()
drive mbot dist | dist > 20 = forward mbot
                | otherwise = left mbot

setLights :: ReactiveMBot -> Float -> IO ()
setLights mbot dist | dist > 20 = green leftLight mbot
                    | otherwise = blue leftLight mbot

-- Combinatie van beide. We <$>-mappen onze stroom van afstanden naar een
-- Event (IO ()), of een stroom van reacties op die afstanden. Met reactimate
-- brengen we dit in het MomentIO () netwerk.

avoidWall :: ReactiveMBot -> MomentIO ()
avoidWall mbot = do dist <- distance mbot
                    reactimate $ (setLights mbot) <$> dist
                    reactimate $ (drive mbot) <$> dist
