import System.Random

breedte = 64
hoogte = 10000000

ftie (coord, gen) = ((fst (randomR (1, breedte) gen) , fst (randomR (1,hoogte) (snd (randomR (1, breedte) gen)))), snd (randomR (1,hoogte) (snd (randomR (1, breedte) gen))))