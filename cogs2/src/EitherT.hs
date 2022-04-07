module EitherT where

import Control.Monad.Trans
import ParserMonad

errChar c = "can't find a \'"++[c]++"\'."
errString s = "can't find \""++s++"\"."
errCoord = "invalid (y,x)-pair found:"
errNumber = "can't find a number."
errParse = "Error while parsing: "
errNot = "expected more input."
errAmb = "ambiguous parse."

parse' :: EitherT e Parser a -> String -> Either e a
parse' m s = one [ x | (x,t) <- apply m s, t==""]
  where
    one []        = Left errParse++errNot
    one [x]       = x
    one xs | length xs > 1 = Left errParse++errAmb
