module Debug where

import Debug.Trace (trace)

traceNothing :: String -> Maybe a
traceNothing msg = trace msg Nothing
