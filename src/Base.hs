module Base where

type Texel = (Float, Float)

newtype TraceError
 = GeneralError String
 deriving (Show, Eq)

abort :: String -> Either TraceError b
abort = Left . GeneralError