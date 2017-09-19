module Kodachi.GCode.Internal.Types where

import           Data.ByteString (ByteString)
import           Data.Scientific (Scientific)

data Token
    = Tok_WhiteSpace !ByteString
    | Tok_Comment !ByteString
    | Tok_GCode !Char !Scientific
    deriving (Eq, Show)
