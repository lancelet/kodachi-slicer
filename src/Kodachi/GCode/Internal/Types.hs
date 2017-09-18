module Kodachi.GCode.Internal.Types where

import           Data.ByteString.Streaming.Char8 (ByteString)
import           Data.Scientific                 (Scientific)

data Token m r
    = Tok_Whitespace (ByteString m r)
    | Tok_Comment (ByteString m r)
    | Tok_GCode !Char !Scientific
