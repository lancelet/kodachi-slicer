module Kodachi.GCode.Internal.Tokenizer where

import Kodachi.GCode.Internal.Types (Token(..))

import qualified Data.Attoparsec.ByteString.Char8 as A (scientific, Parser)

tokParser :: A.Parser (Token m r)
tokParser = undefined
