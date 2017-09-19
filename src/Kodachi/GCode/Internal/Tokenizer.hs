module Kodachi.GCode.Internal.Tokenizer where

import           Kodachi.GCode.Internal.Types     (Token (..))

import           Control.Applicative              (many, (<|>))
import           Data.Attoparsec.ByteString       (Parser, choice, endOfInput,
                                                   takeTill, takeWhile1)
import qualified Data.Attoparsec.ByteString.Char8 as C8 (anyChar, char,
                                                         endOfLine, isEndOfLine,
                                                         isSpace_w8, scientific)

import           Prelude                          hiding (takeWhile)



tokenListParser :: Parser [Token]
tokenListParser = many tokenParser


tokenParser :: Parser Token
tokenParser = choice
              [ whitespaceParser
              , commentParser
              , gcodeParser
              ]

  where

    whitespaceParser :: Parser Token
    whitespaceParser = Tok_WhiteSpace
                   <$> takeWhile1 C8.isSpace_w8

    commentParser :: Parser Token
    commentParser = Tok_Comment
                <$> (  C8.char ';'
                    *> takeWhile1 (not . C8.isEndOfLine)
                    <* (C8.endOfLine <|> endOfInput)
                    )

    gcodeParser :: Parser Token
    gcodeParser = Tok_GCode
              <$> C8.anyChar
              <*> C8.scientific
