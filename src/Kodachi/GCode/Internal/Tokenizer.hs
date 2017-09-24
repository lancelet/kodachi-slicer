{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Kodachi.GCode.Internal.Tokenizer where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import Data.Word8 (_parenleft, _parenright, _semicolon, _tab, _plus, _hyphen, _period)
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Byte as P

data PosToken
    = PosToken !P.SourcePos !Token
    deriving (Eq, Show)

data Token
    = TokLetter !Word8
    | TokDigit !Word8
    | TokPeriod
    | TokPlus
    | TokMinus
    | TokCmtInline !ByteString
    | TokCmtEol !ByteString
    deriving (Eq, Show)

type Error = P.ParseError Word8 Void

posTokensParser :: P.Parsec Void ByteString [PosToken]
posTokensParser = parsePosTokens

type Parser e s m = ( P.MonadParsec e s m
                    , P.Token s ~ Word8
                    , P.Tokens s ~ ByteString )

-- | Parses a list of positioned tokens.
parsePosTokens :: (Parser e s m) => m [PosToken]
parsePosTokens = P.many parsePosToken <* P.eof

-- | Parses a positioned token.
parsePosToken :: (Parser e s m) => m PosToken
parsePosToken = PosToken <$> P.getPosition <*> (parseToken <* P.space)

-- | Parses a single token.
parseToken :: (Parser e s m) => m Token
parseToken = letter
         <|> digit
         <|> period
         <|> plus
         <|> minus
         <|> cmtInline
         <|> cmtEol
  where

    letter = TokLetter <$> P.letterChar
    digit  = TokDigit  <$> P.digitChar

    period = P.char _period *> pure TokPeriod
    plus   = P.char _plus   *> pure TokPlus
    minus  = P.char _hyphen *> pure TokMinus

    cmtInline = TokCmtInline <$> (st *> cm <* en)
      where
        st = P.char _parenleft
        en = P.char _parenright
        cm = P.takeWhileP (Just "inline comment contents") isCommentInlineChar

    cmtEol = TokCmtEol <$> (st *> cm)
      where
        st = P.char _semicolon
        cm = P.takeWhileP (Just "eol comment contents") isCommentEOLChar

-------------------------------------------------------------------------------
-- Character classes

isCommentInlineChar :: Word8 -> Bool
isCommentInlineChar c
    = (c >= 32 && c <= 40)
   || (c >= 42 && c <= 126)
   || (c == _tab)

isCommentEOLChar :: Word8 -> Bool
isCommentEOLChar c
    = (c >= 32 && c <= 126)
   || (c == _tab)
