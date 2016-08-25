module CLI.Recons (ErrorMsg,tokenize) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text as A
import Data.Attoparsec.Recons as P
import qualified Data.Text as T

type ErrorMsg = String

tokenize :: String -> Either ErrorMsg [String]
tokenize s = parseOnly p (T.pack s)
  where p = skipSpace >> (atom <|> stringLiteral) `sepBy'` (many1 space)
        atom = T.unpack <$> P.atom
        stringLiteral = T.unpack <$> P.stringLiteral

