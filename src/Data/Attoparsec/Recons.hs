{-# Language OverloadedStrings #-}
module Data.Attoparsec.Recons (atom,stringLiteral) where


import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Char as C
import Data.Maybe
import Data.Text as T hiding (zip)

import qualified Data.List as L

atom :: Parser Text
atom = A.takeWhile1 (inClass ".~!?@#$%^&*+-=_{}/0-9a-zA-Z")

stringLiteral :: Parser Text
stringLiteral =  do
  char '"'
  s <- T.pack <$> many (quotedChr <|> normalChr)
  char '"'
  return s

  where
    esc = "\\\"rntbfa"
    lit = zip esc "\\\"\r\n\t\b\f\a" :: [(Char, Char)]

    quotedChr = choice [ string "\\x" >> hexadecimal >>= return . C.chr

                       , char '\\' >> decimal >>= return . C.chr

                       , char '\\' >> (char 'o' <|> char 'O')
                                   >> A.takeWhile C.isOctDigit
                                   >>= return . C.chr . octal

                       , char '\\' >> satisfy (inClass esc)
                                   >>= return . fromJust . flip L.lookup lit
                       ]

    normalChr = satisfy (not.inClass "\\\"")

    octal = T.foldl (\acc c -> acc*8 + (C.ord c - C.ord '0')) 0


