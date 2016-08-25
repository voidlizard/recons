{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Recons.Parser where

import Data.Typeable
import Data.Vinyl
import GHC.TypeLits
import Servant.API
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read

type family APIToFields layout :: [(Symbol, *)] where
  APIToFields (QueryParam s a :> b) = '(s, a) ': APIToFields b
  APIToFields (a :> b) = APIToFields b
  APIToFields (Verb a b c d) = '[]

class ReadParam api where
  type TParam api :: *
  readParam :: Proxy api -> String -> Maybe (TParam api)

instance (KnownSymbol s, Read Text) => ReadParam (QueryParam s Text) where
  type TParam (QueryParam s Text) = Text
  readParam _ = Just . T.pack

instance (KnownSymbol s, Read String) => ReadParam (QueryParam s String) where
  type TParam (QueryParam s String) = String
  readParam _ = Just

instance {-# OVERLAPPABLE #-} (KnownSymbol s, Read t) => ReadParam (QueryParam s t) where
  type TParam (QueryParam s t) = t
  readParam _ s = readMaybe s

class ReconsParser api where
  type ParseResult api :: *

  parser :: forall proxy . proxy api -> [String] -> Maybe (ParseResult api)

-- FIXME: threats bad param as Nothing so the command is parsed ok in this case.
-- this may cayse to call api with wrong parameters, although it's harmless.

instance (
    FieldRec (APIToFields b) ~ ParseResult b
  , ReconsParser b
  , KnownSymbol s
  , ReadParam (QueryParam s a)
  , Read a
  ) => ReconsParser (QueryParam s a :> b) where

  type ParseResult (QueryParam s a :> b) = FieldRec ( '(s, a) ': APIToFields b)

  parser _ ss = case ss of
    [] -> Nothing
    (s : ss') -> (:&)
      <$> (fmap Field (readParam qp s) :: Maybe (ElField '(s, a)) )
      <*> (parser (Proxy :: Proxy b) ss')

    where qp = Proxy :: Proxy (QueryParam s a)

instance (
    ReconsParser b
  , FieldRec (APIToFields b) ~ ParseResult b
  , KnownSymbol s
  ) => ReconsParser (s :> b) where

  type ParseResult (s :> b) = ParseResult b

  parser _ ss = case ss of
    [] -> Nothing
    (s:ss') | s == sv -> parser (Proxy :: Proxy b) ss'
            | otherwise -> Nothing

    where sv = symbolVal (Proxy :: Proxy s)

instance ReconsParser (Verb (a :: StdMethod) b c d) where
  type ParseResult (Verb a b c d) = FieldRec '[]
  parser _ [] = Just RNil
  parser _ _  = Nothing

class HasPrefix api where
  prefix :: Proxy api -> [Maybe String]

instance (HasPrefix rest, KnownSymbol s) => HasPrefix (s :> rest) where
  prefix _ = Just ( symbolVal (Proxy :: Proxy s) ) : prefix (Proxy :: Proxy rest)

instance (HasPrefix rest) => HasPrefix (QueryParam s t :> rest) where
  prefix _ = Nothing : prefix (Proxy :: Proxy rest)

instance {-# OVERLAPPABLE #-} (HasPrefix rest) => HasPrefix (whatever :> rest) where
  prefix _ = prefix (Proxy :: Proxy rest)

instance HasPrefix (Verb a b c d) where
  prefix _ = []

