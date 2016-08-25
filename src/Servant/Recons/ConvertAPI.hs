{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Recons.ConvertAPI (ConvertAPI(..),convert) where

import Data.Typeable
import Data.Vinyl
import GHC.TypeLits
import Servant.API
import Servant.Client(HasClient(..),client)

import Servant.Recons.Parser

class HasClient api => ConvertAPI api input where
  type FinalType api :: *

  convertClient :: Proxy api -> input -> Client api -> FinalType api

instance (
    ConvertAPI b (FieldRec as)
  , KnownSymbol s
  , ToHttpApiData a
  ) => ConvertAPI (QueryParam s a :> b) (FieldRec ( '(s, a) ': as)) where
  type FinalType (QueryParam s a :> b) = FinalType b

  convertClient _ (Field a :& as) cl = convertClient (Proxy :: Proxy b) as (cl $ Just a)

instance (
    ConvertAPI b (FieldRec as)
  , KnownSymbol s
  ) => ConvertAPI (s :> b) (FieldRec as) where
  type FinalType (s :> b) = FinalType b
  convertClient _ fs cl = convertClient (Proxy :: Proxy b) fs cl

instance (
    ReflectMethod a
  , MimeUnrender ct d
  ) => ConvertAPI (Verb (a :: StdMethod) b (ct ': cts) d) (FieldRec fields) where
  type FinalType (Verb a b (ct ': cts) d) = Client (Verb a b (ct ': cts) d)

  convertClient _ _ cl = cl

convert :: ( ConvertAPI api input
           , input ~ ParseResult api
           , HasClient api
           , ReconsParser api
           )
        => Proxy api -> Maybe input -> Maybe (FinalType api)

convert _ Nothing = Nothing

convert prx (Just input) = Just $! convertClient prx input (client prx)


