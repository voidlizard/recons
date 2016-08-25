{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Recons.Client ( FromAPI(..)
                             , parse
                             , perform
                             , DictType
                             , module Dict
                             ) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Either
import Data.Monoid((<>))
import Data.Typeable
import GHC.Prim
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client

import Data.Recons.Dict as Dict
import Servant.Recons.ConvertAPI
import Servant.Recons.Parser

type family GetFinalType a where
  GetFinalType (Verb a b c d) = d
  GetFinalType ( a :> b ) = GetFinalType b

data Aux c = forall a . c a => Aux { unC :: a }

type ReconsClient a  = Manager -> BaseUrl -> ClientM a
type ReconsAction c  = [String] -> Maybe (ReconsClient (Aux c))

type DictType c = GTrieDictMap (TrieDictSkipKey String) (ReconsAction c)

class FromAPI c api where
  fromAPI :: Proxy c -> Proxy api  -> DictType c

instance (FromAPI c a, FromAPI c b) => FromAPI c ( a :<|> b ) where
  fromAPI pc _ = fromAPI pc (Proxy :: Proxy a) `union` fromAPI pc (Proxy :: Proxy b)

instance {-# OVERLAPPABLE #-}
        ( ReconsParser a
        , HasPrefix a
        , HasClient a
        , ConvertAPI a (ParseResult a)
        , out ~ (GetFinalType a)
        , FinalType a ~ ReconsClient out
        , c out
        ) => FromAPI c a where

  fromAPI _ api = Dict.singleton (TrieDictSkipKey (prefix api)) fn
    where fn = \ss -> fmap ek $ convert (Proxy :: Proxy a) (parser (Proxy :: Proxy a) ss)
                 where ek f = \mngr bu -> fmap Aux (f mngr bu)

parse :: ReconsAction c -> [String] -> Maybe (ReconsClient (Aux c))
parse action args = action args

perform :: (forall a . c a => a -> b)
        -> Manager
        -> BaseUrl
        -> ReconsClient (Aux c)
        -> IO (Either ServantError b)

perform  f man burl client = either (Left . id) (Right . unaux) <$> runExceptT (client man burl)
  where unaux (Aux r) = f r


