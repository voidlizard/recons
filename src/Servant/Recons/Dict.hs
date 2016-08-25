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

module Servant.Recons.Dict  (makeDict,Prefix,PrefixPart(..)) where

import Data.List (unfoldr,nub)
import Data.Monoid ((<>))
import Data.Typeable
import GHC.TypeLits
import Servant.API

import Data.Recons.Dict
import Data.Recons.Dict.TrieDictMap

data PrefixPart = W String | Q String
                  deriving (Eq,Ord,Show)

type Prefix = [PrefixPart]

class PrefixParts api where
  prefixParts :: forall proxy . proxy api -> Prefix

class BranchPrefix api where
  prefixes :: Proxy api -> [Prefix]

instance (BranchPrefix a, BranchPrefix b) => BranchPrefix  ( a :<|> b ) where
  prefixes _ = prefixes (Proxy :: Proxy a) <> prefixes (Proxy :: Proxy b)

instance {-# OVERLAPPABLE #-} (PrefixParts a) => BranchPrefix a where
  prefixes _ = [prefixParts (Proxy :: Proxy a)]

instance (PrefixParts b, KnownSymbol sym) => PrefixParts (sym :> b) where
  prefixParts _ = W (symbolVal (Proxy :: Proxy sym)) : prefixParts (Proxy :: Proxy b)

instance (PrefixParts b, KnownSymbol sym) => PrefixParts (QueryParam sym t :> b) where
  prefixParts _ = Q (symbolVal (Proxy :: Proxy sym)) : prefixParts (Proxy :: Proxy b)

instance PrefixParts (Verb (a :: StdMethod) b c d) where
  prefixParts _ = []

makeDict :: BranchPrefix api => Proxy api -> GTrieDictMap (TrieDictSkipKey String) [PrefixPart]
makeDict p = fromListWith merge $ foldMap entriesOf (prefixes p)
  where ps :: [Prefix]
        ps = prefixes p
        merge a b = nub (a <> b)

entriesOf :: Prefix -> [(TrieDictSkipKey String, [PrefixPart])]
entriesOf p = unfoldr fn (mempty,p)
  where
        fn (prefix,(p:ps)) = Just ((prefix,[p]), (prefix <> newKey p,ps))
        fn (_,[]) = Nothing

        newKey (Q s) = TrieDictSkipKey [Nothing]
        newKey (W s) = TrieDictSkipKey [Just s]
