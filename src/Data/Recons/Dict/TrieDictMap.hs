{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
module Data.Recons.Dict.TrieDictMap where

import Data.Monoid ((<>))
import qualified Data.Map as M
import Data.Maybe

class TrieDictMapKey k where
  data GTrieDictMap k :: * -> *

  empty  :: GTrieDictMap k v

  singleton :: k -> v -> GTrieDictMap k v
  singleton k v = fromList [(k,v)]

  toList :: GTrieDictMap k v -> [(k,v)]

  union :: GTrieDictMap k v -> GTrieDictMap k v -> GTrieDictMap k v
  union a b = fromList (toList a <> toList b)

  unions :: [GTrieDictMap k v] -> GTrieDictMap k v
  unions = fromList . foldMap toList

  unionWith :: (v -> v -> v) -> GTrieDictMap k v -> GTrieDictMap k v -> GTrieDictMap k v
  unionWith f a b = fromListWith f (toList a <> toList b)

  unionsWith :: (v -> v -> v) -> [GTrieDictMap k v] -> GTrieDictMap k v
  unionsWith fn = fromListWith fn . foldMap toList

  fromList :: [(k,v)] -> GTrieDictMap k v
  fromList = fromListWith (\a b -> b)

  fromListWith :: (v -> v -> v) -> [(k,v)] -> GTrieDictMap k v
  fromListWith fn elems = foldl (\t (k,v) -> insertWith fn k v t) empty elems

  insertWith :: ( v -> v -> v ) -> k -> v -> GTrieDictMap k v -> GTrieDictMap k v

  insert :: k -> v -> GTrieDictMap k v -> GTrieDictMap k v
  insert = insertWith (\a b -> b)

  lookup :: k -> GTrieDictMap k v -> Maybe v

  lookupPartial :: k -> GTrieDictMap k v -> GTrieDictMap k v

instance (TrieDictMapKey k) => Monoid (GTrieDictMap k v) where
  mempty = empty
  mappend = union

