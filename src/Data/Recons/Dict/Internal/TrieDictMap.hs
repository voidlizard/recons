{-# Language TypeFamilies #-}
module Data.Recons.Dict.Internal.TrieDictMap ( T.empty
                                             , T.lookup
                                             , T.lookupPartial
                                             , T.insertWith) where

import qualified Data.Map as M
import Data.Recons.Dict.TrieDictMap as T
import Data.Monoid ((<>))

instance Ord k => TrieDictMapKey [k] where
  data GTrieDictMap [k] v = TNil | TNode (Maybe v) (M.Map k (GTrieDictMap [k] v))
                            deriving (Eq,Show)

  empty = TNil

  lookup  k trie = find k trie
    where find _ TNil = Nothing

          find (x:xs) (TNode _ tries) = maybe Nothing (find xs) (M.lookup x tries)
          find [] (TNode mv _) = mv

  lookupPartial k t = find k t
    where find _ TNil = TNil
          find (x:xs) (TNode _ mtries) = maybe TNil (find xs) (M.lookup x mtries)
          find [] e = e

  insertWith fn k v trie = ins k trie
    where
      ins (x:xs) TNil = TNode Nothing (M.singleton x (ins xs TNil))
      ins [] TNil     = TNode (Just v) M.empty

      ins (x:xs) (TNode mv mtries) = TNode mv (M.alter (alt xs) x mtries)
      ins []     (TNode mv mtries) = TNode (Just $ maybe v (`fn` v) mv) mtries

      alt xs = Just . maybe (ins xs empty) (ins xs)

  toList trie = go [] trie
    where
      go _ TNil = []
      go key (TNode mv mtries) = r
        where r = maybe [] (\v -> [(key,v)]) mv <> foldMap (\(k,t) -> go (key <> [k]) t) (M.toList mtries)

