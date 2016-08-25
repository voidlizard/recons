{-# Language TypeFamilies #-}
module Data.Recons.Dict.Internal.TrieDictSkipMap where

import Data.Monoid
import qualified Data.Map as M
import Data.Recons.Dict.TrieDictMap as T

newtype TrieDictSkipKey k = TrieDictSkipKey { unwrap :: [Maybe k] }
                            deriving (Eq,Show)


instance Monoid (TrieDictSkipKey k) where
  mempty = TrieDictSkipKey []
  mappend (TrieDictSkipKey s1) (TrieDictSkipKey s2) = TrieDictSkipKey (s1 <> s2)

instance Functor TrieDictSkipKey where
  fmap f (TrieDictSkipKey q) = TrieDictSkipKey (fmap (fmap f) q)

type TrieImpl k v = GTrieDictMap (TrieDictSkipKey k) v

instance Ord k => TrieDictMapKey (TrieDictSkipKey k) where
  data GTrieDictMap (TrieDictSkipKey k) v = Nil
                                          | NodeA (Maybe v) (M.Map k (TrieImpl k v))
                                          | NodeB (Maybe v) (TrieImpl k v)
                                          deriving (Eq,Show)

  empty = Nil

  lookup  k trie = find (unwrap k) trie
    where find _ Nil = Nothing
          find (Just x:xs) (NodeA _ tries) = maybe Nothing (find xs) (M.lookup x tries)
          find (Nothing:_) (NodeA _ _) = Nothing
          find (_:xs) (NodeB _ trie) = find xs trie
          find [] (NodeA mv _) = mv
          find [] (NodeB mv _) = mv

  lookupPartial k t = find (unwrap k) t
    where find _ Nil = Nil
          find (Just x:xs) (NodeA _ mtries) = maybe Nil (find xs) (M.lookup x mtries)
          find (Nothing:_) (NodeA _ _) = Nil
          find (_:xs) (NodeB _ trie) = find xs trie
          find [] e = e

  toList trie = go mempty trie
    where

      go :: TrieDictSkipKey k -> TrieImpl k v -> [(TrieDictSkipKey k, v)]

      go _ Nil = []

      go key (NodeA mv mtries) = r
        where r = maybe [] (\v -> [(key,v)]) mv <> foldMap (\(k,t) -> go (key <> keyA k) t) (M.toList mtries)

      go key (NodeB mv trie)   = r
        where r = maybe [] (\v -> [(key,v)]) mv <> go (key <> keyB) trie

      keyA = TrieDictSkipKey . pure . pure
      keyB = (TrieDictSkipKey . pure) Nothing

  insertWith fn k v trie = ins (unwrap k) trie
    where
      ins [] Nil   = NodeA (pure v) M.empty
      ins [] (NodeA mv mtries) = NodeA (Just $ maybe v (`fn` v) mv) mtries
      ins [] (NodeB mv trie)   = NodeB (Just $ maybe v (`fn` v) mv) trie

      ins (Nothing:xs) Nil = NodeB Nothing (ins xs Nil)
      ins (Nothing:xs) (NodeA mv mtries) = NodeB mv (ins xs (unionsWith fn $ M.elems mtries))
      ins (Nothing:xs) (NodeB mv trie) = NodeB mv (ins xs trie)

      ins (Just x:xs) Nil = NodeA Nothing (M.singleton x (ins xs Nil))
      ins (Just x:xs) (NodeA mv mtries) = NodeA mv (M.alter (alt xs) x mtries)
      ins (Just _:xs) (NodeB mv trie) = NodeB mv (ins xs trie)

      alt xs = Just . maybe (ins xs empty) (ins xs)

makeKey :: [k] -> TrieDictSkipKey k
makeKey = TrieDictSkipKey . fmap pure

