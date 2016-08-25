{-# Language TypeFamilies #-}
{-# Language GADTs #-}
module Data.Recons.Dict (
                          TrieDictSkipKey(..)
                        , makeKey
                        , module TDict
                        ) where

import Data.Recons.Dict.Internal.TrieDictMap()
import Data.Recons.Dict.Internal.TrieDictSkipMap (TrieDictSkipKey(..),makeKey)
import Data.Recons.Dict.TrieDictMap as TDict
