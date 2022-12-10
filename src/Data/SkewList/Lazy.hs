{-# LANGUAGE Safe #-}

-- | Skewed binary lists.
--
-- This module is intended to be imported qualified, to avoid name clashes with Prelude functions:
--
-- @
-- import qualified Data.SkewList.Lazy as Skew
-- @
--
module Data.SkewList.Lazy (
    SkewList (Cons, Nil),
    -- * Construction
    empty,
    singleton,
    cons,
    append,
    -- * Indexing
    (!),
    (!?),
    uncons,
    length,
    null,
    -- * Conversions
    toList,
    fromList,
    -- * Folding
    foldMap,
    foldMap',
    foldr,
    foldl',
    -- ** Indexed
    ifoldMap,
    ifoldr,
    -- * Mapping
    adjust,
    map,
    -- ** Indexed
    imap,
    itraverse,
    -- * Debug
    valid,
    explicitShow,
    explicitShowsPrec,
) where

import Data.SkewList.Lazy.Internal
import Prelude ()
