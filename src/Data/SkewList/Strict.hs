{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE Safe                   #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ViewPatterns           #-}
module Data.SkewList.Strict (
    SkewList (Cons, Nil), -- TODO: raname to SkewList
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

import Prelude
       (Bool (..), Eq ((==)), Functor (..), Int, Maybe (..), Num (..), Ord (..),
       Show (..), ShowS, String, fromIntegral, otherwise, seq, showChar,
       showParen, showString, ($), (&&), (.))

import Control.Applicative (Applicative (..), (<$>))
import Control.DeepSeq     (NFData (..))
import Data.Bits           (popCount, unsafeShiftL, unsafeShiftR, (.|.))
import Data.Hashable       (Hashable (..))
import Data.Monoid         (Monoid (..))
import Data.Semigroup      (Semigroup (..))
import Data.Word           (Word)
import GHC.Stack           (HasCallStack)

import qualified Data.Foldable    as I (Foldable (..))
import qualified Data.List        as L
import qualified Data.Traversable as I (Traversable (..))
import qualified Test.QuickCheck  as QC

import qualified Data.Foldable.WithIndex    as WI (FoldableWithIndex (..))
import qualified Data.Functor.WithIndex     as WI (FunctorWithIndex (..))
import qualified Data.Traversable.WithIndex as WI (TraversableWithIndex (..))

import qualified TrustworthyCompat as TC

import Prelude (error)

-- $setup
-- >>> import Prelude (Int, ($), (<>), (==), Bool (..))
-- >>> import Data.Char (toUpper)
-- >>> import Data.Hashable (hash)

-------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

-- | List with efficient random access.
--
-- Implemented using skewed binary.
--
data SkewList a
    = Nil
    | Cons_
        {-# UNPACK #-} !Word -- ^ size of the head tree
        !(Tree a)
        !(SkewList a)
  deriving (Eq, Functor, I.Traversable)

-- |
-- This instance provides total ordering, but this ordering /is not lexicographic/.
-- I.e. it is different order than on ordinary lists.
deriving instance Ord a => Ord (SkewList a)

-- | A complete binary tree (completeness not enforced)
data Tree a
    = Lf !a
    | Nd !a !(Tree a) !(Tree a)
  deriving (Eq, Ord, Show, Functor, I.Traversable)

-------------------------------------------------------------------------------
-- Validity
-------------------------------------------------------------------------------

-- | Check invariants.
--
-- * Trees are stored in increasing order.
--
-- * Only first two trees can have the same size.
--
-- * Tree sizes should be of form @2^n - 1@.
--
-- * Trees should be balanced.
--
valid :: SkewList a -> Bool
valid Nil                            = True
valid (Cons_ s  t Nil)               = validTree s t
valid (Cons_ s1 t1 (Cons_ s2 t2 xs)) =
    s1 <= s2 && validTree s1 t1 && validTree s2 t2 && valid' s2 xs

valid' :: Word -> SkewList a -> Bool
valid' _ Nil            = True
valid' p (Cons_ s t xs) = p < s && validTree s t && valid' s xs

validTree
    :: Word
    -> Tree a
    -> Bool
validTree size tree = popCount (size + 1) == 1 && go size tree
  where
    go 1 (Lf _)     = True
    go _ (Lf _)     = False
    go n (Nd _ l r) = go n' l && go n' r where n' = sizeDown n

-------------------------------------------------------------------------------
-- Size helpers
-------------------------------------------------------------------------------

sizeDown :: Word -> Word
sizeDown n = unsafeShiftR n 1
{-# INLINE sizeDown #-}

-- | Double plus one. @sizeUp n = 2 * n + 1@.
sizeUp :: Word -> Word
sizeUp n = unsafeShiftL n 1 .|. 1

-------------------------------------------------------------------------------
-- Patterns
-------------------------------------------------------------------------------

-- | 'Cons' and 'Nil' form complete pattern match.
pattern Cons :: a -> SkewList a -> SkewList a
pattern Cons x xs <- (uncons -> Just (x, xs))
  where Cons x xs = cons x xs

{-# COMPLETE Cons, Nil #-}

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance I.Foldable SkewList where
    foldMap = foldMap

#if MIN_VERSION_base(4,13,0)
    foldMap' = foldMap'
#endif

    foldr   = foldr
    foldl'  = foldl'

    length = length
    null   = null

    sum     = sum
    product = product

instance I.Foldable Tree where
    foldMap = foldMapTree
    foldr   = foldrTree
    null _  = False

instance NFData a => NFData (SkewList a) where
    rnf Nil            = ()
    rnf (Cons_ _ t xs) = rnf t `seq` rnf xs

instance NFData a => NFData (Tree a) where
    rnf (Lf a)     = rnf a
    rnf (Nd x l r) = rnf x `seq` rnf l `seq` rnf r

-- | The hash value are different then for an ordinary list:
--
-- >>> hash (fromList "foobar") == hash "foobar"
-- False
--
-- >>> hash (fromList "foo", fromList "bar") == hash (fromList "foobar", fromList "")
-- False
--
instance Hashable a => Hashable (SkewList a) where
    hashWithSalt salt Nil            = salt
        `hashWithSalt` (0 :: Int)
    hashWithSalt salt (Cons_ s t xs) = salt
        `hashWithSalt` s   -- s /= 1, acts as "constructor tag"
        `hashWithSalt` t
        `hashWithSalt` xs

instance Hashable a => Hashable (Tree a) where
    hashWithSalt = foldlTree' hashWithSalt

-- |
--
-- >>> fromList "abc" <> fromList "xyz"
-- "abcxyz"
--
instance Semigroup (SkewList a) where
    (<>) = append

instance Monoid (SkewList a) where
    mempty  = empty
    mappend = (<>)

instance WI.FunctorWithIndex Int SkewList where
    imap = imap

instance WI.FoldableWithIndex Int SkewList where
    ifoldMap = ifoldMap
    ifoldr   = ifoldr

instance WI.TraversableWithIndex Int SkewList where
    itraverse = itraverse

-------------------------------------------------------------------------------
-- Showing
-------------------------------------------------------------------------------

instance Show a => Show (SkewList a) where
    showsPrec d xs = showsPrec d (toList xs)

explicitShow :: Show a => SkewList a -> String
explicitShow xs = explicitShowsPrec 0 xs ""

explicitShowsPrec :: Show a => Int -> SkewList a -> ShowS
explicitShowsPrec _ Nil             = showString "Nil"
explicitShowsPrec d (Cons_ s t Nil) = showParen (d > 10)
    $ showString "Cons_ "
    . showsPrec 11 s
    . showChar ' '
    . showsPrec 11 t
    . showString " Nil"
explicitShowsPrec d (Cons_ s t xs)  = showParen (d > 0)
    $ showString "Cons_ "
    . showsPrec 11 s
    . showChar ' '
    . showsPrec 11 t
    . showString " $ "
    . explicitShowsPrec 0 xs

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Empty 'SkewList'.
--
-- >>> empty :: SkewList Int
-- []
--
empty :: SkewList a
empty = Nil

-- | Single element 'SkewList'.
--
-- >>> singleton True
-- [True]
--
singleton :: a -> SkewList a
singleton x = Cons_ 1 (Lf x) Nil

-- |
--
-- >>> cons 'x' (fromList "foo")
-- "xfoo"
--
cons :: a -> SkewList a -> SkewList a
cons x (Cons_ s1 t1 (Cons_ s2 t2 xs)) | s1 == s2 = Cons_ (sizeUp s1) (Nd x t1 t2) xs
cons x xs                                        = Cons_ 1           (Lf x)       xs

-- |
--
-- >>> append (fromList "foo") (fromList "bar")
-- "foobar"
--
append :: SkewList a -> SkewList a -> SkewList a
-- append xs ys = foldr cons ys xs
append Nil            ys = ys
append (Cons_ s t xs) ys = appendTree s t (append xs ys)

appendTree :: Word -> Tree a -> SkewList a -> SkewList a
appendTree !_   (Lf x)     xs
                = cons x xs
appendTree s1 t@(Nd x l r) xs@(Cons_ s2 _ (Cons_ s3 _ _))
    | s2 == s3  = cons x (appendTree s' l (appendTree s' r xs))
    | s1 <= s2  = Cons_ s1 t xs
    | otherwise = cons x (appendTree s' l (appendTree s' r xs))
  where
    s' = sizeDown s1
appendTree s1 t@(Nd x l r) xs@(Cons_ s2 _ Nil)
    | s1 <= s2  = Cons_ s1 t xs
    | otherwise = cons x (appendTree s' l (appendTree s' r xs))
  where
    s' = sizeDown s1
appendTree s1 t Nil
                = Cons_ s1 t Nil

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

infixl 9 !, !?

-- | List index.
--
-- >>> fromList ['a'..'f'] ! 0
-- 'a'
--
-- >>> fromList ['a'..'f'] ! 5
-- 'f'
--
-- >>> fromList ['a'..'f'] ! 6
-- *** Exception: SkewList.!
-- CallStack (from HasCallStack):
--   error...
--   !, called at <interactive>...
--
(!) :: HasCallStack => SkewList a -> Int -> a
(!) t i
    | i < 0     = err
    | otherwise = unsafeIndex err t (fromIntegral i)
  where
    err = error "SkewList.!"

unsafeIndex :: a -> SkewList a -> Word -> a
unsafeIndex d Nil            !_ = d
unsafeIndex d (Cons_ s t xs) !i
    | i < s     = unsafeIndexTree d s i t
    | otherwise = unsafeIndex d xs (i - s)

unsafeIndexTree
    :: a       -- ^ default value
    -> Word    -- ^ tree size
    -> Word    -- ^ index
    -> Tree a  -- ^ tree
    -> a
unsafeIndexTree _ !_ !0 (Lf x)       = x
unsafeIndexTree d  _  _ (Lf _)       = d
unsafeIndexTree _  _  0 (Nd x _ _)   = x
unsafeIndexTree d  s  i (Nd _ t1 t2)
    | i <= s'   = unsafeIndexTree d s' (i - 1)      t1
    | otherwise = unsafeIndexTree d s' (i - 1 - s') t2
  where
    s' = sizeDown s

-- | safe list index.
--
-- >>> fromList ['a'..'f'] !? 0
-- Just 'a'
--
-- >>> fromList ['a'..'f'] !? 5
-- Just 'f'
--
-- >>> fromList ['a'..'f'] !? 6
-- Nothing
--
(!?) :: SkewList a -> Int -> Maybe a
(!?) t i
    | i < 0     = Nothing
    | otherwise = safeIndex t (fromIntegral i)

safeIndex :: SkewList a -> Word -> Maybe a
safeIndex Nil            !_ = Nothing
safeIndex (Cons_ s t xs) !i
    | i < s     = safeIndexTree s i t
    | otherwise = safeIndex xs (i - s)

safeIndexTree
    :: Word    -- ^ tree size
    -> Word    -- ^ index
    -> Tree a  -- ^ tree
    -> Maybe a
safeIndexTree !_ !0 (Lf x)       = Just x
safeIndexTree  _  _ (Lf _)       = Nothing
safeIndexTree  _  0 (Nd x _ _)   = Just x
safeIndexTree  s  i (Nd _ t1 t2)
    | i <= s'   = safeIndexTree s' (i - 1)      t1
    | otherwise = safeIndexTree s' (i - 1 - s') t2
  where
    s' = sizeDown s

-- | Inverse of 'cons'.
--
-- >>> uncons (fromList ['a'..'f'])
-- Just ('a',"bcdef")
--
-- >>> uncons Nil
-- Nothing
--
uncons :: SkewList a -> Maybe (a, SkewList a)
uncons Nil                        = Nothing
uncons (Cons_  _ (Lf x)       xs) = Just (x, xs)
uncons (Cons_  s (Nd x t1 t2) xs) = Just (x, Cons_ s' t1 (Cons_ s' t2 xs)) where s' = sizeDown s

-- | Length, /O(log n)/.
length :: SkewList a -> Int
length = go 0 where
    go !n Nil            = n
    go  n (Cons_ s _ xs) = let !n' = n + fromIntegral s in go n' xs

-- | Is the list empty? /O(1)/.
null :: SkewList a -> Bool
null Nil           = True
null (Cons_ _ _ _) = False

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

instance TC.IsList (SkewList a) where
    type Item (SkewList a) = a
    toList   = toList
    fromList = fromList

-- | Convert 'SkewList' to ordinary list.
toList :: SkewList a -> [a]
toList Nil            = []
toList (Cons_ _ t xs) = toListTree t (toList xs)

toListTree :: Tree a -> [a] -> [a]
toListTree (Lf x)       zs = x : zs
toListTree (Nd x xs ys) zs = x : toListTree xs (toListTree ys zs)

-- | Convert ordinary list to 'SkewList'.
--
-- >>> fromList ['a' .. 'f']
-- "abcdef"
--
-- >>> explicitShow $ fromList ['a' .. 'f']
-- "Cons_ 3 (Nd 'a' (Lf 'b') (Lf 'c')) $ Cons_ 3 (Nd 'd' (Lf 'e') (Lf 'f')) Nil"
--
-- >>> explicitShow $ fromList ['a' .. 'e']
-- "Cons_ 1 (Lf 'a') $ Cons_ 1 (Lf 'b') $ Cons_ 3 (Nd 'c' (Lf 'd') (Lf 'e')) Nil"
--
fromList :: [a] -> SkewList a
fromList = L.foldr cons empty

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

-- | 'I.foldMap'.
foldMap :: Monoid m => (a -> m) -> SkewList a -> m
foldMap _ Nil            = mempty
foldMap f (Cons_ _ t xs) = foldMapTree f t <> foldMap f xs

foldMapTree :: Semigroup m => (a -> m) -> Tree a -> m
foldMapTree f (Lf x)     = f x
foldMapTree f (Nd x l r) = f x <> foldMapTree f l <> foldMapTree f r

-- | Strict 'foldMap'.
foldMap' :: Monoid m => (a -> m) -> SkewList a -> m
foldMap' _ Nil            = mempty
foldMap' f (Cons_ _ t xs) =
    a <> b
  where
    !a = foldMapTree' f t
    !b = foldMap' f xs

foldMapTree' :: Semigroup m => (a -> m) -> Tree a -> m
foldMapTree' f (Lf x) = f x
foldMapTree' f (Nd x l r) =
    xl <> r'
  where
    !x' = f x
    !l' = foldMapTree' f l
    !r' = foldMapTree' f r
    !xl = x' <> l'

-- | Right fold.
foldr :: (a -> b -> b) -> b -> SkewList a -> b
foldr _ z Nil            = z
foldr f z (Cons_ _ t xs) = foldrTree f (foldr f z xs) t

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree f z (Lf x) = f x z
foldrTree f z (Nd x l r) = f x (foldrTree f (foldrTree f z r) l)

-- | Strict left fold.
foldl' :: (b -> a -> b) -> b -> SkewList a -> b
foldl' _  z Nil           = z
foldl' f z (Cons_ _ t xs) = foldl' f z' xs
  where
    !z' = foldlTree' f z t

foldlTree' :: (b -> a -> b) -> b -> Tree a -> b
foldlTree' f z (Lf x)     = f z x
foldlTree' f z (Nd x l r) = foldlTree' f l' r
  where
    !x' = f z x
    !l' = foldlTree' f x' l

sum :: Num a => SkewList a -> a
sum = foldl' (+) 0

product :: Num a => SkewList a -> a
product = foldl' (*) 1

-------------------------------------------------------------------------------
-- Indexed Folding
-------------------------------------------------------------------------------

-- | Indexed 'I.foldMap'.
ifoldMap :: Monoid m => (Int -> a -> m) -> SkewList a -> m
ifoldMap = ifoldMapOff 0

ifoldMapOff :: Monoid m => Int -> (Int -> a -> m) -> SkewList a -> m
ifoldMapOff _ _ Nil            = mempty
ifoldMapOff o f (Cons_ s t xs) = ifoldMapTreeOff o s f t <> ifoldMapOff (o + fromIntegral s) f xs

ifoldMapTreeOff :: Semigroup m => Int -> Word -> (Int -> a -> m) -> Tree a -> m
ifoldMapTreeOff o _ f (Lf x)     = f o x
ifoldMapTreeOff o s f (Nd x l r) = f o x <> ifoldMapTreeOff (o + 1) s' f l <> ifoldMapTreeOff (o + 1 + fromIntegral s') s' f r
  where
    s' = sizeDown s

-- | Indexed right fold.
ifoldr :: (Int -> a -> b -> b) -> b -> SkewList a -> b
ifoldr = ifoldrOff 0

ifoldrOff :: Int -> (Int -> a -> t -> t) -> t -> SkewList a -> t
ifoldrOff _ _ z Nil            = z
ifoldrOff o f z (Cons_ s t xs) = ifoldrTreeOff o s f (ifoldrOff (o + fromIntegral s) f z xs) t

ifoldrTreeOff :: Int -> Word -> (Int -> a -> b -> b) -> b -> Tree a -> b
ifoldrTreeOff o _ f z (Lf x) = f o x z
ifoldrTreeOff o s f z (Nd x l r) = f o x (ifoldrTreeOff (o + 1) s' f (ifoldrTreeOff (o + 1 + fromIntegral s') s' f z r) l) where
    s' = sizeDown s

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

-- | Adjust a value in the list.
--
-- >>> adjust 3 toUpper $ fromList "bcdef"
-- "bcdEf"
--
-- If index is out of bounds, the list is returned unmodified.
--
-- >>> adjust 10 toUpper $ fromList "bcdef"
-- "bcdef"
--
-- >>> adjust (-1) toUpper $ fromList "bcdef"
-- "bcdef"
--
adjust :: Int -> (a -> a) -> SkewList a -> SkewList a
adjust i f xs
    | i < 0     = xs
    | otherwise = adjustOff (fromIntegral i) f xs

adjustOff :: Word -> (a -> a) -> SkewList a -> SkewList a
adjustOff _ _ Nil = Nil
adjustOff i f (Cons_ s t xs)
    | i < s     = Cons_ s (adjustOffTree i s f t) xs
    | otherwise = Cons_ s t                       (adjustOff (i - s) f xs)

adjustOffTree :: Word -> Word -> (a -> a) -> Tree a -> Tree a
adjustOffTree 0 _ f   (Lf x)     = Lf (f x)
adjustOffTree _ _ _ t@(Lf _)     = t
adjustOffTree 0 _ f   (Nd x l r) = Nd (f x) l r
adjustOffTree i s f   (Nd x l r)
    | i <= s'   = Nd x (adjustOffTree (i - 1) s' f l) r
    | otherwise = Nd x l                              (adjustOffTree (i - 1 - s') s' f r)
  where
    s' = sizeDown s

-- | Map over elements.
--
-- >>> map toUpper (fromList ['a'..'f'])
-- "ABCDEF"
--
map :: (a -> b) -> SkewList a -> SkewList b
map = fmap

-- | Indexed map.
--
-- >>> imap (,) $ fromList ['a' .. 'f']
-- [(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e'),(5,'f')]
--
imap :: (Int -> a -> b) -> SkewList a -> SkewList b
imap = imapOff 0

imapOff :: Int -> (Int -> a -> b) -> SkewList a -> SkewList b
imapOff _ _ Nil            = Nil
imapOff o f (Cons_ s t xs) = Cons_ s (imapTreeOff o s f t) (imapOff (o + fromIntegral s) f xs)

imapTreeOff :: Int -> Word -> (Int -> a -> b) -> Tree a -> Tree b
imapTreeOff o _ f (Lf x)     = Lf (f o x)
imapTreeOff o s f (Nd x l r) = Nd (f o x)
    (imapTreeOff (o + 1)                   s' f l)
    (imapTreeOff (o + 1 + fromIntegral s') s' f r)
  where
    s' = sizeDown s

-- | Indexed 'I.traverse'.
itraverse :: Applicative f => (Int -> a -> f b) -> SkewList a -> f (SkewList b)
itraverse = itraverseOff 0

itraverseOff :: Applicative f => Int -> (Int -> a -> f b) -> SkewList a -> f (SkewList b)
itraverseOff _ _ Nil            = pure Nil
itraverseOff o f (Cons_ s t xs) = Cons_ s <$> itraverseTreeOff o s f t <*> itraverseOff (o + fromIntegral s) f xs

itraverseTreeOff :: Applicative f => Int -> Word -> (Int -> a -> f b) -> Tree a -> f (Tree b)
itraverseTreeOff o _ f (Lf x)     = Lf <$> f o x
itraverseTreeOff o s f (Nd x l r) = Nd <$> f o x
    <*> itraverseTreeOff (o + 1)                   s' f l
    <*> itraverseTreeOff (o + 1 + fromIntegral s') s' f r
  where
    s' = sizeDown s

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

instance QC.Arbitrary1 SkewList where
    liftArbitrary = fmap fromList . QC.liftArbitrary
    liftShrink shr = fmap fromList . QC.liftShrink shr . toList

instance QC.Arbitrary a => QC.Arbitrary (SkewList a) where
    arbitrary = QC.arbitrary1
    shrink    = QC.shrink1

instance QC.CoArbitrary a => QC.CoArbitrary (SkewList a) where
    coarbitrary = QC.coarbitrary . toList

instance QC.Function a => QC.Function (SkewList a) where
    function = QC.functionMap toList fromList
