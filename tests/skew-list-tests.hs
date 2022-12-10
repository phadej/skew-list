module Main (main) where

import Test.QuickCheck
       (Arbitrary (..), Fun, Gen, Property, applyFun, chooseInt, elements,
       label, oneof, property, sized, vector, (.&&.), (===))
import Test.QuickCheck.Poly  (A, B)
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.Foldable.WithIndex as WI
import qualified Data.Functor.WithIndex  as WI
import qualified Data.List               as L
import qualified Data.SkewList.Strict    as S

main :: IO ()
main = defaultMain $ testGroup "skewed"
    [ testProperty "valid" valid_prop
    , testProperty "fromList . toList" $ \xs ->
        xs === S.fromList (S.toList (xs :: S.SkewList Int))
    , testProperty "toList . fromList" $ \xs ->
        xs === S.toList (S.fromList (xs :: [Int]))

    , testProperty "uncons" $ \xs ->
        L.uncons xs === fmap (fmap S.toList) (S.uncons (S.fromList (xs :: [Int])))
    , testProperty "length" $ \xs ->
        L.length xs === S.length (S.fromList (xs :: [A]))
    , testProperty "null" $ \xs ->
        L.null xs === S.null (S.fromList (xs :: [A]))

    , testProperty "eq" eq_prop
    , testProperty "compare" compare_prop

    , testProperty "map"  map_prop
    , testProperty "imap" imap_prop
    , testProperty "append" append_prop
    , testProperty "append" append_prop_valid

    , testProperty "foldr" foldr_prop
    , testProperty "foldMap" foldMap_prop
    , testProperty "ifoldr" ifoldr_prop
    , testProperty "ifoldMap" ifoldMap_prop

    , testProperty "model" model_prop
    ]

data SmallA = A0 | A1 | A2 deriving (Eq, Ord, Show)

instance Arbitrary SmallA where
    arbitrary = elements [A0,A1,A2]

valid_prop :: S.SkewList A -> Property
valid_prop xs = property (S.valid xs)

eq_prop :: [SmallA] -> [SmallA] -> Property
eq_prop xs ys = label (show (xs == ys)) $
    (xs == ys) === (S.fromList xs == S.fromList ys)

compare_prop :: [SmallA] -> [SmallA] -> [SmallA] -> Property
compare_prop xs ys zs = label (show (compare xs' ys', compare ys' zs')) $ trans
    (compare xs' ys')
    (compare ys' zs')
    (compare xs' zs')
  where
    xs' = S.fromList xs
    ys' = S.fromList ys
    zs' = S.fromList zs

    trans :: Ordering -> Ordering -> Ordering -> Bool
    trans LT LT o = o == LT
    trans LT EQ o = o == LT
    trans LT GT _ = True
    trans EQ LT o = o == LT
    trans EQ EQ o = o == EQ
    trans EQ GT o = o == GT
    trans GT LT _ = True
    trans GT EQ o = o == GT
    trans GT GT o = o == GT

map_prop :: Fun A B -> [A] -> Property
map_prop f' xs = S.fromList (L.map f xs) === S.map f (S.fromList xs)
  where
    f = applyFun f'

imap_prop :: Fun (Int, A) B -> [A] -> Property
imap_prop f' xs = WI.imap f xs === S.toList (S.imap f (S.fromList xs))
  where
    f i x = applyFun f' (i, x)

append_prop :: [A] -> [A] -> Property
append_prop xs ys = S.fromList (xs ++ ys) === S.append (S.fromList xs) (S.fromList ys)

append_prop_valid :: [A] -> [A] -> Property
append_prop_valid xs ys = property (S.valid (S.append (S.fromList xs) (S.fromList ys)))

foldr_prop :: Fun (A, B) B -> B -> [A] -> Property
foldr_prop f' z xs = L.foldr f z xs === S.foldr f z (S.fromList xs) where
    f a b = applyFun f' (a, b)

ifoldr_prop :: Fun (Int, A, B) B -> B -> [A] -> Property
ifoldr_prop f' z xs = WI.ifoldr f z xs === S.ifoldr f z (S.fromList xs) where
    f i a b = applyFun f' (i, a, b)

foldMap_prop :: Fun A [B] -> [A] -> Property
foldMap_prop f' xs = foldMap f xs === S.foldMap f (S.fromList xs) where
    f = applyFun f'

ifoldMap_prop :: Fun (Int, A) [B] -> [A] -> Property
ifoldMap_prop f' xs = WI.ifoldMap f xs === WI.ifoldMap f (S.fromList xs) where
    f i a = applyFun f' (i, a)

-- | Model of construction operators.
data Model a
    = Empty
    | Singleton a
    | FromList [a]
    | Cons a (Model a)
    | Uncons (Model a)
    | Append (Model a) (Model a)
  deriving Show

instance Arbitrary a => Arbitrary (Model a) where
    arbitrary = sized model

model :: Arbitrary a => Int -> Gen (Model a)
model n
    | n <= 1
    = oneof [ pure Empty, Singleton <$> arbitrary ]

    | otherwise
    = oneof
        [ Cons <$> arbitrary <*> model (n - 2)
        , Uncons <$> model (n - 2)
        , FromList <$> vector n
        , do
            k <- chooseInt (1, n - 1)
            Append <$> model k <*> model (n - 1 - k)
        ]

modelList :: Model a -> [a]
modelList Empty          = []
modelList (Singleton x)  = [x]
modelList (FromList xs)  = xs
modelList (Cons x xs)    = x : modelList xs
modelList (Uncons xs)    = maybe [] snd (L.uncons (modelList xs))
modelList (Append xs ys) = modelList xs ++ modelList ys

modelSkewList :: Model a -> S.SkewList a
modelSkewList Empty          = S.empty
modelSkewList (Singleton x)  = S.singleton x
modelSkewList (FromList xs)  = S.fromList xs
modelSkewList (Cons x xs)    = S.cons x (modelSkewList xs)
modelSkewList (Uncons xs)    = maybe S.empty snd (S.uncons (modelSkewList xs))
modelSkewList (Append xs ys) = S.append (modelSkewList xs) (modelSkewList ys)

model_prop :: Model A -> Property
model_prop m = S.valid (modelSkewList m) .&&. S.fromList (modelList m) === modelSkewList m
