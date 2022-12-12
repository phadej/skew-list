module Main where

import Criterion.Main (bench, bgroup, defaultMain, nf, whnf)
import Data.Foldable  (foldl')

import qualified Data.List            as L
import qualified Data.RAList          as R
import qualified Data.Sequence        as Q
import qualified Data.SkewList.Strict as S
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as U

size :: Int
-- size = 16383 -- bad size for RAList
size = 16384 -- good size for RAList

idx :: Int
idx = 8888

list :: [Int]
list = [1 .. size]

ralist :: R.RAList Int
ralist = R.fromList list

skewed :: S.SkewList Int
skewed = S.fromList list

vector :: V.Vector Int
vector = V.fromList list

uvector :: U.Vector Int
uvector = U.fromList list

sequ :: Q.Seq Int
sequ = Q.fromList list

main :: IO ()
main = defaultMain
    [ bgroup "Index"
        [ bench "List"           $ whnf (\xs -> xs L.!! idx) list
        , bench "RAList"         $ whnf (\xs -> xs R.!  idx) ralist
        , bench "Vector"         $ whnf (\xs -> xs V.!  idx) vector
        , bench "Vector.Unboxed" $ whnf (\xs -> xs U.!  idx) uvector
        , bench "Seq"            $ whnf (\xs -> xs `Q.index`  idx) sequ
        , bench "SkewList"       $ whnf (\xs -> xs S.!  idx) skewed
        ]
    , bgroup "Cons"
        [ bench "List"           $ whnf (0 :)      list
        , bench "RAList"         $ whnf (R.cons 0) ralist
        , bench "Vector"         $ whnf (V.cons 0) vector
        , bench "Vector.Unboxed" $ whnf (U.cons 0) uvector
        , bench "Seq"            $ whnf (0 Q.<|) sequ
        , bench "SkewList"       $ whnf (S.cons 0) skewed
        ]
    , bgroup "Length"
        [ bench "List"           $ whnf L.length list
        , bench "RAList"         $ whnf R.length ralist
        , bench "Vector"         $ whnf V.length vector
        , bench "Vector.Unboxed" $ whnf U.length uvector
        , bench "Seq"            $ whnf Q.length sequ
        , bench "SkewList"       $ whnf S.length skewed
        ]
    , bgroup "IndexAfterCons"
        [ bench "List"           $ whnf (\xs -> (0 : xs)    L.!! idx) list
        , bench "RAList"         $ whnf (\xs -> R.cons 0 xs R.!  idx) ralist
        , bench "Vector"         $ whnf (\xs -> V.cons 0 xs V.!  idx) vector
        , bench "Vector.Unboxed" $ whnf (\xs -> U.cons 0 xs U.!  idx) uvector
        , bench "Seq"            $ whnf (\xs -> (0 Q.<| xs) `Q.index` idx) sequ
        , bench "SkewList"       $ whnf (\xs -> S.cons 0 xs S.!  idx) skewed
        ]

    , bgroup "Append"
        [ bench "List"           $ nf (\xs -> xs <> xs) list
        , bench "RAList"         $ nf (\xs -> xs <> xs) ralist
        , bench "Vector"         $ nf (\xs -> xs <> xs) vector
        , bench "Vector.Unboxed" $ nf (\xs -> xs <> xs) uvector
        , bench "Seq"            $ nf (\xs -> xs <> xs) sequ
        , bench "SkewList"       $ nf (\xs -> xs <> xs) skewed
        , bench "SkewList slow"  $ nf (\xs -> S.foldr S.cons xs xs) skewed
        ]
    , bgroup "IndexAfterAppend"
        [ bench "List"           $ nf (\xs -> (xs <> xs) L.!! idx) list
        , bench "RAList"         $ nf (\xs -> (xs <> xs) R.!  idx) ralist
        , bench "Vector"         $ nf (\xs -> (xs <> xs) V.!  idx) vector
        , bench "Vector.Unboxed" $ nf (\xs -> (xs <> xs) U.!  idx) uvector
        , bench "Seq"            $ nf (\xs -> (xs <> xs) `Q.index` idx) sequ
        , bench "SkewList"       $ nf (\xs -> (xs <> xs) S.!  idx) skewed
        , bench "SkewList slow"  $ nf (\xs -> S.foldr S.cons xs xs S.! idx) skewed
        ]

    , bgroup "Sum"
        [ bench "List"            $ whnf sum list
        , bench "List foldl'"     $ whnf (foldl' (+) 0) list
        , bench "RAList"          $ whnf sum ralist
        , bench "Vector"          $ whnf sum vector
        , bench "Seq"             $ whnf sum sequ
        , bench "SkewList"        $ whnf sum skewed
        , bench "SkewList foldl'" $ whnf (foldl' (+) 0) skewed
        ]
    ]
