module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Lazy
import qualified Strict

main :: IO ()
main = defaultMain $ testGroup "skewed"
    [ Strict.tests
    , Lazy.tests
    ]
