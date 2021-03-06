{-# OPTIONS_GHC -fno-full-laziness #-}
module Main where

import Prelude
import Prelude.Unicode
import Data.Function

import Data.List qualified as List

import Test.Tasty.QuickCheck
import Test.Tasty.Bench

primes ∷  [(String, Int -> [Integer])]
primes =
  [ ("primesFromUnfaithfulSieve", primesFromUnfaithfulSieve)
  , ("primesFromTrueSieve", primesFromTrueSieve)
  , ("primesFromTrialDivision", primesFromTrialDivision)
  , ("primesFromBird", primesFromBird) ]

primesFromUnfaithfulSieve, primesFromTrueSieve, primesFromTrialDivision, primesFromBird ∷ Int -> [Integer]

primesFromUnfaithfulSieve n = take n $ unfaithfulSieve [2..] where unfaithfulSieve (p: xs) = p: unfaithfulSieve [x | x ← xs, x `mod` p > 0]

(\\) ∷ Ord a ⇒ [a] → [a] → [a]
xs \\ ys = case xs of
  [ ] → [ ]
  (x: xs') → case ys of
    [ ] → xs
    (y: ys') → case x `compare` y of
      LT → x: (xs' \\ ys)
      GT → xs \\ ys'
      EQ → xs' \\ ys'

primesFromTrueSieve n = take n $ trueSieve [2..] where trueSieve (p: xs) = p: trueSieve (xs \\ [p^2, p^2 + p..])

primesFromTrialDivision n = take n $ fix \primes →
  let
    isPrime x = all (\p → x `mod` p > 0) (factorsToTry x)
    factorsToTry x = takeWhile (\p → p^2 ≤ x) primes
  in 2: [x | x ← [3..], isPrime x]

primesFromBird n = take n $ fix \primes →
  let
    composites = mergeAll [map (p*) [p..] | p <- primes]
  in 2: [3..] \\ composites

mergeAll ∷ _
mergeAll (xs:xss) = xmerge xs (mergeAll xss)
mergeAll _ = error "This function works only on streams."

xmerge ∷ _
xmerge (x:xs) ys = x:merge xs ys
xmerge _ _ = error "This function works only on streams."

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x<y = x:merge xs (y:ys)
  | x==y = x:merge xs ys
  | x>y = y:merge (x:xs) ys
merge _ _ = error "This function works only on streams."

main ∷ IO ( )
main = defaultMain
  [ bgroup "properties"
    [ testProperty "" \ (Ordered xs) (Ordered ys) → (xs :: [Int]) \\ ys === xs List.\\ ys
    , testProperty "" \ n → primesFromTrialDivision n === primesFromUnfaithfulSieve n
    , testProperty "" \ n → primesFromTrialDivision n === primesFromTrueSieve n
    , testProperty "" \ n → primesFromTrialDivision n === primesFromBird n
    ]
  , bgroup "bench marks"
    [ bgroup  (show size) [ bench name (nf primes size)
    | (name, primes) ← primes ] | n ← [1..13], let size = 2^n ]
  ]
