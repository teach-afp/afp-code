module QuizPrimeCheck where

-- | Find element in an ascending stream.
findAscend :: Ord a => a -> [a] -> Bool
findAscend k (p : ps) = case compare p k of
    LT -> findAscend k ps
    EQ -> True
    GT -> False

-- | Stream of primes.
primes :: [Integer]
primes = 2 : sieve odds
  where
    sieve (p : ns)   = p : sieve (filter ((0 /=) . (`mod` p)) ns)
    odds             = everyOtherFrom 3
    everyOtherFrom n = n : everyOtherFrom (n + 2)

-- | Raise error if given number is not prime.
checkPrime :: Integer -> ()
checkPrime k = if findAscend k primes then () else error "not prime"

-- | Pass through primes, raise error for non-primes.
ensurePrime :: Integer -> Integer
ensurePrime k =
  let _ = checkPrime k
  in  k

-- menti.com 8725 3240 https://www.menti.com/aloyjb5u3jgo?source=voteCode
main = print $ ensurePrime 4
