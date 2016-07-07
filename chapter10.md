# Chapter 10 Exercises
## Understanding Folds
1. `foldr (*) 1 [1..5]`

    1. no, because it's not a fold, it will type error
    2. yes, because multiplication is commutative
    3. yes, the same reason as in b

2. Evaluation steps for `foldl (flip (*)) 1 [1..3]`

        (((((1 * 1) * 2) * 3) * 4) * 5)
        ((((1 * 2) * 3) * 4) * 5)
        (((2 * 3) * 4) * 5)
        ((6 * 4) * 5)
        (24 * 5)
        120

3. c - `folr`, but not `foldl`, associates to the right
4. a - reduce structure
5. fix errors

    a) `foldr (++) [] ["woot", "WOOT", "woot"]`
    b) `foldr max '\x00' "fear is the little death"`
    c) `foldr (&&) True [False, True]`
    d) `foldr (||) False [False, True]`
    e) `foldl (flip ((++) . show)) "" [1..5]`
    f) `foldr (flip const) 'a' [1..5]`
    g) `foldr (flip const) 0 "tacos"`
    h) `foldl const 0 "burritos"`
    i) `foldl const 'z' [1..5]`

## Database Processing

    import Data.Time

    data DatabaseItem = DbSTring String
                      | DbNumber Integer
                      | DbDate   UTCTime
                      deriving (Eq, Ord, Show)

    theDatabase :: [DatabaseItem]
    theDatabase =
      [ DbDate (UTCTime
                (fromGregorian 1911 5 1)
        (secondsToDiffTime 34123))
      , DbNumber 9001
      , DbString "Hello, World!"
      , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
      ]

1. function that filters for `DbDate` values and returns a list of the
   `UTCTime` values.

        filterDbDate :: [DatabaseItem] -> [UTCTime]
        filterDbDate = foldr f []
          where f (DbDate d) b = d:b
                f _          b = b

2. Write a function that filters for `DbNumber` values and returns a list of the
   `Integer` values inside them.

        filterDbNumber :: [DatabaseItem] -> [Integer]
        filterDbNumber = foldr f []
          where f (DbNumber n) b = n:b
                f _            b = b

3. Write a function that gets the most recent date.

        mostRecent :: [DatabaseItem] -> UTCTime
        mostRecent = foldr f startDate
          where f (DbDate d) b = max d b
                f _          b = b
                startDate = UTCTime
                             (fromGregorian 0 1 1)
                             (secondsToDiffTime 0)

4. Write a function that sums all of the `DbNumber` values.

        sumDb :: [DatabaseItem] -> Integer
        sumDb = foldr f 0
          where f (DbNumber n) b = n + b
                f _            b = b

5. Write a function that gets the average of the `DbNumber` values.

        avgDb :: [DatabaseItem] -> Double
        avgDb db = fromIntegral (sumDb db) / fromIntegral (length $ filterDbNumber db)

## Scans

1. Modify your `fibs` function to only return the first 20 Fibonacci numbers.

        fibs :: [Int]
        fibs = take 20 fibs'
          where fibs' = 1 : scanl (+) 1 fibs'

2. Modify `fibs` to return the Fibonacci numbers that are less than 100.

        fibs :: [Int]
        fibs = takeWhile (<100) fibs'
          where fibs' = 1 : scanl (+) 1 fibs'

3. Try to write `factorial` function from Recursion as a ascan. You'll want
   `scanl` again, and your start value will be 1. Warning: this will also
   generate an infinite list, so you may want to pass it through a `take`
   function or similar.


        factorial :: [Int]
        factorial = scanl (*) 1 [2..]

## Warm-up and review
1. Given the following sets of consonants and vowels:

    `stops  = "pbtdkg"`
    `vowels = "aeiou"`

    a. Write a function that takes inputs from `stops` and `vowels` and makes
    3-tuples of all possible stop-vowel-stop combinations. These will not all
    correspond to real words in English, although the stop-vowel-stop pattern
    is common enough that many of them will.

            combinations :: [String]
            combinations = [[s1, v, s2] | s1 <- stops, v <- vowels, s2 <- stops]

    b. Modify that function so that it only returns the combinations that begin
    with a `p`.

            combinations :: [String]
            combinations = [['p', v, s] | v <- vowels, s <- stops]

    c. Now set up lists of nouns and verbs (instead of stops and vowels) and
    modify the function to make tuples representing possible noun-verb-noun
    sentences.

            nouns = ["dog", "cat", "man", "house", "pen"]
            verbs = ["barks", "eats", "sings", "sits"]

            combinations :: [(String, String, String)]
            combinations = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

2. What does the following mystery function do? What is its type?

        seekritFunc x =
          div (sum (map length (words x)))
              (length (words x))

    It calculate average length of words in a given string.

    Type: `seekritFunc :: String -> Int`

3. We'd really like the answer to be more precise. Can you rewrite that using
   fractional division?

        seekritFunc x =
          fromIntegral (sum (map length (words x))) /
          fromIntegral (length (words x))

## Rewriting functions using folds

1. `myOr` returns `True` if any `Bool` in the list is `True`.

    1. direct recursion

            myOr :: [Bool] -> Bool
            myOr [] = False
            myOr (x:xs) =
              if x == True
              then True
              else myOr xs

    2. direct recursion using operator

            myOr :: [Bool] -> Bool
            myOr [] = False
            myOr (x:xs) = x || myOr xs

    3. fold, not point-free in the folding function

            myOr :: [Bool] -> Bool
            myOr = foldr
                   (\a b ->
                     if a == True
                     then True
                     else b) False

    4. fold, both `myOr` and the folding function are point-free

            myOr :: [Bool] -> Bool
            myOr = foldr (||) False

2. `myAny` returns `True if `a -> Bool` applied to any of the values in the
   list returns `True`

    1. direct recursion

            myAny :: (a -> Bool) -> [a] -> Bool
            myAny _ []     = False
            myAny f (x:xs) =
              if f x
              then True
              else myAny f xs

    2. direct recursion using operator

            myAny :: (a -> Bool) -> [a] -> Bool
            myAny _ []     = False
            myAny f (x:xs) = f x || myAny f xs

    3. fold, not point-free in the folding function

            myAny :: (a -> Bool) -> [a] -> Bool
            myAny f = foldr
                      (\a b ->
                        if f a
                        then True
                        else b) False

    4. fold, both `myAny` and the folding function are point-free

            myAny :: (a -> Bool) -> [a] -> Bool
            myAny f = foldr (\a b -> f a || b) False

3. In addition to the recursive and fold based `myElem`, write a version that
   uses `any`.

    1. direct recursion

            myElem :: Eq a => a -> [a] -> Bool
            myElem _ [] = False
            myElem a (x:xs) =
              if a == x
              then True
              else myElem a xs

    2. direct recursion using operator

            myElem :: Eq a => a -> [a] -> Bool
            myElem _ [] = False
            myElem a (x:xs) = a == x || myElem a xs

    3. fold, not point-free in the folding function

            myElem :: Eq a => a -> [a] -> Bool
            myElem x = foldr (\a b ->
                             if a == x
                             then True
                             else b) False

    4. fold, point free

            myElem :: Eq a => a -> [a] -> Bool
            myElem x = foldr (\a b -> a == x || b) False

    5. using `any`

            myElem x = any (x ==)

4. Implement `myReverse`, don't worry about trying to make it lazy.

    1. direct recursion

            myReverse :: [a] -> [a]
            myReverse [] = []
            myReverse (x:xs) = myReverse xs ++ [x]

    2. fold, not point-free

            myReverse :: [a] -> [a]
            myReverse = foldl (\b a -> b ++ [a]) []

    3. fold, point-free

            myReverse :: [a] -> [a]
            myReverse = foldl (flip (:)) []

5. Write `myMap` in terms of `foldr`. It should have the same behavior as the
   build-in `map`.

        myMap :: (a -> b) -> [a] -> [b]
        myMap f = foldr (\a b -> f a : b) []

6. Write `myFilter` in therms of `foldr`. It should have the same behavior as
   the build-in `filter`.

        myFilter :: (a -> Bool) -> [a] -> [a]
        myFilter f = foldr (\a b ->
                           if f a
                           then a : b
                           else b) []

7. `squish` flattens a list of lists into a list.

        squish :: [[a]] -> [a]
        squish = foldr (++) []

8. `squishMap` maps a function over a list and concatenates the results.

        squishMap :: (a -> [b]) -> [a] -> [b]
        squishMap f = foldr (\a b -> f a ++ b) []

9. `squishAgain` flattens a list of a lists into a list. This time re-use the
   `squishMap` function.

        squishAgain :: [[a]] -> [a]
        squishAgain = squishMap id

10. `myMaximumBy` takes a comparision function and a list and returns the
    greatest element of the list based on the last value that the comparison
    returned `GT` for.

        myMaximumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
        myMaximumBy f (x:xs) = foldl (\b a ->
                                      case f a b of
                                        GT -> a
                                        _  -> b) x xs

11. `myMinimumBy` takes a comparison function and a list and returns the least
    element of the list based on the last value that the comparison returned
    `LT` for.

        myMinimumBy :: Ord a => (a -> a -> Ordering) -> [a] -> a
        myMinimumBy f (x:xs) = foldl (\b a ->
                                      case f a b of
                                        LT -> a
                                        _  -> b) x xs
