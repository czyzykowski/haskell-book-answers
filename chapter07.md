# Chapter 7 Exercises
## Exercises: Grab Bag
1. Which (two or more) of the following are equivalent?

    a) `mTh x y z = x * y * z`
    b) `mTh x y = \z -> x * y * z`
    c) `mTh x = \y -> \z -> x * y * z`
    d) `mTh = \x -> \y -> \z -> x * y * z`

2. The type of `mTh` (above) is `Num a => a -> a -> a -> a`. Which is the type
   of `mTh 3`?

   d) `Num a => a -> a -> a`

3. Next, we'll practice writing anonymous lambda syntax.

    a) Rewrite the `f` function in the where clause.

            addOneIfOdd n = case odd n of
              True -> f n
              False -> n
              where f n = n + 1

            addOneIfOdd n = case odd n of
              True -> f n
              False -> n
              where f = \n -> n + 1

    b) Rewrite the following to use anonymous lambda syntax:

            addFive x y = (if x > y then y else x) + 5

            addFive = \x -> \y -> (if x > y then y else x) + 5

    c) Rewrite the following so that it doesn't use anonymous lambda syntax:

            mflip f = \x -> \y -> f y x

            mflip f x y = f y x

## Exercises: Variety Pack
1. Given the following declarations

        k (x, y) = x
        k1 = k ((4-1), 10)
        k2 = k ("three", (1 + 2))
        k3 = k (3, True)

    a) What is the type of `k`? It is: `k :: (a, b) -> a`

    b) What is the type of `k2`? Is it the same type as `k1` or `k3`? The type
    of `k2` is: `k2 :: String` and it's not the same type as `k1` or `k3`.

    c) Of `k1`, `k2`, `k3`, which will return the number `3` as the result?
    `k1` and `k3`.

2. Fill in the definition of the following function:

        -- Remember: Tuples have the same syntax for their
        -- type constructors and their data constructors.

        f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
        f (x, y, z) (u, v, w) -> ((x, u), (z, w))

## Exercises: Case Practice
First, rewrite `if-then-else` expressions into case expressions.

1. The following should return `x` when `x` is greater than `y`.

        functionC x y = if (x > y) then x else y

        functionC x y = case x > y of
          True -> x
          False -> y

2. The following will add 2 to even numbers and otherwise simply return the
   input value.

        ifEvenAdd2 n = if even n then (n+2) else n

        ifEvenAdd2 n = case even n of
          True -> n + 2
          False -> n

The next exercise doesn't have all the cases covered. See if you can fix it.

1. The following compares a value, `x`, to zero and returns an indicator for
   whether `x` is a positive number or negative number. But what if `x` is 0?
   You may need to play with `compare` function a bit to find what to do.

        nums x =
          case compare x 0 of
            LT -> -1
            GT -> 1
            EQ -> 0

## Exercises: Artful Dodgy
Given the following definitions tell us what value results from further
applications.

        dodgy x y = x + y * 10
        oneIsOne = dodgy 1
        oneIsTwo = (flip dodgy) 2

1. `dodgy 1 0`

        1

2. `dodgy 1 1`

        11

3. `dodgy 2 2`

        22

4. `dodgy 1 2`

        21

5. `dodgy 2 1`

        12

6. `oneIsOne 1`

        11

7. `oneIsOne 2`

        21

8. `oneIsTwo 1`

        21

9. `oneIsTwo 2`

        22

10. `oneIsOne 3`

        31

11. `oneIsTwo 3`

        23

## Exercises: Guard Duty

1. It is probably clear to you why you wouldn’t put an otherwise in your
   top-most guard, but try it with avgGrade anyway and see what happens. It’ll
   be more clear if you rewrite it as an actual otherwise match: `| otherwise = 'F'`.
   What happens now if you pass a 90 as an argument? 75? 60?

        avgGrade :: (Fractional a, Ord a) => a -> Char
        avgGrade x
           | otherwise = 'F'
           | y >= 0.9 = 'A'
           | y >= 0.8 = 'B'
           | y >= 0.7 = 'C'
           | y >= 0.59 = 'D'
           | y < 0.59 = 'F'
           where y = x / 100

    After this change the only possible result of the function is `F`.

2. What happens if you take avgGrade as it is written and reorder the guards?
   Does it still typecheck and work the same? Try moving `| y >= 0.7 = 'C'` and
   passing it the argument 90, which should be an `'A.'` Does it return an `'A'`?

    After reordering the guards the function typechecks, but doesn't work the
    same.

3. The following function returns

        pal xs
            | xs == reverse xs = True
            | otherwise        = False

    b) `True` when `xs` is a palindrome

4. What types of arguments can pal take?

    Any list which contains elements of the class `Eq`.

5. `pal :: Eq a => [a] -> [a]`

6. The following function returns

        numbers x
            | x < 0  = -1
            | x == 0 = 0
            | x > 0  = 1


    c) an indication of whether its argument is a positive or negative number
    or zero

7. What types of arguments can `numbers` take?

    Anything of the class `Num`.

8. What is the type of the function `numbers`?

        numbers :: (Ord a, Num a, Num b) => a -> b

## Chapter Exercises
### Multiple choice

1. A polymorphic function

    d) may resolve to values of different types, depending on inputs

2. Two functions named `f` and `g` have types `Char -> String` and `String ->
   [String]` respectively. The composed function `g . g` has the type

    b) `Char -> [String]`

3. A function `f` has the type `Ord a => a -> a -> Bool` and we apply it to one
   numeric value. What is the type now?

    d) `(Ord a, Num a) => a -> Bool`

4. A function with the type `(a -> b) -> c`

    b) is a higher-order function

5. Given the following definition of `f`, what is the type of `f True`?

        f :: a -> a
        f x = x

    a) `f True :: Bool`

### Let's write code
1. The following function returns the tens digit of an integral argument.

        tensDigit :: Integral a => a -> a
        tensDigit x = d
           where xLast = x `div` 10
                 d     = xLast `mod` 10

    a) First, rewrite it using `divMod`.

        tensDigit :: Integral a => a -> a
        tensDigit x = d `mod` 10
           where (d, _) = x `divMod` 10

    b) Does the `divMod` version have the same type as the original version?

    Yes.

    c) Next, let's change it so that we’re getting the hundreds digit instead.

        hunsD :: Integral a => a -> a
        hunsD x = d2
           where d  = x `div` 10
                    d2 = (d `div` 10) `mod` 10

2. Implement the function of type `a -> a -> Bool -> a` once each using a case
   expression and once with a guard.

        foldBool :: a -> a -> Bool -> a
        foldBool a b c = case c of
                            True -> a
                            False -> b

        foldBool :: a -> a -> Bool -> a
        foldBool a b c
          | c         = a
          | otherwise = b

3. Fill in the definition. Note that the first argument to our function is also
   a function which can be applied to values. Your second argument is a tuple,
   which can be used for pattern matching:

        g :: (a -> b) -> (a, c) -> (b, c)
        g f (x, y) = (f x, y)

4. For this next exercise, you’ll experiment with writing pointfree versions of
   existing code. This involves some new information, so read the following
   explanation carefully.

    Typeclasses are dispatched by type. `Read` is a typeclass like `Show`, but
    it is the dual or "opposite" of `Show`. In general, the `Read` typeclass
    isn’t something you should plan to use a lot, but this exercise is
    structured to teach you something about the interaction between typeclasses
    and types.

    The function read in the `Read` typeclass has the type:

        read :: Read a => String -> a

    Notice pattern?

        read :: Read a => String -> a
        show :: Show a => a -> String


    Write the following code into a source file. Then load it and run it in
    GHCi to make sure you understand why the evaluation results in the answers
    you see.

        -- arith4.hs
        module Arith4 where

        -- id :: a -> a
        -- id x = x

        roundTrip :: (Show a, Read a) => a -> a
        roundTrip a = read (show a)

        main = do
          print (roundTrip 4)
          print (id 4)

5. Next, write a pointfree version of `roundTrip`.

        roundTrip :: (Show a, Read a) => a -> a
        roundTrip = read . show

6. We will continue to use the code in `module Arith4` for this exercise as well.

    When we apply `show` to a value such as `(1 :: Int)`, the `a` that
    implements `Show` is `Int`, so GHC will use the `Int` instance of the
    `Show` typeclass to stringify our `Int` of `1.`

    However, `read` expects a `String` argument in order to return an `a`. The
    `String` argument that is the first argument to read tells the function
    nothing about what type the de-stringified result should be. In the type
    signature `roundTrip` currently has, it knows because the type variables
    are the same, so the type that is the input to show has to be the same type
    as the output of read.

    Your task now is to change the type of `roundTrip` in `Arith4` to `(Show a,
    Read b) => a -> b`. How might we tell GHC which instance of Read to
    dispatch against the String now? Make the application of your pointfree
    version of roundTrip to the argument 4 on line 10 work. You will only need
    the *has the type* syntax of `::` and parentheses for scoping.

        main = do
          print ((roundTrip 4) :: Int)
