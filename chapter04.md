# Chapter 4 Exercises
## Mood Swing
Given the following datatype, answer the following questions:

    data Mood = Blah | Woot deriving Show

1. What is the type constructor, or name of this type?

    `Mood`

2. If the function require a `Mood` value, what are the values you could
   possibly use there?

    `Blah` and `Woot`

3. We are trying to write a function `changeMood` to change Chris's mood
   instantaneously. So far, we've written a type signature `changeMood :: Mood
   -> Woot`. What's wrong with that?

    `Woot` is a data constructor, not a type name, so it cannot be used in the
    type signature.

4. Now we want to write the function that changes his mood. Given an input
   mood, it gives us the other one. Fix any mistakes and complete the function:

        changeMood :: Mood -> Mood
        changeMood Blah = Woot
        changeMood    _ = Blah

5. Enter all of the above -- datatype (including the "deriving Show" bit), your
   corrected type signature, and the corrected function into a source file.

        module Main where

        data Mood = Blah | Woot deriving Show

        changeMood :: Mood -> Mood
        changeMood Blah = Woot
        changeMood    _ = Blah

        main :: IO ()
        main = print (changeMood Woot)

## Find the Mistakes
The following lines of code may have mistakes -- same of them won't compile!
You know what you need to do.

1. `not True && true`

    fixed: `not True && True`

2. `not (x = 6)`

    fixed: `not (x == 6)`

3. `(1 * 2) > 5`

    fixed: `(1 * 2) < 5`

4. `[Merry] > [Happy]`

    fixed: `Merry > Happy`

5. `[1, 2, 3] ++ "look at me!"`

    `"1, 2, 3" ++ "look at me!"`

## Chapter Exercises
All exercises assume that the following three definitions are in the scope:

        awesome = ["Papuchon", "curry", ":)"]
        alsoAwesome = ["Quake", "The Simons"]
        allAwesome = [awesome, alsoAwesome]

1. Given the definition of `length` what would the type signature be? How many
   arguments, of what type does it take? What is the type of the result it
   evaluates to?

    Type signature: `length :: [a] -> Int`

    It takes one argument.

    The type of the result: `Int`.

2. What are the results of the following expressions?

    1. `length [1, 2, 3, 4, 5]`

            5

    2. `length [(1, 2), (2, 3), (3, 4)]`

            3

    3. `length allAwesome`

            2

    4. `length (concat allAwesome)`

            5

3. Given what we know about numeric types and the type signature of `length`,
   look at those two expressions. One works and one returns an error. Determine
   which will return an error and why.

        Prelude> 6 / 3
        -- and
        Prelude> 6 / length [1, 2, 3]

    Second line will error becuase type of `(/) :: Fractional a => a -> a -> a`
    and `length :: Foldable t => t a -> Int`. It is not `Fractional`.

4. How can you fix the broken code from the preceding exercise using a
   different division function/operator?

        Prelude> 6 / fromIntegral (length [1, 2, 3])

5. What is the type of the expression `2 + 3 == 5`? What would we expect as a
   result?

    It will be of type `Bool`. The result will be `True`.

6. What is the type and expected result value of the following:

        Prelude> let x = 5
        Prelude> x + 3 == 5

    Type is `Bool`.

7. Below are some bits of code. Which will work? Why or why not? If they will
   work, what value would these reduce to?

        Prelude> length allAwesome == 2
        True

        Prelude> length [1, 'a', 3, 'b']
        -- error because the array elements are not of the same type

        Prelude> length allAwesome + length awesome
        5

        Prelude> (8 == 8) && ('b' < 'a')
        False

        Prelude> (8 == 8) && 9
        -- type error 9 is not of a type Bool (although the real error is much
        -- more confusing)

8. Write a function that tells you whether or not a given String (or list) is a
   palindrome. Use `reverse` function.

        isPalindrome :: (Eq a) => [a] -> Bool
        isPalindrome x = x == reverse x

9. Write a function to return the absolute value of a number using
   if-then-else.

        myAbs :: Integer -> Integer
        myAbs x = if x < 0 then -x else x

10. Fill in the definition of the following function using `fst` and `snd`

        f :: (a, b) -> (c, d) -> ((b, d), (a, c))
        f x y = ((snd x, snd y), (fst x, fst y))


## Correcting syntax
In the following examples, you'll be shown syntactically incorrect code. Type
it in and try to correct it in your text editor, validating it with GHC or
GHCi.

1. Here, we want a function that adds 1 to the length of a string argument and
   returns the result.

        x = (+)

        F xs = w 'x' 1
            where w = length xs

    Corrected

        f xs = w `x` 1
            where w = length xs

2. This is supposed to be the identity function, `id`.

        \ X = x

    Corrected

        \x -> x

3. When fixed, this function will return `1` from the value `[1, 2, 3]`.

        \ x : xs -> x

    Corrected

        \(x:xs) -> x

4. When fixed, this function will return `1` from the value `(1, 2)`.

        f (a b) = A

    Corrected

        f (a, b) = a

## Match the function names to their types

1. Which of the following types is the type of `show`?

    c) `Show a => a -> String`

2. Which of the following types is the type of `(==)`?

    b) `Eq a => a -> a -> Bool`

3. Which of the following types is the type of `fst`?

    a) `(a, b) -> a`

4. Which of the following types is the type of `(+)`?

    d) `Num a => a -> a -> a`
