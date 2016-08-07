# Chapter 6 Exercises
## Will They Work?
Next, take a look at the following code examples and try to decide if they will
work, what result they will return if they do, and why or why not (be sure, as
always, to test them in your REPL once you have decided on your answer):

1. `max (length [1, 2, 4]) (length [8, 9, 10, 11, 12])`

    The result will be 5.

2. `compare (3 * 4) (3 * 5)`

    The result will be `LT`.

3. `compare "Julie" True`

    This will error because the arguments to compare are of two distinct types:
    `String` and `Bool`.

4. `(5 + 3) > (3 + 6)`

    The result will be `False`.

## Chapter Exercises
### Multiple choice

1. The `Eq` class

    c) makes equality tests possible

2. The typeclass `Ord`

    a) allows any two values to be compared
    b) is a subclass of `Eq`

3. Suppose the typeclass `Ord` has an operator `>`. What is the type of `>`?

    a) `Ord a => a -> a -> Bool`

4. In `x = divMod 16 12`

    c) the type of `x` is a tuple

5. The typeclass `Integral` includes

    a) `Int` and `Integer` numbers

### Does it typecheck?
Examine the following code and decide whether it will typecheck. Then load it
in GHCi and see if you were correct. If it doesn't typecheck, try to match
the type error against your understanding of why it didn't work. If you can,
fix the error and re-run the code.

1. Does the following code typecheck? If not, why not?

        data Person = Person Bool

        printPerson :: Person -> IO ()
        printPerson person = putStrLn (show person)

    Will fail because `Person` type doesn't implement `Show`.

    Fixed:

        data Person = Person Bool deriving Show

        printPerson :: Person -> IO ()
        printPerson person = putStrLn (show person)

2. Does the following typecheck? If not, why not?

        data Mood = Blah
                  | Woot deriving Show

        settleDown x = if x == Woot
                         then Blah
                         else x

    Will fail becaus `Mood` type doesn't impolement `Eq`.

    Fixed:

        data Mood = Blah
                  | Woot deriving (Show, Eq)

        settleDown x = if x == Woot
                         then Blah
                         else x

3. If you were able to get `settleDown` to typecheck:

    a) What values are acceptable inputs to that function? `Blah` and `Woot`

    b) What will happen if you try to run `settleDown 9`? Why? There will be a
    type error becuase 9 is not of type `Mood`.

    c) What will happen if you try to run `Blah > Woot`? Why? There will be a
    type error because `Mood` doesn't implement `Ord`.

4. Does the following typecheck? If not, why not?

        type Subject = String
        type Verb = String
        type Object = String

        data Sentence =
          Sentence Subject Verb Object
          deriving (Eq, Show)

        s1 = Sentence "dogs" "drool"
        s2 = Sentence "Julie" "loves" "dogs"

    It will fail because `s1` is missing `Object` argument.

    Fixed:

        type Subject = String
        type Verb = String
        type Object = String

        data Sentence =
          Sentence Subject Verb Object
          deriving (Eq, Show)

        s1 = Sentence "dogs" "eat" "bones"
        s2 = Sentence "Julie" "loves" "dogs"

### Given a datatype declaration, what can we do?
Given the following datatype definitions:

        data Rocks =
          Rocks String deriving (Eq, Show)

        data Yeah =
          Yeah Bool deriving (Eq, Show)

        data Papu =
          Papu Rocks Yeah
          deriving (Eq, Show)

Which of the following will typecheck? For the ones that don't typecheck, why
don't they?

1. `phew = Papu "chases" True`

    It will not typeckeck because `Papu` is of type `Rocks -> Yeah -> Papu` and
    the supplied arguments are of type `String` and `Bool`.

2.

        truth = Papu (Rocks "chamskydoz")
                     (Yeah True)

It will typecheck.

3.

        equalityForall :: Papu -> Papu -> Bool
        equalityForall p p' = p == p'


It will typecheck.

4.

        comparePapus :: Papu -> Papu -> Bool
        comparePapus p p' = p > p'


It will fail to typecheck because `Papu` doesn't implement `Ord` typeclass.

### Match the types
We're going to give you two types and their implementations. Then we're going
to ask you if you can substitute the second type for the first. You can test
this by typing the first declaration and its type into a file and editing in
the new one, loading to see if it fails. Don't just guess, test all your
answers!

1. a)

            i :: Num a => a
            i = 1
    b)

            i :: a

    You cannot substitute the second type because `1` constant is of type `Num`.

2. a)

            f :: Float
            f = 1.0
    b)

            f :: Num a => a

    You cannot substitute the second type because `1.0` constant is of type
    `Fractional`.

3. a)

            f :: Float
            f = 1.0
    b)

            f :: Fractional a => a

    You can substitute the second type because the `1.0` is of the type
    `Fractional` already.

4. a)

            f :: Float
            f = 1.0
    b)

            f :: RealFrac a => a

    You can substitute the second type because `Float` is a implements
    `RealFrac` typeclass.

5. a)

            freud :: a -> a
            freud x = x
    b)

            freud :: Ord a => a -> a

    You can substitute the second type because it's more concrete.

6. a)

            freud' :: a -> a
            freud' x = x
    b)

            freud' :: Int -> Int

    You can substitute the second type because it's more concrete.

7. a)

            myX = 1 :: Int

            sigmund :: Int -> Int
            signund x = myX
    b)

            signumd :: a -> a

    You cannot substitute the second type because it's more general.

8. a)

            myX = 1 :: Int

            sigmund' :: Int -> Int
            sigmund' x = myX
    b)

            sigmund' :: Num a => a -> a

    You cannot substitute the second type because it's more general.

9. a)

            jung :: Ord a => [a] -> a
            jung xs = head (sort xs)
    b)

            jung :: [Int] -> Int

    You can substitute the second type because it's more concrete.

10. a)

            young :: [Char] -> Char
            young xs = head (sort xs)
    b)

            young :: Ord a => [a] -> a

    You cannot substitute the second type because it's more general.

11. a)

            mySort :: [Char] -> [Char]
            mySort = sort

            signifier :: [Char] -> Char
            signifier xs = head (my)sort xs)
    b)

            signifier :: Ord a => [a] -> a

    You cannot substitute the second type because it's more general.

### Type-Known-Do Two: Electric Typealoo
1.

        chk :: Eq b => (a, b) -> a -> b -> Bool
        chk (x, y) x' y' = (x, y) == (x', y')

2.

        -- Hint: use some arithmetic operation to
        -- combine values of type 'b'. Pick one.
        arith :: Num b => (a -> b) -> Integer -> a -> b
        arith f n x = (f x) ^ n
