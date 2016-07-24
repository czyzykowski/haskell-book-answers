# Chapter 5 Exercises
## Type Matching
Match the function to its type signature.

1. Functions:

    1. `not`
    2. `length`
    3. `concat`
    4. `head`
    5. `(<)`

2. Type signatures:

    1. `_ :: [a] -> a`
    2. `_ :: [[a]] -> [a]`
    3. `_ :: Bool -> Bool`
    4. `_ :: [a] -> Int`
    5. `_ :: Ord a => a -> a -> Bool`

Answer:

    - `not :: Bool -> Bool`
    - `length :: [a] -> Int`
    - `concat :: [[a]] -> [a]`
    - `head :: [a] -> a`
    - `(<) :: Ord a => a -> a -> Bool`

## Type Arguments
Given a function and its type, tell us what type results from applying some or
all of the arguments.

1. If the type of `f` is `a -> a -> a -> a`, and the type of `x` is `Char` then
   the type of `f x` is

    a) `Char -> Char -> Char`

2. If the type of `g` is `a -> b -> c -> b`, then the type of `g 0 'c' "woot"` is

    d) `Char`

3. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h
   1.0 2` is

    d) `Num b => b`

4. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of the
   `h 1 (5.5 :: Double)` is

    c) `Double`

5. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of
   `jackal "keyboard" "has the word jackal in it"`

    a) `[Char]`

6. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of
   `jackal "keybaord"`

    e) `Eq b => b -> [Char]`

7. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
   `kessel 1 2` is

    d) `(Num a, Ord a) => a`

8. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
    `kessel 1 (2 :: Integer)` is

    a) `(Num a, Ord a) => a`

9. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
    `kessel (1 :: Integer) 2` is

    c) `Integer`

## Parametricity
All you can really do with a parametrically polymorphic value is pass or not
pass it to some other expression. Prove that to yourself with these small
demonstrations.

1. Given the type `a -> a`, which is the type for `id`, attempt to make a
   function that is not bottom and terminates successfully that does something
   other than returning the same value. This is impossible, but you should try
   it anyway.

    It's impossible.

2. We can get a more comfortable appreciation of parametricity by looking at `a
   -> a -> a`. This hypothetical function `a -> a -> a` has two-and only
   two-implementations. Write both possible versions of `a -> a -> a`. After
   doing so, try to violate the constraints of parametrically polymorphic
   values we outlined above.

        f1 :: a -> a -> a
        f1 x y = x

        f2 :: a -> a -> a
        f2 x y = y

3. Implement `a -> b -> b`. How many implementations can it have? Does the
   behavior change when the types of `a` and `b` change?

        f :: a -> b -> b
        f x y = y

    The behavior doesn't depend on the types of `a` and `b`.

## Apply Yourself
Look at these pairs of functions. One function is unapplied, so the compiler
will infer maximally polymorphic type. The second function has been applied to
a value, so the inferred type signature may have become concrete, or at least
less polymorphic. Figure out how the type would change and why, make a note of
what you think the new inferred type would be and then check your work in GHCi.

1.

        -- Type signature of general function
        (++) :: [a] -> [a] -> [a]

        -- How might that change when we apply it to the following value?
        myConcat x = x ++ " yo"

        -- The type of myConcat is
        myConcat :: [Char] -> [Char]

2.

        -- General function
        (*) :: Num a => a -> a -> a

        -- Applied to a value
        myMult x = (x / 3) * 5

        -- The type of myMult is
        myMult :: Fractional a => a -> a

3.

        take :: Int -> [a] -> [a]

        myTake x = take x "key you"

        myTake :: Int -> [Char]

4.

        (>) :: Ord a => a -> a -> Bool

        myCom x = x > (length [1..10])

        myCom :: Int -> Bool

5.

        (<) :: Ord a => a -> a -> Bool

        myAlph x = x < 'z'

        myAlph :: Char -> Bool

## Chapter Exercises
### Multiple choice

1. A value of type `[a]` is

    c) a list whose elements are all of some type `a`

2. A function of type `[[a]] -> [a]` could

    a) take a list of strings as an argument

3. A function of type `[a] -> Int -> a`

    b) returns one element of type `a` from a list

4. A function of type `(a, b) -> a`

    c) takes a tuple argument and returns the first value

### Determine the type
For the following functions, determine the type of the specified value. Note:

1. All function applications return a value. Determine the value returned by
   these function applications and the type of that value.

    1. `(* 9) 6` = `54 :: Num a => a`
    2. `head [(0, "doge"), (1, "kitteh")]` = `(0, "doge") :: Num a => (a, [Char])`
    3. `head [(0 :: Integer, "doge"), (1, "kitteh")]` = `(0, "doge") :: (Integer, [Char])`
    4. `if False then True else False` = `False :: Bool`
    5. `length [1, 2, 3, 4, 5]` = `5 :: Int`
    6. `(length [1, 2, 3, 4]) > (length "TACOCAT")` = `False :: Bool`

2. Given

        x = 5
        y = x + 5
        w = y * 10

    What is the type of `w`?

    `w :: Num a => a`

3. Given

        x = 5
        y = x + 5
        z y = y * 10

    What is the type of `z`?

    `z :: Num a => a -> a`

4. Given

        x = 5
        y = x + 5
        f = 4 / y

    What is the type of `f`?

    `f :: Fractional a => a`

5. Given

        x = "Julie"
        y = " <3 "
        z = "Haskell"
        f = x ++ y ++ z

    What is the type of `f`?

    `f :: [Char]`

### Does it compile?
For each set of expressions, figure out which expression, if any, causes the
compiler to squawk at you (n.b. we do not mean literal squawking) and why. Fix
it if you can.

1.

        bigNum = (^) 5 $ 10
        wahoo = bigNum $ 10     -- this will fail because bigNum is not a function

2.

        x = print
        y = print "woohoo!"
        z = x "hello world"
        -- all good here

3.

        a = (+)
        b = 5
        c = b 10    -- will fail because b is not a function
        d = c 200   -- will fail because c is not a function

        -- potential fix

        a = (+)
        b = a 5
        c = b 10
        d = a c 200

4.

        a = 12 + b
        b = 10000 * c   -- will fail becaue c is undefined

### Type variable or specific type constructor?

1. You will be shown a type declaration, and you should categorize each type.
   The choices are a fully polymorphic type variable, constrained polymorphic
   type variable, or concrete type constructor.

        f :: Num a => a -> b -> Int -> Int
        --           [0]  [1]   [2]    [3]

        [0] - constrained polymorphic type variable
        [1] - fully polymorphic type variable
        [2] - concrete type constructor
        [3] - concrete type constructor

2. Categorize each component of the type signature as described in the previous
   example.

        f :: zed -> Zed -> Blah
        --   [0]    [1]    [2]

        [0] - fully polymorphic type variable
        [1] - concrete type constructor
        [2] - concrete type constructor

3. Categorize each component of the type signature

        f :: Enum b => a -> b -> C
        --            [0]  [1]  [2]

        [0] - fully polymorphic type variable
        [1] - constrained polymorphic type variable
        [2] - concrete type constructor

4. Categorize each component of the type signature

        f :: f -> g -> C
        --  [0]  [1]  [2]

        [0] - fully polymorphic type variable
        [1] - fully polymorphic type variable
        [2] - concrete type constructor

### Write a type signature
For the following expressions, please add a type signature.

1. While we haven't fully explained this syntax yet, you've seen it in Chapter
   2 and as a solution to an exercise in Chapter 4. This syntax is a way of
   destructuring a single element of a list.

        functionH :: [a] -> a
        functionH (x:_) = x

2.

        functionC :: Ord a => a -> a -> Bool
        functionC x y = if (x > y) then True else False

3.

        functionS :: (a, b) -> b
        functionS (x, y) = y

### Given a type, write the function
You will be shown a type and a function that needs to be written. Use the
information the type provides to determine what the function should do. We'll
also tell you how many ways there are to write the function. (Syntactically
different but semantically equivalent implementations are not countead being
different).

1. There is only one implementation that typechecks.

        i :: a -> a
        i x = x

2. There is only one version that works.

        c :: a -> b -> a
        c x y = x

3. Given alpha equivalence are c'' and c (see above) the same thing?

        c'' :: b -> a -> b
        c'' x y = x

    Yes, they are the same.

4. Only one version that works.

        c' :: a -> b -> b
        c' x y = y

5. There are multiple possibilities, at least two of which you've seen in
   previous chapters.

        r :: [a] -> [a]
        r (x:_) = [x]

        r xs = xs

        r xs = init xs


6. One one version that will typecheck.

        co :: (b -> c) -> (a -> b) -> (a -> c)
        co f g = \x -> f (g x)

7. One version will typecheck.

        a :: (a -> c) -> a -> a
        a _ x = x

8. One version will typecheck.

        a' :: (a -> b) -> a -> b
        a' f x = f x

### Fix it
Won't someone take pity on this poor broken code and fix it up? Be sure to check
carefully for things like capitalization, parentheses, and indentation.

1.

        -- broken
        module sing where

        fstString :: [Char] ++ [Char]
        fstString x = x ++ " in the rain"

        sndString :: [Char] -> Char
        sndString x = x ++ " over the rainbow"

        sing = if (x > y) then fstString x or sndString y
        where x = "Singin"
              x = "Somwhere"

        -- fixed
        module Sing where

        fstString :: [Char] -> [Char]
        fstString x = x ++ " in the rain"

        sndString :: [Char] -> [Char]
        sndString x = x ++ " over the rainbow"

        sing = if x > y then fstString x else sndString y
          where x = "Singin"
                y = "Somwhere"


2. Now that it's fixed, make a minor change and make it sing the other song. If
   you're lucky, you'll end up with both songs stuck in your head!

        sing = if x < y then fstString x else sndString y
          where x = "Singin"
                y = "Somwhere"

3.

        -- broken
        module Arith3Broken where

        main :: IO ()
        Main = do
          print 1 + 2
          putStrLn 10
          print (negate -1)
          print ((+) 0 blah)
          where blah = negate 1

        -- fixed
        module Arith3Broken where

        main :: IO ()
        main = do
          print (1 + 2)
          print 10
          print (negate (-1))
          print ((+) 0 blah)
          where blah = negate 1

### Type-Kwon-Do
1.

        f :: Int -> String
        f = undefined

        g :: String -> Char
        g = undefined

        h :: Int -> Char
        h x = g (f x)

2.

        data A
        data B
        data C

        q :: A -> B
        q = undefined

        w :: B -> C
        w = undefined

        e :: A -> C
        e x = w (q x)

3.

        data X
        data Y
        data Z

        xz :: X -> Z
        xz = undefined

        yz :: Y -> Z
        yz = undefined

        xform :: (X, Y) -> (Z, Z)
        xform (x, y) = (xz x, yz y)

4.

        munge :: (x -> y) -> (y -> (w, z)) -> x -> w
        munge f g x = fst (g (f x))
