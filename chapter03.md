# Chapter 3 Exercises
## Syntax Errors

1. `++ [1, 2, 3] [4, 5, 6]`

    Fixed version: `[1, 2, 3] ++ [4, 5, 6]`

2. `'<3' ++ ' Haskell'

    Fixed version: `"<3" ++ " Haskell"`

3. `concat ["<3", " Haskell"]`

    This will compile.

## Chapter Exercises
## Reading syntax
1. For the following lines of code, read the syntax carefully and decide if
   they are written correctly. Test them in your REPL after you've decided to
   check your work. Correct as many as you can.

    1. `concat [[1, 2, 3], [4, 5, 6]]`

        It's correct.

    2. `++ [1, 2, 3] [4, 5, 6]`

        Fixed: `[1, 2, 3] ++ [4, 5, 6]`

    3. `(++) "hello" " world"`

        It's correct.

    4. `["hello" ++ " world]`

        Fixed: `["hello" ++ " world"]`

    5. `4 !! "hello"`

        Fixed: `"hello" !! 4`

    6. `(!!) "hello" 4`

        It's correct.

    7. `take "4 lovely"`

        Fixed: `take 4 "lovely"`

    8. `take 3 "awesome"`

        It's correct.

2. Next we have two sets: the first set is lines of code and the other is a set
   of results. Read the code and figure out which results came from which lines
   of code.

    Set of lines of code:

    1. `concat [[1 * 6], [2 * 6], [3 * 6]]`
    2. `"rain" ++ drop 2 "elbow"`
    3. `10 * head [1, 2, 3]`
    4. `(take 3 "Julie") ++ (tail "yes")`
    5.
            concat [tail [1, 2, 3],
                    tail [4, 5, 6],
                    tail [7, 8, 9]]

    Set of results

    1. `"Jules"`
    2. `[2, 3, 5, 6, 8, 9]`
    3. `"rainbow"`
    4. `[6, 12, 18]`
    5. `10`

    Answers:

    - code 1 with result 4
    - code 2 with result 3
    - code 3 with result 5
    - code 4 with result 1
    - code 5 with result 2

## Building functions

1. Given the list-manipulation functions mentioned in this chapter, write
   functions that take the following inputs and return the expected outputs. Do
   them directly in your REPL and use the `take` and `drop` functions you've
   already seen.

    1.
            -- Given
            "Curry is awesome"
            -- Return
            "Curry is awesome!"

            Prelude> "Curry is awesome" ++ "!"
            "Curry is awesome!"

    2.
            -- Given
            "Cury is awesome!"
            -- Return
            "y"

            Prelude> take 1 $ drop 4 "Curry is awesome!"
            "y"

    3.
            -- Given
            "Curry is awesome!"
            -- Return
            "awesome!"

            Prelude> drop 9 "Curry is awesome!"

2. Now take each of the above and rewrite it in a source file as a general
   function that could take different string inputs as arguments but retain the
   same behavior. Use a variable as the argument to your (named) function.

    1.
            f :: String -> String
            f s = s ++ "!"

    2.
            g :: String -> String
            g s = take 1 $ drop 4 s

    3.
            h :: String -> String
            h s = drop 9 s

3. Write a function of type `String -> Char` which returns the third character
   in a String. Remember to give the function a name and apply it to a
   variable, not a specific String, so that it could be reused for different
   String inputs, as demonstrated.

        thirdLetter :: String -> Char
        thirdLetter x = x !! 4

4. Now change that function so that the string input is alwasy the same and the
   variable returns the number of the letter you want to return.

        letterIndex :: Int -> Char
        letterIndex x = "Curry is awesome!" !! x

5. Using the `take` and `drop` functions we looked at above, see if you can
   write a function called `rvrs`. `rvrs` should take the string "Curry is
   aesome" and return the result "awesome is Curry".

        rvrs :: String -> String
        rvrs s = drop 9 s ++ " " ++ (take 2 $ drop 6 s) ++ " " ++ take 5 s

6. Let's see if we can expand that function into a module.

        module Reverse where

        rvrs :: String -> String
        rvrs s = drop 9 s ++ " " ++ (take 2 $ drop 6 s) ++ " " ++ take

        main :: IO ()
        main = print (rvrs "Curry is awesome")
