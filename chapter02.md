# Chapter 2 Exercises
## Comprehension Check
1. Given the following lines of code as they might appear in a source file, how
   would you change them to use them directly in REPL?

    1. `half x = x / 2`

        `Prelude> let half x = x / 2`

    2. `square x = x * x`

        `Prelude> let square x = x * x`

2. Write one function that can accept one argument and work for all the
   following expressions. Be sure to name the function.

        3.14 * (5 * 5)
        3.14 * (10 * 10)
        3.14 * (2 * 2)
        3.14 * (4 * 4)

        area r = 3.14 * (r * r)

## Parentheses and Association

Below are some pairs of functions that are alike except for parenthesization.
Read them carefully and decide if the parentheses change the results of the
function.

1. a) `8 + 7 * 9`

    b) `(8 + 7) * 9`

    The parenthization changes the result. a = 71, b = 135

2. a) `perimiter x y = (x * 2) + (y * 2)`

    b) `perimiter x y = x * 2 + y * 2`

    The parenthization doesn't change the result.

3. a) `f x = x / 2 + 9`

    b) `f x = x / (2 + 9)`

    The parenthization changes the result.

## Heal the Sick
The following code samples are broken and won't compile. The first two are as
you might enter into the REPL; the third is from a source file. Find the
mistakes and fix them so that they will.

1. `let area x = 3. 14 * (x * x)`

    `let area x = 3.14 * (x * x)` (unnecessary space after the dot)

2. `let double x = b * 2`

    `let double x = x * x` (the x from the artument list were not used in the
    function's body)

3. wrong

        x = 7
         y = 10
        f = x + y

    corrected

        x = 7
        y = 10
        f = x + y

## A Head Code
Determine in your head what the following expressions will return, then
validate in the REPL:

1. `let x = 5 in x` Result: `5`
2. `let x = 5 in x * x` Result: `25`
3. `let x = 5; y = 6 in x * y` Result: 30
4. `let x = 3; y = 1000 in x + 3` Result: `6`

## Exercises with let and where!
Rewrite the following `let` expressions into declarations with `where` clauses:

1. `let x = 3; y = 1000 in x * 3 + y`

        m = x * 3 + y
          where x = 3
                y = 1000

2. `let y = 10; x = 10 * 5 + y in x * 5`

        m = x * 5
          where y = 10
                x = 10 * 5 + y

3. `let x = 7; y = negate x; z = y * 10 in z / x + y`

        m = z / x + y
          where x = 7
                y = negate x
                z = y * 10

## Chapter Exercises
### Parenthesization
Given the precedence of `(*)`, `(+)`, and `(^)` how can we parenthesize the
following expressions more explicitly without changing their results?

1. `2 + 2 * 3 - 1` -> `2 + (2 * 2) - 1`
2. `(^) 10 $ 1 + 1` -> `(^) 10 $ (1 + 1)`
3. `2 ^ 2 * 4 ^ 5 + 1` -> `(2 ^ 2) * (4 ^ 5) + 1`

### Equivalent expressions
Which of the following pairs of expressions will return the same result when
evaluated?

1. `1 + 1`

    `2`

    The same

2. `10 ^ 2`

    `10 + 9 * 10`

    The same

3. `400 - 37`

    `(-) 37 400`

    Not the same. `363` vs. `-363`

4. `div 100 3`

    `100 / 3`

    Not the same. Integer division vs. rational.

5. `2 * 5 + 18`

    `2 * (5 + 18)`

    Not the same. `28` vs. `46`
