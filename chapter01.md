# Chapter 1 Exercises
## Equivalence Exercises

Choose an answer that is equivalent to the listed lambda term.

1. `λxy.xz`

    b) `λmn.mz (m = x)`

2. `λxy.xxy`

    c) `λa(λb.aab) (a = x, b = y)`

3. `λxyz.zx`

    b) `λtos.st (t = x, o = y, s = z)`

## Chapter Exercises

### Combinators

*All answers to those exercises are provided in the book. They are here because
I was doing them all myself before checking the answers.*

Determine if each of the following are combinators or not.

1. `λx.xxx` - a combinator
2. `λxy.zx` - not a combinator
3. `λxyz.xy(zx)` - a combinator
4. `λxyz.xy(zxy)` - a combinator
5. `λxy.xy(zxy)` - not a combinator

Determine if each of the following can be reduced to a normal form or if they
diverge.

1. `λx.xxx` - will not diverge
2. `(λz.zz)(λy.yy)` - will diverge
3. `(λx.xxx)z` - will not diverge

Evaluate (that is, beta reduce) each of the following expressions to normal
form.

1. `(λabc.cba)zz(λwv.w)`

        (λa.λb.λc.cba)(z)z(λw.λv.w)
        (λb.λc.cbz)(z)(λw.λv.w)
        (λc.czz)(λw.λv.w)
        (λw.λv.w)(z)z
        (λv.z)(z)
        z

2. `(λx.λy.xyy)(λa.a)b`

        (λy.(λa.a)yy)(b)
        (λa.a)(b)b
        bb

3. `(λy.y)(λx.xx)(λz.zq)`

        (λx.xx)(λz.zq)
        (λz.zq)(λz.zq)
        (λz.zq)(q)
        qq

4. `(λz.z)(λz.zz)(λz.zy)`

        (λz.zz)(λz.zy)
        (λz.zy)(λz.zy)
        (λz.zy)(y)
        yy

5. `(λx.λy.xyy)(λy.y)y`

        (λy.(λy.y)yy)(y)
        (λy.y)(y)y
        yy

6. `(λa.aa)(λb.ba)c`

        (λb.ba)(λb.ba)c
        (λb.ba)(a)c
        aac

7. `(λxyz.xz(yz))(λx.z)(λx.a)`

        (λx.λy.λz.xz(yz))(λx.z)(λx.a)
        (λy.λz1.(λx.z)z1(yz1))(λx.a)
        (λz1.(λx.z)(z1)((λx.a)z1))
        (λz1.z((λx.a)(z1)))
        (λz1.za)
