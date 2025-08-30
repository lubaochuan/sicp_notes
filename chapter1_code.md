
```scheme
#lang Racket

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial1 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib1 n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
```

### 1.1.6

```scheme
#lang Racket

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; (abs -2) => 2
; (abs 2) => 2
```

### 1.1.7

```scheme
#lang Racket

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;(sqrt 9) => 3.00009155413138
;(sqrt (+ 100 37)) => 11.704699917758145
;(sqrt (+ (sqrt 2) (sqrt 3))) => 1.7739279023207892
;(square (sqrt 1000)) => 1000.000369924366
```

With `(require racket/trace)`, we can trace the sequence of
approximations.

```scheme
#lang Racket
(require racket/trace)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(trace sqrt-iter)

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

#|
> (sqrt 9)
>(sqrt-iter 1.0 9)
>(sqrt-iter 5.0 9)
>(sqrt-iter 3.4 9)
>(sqrt-iter 3.023529411764706 9)
>(sqrt-iter 3.00009155413138 9)
<3.00009155413138
3.00009155413138
|#
```

### 1.2.1

```scheme
#lang Racket

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

  ; (factorial 5) => 120

  (define (factorial1 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))

; (factorial1 5) => 120
  ```

### 1.2.2

```scheme
#lang Racket

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;(fib 8) => 21

(define (fib1 n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; (fib1 8) => 21
```

### 1.2.2

Counting change

How many way can we make change of a given amount of money, `a`,
using n kinds of coins?

Suppose, we think of the types of coins available as arranged
in some order. Then the following relation holds.

The number of ways to change amount `a` using `n` kinds of coins
equal
* the number of ways to change amount `a` using all but the first
kind of coin, plus
* the number of ways to change amount `a-d` using all `n` kinds
of coins where `d` is the denomination of the first kind of coin.

We can recursively reduce the problem of changing a given amount
to the problem of changing smaller amount using fewer kinds of
coins. To finish the algorithm we must specify the following
degenerate cases.
* If `a` is exactly `0`, we should count that as `1` way to make
change.
* If `a` is less than `0`, we should count that as `0` ways to
change.
* If `n` is `0`, we should count that as `0` ways to make change.

```scheme
#lang Racket

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; (count-change 100) => 292
```

### 1.2.4

We can the exponential of a given number using a linear
recursive process (`Θ(n)` steps and `Θ(n)` space) or a linear iterative process (`Θ(n)`steps and `Θ(1)` space).

```scheme
#lang Racket

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; (expt 2 5) => 32

(define (expt1 b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

; (expt1 2 5) => 32
```

We can take advantage of successive squaring in computing
exponentials in general if we use the rule

$$
\begin{cases}
  {b}^{n} = {({b}^{n/2})}^{2} & \quad \text{if } n  \text{ is even}\\
  {b}^{n} = b\cdot{b}^{n-1} & \quad \text{if } n \text{ is odd}
\end{cases}
$$

```schenme
#lang Racket

(define (square x)
  (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; (fast-expt 2 5) => 32
```

The process evolved by `fast-expt` grows logrithmically $\Theta(logn)$ with `n`
in both space and number of steps.

It is also possible to use the idea of successive squaring to devise an
iterative algorithm that computes exponentials with a logarithmic number of
steps, although, as is often the case with iterative algorithms, this is
not written down so straightforwardly as the recursive algorithm.

### 1.2.6

One way to test if a number is prime is to find the number's divisors.
The following program finds the smallest integral divisor (greater than 1)
of a given number `n`.

```scheme
#lang Racket

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

; (smallest-divisor 47) => 47
; (smallest-divisor 49) => 7
```

The end test for `find-divisor` is based on the fact that
if `n` is not prime it must have a divisor less than or
equal to $\sqrt{n}$. This means that the algorithm need
only test divisors between 1 and $\sqrt{n}$. Consequently,
the number of steps required to identify `n` as prime will
have order of growth $\Theta(\sqrt{n})$.

### 1.3.1 Procedures as Arguments

It is essential for a powerful programming language to provide
the ability to build abstractions by assigning names to common
patterns and then to work in terms of the abstractions directly.

Often the same programming pattern will be used with a number
of different procedures. To express such patterns as concepts,
we will need to construct procedures that can accept procedures
as arguments or return procedures as values. Procedures that
manipulate procedures are call ***higher-order*** procedures.

The following show how the "summation of a series" pattern
can be separated from the particular series being summed.
This abstraction expresses the concept of 'sigma notion' in
math.

```scheme
#lang Racket

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)) (pi-sum (+ a 4) b)))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes1 a b)
  (sum cube a inc b))

(define (cube x)
  (* x x x))

(define (inc n)
  (+ n 1))

; (sum-cubes 1 10) => 3025
; (sum-cubes1 1 10) => 3025

(define (sum-integers1 a b)
  (sum identity a inc b))

(define (identify x) x)

; (sum-integers 1 10) => 55
; (sum-integers1 1 10) =>55

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;(integral cube 0 1 0.01) => 0.24998750000000042
;(integral cube 0 1 0.001) => 0.249999875000001
```

### 1.3.3 Procedures as General Methods

With higher-order procedures, such as the `integral` procedure,
we began to see a more powerful kind of abstractio: procedures
used to express general methods of compuation, independent of
the particular functions involved. The following examples show
that methods, such as that for finding zeros and fixed-points
of functions can be expressed directly as procedures.

```scheme
#lang Racket

(define  (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negtive? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (negtive? x)
  (< x 0))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))
#|
> (half-interval-method sin 2.0 4.0)
3.14111328125
> (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                        1.0
                        2.0)
1.89306640625
|#

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

#|
> (fixed-point cos 1.0)
0.7390822985224023
> (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
1.2587315962971173
|#

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; (sqrt 9) => 3
```

### 1.3.4 Procedures as Return Values

`average-damp` is a procedure that takes as its argument a procedure `f`
and returns as its value a procedure (produced by `lambda`) that when
applied to a number x, produces the average of `x` and `(f x)`.

The new formulation of `sqrt` makes explicit the three ideas in the
method: fixed-point search, average damping, and the function
$y \mapsto x/y$.

In general, there are many ways to formulate a process as a procedure.
Experienced programmers know how to choose procedural formulations
that are perspicuous, and where useful elements of the process are
expressed as separate entities that can be reused in other applications.
As a simple example of reuse, notice that the cube root of `x` is
a fixed point of the function $y \mapsto x/y^{2}$, so we can
immediately generalize our square-root procedure to one that extracts
cube root.

```scheme
#lang Racket

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

;((average-damp square) 10) => 55

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

; (sqrt 9) => 3.0

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y))))
               1.0))

; (cube-root 50) => 3.684033875854678
```

