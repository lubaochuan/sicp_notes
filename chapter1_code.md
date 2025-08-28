
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
```
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
  {b}^{n} = {({b}^{n/2})}^{2} \quad \text{if } n  \text{ is even}\\
  {b}^{n} = b\cdot{b}^{n-1} \quad \text{if } n \text{ is odd}
\end{cases}
$$