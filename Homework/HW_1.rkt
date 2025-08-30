#lang eopl
(require rackunit "../Extras/Marco_Provided_Code/eopl-extras.rkt")

;; HW 1 Questions 2.7 - 2.9

#|
Exercise 2.7: Rewrite apply-env in ﬁgure 2.1 to give a more informative error
message.

Exercise 2.8: Add to the environment interface an observer called empty-env?
and implement it using the a-list representation.

Exercise 2.9: Add to the environment interface an observer called has-binding?
that takes an environment env and a variable s and tests to see if s has an associated
value in env. Implement it using the a-list representation.
|#