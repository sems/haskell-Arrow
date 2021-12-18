
# Open questions

## Exercise 4

While Happy is compatible with both left and right recursion, left recursion is more efficient en thuss prefered. On the other hand with parser combinators, left recursion tends to be avoided.

## Exercise 10

The 'longer' the recursion sequence is, the more deeper, and thus the larger the stack becomes; if you use the middle of the command sequence. If you otherwise use/make the recursive call at the end of the command sequence, then the stack will become smaller (compared to the middle sequence), because there are less commands to be stacked. This last is because all recursive calls are already stacked/handled. The answer for this questions is kind of treated Functional Programming course ;)