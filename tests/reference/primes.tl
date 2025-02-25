(d nil ())
(d list (q (args args)))
(d lambda
    (q (()
    (params expr)
    (list params expr)
    ))
)
(d quote-each
    (lambda (lyst)
    (i lyst
    (c
    (list (q q) (h lyst))
    (quote-each (t lyst))
    )
    nil
    )
    )
)
(quote-each (list 1 2 3))
(d apply
    (lambda (func arglist)
    (v (c (q func) (quote-each arglist)))
    )
)
(d map
    (lambda (func lyst)
    (i lyst
    (c
    (apply func (list (h lyst)))
    (map func (t lyst))
    )
    nil
    )
    )
)
(d partial
    (lambda (func arg)
    (list (q arglist) (list (q apply) (list (q q) func) (list (q c) arg (q arglist))))
    )
)
(map (partial s 0) (list 1 2 3 4))

(d foldl*
    (lambda (func lyst accum)
    (i lyst
    (foldl* func (t lyst) (apply func (list accum (h lyst))))
    accum
    )
    )
)
(d -
    (lambda arglist
    (i arglist
    (i (t arglist)
    (foldl* s (t arglist) (h arglist))
    (s 0 (h arglist))
    )
    0
    )
    )
)
(d +
    (lambda arglist
    (s 0 (apply - (c 0 arglist)))
    )
)
(- 10 4 3 2)
(+ 1 2 3 4 5)

(d divides?
    (lambda (factor int)
    (i int
    (i (l int factor)
    0
    (divides? factor (s int factor))
    )
    1
    )
    )
)
(d prime?*
    (lambda (int test-factor)
    (i (l 1 test-factor)
    (i (divides? test-factor int)
    0
    (prime?* int (s test-factor 1))
    )
    1
    )
    )
)
(d prime?
    (lambda (int)
    (i (e int 1)
    0
    (prime?* int (s int 1))
    )
    )
)
(map prime? (list 1 2 3 4 5 23 10000))

(d even?
    (lambda (int)
    (i int
    (i (l int 0)
    (even? (s 0 int))
    (odd? (s int 1))
    )
    1
    )
    )
)
(d odd?
    (lambda (int)
    (i int
    (i (l int 0)
    (odd? (s 0 int))
    (even? (s int 1))
    )
    0
    )
    )
)
(map even? (list 0 1 2 3 14 159 2653 58979 323846))