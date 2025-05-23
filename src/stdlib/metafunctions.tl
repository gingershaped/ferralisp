(load lib/utilities)
(load lib/lists)

(def apply
  (lambda (func arglist)
    (eval
      (cons (q func) (quote-each arglist)))))

(def _map
  (lambda (func ls accum)
    (if ls
      (_map func
        (tail ls)
        (cons (func (head ls)) accum))
      (reverse accum))))

(def map
  (lambda (func ls) (_map func ls nil)))

(def _filter
  (lambda (func ls accum)
    (if ls
      (_filter func
        (tail ls)
        (if (func (head ls))
          (cons (head ls) accum)
          accum))
      (reverse accum))))

(def filter
  (lambda (func ls) (_filter func ls nil)))

(def _take-while
  (lambda (func ls accum)
    (if
      (if ls (func (head ls)) nil)
      (_take-while func
        (tail ls)
        (cons (head ls) accum))
      (reverse accum))))

(def take-while
  (lambda (func ls) (_take-while func ls nil)))

(def _foldl
  (lambda (func accum ls)
    (if ls
      (_foldl func
        (func accum (head ls))
        (tail ls))
      accum)))

(def foldl-default
  (lambda (func ls default)
    (if ls
      (_foldl func (head ls) (tail ls))
      default)))

(def foldl
  (lambda (func ls) (foldl-default func ls nil)))

(def _chain-last
  (macro args
    (if (tail args)
      (list (q (arg)) (head args))
      (eval
        (cons
          (q _chain-last)
          (cons
            (insert-end (head args) (htail args))
            (ttail args)))))))

(def chain-last
  (macro args
    (eval
      (cons
        (q _chain-last)
        (cons
          (insert-end (q arg) (head args))
          (tail args))))))

(def partial
  (lambda fn-and-params
    (list
      (filter
        (lambda (param) (type? param Name))
        (tail fn-and-params))
      (cons (quote (head fn-and-params))
        (map
          (lambda (param)
            (if (type? param Name)
              param
              (quote (htail param))))
          (tail fn-and-params))))))

(comment TODO: a more user-friendly version that can be called something like this: (partial add2 ? 42))
(comment and produces something like this: ((#0) ((q <builtin function tl_add2>) #0 (q 42))))

(comment Example of a partial function: add-n returns a single-argument function that adds a number to its argument)
(def add-n
  (lambda (num)
    (partial add2
      (q x)
      (list (q _num) num))))

(comment Another approach using substitute)
(def add-n-2
  (lambda (num)
    (substitute
      (q _num)
      (quote num)
      (lambda (x) (add2 x _num)))))
