;; -*- no-byte-compile:t' -*-
(let (
      (a (lambda ()))))

(lambda ( a b )
  a b)

(defadvice a ( (b) )
  b)
