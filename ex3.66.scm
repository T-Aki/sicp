;ex3.66.scm

(1 1) :1
(1 2) :2
(2 2) :3
(1 3) :4
(2 3) :5
(1 4) :6
(3 3) :7
(1 5) :8
(2 4)
(1 6)
(3 4) :11
(1 7)
(2 5)
(1 8)
(4 4) : 15

;一般化すると
;(n n) : 2^n - 1
;(1 k) : 2(k-1) (ただし、k > 1)
;(k-1 k): 3*2^(k-2) - 1 (ただし、k > 1)

;となるので
;(1 100)は198番目の対
;(99 100)は 3*2^(98) -1 番目の対
;(100 100)は 2^(100)-1番目の対

