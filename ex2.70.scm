;ex2.70.scm
(define tree1
  (generate-huffman-tree '((A 2)
                           (BOOM 1)
                           (GET 2)
                           (JOB 2)
                           (NA 16)
                           (SHA 3)
                           (YIP 9)
                           (WAH 1))))

;((leaf NA 16) ((leaf YIP 9) (((leaf A 2) ((leaf WAH 1) (leaf BOOM 1) (WAH BOOM) 2) (A WAH BOOM) 4) ((leaf SHA 3) ((leaf JOB 2) (leaf GET 2) (JOB GET) 4) (SHA JOB GET) 7) (A WAH BOOM SHA JOB GET) 11) (YIP A WAH BOOM SHA JOB GET) 20) (NA YIP A WAH BOOM SHA JOB GET) 36)

(define song '(Get a job
Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom))

(encode song tree1)
; ? ERROR
