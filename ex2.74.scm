;ex2.74.scm
#|
a
getで従業員名が取り出せるようにように構造化する。

事務所名を型タグとして追加する
|#

(define (get-employee branch employee)
  ((get 'get-employee 'branch) employee))

;b
(define (get-salary branch salary)
  ((get 'get-salary 'branch) salary))

;getで給料が取り出せるようにように構造化する。


;c
(define (find-employee-record branch-list employee-name)
  (if (null? branch-list) '())
  (append (get-employee (car branch-list) employee-name)
          (find-employee-record (cdr branch-list) employee-name)))

;d
#|
新しい会社のデータに会社タグを追加し、
必要なgetとputを作成する
|#


