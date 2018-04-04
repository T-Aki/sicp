;
(controller
  (assign guss (const 1.0))
  test-good-enough?
  (test (op good-enough) (reg guess))
  (branch (label done))
  (assign guess (op improve) (reg guess))
  (goto test-good-enough?)
  done)
