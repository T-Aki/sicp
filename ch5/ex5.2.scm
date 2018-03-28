(controller
  (assign p (const 1))
  (assign c (const 1))
  (assign np (op *) (reg c) (reg p))
  test-c
  (test (op >) (reg c) (reg n))
  (branch (label fac-done))
  (assign np (op *) (reg c) (reg p))
  (assign nc (op +) (reg c) (const 1))
  (assign p (reg np))
  (assign c (reg nc))
  (goto (label test-c))
  fac-done)