(declare-fun x () (_ BitVec 64))
(declare-fun y () (_ BitVec 64))
(assert (not (= (bvand (bvnot x) (bvnot y)) (bvnot (bvor x y)))))
(check-sat)