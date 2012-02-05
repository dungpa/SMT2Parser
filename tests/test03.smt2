(set-option :print-success true) 
(set-option :produce-unsat-cores true) ; enable generation of unsat cores
;; enable model generation
(set-option :produce-models true)
(set-option :produce-proofs true)
(declare-fun x () Int)
(set-option :produce-proofs false)