#lang racket/base

(require racket/format
         racket/struct)

(provide (struct-out unit)
         unit-run
         control-unit?)

(struct unit (name type k-ins k-outs au-ins au-outs buffers procedure)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (v) 'unit)
      (lambda (v)
        (list (unquoted-printing-string (~a (unit-name v)))))))])

(define (unit-run u eng num-frames)
  ((unit-procedure u) u eng num-frames))

(define (control-unit? u)
  (eq? 'control (unit-type u)))

