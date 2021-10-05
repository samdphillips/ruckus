#lang racket/base

(require portaudio
         racket/contract
         racket/flonum
         racket/match
         "unit.rkt")

(provide (contract-out
          [make-engine (->* ()
                            (#:ext-in            (-> any)
                             #:ext-out           (-> any/c any)
                             #:sample-rate       exact-nonnegative-integer?
                             #:num-audio-buses   exact-nonnegative-integer?
                             #:num-control-buses exact-nonnegative-integer?
                             #:num-buffers       exact-nonnegative-integer?
                             #:initial-units     (listof unit?))
                            engine?)])
         engine-sample-rate
         engine-audio-bus-ref
         engine-control-bus-ref
         engine-control-bus-set!
         engine-current-time
         engine-buffer-filler
         engine-start
         engine-stop

         au-left-ch-bus
         au-right-ch-bus)

(define au-left-ch-bus  0)
(define au-right-ch-bus 1)

(define (make-engine #:ext-in            [ext-in (lambda () #f)]
                     #:ext-out           [ext-out void]
                     #:sample-rate       [sample-rate 44100]
                     #:num-audio-buses   [num-audio-bus 2]
                     #:num-control-buses [num-control-bus 0]
                     #:num-buffers       [num-buffers 0]
                     #:initial-units     [initial-units null])
  (engine ext-in ext-out sample-rate 0
          (make-audio-bus* num-audio-bus)
          (make-flvector   num-control-bus)
          ;; XXX: buffers don't need to be homogenous,
          ;; so this is probably not the right API for them
          #;
          (make-audio-bus* num-buffers)
          ;; XXX: since this is unused just leave it empty
          #f
          initial-units
          #f #f #f))

(struct engine
  (ext-in
   ext-out
   sample-rate
   [frames       #:mutable]
   [audio-bus*   #:mutable]
   [control-bus* #:mutable]
   [buffers*     #:mutable]
   [units        #:mutable]
   [pa-timer     #:mutable]
   [pa-stats     #:mutable]
   [pa-stop      #:mutable]))

(define (engine-audio-bus-ref eng i)
  (vector-ref (engine-audio-bus* eng) i))

(define (engine-audio-left-ch eng)
  (engine-audio-bus-ref eng au-left-ch-bus))

(define (engine-audio-right-ch eng)
  (engine-audio-bus-ref eng au-right-ch-bus))

(define (engine-control-bus-ref eng i)
  (flvector-ref (engine-control-bus* eng) i))

(define (engine-control-bus-set! eng i v)
  (flvector-set! (engine-control-bus* eng) i v))

;; handle external events:
;; - add/remove buses
;; - modify control buses
;; - list/add/remove/modify units
;; - stop engine
;; XXX: maybe this could be handled by a special control unit?
(define (engine-process-ext est)
  (void))

(define (engine-run1 eng num-frames)
  (define ef (engine-frames eng))
  (define control? (zero? (modulo ef 64)))
  (define-syntax-rule (run? u)
    (if (control-unit? u) control? #t))

  (when control? (engine-process-ext eng))
  (for ([u (in-list (engine-units eng))]
        #:when (run? u))
    (unit-run u eng num-frames))
  (set-engine-frames! eng (+ ef num-frames)))

(define (engine-current-time eng)
  (fl/ (->fl (engine-frames eng))
       (->fl (engine-sample-rate eng))))

(define (make-audio-bus* size)
  (build-vector size (lambda (i) (make-flvector 64))))

(define real->s16
  (let ([m (->fl 32767)])
    (lambda (x)
      (fl->exact-integer (flround (fl* m x))))))

(define ((engine-buffer-filler eng) setter num-frames)
  (define (run1 start num-frames)
    (engine-run1 eng num-frames)
    (define m (* 2 start))
    (define n (+ m (* 2 num-frames)))
    (for ([i  (in-range m n 2)]
          [lv (in-flvector (engine-audio-left-ch eng))]
          [rv (in-flvector (engine-audio-right-ch eng))])
      (setter i (real->s16 lv))
      (setter (add1 i) (real->s16 rv))))

  (unless (zero? num-frames)
    (define prefix
      (let ([n (modulo (engine-frames eng) 64)])
        (if (zero? n) n (- 64 n))))
    (define-values (repeat suffix)
      (quotient/remainder (- num-frames prefix) 64))

    (unless (zero? prefix)
      (run1 0 prefix))
    (for ([x (in-range prefix (+ prefix (* 64 repeat)) 64)])
      (run1 x 64))
    (unless (zero? suffix)
      (run1 (+ prefix (* 64 repeat)) suffix))))

(define (engine-start eng)
  (match-define (list timer stats stopper)
    (stream-play (engine-buffer-filler eng) 0.05 (engine-sample-rate eng)))
  (set-engine-pa-timer! eng timer)
  (set-engine-pa-stats! eng stats)
  (set-engine-pa-stop! eng stopper))

(define (engine-stop eng)
  (define stop (engine-pa-stop eng))
  (when stop (stop)))
