#lang racket/base

(require racket/flonum
         racket/format
         racket/match
         racket/math
         racket/struct

         portaudio)

(define leftb  0)
(define rightb 1)

(struct unit (name k-ins k-outs au-ins au-outs buffers procedure)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (v) 'unit)
      (lambda (v)
        (list (unquoted-printing-string (~a (unit-name v)))))))])

(define (unit-run1 u eng num-frames)
  ((unit-procedure u) u eng num-frames))

(struct engine
  (ext-in ext-out frames audio-bus* control-bus* buffers* units)
  #:mutable)

(define (engine-audio-bus-ref eng i)
  (vector-ref (engine-audio-bus* eng) i))

(define (engine-control-bus-ref eng i)
  (flvector-ref (engine-control-bus* eng) i))

(define (engine-control-bus-set! eng i v)
  (flvector-set! (engine-control-bus* eng) i v))

;; handle external events:
;; - add/remove buses
;; - modify control buses
;; - list/add/remove/modify units
;; - stop engine
(define (engine-process-ext est)
  (void))

(define (engine-run1 eng num-samples)
  (engine-process-ext eng)
  (for ([u (in-list (engine-units eng))])
    (unit-run1 u eng num-samples)))

(define (engine-current-time eng)
  (/ (->fl (engine-frames eng)) sample-rate))

(define sample-rate 44100)
(define time/sample (exact->inexact (/ 1 sample-rate)))

(define alloc-frames 44100)

(define two-pi (* 2 pi))

(define (make-audio-bus* size)
  (build-vector size (lambda (i) (make-flvector alloc-frames))))

(define (make-envf a d v)
  (define b (fl- v (fl/ (fl* (fl- v) a) d)))
  (define ma (fl/ v a))
  (define md (fl/ (- v) d))
  (define d+a (fl+ d a))
  (lambda (t)
    (cond
      [(fl<= t a) (fl* t (fl/ v a))]
      [(fl<= t d+a) (fl+ (fl* t md) b)]
      [else 0.0])))

(define envf (make-envf 0.1 1.0 0.75))

(define (make-env-unit trigger-b level-b)
  (unit (gensym 'env)
        (vector trigger-b)
        (vector level-b)
        (vector)
        (vector)
        (vector)
        (let ([start #f])
          (lambda (u eng num-samples)
            (match-define
              (unit _ (vector trigger-id) (vector level-id) _ _ _ _) u)
            (define trigger (engine-control-bus-ref eng trigger-id))
            (define level
              (cond
                ;; post-trigger
                [(and (zero? trigger) start)
                 (define level (envf (- (engine-current-time eng) start)))
                 (when (zero? level)
                   (set! start #f))
                 level]
                ;; not-triggered
                [(zero? trigger) 0.0]
                [else
                 ;; triggered
                 (set! start (engine-current-time eng))
                 ;; reset trigger
                 (engine-control-bus-set! eng trigger-id 0.0)
                 0.0]))
            (engine-control-bus-set! eng level-id level)))))

(define (make-sine-unit freq-b out-b)
  (unit (gensym 'sine)
        (vector freq-b)
        (vector)
        (vector)
        (vector out-b)
        (vector)
        (let ([phase 0.0])
          (lambda (u eng num-frames)
            (define buffer-time (fl* (->fl num-frames) time/sample))
            (match-define
              (unit _ (vector freq-id) _ _ (vector out-id) _ _) u)
            (define freq (engine-control-bus-ref eng freq-id))
            (define f (* freq two-pi))
            (define outb (engine-audio-bus-ref eng out-id))
            (define end-phase (fl+ phase buffer-time))
            (for ([i (in-range num-frames)]
                  [t (in-range phase end-phase time/sample)])
              (define v (flsin (fl* f t)))
              (flvector-set! outb i v))
            (set! phase end-phase)))))

(define (make-mult~ v in out)
  ;; XXX: allow name overrides
  (unit (gensym 'mult~)
        (vector)
        (vector)
        (vector in)
        (vector out)
        (vector)
        (lambda (u eng num-frames)
          (match-define
            (unit _ _ _ (vector in-id) (vector out-id) _ _) u)
          (define inb (engine-audio-bus-ref eng in-id))
          (define outb (engine-audio-bus-ref eng out-id))
          (for ([i (in-range num-frames)])
            (flvector-set! outb i (fl* v (flvector-ref inb i)))))))

(define (make-multk~ val-bus in-bus out-bus)
  (unit (gensym 'multk~)
        (vector val-bus)
        (vector)
        (vector in-bus)
        (vector out-bus)
        (vector)
        (lambda (u eng num-frames)
          (match-define
            (unit _ (vector val-id) _ (vector in-id) (vector out-id) _ _) u)
          (define in-buf (engine-audio-bus-ref eng in-id))
          (define out-buf (engine-audio-bus-ref eng out-id))
          (define val (engine-control-bus-ref eng val-id))
          (for ([i (in-range num-frames)])
            (flvector-set! out-buf i (fl* val (flvector-ref in-buf i)))))))

(define (make-copy~ in out)
  (unit (gensym 'copy~)
        (vector)
        (vector)
        (vector in)
        (vector out)
        (vector)
        (lambda (u eng num-frames)
          (match-define
            (unit _ _ _ (vector in-id) (vector out-id) _ _) u)
          (define inb (engine-audio-bus-ref eng in-id))
          (define outb (engine-audio-bus-ref eng out-id))
          (for ([i (in-range num-frames)])
            (flvector-set! outb i (flvector-ref inb i))))))

(define real->s16
  (let ([m (->fl 32767)])
    (lambda (x)
      (fl->exact-integer (flround (fl* m x))))))

(define ((buffer-filler eng) setter num-frames)
  (engine-run1 eng num-frames)
  (set-engine-frames! eng (+ (engine-frames eng) num-frames))
  (for ([i (in-range num-frames)]
        [l (in-flvector (engine-audio-bus-ref test-engine leftb))]
        [r (in-flvector (engine-audio-bus-ref test-engine rightb))])
    (setter (* i 2) (real->s16 l))
    (setter (+ 1 (* i 2)) (real->s16 r))))

(define test-engine
  (engine #f #f 0
          (make-audio-bus* 3)
          (make-flvector   3)
          (make-audio-bus* 0)
          (list (make-sine-unit 0 2)
                (make-env-unit 1 2)
                (make-multk~ 2 2 leftb)
                (make-copy~ leftb rightb))))

(define (note f)
  (engine-control-bus-set! test-engine 0 f)
  (engine-control-bus-set! test-engine 1 1.0))

#;
(match-define (list timer stats stopper)
  (stream-play (buffer-filler test-engine) 0.2 sample-rate))