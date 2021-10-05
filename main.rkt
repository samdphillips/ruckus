#lang racket/base

(require racket/flonum
         racket/match
         racket/math
         "engine.rkt"
         "unit.rkt")

;; XXX: use structs for bus addresses can use to guard creation
;; of units and engine calls

(define two-pi (* 2 pi))

(define (make-envf a d v)
  (define b (fl- v (fl/ (fl* (fl- v) a) d)))
  (define ma (fl/ v a))
  (define md (fl/ (- v) d))
  (define d+a (fl+ d a))
  (lambda (t)
    (cond
      [(fl<= t a) (fl* t ma)]
      [(fl<= t d+a) (fl+ (fl* t md) b)]
      [else 0.0])))

(define envf (make-envf 0.15 0.9 0.8))

(define (make-env-unit trigger-b level-b)
  (unit (gensym 'env)
        'control
        (vector trigger-b)
        (vector level-b)
        (vector)
        (vector)
        (vector)
        (let ([start #f])
          (lambda (u eng num-samples)
            (match-define
              (unit _ _ (vector trigger-id) (vector level-id) _ _ _ _) u)
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
        'audio
        (vector freq-b)
        (vector)
        (vector)
        (vector out-b)
        (vector)
        (let ([phase 0.0])
          (lambda (u eng num-frames)
            (define time/sample (exact->inexact (/ 1 (engine-sample-rate eng))))
            (define buffer-time (fl* (->fl num-frames) time/sample))
            (match-define
              (unit _ _ (vector freq-id) _ _ (vector out-id) _ _) u)
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
        'audio
        (vector)
        (vector)
        (vector in)
        (vector out)
        (vector)
        (lambda (u eng num-frames)
          (match-define
            (unit _ _ _ _ (vector in-id) (vector out-id) _ _) u)
          (define inb (engine-audio-bus-ref eng in-id))
          (define outb (engine-audio-bus-ref eng out-id))
          (for ([i (in-range num-frames)])
            (flvector-set! outb i (fl* v (flvector-ref inb i)))))))

(define (make-multk~ val-bus in-bus out-bus)
  (unit (gensym 'multk~)
        'audio
        (vector val-bus)
        (vector)
        (vector in-bus)
        (vector out-bus)
        (vector)
        (lambda (u eng num-frames)
          (match-define
            (unit _ _ (vector val-id) _ (vector in-id) (vector out-id) _ _) u)
          (define in-buf (engine-audio-bus-ref eng in-id))
          (define out-buf (engine-audio-bus-ref eng out-id))
          (define val (engine-control-bus-ref eng val-id))
          (for ([i (in-range num-frames)])
            (flvector-set! out-buf i (fl* val (flvector-ref in-buf i)))))))

(define (make-copy~ in out)
  (unit (gensym 'copy~)
        'audio
        (vector)
        (vector)
        (vector in)
        (vector out)
        (vector)
        (lambda (u eng num-frames)
          (match-define
            (unit _ _ _ _ (vector in-id) (vector out-id) _ _) u)
          (define inb (engine-audio-bus-ref eng in-id))
          (define outb (engine-audio-bus-ref eng out-id))
          (for ([i (in-range num-frames)])
            (flvector-set! outb i (flvector-ref inb i))))))

(define test-engine
  (make-engine #:num-audio-buses 3
               #:num-control-buses 3
               #:initial-units
               #;
               (list (make-sine-unit 0 0))
               (list (make-sine-unit 0 2)
                     (make-env-unit 1 2)
                     (make-multk~ 2 2 au-left-ch-bus)
                     (make-copy~ au-left-ch-bus au-right-ch-bus))))

(define (note-out f)
  (when f
    (engine-control-bus-set! test-engine 0 f)
    (engine-control-bus-set! test-engine 1 1.0)))

(define note->freq
  (let ()
    (define ref 440.0)
    (define a (expt 2 1/12))
    (lambda (n)
      (and n (* ref (expt a n))))))

(define key->note
  (let ()
    (define keys
      (list "1234567890"
            "qwertyuiop"
            "asdfghjkl;"
            "zxcvbnm,./"))
    (define keymap
      (for/fold ([h (hash)]) ([n (in-naturals)]
                              [r (in-list keys)])
        (for/fold ([h h]) ([c (in-string r)]
                           [i (in-naturals)])
          (hash-set h c (- (+ n (* 3 i)) 21)))))
    (lambda (c) (hash-ref keymap c #f))))

(require racket/class
         (except-in racket/gui
                    unit
                    unit?)
         threading)

(define w
  (let ()
    (define keyb-frame%
      (class frame%
        (super-new)
        (define/override (on-subwindow-char w e)
          (define keycode
            (send e get-key-code))
          (unless (eq? 'release keycode)
            (~> (key->note keycode)
                note->freq
                note-out)))))

    (new keyb-frame%
         [label "keys"]
         [width 300]
         [height 300])))

(engine-start test-engine)
(send w show #t)



#|
(note 440.0)
(engine-start test-engine)
(sleep 2)
(engine-stop test-engine)
|#

#|
(require plot)
(define data null)
((engine-buffer-filler test-engine)
 (lambda (i v)
   (when (even? i)
     (define x (/ i 2))
     (set! data (cons (list x v) data))))
 44100)
(plot (list (points (sort data < #:key car))))
|#