#lang racket

(require furtle)
(require furtle/when)

(define DEBUG #f)
(define MAXDEPTH 16)

(define (ftree len a1 a2 d1 d2 level)
  (when DEBUG
    (displayln (format "ftree level: ~a~%" level)))
  (turtle-when (and (< level MAXDEPTH) (> len 1))
               (save)
               (forward len)
               (save)
               (right a1)
               (ftree (- len (abs (if (= d1 0) 1 d1))) a1 a2 d1 d2 (+ level 1))
               (restore)
               (left a2)
               (ftree (- len (abs (if (= d2 0) 1 d2))) a1 a2 d1 d2 (+ level 1))
               (restore)))

(define (tiny-variation-of x max-dx)
  (let ([should-vary (if (> (random 100) 50) #t #f)]
        [vary-direction (if (> (random 100) 50) (- 1) 1)])
    (if should-vary
        (+ x (* (random (+ 1 max-dx)) vary-direction))
        x)))
               
(define (gen-samples init-vals sample-size max-limit)
  (for/list ([i sample-size])
    (if (list? max-limit)
        (map (λ (x l) (tiny-variation-of x l)) init-vals max-limit)
        (map (λ (x) (tiny-variation-of x max-limit)) init-vals))))

(define serial-drawer% (class object%
                         [init-field [samples '()]]
                         [field (samples-vec (apply vector samples))
                                (config-vec (make-vector (vector-length samples-vec)))
                                (index 0)]
                         (define/public (sample-list)
                           samples)
                         (define/public (draw-next)
                           (if (and (>= index 0)
                                    (< index (vector-length samples-vec)))
                               (let* ([config (vector-ref samples-vec index)]
                                      [drawing (draw (apply ftree (append config '(0)))
                                                     #:width 200
                                                     #:height 200)])
                                 (when DEBUG
                                   (displayln (format "index=~a, sample=~a~%"
                                                      index
                                                      config)))
                                 (vector-set! config-vec index config)
                                 (set! index (+ index 1))
                                 (list (- index 1) config drawing))
                               #f))
                         (define/public (new-setup #:number [number (- 1)])
                           (cond
                             ((= number (- 1))
                              (if (<= index (vector-length samples-vec))
                                  (setup-drawer (vector-ref samples-vec (- index 1)))
                                  #f))
                               (else
                                (if (and (>= number 0) (< number (vector-length config-vec)))
                                    (setup-drawer (vector-ref config-vec number))
                                    #f))))
                         (define/public (reset-with! number
                                                     #:sample-size [sample-size 10]
                                                     #:variation-limit [variation-limit '(10 90 90 3 4)])
                           (if (and (>= number 0) (< number (vector-length config-vec)))
                               (begin
                                 (set! samples (gen-samples (vector-ref config-vec number)
                                                            sample-size
                                                            variation-limit))
                                 (set! samples-vec (apply vector samples))
                                 (set! config-vec (make-vector (vector-length samples-vec)))
                                 (set! index 0))
                               #f))
                         (super-new)))

(define (setup-drawer init-list
                      #:sample-size [sample-size 10]
                      #:variation-limit [variation-limit '(10 90 90 3 4)])
  (new serial-drawer% [samples (gen-samples init-list
                                            sample-size
                                            variation-limit)]))
