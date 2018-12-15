#lang racket

(require racket/gui)
(require racket/draw)

(require "evodevo.rkt")

(define ui% (class object%
              (init-field draw-object)
              (field (frm #f)
                     (canvas #f)
                     (btn-next #f)
                     (btn-prev #f)
                     (btn-select #f)
                     (lbl-config #f))
              (define/public (reset)
                (set! frm (new frame%
                               [label "Life"]
                               [width 600]
                               [height 400]
                               [stretchable-width #f]
                               [stretchable-height #f]))
                (define vbox (new vertical-panel% [parent frm]))
                (define hbox1 (new horizontal-panel% [parent vbox]))
                (define hbox2 (new horizontal-panel% [parent vbox]))
                (set! canvas (new canvas%
                                  [parent hbox1]
                                  [min-width 200]
                                  [min-height 200]
                                  [paint-callback (λ (c dc) (draw-dc dc 200 200))]))
                (set! lbl-config (new message%
                                      [parent hbox1]
                                      [label ""]
                                      [min-width 80]))
                (set! btn-next (new button%
                                    [parent hbox2]
                                    [label "Next"]
                                    [min-width 40]
                                    [callback (λ (b e) (send draw-object next) (reset-draw))]))
                (set! btn-prev (new button%
                                    [parent hbox2]
                                    [label "Prev"]
                                    [min-width 40]
                                    [callback (λ (b e) (send draw-object previous) (reset-draw))]))
                (set! btn-select (new button%
                                      [parent hbox2]
                                      [label "Select"]
                                      [min-width 40]
                                      [callback (λ (b e)
                                                  (send draw-object
                                                        reset-with!
                                                        (send draw-object current-index))
                                                  (reset-draw))]))
                (reset-draw))
              (define/public (show) (when frm (send frm show #t)))
              (define/private (reset-draw)
                (send lbl-config set-label (format "Index: ~a" (send draw-object current-index)))
                (send canvas on-paint))
              (define/private (draw-dc dc w h)
                (let ([drawing (send draw-object draw-now)])
                  (if drawing
                      (send dc draw-bitmap (third drawing) 0 0)
                      (begin
                        (send dc set-brush "gray" 'solid)
                        (send dc draw-rectangle 0 0 w h)))))
              (super-new)))

(define (main)
  (let* ((dobj (setup-drawer '(20 30 90 2 3)))
         (ui (new ui% [draw-object dobj])))
    (send ui reset)
    (send ui show)))
 
