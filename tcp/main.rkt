;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> tcp/main.rkt <<<                            ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require (prefix-in infrabel: "../infrabel/main.rkt")
         (prefix-in provider: "../provider/main.rkt"))

(define tcp-connection%
  (class object%
    (super-new)
    (define infrabel (λ (DUMMY) (void)))
    (define/public (setup object)
      (set! infrabel object))
    ; trains
    (define/public (get-train-list)
      (send infrabel get-train-list))
    (define/public (add-loco id previous-segment current-segment)
      (send infrabel add-train! id previous-segment current-segment))
    (define/public (get-test-train-list)
      (send infrabel get-test-train-list))
    ; segments
    (define/public (get-segment-ids)
      (send infrabel get-segment-ids))
    ; detection-blocks
    (define/public (get-detection-block-ids)
      (send infrabel get-detection-block-ids))
    (define/public (is-occupied?)
      (send infrabel is-occupied?))
    ; switches
    (define/public (get-switch-ids)
      (send infrabel get-switch-ids))
    (define/public (get-switch-position switch)
      (send infrabel get-switch-position switch))
    (define/public (set-switch-position! switch new-position)
      (send infrabel set-switch-position! switch new-position))
    ; crossings
    (define/public (get-crossing-ids)
      (send infrabel get-crossing-ids))
    (define/public (get-crossing-state crossing)
      (send infrabel get-crossing-state crossing))
    (define/public (open-crossing! crossing)
      (send infrabel set-crossing-state! crossing 'open))
    (define/public (close-crossing! crossing)
      (send infrabel set-crossing-state! crossing 'close))
    ; lights
    (define/public (get-light-ids)
      (send infrabel get-light-ids))
    (define/public (get-light-signal light)
      (send infrabel get-light-signal light))
    (define/public (set-sign-code! light new-signal)
      (send infrabel set-light-signal! light new-signal))
    ))

(define test
(let* ((connection (new tcp-connection%))
       (infrabel   (infrabel:temp-startup))
       )
  (send connection setup infrabel)
  (provider:temp-startup "NMBS" connection)
  )
)