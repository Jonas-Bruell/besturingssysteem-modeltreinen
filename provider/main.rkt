;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                          >>> provider/main.rkt <<<                         ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "../railway/main.rkt"
         "../gui/main.rkt")
(provide temp-startup   ; (/ -> /)
         ;tcp-connection% ; (/ -> object)
         )

(define tcp-connection%
  (class object%
    (super-new)
    ))

(define provider%
  (class railway%
    (init-field connection)
    (super-new)
    (inherit setup-object)
    (inherit add-a-train!)
    (inherit get-the-test-train-list)
    ;trains
    (define/public (add-train! id previous-segment current-segment)
      (add-a-train! connection id previous-segment current-segment))
    (define/public (get-test-train-list)
      (get-the-test-train-list connection))
    ; segments
    (define/override (segment-list segment%)
      (thunk (setup-object segment%
                           connection
                           (send connection get-segment-ids)
                           (λ (id) #f))))
    ; detection-blocks
    (define/override (detection-block-list detection-block%)
      (thunk (setup-object detection-block%
                           connection
                           (send connection get-detection-block-ids)
                           (λ (id) #f))))
    ; switches
    (define/override (switch-list switch%)
      (thunk (setup-object switch%
                           connection
                           (send connection get-switch-ids)
                           (λ (id) (send connection get-switch-position id)))))
    ; crossings
    (define/override (crossing-list crossing%)
      (thunk (setup-object crossing%
                           connection
                           (send connection get-crossing-ids)
                           (λ (id) (send connection get-crossing-state id)))))
    ; lights
    (define/override (light-list light%)
      (thunk (setup-object light%
                           connection
                           (send connection get-light-ids)
                           (λ (id) (send connection get-light-signal id)))))
    ))

(define (temp-startup provider-name connection)
  (let ((provider (new provider% (connection connection))))
    (start-gui (string-append "GUI " provider-name) provider)
    provider))