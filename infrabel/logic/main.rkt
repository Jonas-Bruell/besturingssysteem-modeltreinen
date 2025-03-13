;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                          >>> infrabel/main.rkt <<<                         ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "../../railway/interface.rkt")
(require "../../track/interface.rkt")
(define track (new track%))
(send track set-version! "hardware")
(send track config 'sim 'hardware)
(send track start)



(define infrabel%
  (class railway%
    (super-new)
    ))



(define infrabel (new infrabel% (track track)))

#|



(provide start-infrabel   ; (/ -> /)
         )

(define (start-infrabel socket version)
  (new infrabel% (connection socket) (track-version version))
  )

(define infrabel*%
  (class railway%
    (init-field track)
    (super-new)
    (inherit setup-railway-objects)
    ; track
    (define/override (track)
      (dynamic-require
       (string-append "infrabel/routes/" track-version ".rkt") 'TRACK))
    ; segments
    (define/override (make-segments-list segment%)
      (thunk (setup-railway-objects
              segment%
              track
              '(U-1 U-2 U-3 U-4 U-5 U-6 U-7)
              (λ (id) 'free))))
    ; detection-blocks
    (define/override (make-detection-blocks-list detection-block%)
      (thunk (setup-railway-objects
              detection-block%
              track
              (send connection get-detection-block-ids)
              (λ (id)
                (if (member id (send connection get-occupied-detection-blocks))
                    'occupied
                    'free)))))
    ; switches
    (define/override (make-switches-list switch%)
      (thunk (setup-railway-objects
              switch%
              track
              (send connection get-switch-ids)
              (λ (id) (send connection get-switch-position id)))))
    ; crossings
    (define/override (make-crossings-list crossing%)
      (thunk (setup-railway-objects
              crossing%
              track
              '(C-1 C-2)
              (λ (id) 1))))
    ; lights
    (define/override (make-lights-list light%)
      (thunk (setup-railway-objects
              light%
              track
              '(L-1 L-2)
              (λ (id) 'Hp1))))
    ;trains
    (inherit add-a-train!)
    (inherit get-the-test-train-list)
    (define/public (add-train! id previous-segment current-segment)
      (add-a-train! connection id previous-segment current-segment))
    (define/public (get-test-train-list)
      (get-the-test-train-list connection))
    ))
|#