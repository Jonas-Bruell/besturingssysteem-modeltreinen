;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                          >>> infrabel/main.rkt <<<                         ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require (prefix-in udp: "../udp/socket.rkt")
         "../../railway/main.rkt")

(provide start-infrabel   ; (/ -> /)
         )

(define (start-infrabel socket)
  (new infrabel% (connection socket))
  )

(define infrabel%
  (class railway%
    (init-field connection)
    (super-new)
    (inherit setup-object)
    ; track
    (define/override track 'track)
    ; segments
    (define/override (segment-list segment%)
      (thunk (setup-object
              segment%
              connection
              '(U-1 U-2 U-3 U-4 U-5 U-6 U-7)
              (λ (id) 'free))))
    ; detection-blocks
    (define/override (detection-block-list detection-block%)
      (thunk (setup-object
              detection-block%
              connection
              (send connection get-detection-block-ids)
              (λ (id)
                (if (member id (send connection get-occupied-detection-blocks))
                    'occupied
                    'free)))))
    ; switches
    (define/override (switch-list switch%)
      (thunk (setup-object
              switch%
              connection
              (send connection get-switch-ids)
              (λ (id) (send connection get-switch-position id)))))
    ; crossings
    (define/override (crossing-list crossing%)
      (thunk (setup-object
              crossing%
              connection
              '(C-1 C-2)
              (λ (id) 1))))
    ; lights
    (define/override (light-list light%)
      (thunk (setup-object
              light%
              connection
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