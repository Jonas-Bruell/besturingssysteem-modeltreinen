;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      >>> adt-udp-communication.rkt <<<                     ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;; Description: ADT UPD communication                                         ;;
;; Arguments: none                                                            ;;
;; Output: dispatch-udp-communication                                         ;;
;; Messages:                                                                  ;;
;;  - start                                                                   ;;
;;  - stop                          : (/ -> /)                                ;;
;;  - add-loco                      : (symbol symbol symbol -> /)             ;;
;;  - get-loco-speed                : (symbol -> number)                      ;;
;;  - set-loco-speed!               : (symbol number -> /)                    ;;
;;  - get-detection-block-ids       : (/ -> pair)                             ;;
;;  - get-occupied-detection-blocks : (/ -> pair)                             ;;
;;  - get-switch-ids                : (/ -> pair)                             ;;
;;  - get-switch-position           : (symbol -> number)                      ;;
;;  - set-switch-position!          : (symbol number -> /)                    ;;
;;  - open-crossing!                : (symbol -> /)                           ;;
;;  - close-crossing!               : (symbol -> /)                           ;;
;;  - set-sign-code!                : (symbol symbol -> /)                    ;;
;; Extra info:                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
(require (prefix-in sim. "../simulator/interface.rkt"))
(provide make-udp-communication)

(define (make-udp-communication version setup)
  ((λ (setup) setup)
   (case setup
     ('hardware (sim.setup-hardware))
     ('straight (sim.setup-straight))
     ('straight-with-switch (sim.setup-straight-with-switch))
     ('loop (sim.setup-loop))
     ('loop-and-switches (sim.setup-loop-and-switches))))

  (define (start)
    ((λ (method) method)
     (case version
       ('sim (sim.start))
       )))

  (define (stop)
    ((λ (method) method)
     (case version
       ('sim (sim.stop))
       )))

  (define (add-loco train-id previous-segment-id current-segment-id)
    ((λ (method) method)
     (case version
       ('sim (sim.add-loco train-id previous-segment-id current-segment-id))
       )))

  (define (get-loco-speed train-id)
    ((λ (method) method)
     (case version
       ('sim (sim.get-loco-speed train-id))
       )))

  (define (set-loco-speed! train-id speed)
    ((λ (method) method)
     (case version
       ('sim (sim.set-loco-speed! train-id speed))
       )))

  (define (get-detection-block-ids)
    ((λ (method) method)
     (case version
       ('sim (sim.get-detection-block-ids))
       )))

  (define (get-occupied-detection-blocks)
    ((λ (method) method)
     (case version
       ('sim (sim.get-occupied-detection-blocks))
       )))

  (define (get-switch-ids)
    ((λ (method) method)
     (case version
       ('sim (sim.get-switch-ids))
       )))

  (define (get-switch-position switch-id)
    ((λ (method) method)
     (case version
       ('sim (sim.get-switch-position switch-id))
       )))

  (define (set-switch-position! switch-id position)
    ((λ (method) method)
     (case version
       ('sim (sim.set-switch-position! switch-id position))
       )))

  (define (open-crossing! crossing-id)
    ((λ (method) method)
     (case version
       ('sim (sim.open-crossing! crossing-id))
       )))

  (define (close-crossing! crossing-id)
    ((λ (method) method)
     (case version
       ('sim (sim.close-crossing! crossing-id))
       )))

  (define (set-sign-code! sign-id code)
    ((λ (method) method)
     (case version
       ('sim (sim.set-sign-code! sign-id code))
       )))
    
  (λ (message . arguments)
    (case message
      ('start (start))
      ('stop  (stop))
      ('add-loco (apply add-loco arguments))
      ('get-loco-speed (apply get-loco-speed arguments))
      ('set-loco-speed! (apply set-loco-speed! arguments))
      ('get-detection-block-ids (get-detection-block-ids))
      ('get-occupied-detection-blocks (get-occupied-detection-blocks))
      ('get-switch-ids (get-switch-ids))
      ('get-switch-position (apply get-switch-position arguments))
      ('set-switch-position! (apply set-switch-position! arguments))
      ('open-crossing! (apply open-crossing! arguments))
      ('close-crossing! (apply close-crossing! arguments))
      ('set-sign-code! (apply set-sign-code! arguments))
      (else (error "UPD-communication: wrong message: " message)))))