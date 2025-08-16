;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                             >>> infrabel/logic/reservation.rkt <<<                             ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "../../railway/algorithms/calculate-reservation-blocks.rkt")

(provide dispatcher%)

(define dispatcher%
  (class object%
    (init-field infrabel add-to-log add-to-update)
    (super-new)

    (define reservation-blocks (calculate-reservation-blocks infrabel))

    (define log-event (add-to-log "Dispatcher"))

    (define/public (for-each-track-element elements-list dblock-λ switch-λ segment-λ)
      (for-each
       (λ (element)
         (let ((type (car element)) (id   (cdr element)))
           (cond ((eq? type 'dblock) (dblock-λ id))
                 ((eq? type 'switch) (switch-λ id))
                 ((eq? type 'segment) (segment-λ id))
                 (else (error "INFRABEL > dispacher > for-each-element - Doesn't exist: " type)))))
       elements-list))

    (define/public (reserve-block curr next)
      (log-event "Reservation of block requested" "reservating block if possible")
      (define reserved 'reserved)
      (define to-be-reserved-list
        (caddr (findf (λ (x) (and (eq? (car x) curr) (eq? (cadr x) next))) reservation-blocks)))
      (let/cc return
        ; check if any elements are already reserved --> return false
        (for-each-track-element
         to-be-reserved-list
         (λ (id) (when (eq? (send infrabel get-detection-block-state id) reserved) (return #f)))
         (λ (id) (when (eq? (send infrabel get-switch-state id) reserved) (return #f)))
         (λ (id) (when (eq? (send infrabel get-segment-state id) reserved) (return #f))))
        ; if no elements where reserved --> reserve all elements
        (for-each-track-element
         to-be-reserved-list
         (λ (id) (send infrabel set-detection-block-state! id reserved))
         (λ (id) (send infrabel set-switch-state! id reserved))
         (λ (id) (send infrabel set-segment-state! id reserved)))
        ; return all reserved elements of false
        to-be-reserved-list))

    (define/public (free-block elements-list)
      (log-event "Free of block requested" "freeing block")
      (define free 'free)
      (for-each-track-element
       elements-list
       (λ (id) (send infrabel set-detection-block-state! id free))
       (λ (id) (send infrabel set-switch-state! id free))
       (λ (id) (send infrabel set-segment-state! id free))))

    ;;
    ;; automatic opening/closing of crossings
    ;;
    (let* ((crossing-ids (send infrabel get-crossing-ids))
           (segment-ids-list (map (λ (id) (send infrabel get-crossing-segments id)) crossing-ids))
           (closed 'closed) (open 'open) (pending 'pending) (occupied 'occupied))
      (define (>=1-segment-occupied? segment-ids)
        (not (let/cc return
               (for-each (λ (segment-id)
                           (when (eq? (send infrabel get-detection-block-state segment-id) occupied)
                             (return #f)))
                         segment-ids))))
      (define (is-state? state crossing-id)
        (eq? state (send infrabel get-crossing-position crossing-id)))
      (define (close-crossing id)
        (unless (is-state? closed id)
          (log-event (string-append "Train on detection-block in crossing '" (symbol->string id))
                     "closing crossing")
          (send infrabel set-crossing-position! id closed)))
      (define (open-crossing id)
        (unless (is-state? open id)
          (log-event (string-append "No train on detection-block in crossing '" (symbol->string id))
                     "opening crossing")
          (send infrabel set-crossing-position! id open)))
      (add-to-update
       (λ () (for-each
              (λ (crossing-id segment-ids)
                (let/cc return
                  (when (eq? (send infrabel get-crossing-position crossing-id) pending)
                    (return #f))
                  (if (>=1-segment-occupied? segment-ids)
                      (close-crossing crossing-id)
                      (open-crossing crossing-id))))
              crossing-ids segment-ids-list))))

    ;;
    ;; automatic red lights
    ;;
    (let* ((light-ids (send infrabel get-light-ids))
           (segment-ids (map (λ (light-id) (send infrabel get-light-segment light-id)) light-ids))
           (red 'Hp0) (occupied 'occupied))
      (define (is-color? state light-id)
        (eq? state (send infrabel get-light-signal light-id)))
      (define (turn-light-red id)
        (unless (is-color? red id)
          (log-event (string-append "Train on detection-block with light '" (symbol->string id))
                     "turning light red")
          (send infrabel set-light-signal! id red)))
      (define (restore-light-previous id)
        (when (is-color? red id)
          (log-event (string-append "No train on detection-block with light '" (symbol->string id))
                     "restoring light to previous signal")
          (send infrabel set-light-signal! id (send infrabel get-prev-light-signal id))))
      (add-to-update
       (λ () (for-each
              (λ (light-id segment-id)
                (if (eq? (send infrabel get-detection-block-state segment-id) occupied)
                    (turn-light-red light-id)
                    (restore-light-previous light-id)))
              light-ids segment-ids))))

    ))