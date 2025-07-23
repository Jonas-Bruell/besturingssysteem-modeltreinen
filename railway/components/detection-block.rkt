;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                         >>> railway/components/detection-block.rkt <<<                         ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "segment.rkt")

(provide detection-block%)

(define detection-block%
  (class segment%
    (super-new)

    ;
    ; This class represents an atomic abstraction of a railroad detection block
    ;
    ; @param add-to-log-callback :: function to add a string to the log callback
    ; @param id :: the name of the railroad segment
    ; @param connection :: lower-level implementation of detection block
    ; @param in :: railway element that goes into this block (clockwise)
    ; @param out :: railway element that exits out of this block (clockwise)
    ;
    (inherit-field add-to-log id connection in out)
    (inherit-field state)

    ;
    ; Possible railway detection-block states
    ;
    (define free     'free)
    (define reserved 'reserved)
    (define occupied 'occupied)

    ;
    ; add-to-log :: add a message of type string to the log
    ;
    ; @param string :: the string to be added to the log
    ;
    (define log-event (add-to-log (string-append "D-Block '" (symbol->string id)))) ; curryied

    ;
    ; get-state :: get the state of the railway detection block
    ;
    ; @returns symbol :: state-field of detection block
    ;
    (define/override (get-state)
      (let ((occupied-list (send connection get-occupied-detection-blocks)))
        (if (member id occupied-list)
            (unless (eq? state occupied)
              (set! state occupied)
              occupied)
            (unless (eq? state reserved)
              (set! state free)
              free))
        state))
    
    ;
    ; set-state! :: change the state of the railway crossing only when it is
    ;               not yet in the required state
    ;
    ; @param new-state symbol :: the new state of the detection-block
    ; @returns boolean :: #f when trying to reserve a reserved state
    ;                        or state = occupied
    ;                     #t when trying to free a state or reserve a free state
    ;
    (define/override (set-state! new-state)
      (cond ((eq? state occupied)
             (log-event "Method set-state! called" "detection block is OCCUPIED")
             #f)
            ((and (eq? new-state state) (eq? reserved state))
             (log-event "Method set-state! called" "detection block is ALREADY RESERVED")
             #f)
            ((and (eq? new-state state) (eq? free state))
             (log-event "Method set-state! called" "detection block is ALREADY FREE")
             #t)
            ((or (and (eq? free new-state) (or (eq? reserved state)    ; TODO : kan dit eenvoudiger?
                                               (eq? occupied state)))
                 (and (eq? free state) (or (eq? reserved new-state)
                                           (eq? occupied new-state)))
                 (and (eq? reserved state) (eq? occupied new-state)))
             (send connection set-state! id new-state)
             (set! state new-state)
             (log-event "Method set-state! called"
                        (string-append "changed state to '" (symbol->string new-state)))
             #t)
            (else (error "segment%: wrong message sent: " new-state))))
    ))