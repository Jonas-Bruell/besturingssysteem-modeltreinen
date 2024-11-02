;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                         >>> detection-block.rkt <<<                        ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "segment.rkt")
(provide detection-block%)

(define detection-block%
  (class segment%
    (super-new)

    ;
    ; This class represents an atomic abstraction of a railroad detection block
    ;
    ; @param id :: the name of the railroad segment
    ; @param connection :: lower-level implementation of detection block
    ; @param in :: railway element that goes into this block (clockwise)
    ; @param out :: railway element that exits out of this block (clockwise)
    ;
    (inherit-field id connection in out)
    (inherit-field state)

    ;
    ; Possible railway detection-block states
    ;
    (define free     'free)
    (define reserved 'reserved)
    (define occupied 'occupied)

    ;
    ; set-state! :: change the state of the railway crossing only when it is
    ; not yet in the required state
    ;
    ; @param new-state symbol :: the new state of the detection-block
    ; @returns boolean :: #f when trying to reserve a reserved state
    ;                        or when trying to occupy a occupied state
    ;                     #t when trying to free a state
    ;
    (define/override (set-state! new-state)
      (cond ((or (and (eq? new-state state) (or (eq? reserved state)
                                                (eq? occupied state)))
                 (and (eq? state occupied) (eq? new-state reserved)))
             #f)
            ((and (eq? new-state state) (eq? free state))
             #t)
            ((or (and (eq? free new-state) (or (eq? reserved state)
                                               (eq? occupied state)))
                 (and (eq? free state) (or (eq? reserved new-state)
                                           (eq? occupied new-state)))
                 (and (eq? reserved state) (eq? occupied new-state)))
             (send connection set-state! new-state)
             (set! state new-state)
             #t)
            (else (error "segment%: wrong message sent: " new-state))))
    ))