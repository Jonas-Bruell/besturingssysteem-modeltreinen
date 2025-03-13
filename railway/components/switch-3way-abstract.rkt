;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                      >>> switch-3way-abstract.rkt <<<                      ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "switch.rkt")
(provide middle switch-3way-abstract%)

(define middle 'middle)

(define switch-3way-abstract%
  (class switch%
    (super-new)
    
    ;
    ; This class represents an atomic abstraction of a railroad 3-way switch
    ;
    ; @param id :: (name switch1 switch2) the name of the railroad 3-way switch
    ;              and the names of the two other switches that make up this
    ;              3-way switch
    ; @param connection :: lower-level implementation of railroad 3-way switch
    ; @param in :: railway element that goes into this segment (clockwise)
    ; @param out :: (left middle right), list of railway element that exits out
    ;               of this segment (clockwise)
    ;
    (inherit-field id connection in out)
    (inherit-field state position)

    ;
    ; Aliasses
    ;
    (define primary-id    (cadr id))
    (define secondary-id  (caddr id))
    (define out-left      (car out))
    (define out-middle    (cadr out))
    (define out-right     (caddr out))
    (define out-S1-L-or-R 'dummy)
    (define out-S2-L      'dummy)
    (define out-S2-R      'dummy)
    
    ;
    ; Subswitches that make up the 3-way switch
    ;
    (abstract make-primary-switch)
    (define primary-switch
      (make-primary-switch primary-id secondary-id out-S1-L-or-R))
    (abstract make-secondary-switch)
    (define secondary-switch
      (make-secondary-switch primary-id secondary-id out-S2-L out-S2-R))
    (abstract set-position-left!)
    (abstract set-position-middle!)
    (abstract set-position-right!)

    ;
    ; get-id :: get the id of the railway 3-way switch
    ;
    ; @returns symbol :: id-field of 3-way switch
    ;
    (define/override (get-id) (car id))

    ;
    ; get-primary-id :: get the id of the primary switch in the 3-way switch
    ;
    ; @returns symbol :: id-field of the primary switch in the 3-way switch
    ;
    (define/public (get-primary-id) (cadr id))

    ;
    ; get-secondary-id :: get the id of the secondary switch in the 3-way switch
    ;
    ; @returns symbol :: id-field of the secondary switch in the 3-way switch
    ;
    (define/public (get-secondary-id) (caddr id))

    ;
    ; get-next-middle :: get the next railway element, in the direction of the
    ;                    2-ended side, in the middle.
    ;
    ; @returns symbol :: middle out-field of switch-3way
    ;
    (define/public (get-next-middle) (cadr out))
    
    ;
    ; get-next-right :: get the next railway element, in the direction of the
    ;                   2-ended side, on the right.
    ;
    ; @returns symbol :: right out-field of switch
    ;
    (define/override (get-next-right) (caddr out))

    ;
    ; set-position! :: change the position of the railway switch only when it is
    ;                  not yet in the required state
    ;
    ; @param new-position symbol :: the new position of the switch
    ;
    (define/override (set-position! new-position)
      (cond
        ((eq? new-position position)
         (void))
        ((eq? new-position left)
         (set-position-left! primary-switch secondary-switch)
         (set! position new-position))
        ((eq? new-position middle)
         (set-position-middle! primary-switch secondary-switch)
         (set! position new-position))
        ((eq? new-position right)
         (set-position-right! primary-switch secondary-switch)
         (set! position new-position))
        (else
         (error "switch-3way%: wrong message sent: " new-position))))))