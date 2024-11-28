;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        >>> switch-3way-left.rkt <<<                        ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "switch.rkt" "switch-3way-abstract.rkt")
(provide switch-3way-left%)

(define switch-3way-left%
  (class switch-3way-abstract%
    (super-new)

    ;
    ; This class represents an atomic abstraction of a railroad 3-way switch
    ;
    ; @param id :: the name of the railroad 3-way switch
    ; @param connection :: lower-level implementation of railroad 3-way switch
    ; @param in :: railway element that goes into this segment (clockwise)
    ; @param out :: (left middle right), list of railway element that exits out
    ;               of this segment (clockwise)
    ;
    (inherit-field id connection in out)
    (inherit-field state position)
    (field
     (switches
      (cons
       (make-object switch%
         (cadr id) (car connection) in (cons (caddr id) (caddr out)))
       (make-object switch%
         (caddr id) (cdr connection) (cadr id) (cons (car out) (cadr out)))
       )))

    ;
    ; SWITCH SETUP:                                  ___ out-left
    ;                         ___ secondary-switch _/___ out-middle
    ; in ___ primary-switch _/__________________________ out-right
    ;
    #|
    (define/override
      (make-primary-switch primary-id secondary-id out-right)
      (thunk
       (make-object switch%
         primary-id connection in (cons secondary-id out-right))))
    
    (define/override
      (make-secondary-switch primary-id secondary-id out-left out-middle)
      (thunk
       (make-object switch%
         secondary-id connection primary-id (cons out-left out-middle))))
    |#

    (define/override (set-position-left! primary-switch secondary-switch)
      (thunk
       (send primary-switch set-position! left)
       (send secondary-switch set-position! left)))
      
    (define/override (set-position-middle! primary-switch secondary-switch)
      (thunk
       (send primary-switch set-position! left)
       (send secondary-switch set-position! right)))

    (define/override (set-position-right! primary-switch secondary-switch)
      (thunk
       (send primary-switch set-position! right)))

    ))