;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        >>> switch-3way-right.rkt <<<                       ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require "switch.rkt" "switch-3way-abstract.rkt")
(provide switch-3way-right%)

(define switch-3way-right%
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
    ; zo iets?
    ;(field (connection (cons primary-switch secondary-switch)))

    ;
    ; SWITCH SETUP:            _________________________ out-left
    ;                         /                      ___ out-middle
    ; in ___ primary-switch _/___ secondary-switch _/___ out-right
    ;    
    (define/override
      (make-primary-switch primary-id secondary-id out-left)
      (thunk
       (make-object switch%
         primary-id connection in (cons out-left secondary-id))))
    
    (define/override
      (make-secondary-switch primary-id secondary-id out-middle out-right)
      (thunk
       (make-object switch%
         secondary-id connection primary-id (cons out-middle out-right))))

    (define/override (set-position-left! primary-switch secondary-switch)
      (thunk
       (send primary-switch set-position! left)))
      
    (define/override (set-position-middle! primary-switch secondary-switch)
      (thunk
       (send primary-switch set-position! right)
       (send secondary-switch set-position! left)))

    (define/override (set-position-right! primary-switch secondary-switch)
      (thunk
       (send primary-switch set-position! right)
       (send secondary-switch set-position! right)))
    
    ))