;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                             >>> train.rkt <<<                             ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(provide train%)

(define train%
  (class object%
    (super-new)

    ; This class represents an atomic abstraction of a train
    ;
    ; @param connection :: lower-level implementation of railroad crossing
    ; @param id :: the name of the railroad crossing
    ; @param previous :: the state of the railroad crossing - open or closed
    ; @param current :: the state of the railroad crossing - open or closed
    ;
    (init-field id connection previous current)
    ; Only works on simulator, retuns false on hardware
    (send connection add-loco id previous current)

    ; This class represents an atomic abstraction of a railroad crossing
    ;
    ; @param connection :: lower-level implementation of railroad crossing
    ; @param id :: the name of the railroad crossing
    ; @param state :: the state of the railroad crossing - open or closed
    ;
    (define/public (get-location) current)

    ; This class represents an atomic abstraction of a railroad crossing
    ;
    ; @param connection :: lower-level implementation of railroad crossing
    ; @param id :: the name of the railroad crossing
    ; @param state :: the state of the railroad crossing - open or closed
    ;
     (define/public (get-next-location) current)

    ; This class represents an atomic abstraction of a railroad crossing
    ;
    ; @param connection :: lower-level implementation of railroad crossing
    ; @param id :: the name of the railroad crossing
    ; @param state :: the state of the railroad crossing - open or closed
    ;
    (define/public (get-train-speed)
      (send connection get-loco-speed id))

    ; This class represents an atomic abstraction of a railroad crossing
    ;
    ; @param connection :: lower-level implementation of railroad crossing
    ; @param id :: the name of the railroad crossing
    ; @param state :: the state of the railroad crossing - open or closed
    ;
    (define/public (set-train-speed! new-speed)
      (send connection set-loco-speed! id new-speed))
    
    ))