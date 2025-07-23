;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                              >>> infrabel/logic/conductor.rkt <<<                              ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(provide conductor%)

(define conductor%
  (class object%
    (init-field train-object provider-name add-to-update)
    (super-new)

    (define (reserve-next)
      




      
      (void))
      





    
    (add-to-update (λ () (reserve-next)))

    (define/public (follow-route route)
      (void))

    ))