#lang racket

(provide conductor%)

(define conductor%
  (class object%
    (init-field train-object add-to-update)
    (super-new)

    (define (reserve-next)
      




      
      (void))
      





    
    (add-to-update (λ () (reserve-next)))

    ))