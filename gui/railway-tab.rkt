;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                           >>> railway-tab.rkt <<<                          ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui
(provide make-tab fill-tab)

(define DUMMY_CALLBACK (λ (tab event) (void)))

(struct tab-panel (super-panel))

(define (make-tab parent callback-test)
    (define super-panel
      (new horizontal-panel%
           (parent parent)))

    (define sub-panel
      (new vertical-panel%
           (parent super-panel)))

    (let ((test-callback callback-test))
      (struct switch-panel
        (super-panel
         (test-callback #:mutable)))
      (let* ((local-panel (new horizontal-panel% (parent sub-panel)))
             (identifier
              (new message%
                   (label "message")
                   (parent local-panel)))
             (position1-button (new button% (label "position 1")
                                    (parent local-panel)
                                    (callback (λ (t e) (println "test")))
                                    ))
             (position2-button (new button% (label "position 2")
                                    (parent local-panel)
                                    (callback (test-callback 5))
                                    ))
             )
        (switch-panel super-panel
                      test-callback))
      )
    (tab-panel super-panel)
    )

(define (fill-tab tab-panels tab-panel tab)
  (send tab-panel
        change-children
        (λ (c*)
          (list (tab-panel-super-panel (list-ref tab-panels tab))))))
  
  
