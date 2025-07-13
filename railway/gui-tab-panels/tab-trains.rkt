#lang racket

(provide tab-trains)

(define tab-trains
  (displayln "tab-trains"))

#|

      ; trains kunnen niet in startup gedefinieerd worden, aangezien geen track

      ;; trains ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let ((group-box-panel
             (new group-box-panel%
                  (label "Trains on railway")
                  (parent config-pane)
                  (alignment '(left center))))
            (providers PROVIDERS))
        (for-each
         (λ (train)
           (let ((h-pane (new horizontal-pane% (parent group-box-panel))))
             (new message%
                  (label (string-append "  train " (symbol->string train)))
                  (parent h-pane))
             (new choice%
                  (label "")
                  (parent h-pane)
                  (choices providers)
                  (callback (λ (t e) (void))))
             (new choice%
                  (label "")
                  (parent h-pane)
                  (choices sim-train-locations)
                  (callback (λ (t e) (void))))
             (new choice%
                  (label "")
                  (parent h-pane)
                  (choices sim-train-locations)
                  (callback (λ (t e) (void))))
             ))
         sim-trains))

|#