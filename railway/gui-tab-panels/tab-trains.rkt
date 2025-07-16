#lang racket/gui

(provide tab-trains%)

(define tab-trains%
  (class vertical-panel%
    (init-field parent connection add-to-log add-to-update)
    (super-new (parent parent))

    (define log-event (add-to-log "Trains Tab")) ; curryied

    (define manage-trains-panel (new group-box-panel% (parent this) (label "Manage Trains")))
    (define train-panels '())
    
    ;(define (remove-train ) )
    #|
    (define (add-train current-segment next-segment)
      (let* ((gbp (new group-box-panel% (parent this) (label "T-1"))))
        (new button% (parent gbp) (label "Remove Train"))

        ))
|#
    
    (let* ((horizontal-pane (new horizontal-pane% (parent this)))
           (vertical-pane (new vertical-pane% (parent horizontal-pane)))
           (train-label (new message% (label "Train T-3") (parent vertical-pane)))
           (route-input (new text-field% (label "Follow Route") (parent vertical-pane)))
           (slider (new slider% (label "Change Speed") (min-value -40) (max-value 40) (init-value 0) (parent vertical-pane) (callback (λ (t e) (send connection set-train-speed! 'T-3 (send t get-value))))))
           (buttons-pane (new vertical-pane% (parent horizontal-pane))))
      (new button% (label "apply") (parent buttons-pane) (callback (λ (t e) (void))))
      (new button% (label "follow") (parent buttons-pane) (callback (λ (t e) (void))))
      (new button% (label "unlock") (parent buttons-pane) (callback (λ (t e) (send connection unlock! 'T-3))))
      (new button% (label "stop") (parent buttons-pane) (callback (λ (t e) (send connection emergency-stop! 'T-3) (send slider set-value 0)))))


    
    (let* ((horizontal-pane (new horizontal-pane% (parent this)))
           (message (new message% (label "T-5") (parent horizontal-pane)))
           (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send connection set-train-speed! 'T-5 (send t get-value))))))
           (buttons-pane (new vertical-pane% (parent horizontal-pane))))
      (new button% (label "apply") (parent buttons-pane) (callback (λ (t e) (void))))
      (new button% (label "unlock") (parent buttons-pane) (callback (λ (t e) (send connection unlock! 'T-5))))
      (new button% (label "stop") (parent buttons-pane) (callback (λ (t e) (send connection emergency-stop! 'T-5) (send slider set-value 0)))))


    
    (let* ((horizontal-pane (new horizontal-pane% (parent this)))
           (message (new message% (label "T-7") (parent horizontal-pane)))
           (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send connection set-train-speed! 'T-7 (send t get-value))))))
           (buttons-pane (new vertical-pane% (parent horizontal-pane))))
      (new button% (label "apply") (parent buttons-pane) (callback (λ (t e) (void))))
      (new button% (label "unlock") (parent buttons-pane) (callback (λ (t e) (send connection unlock! 'T-7))))
      (new button% (label "stop") (parent buttons-pane) (callback (λ (t e) (send connection emergency-stop! 'T-7) (send slider set-value 0)))))


    
    (let* ((horizontal-pane (new horizontal-pane% (parent this)))
           (message (new message% (label "T-9") (parent horizontal-pane)))
           (slider (new slider% (label "") (min-value -40) (max-value 40) (init-value 0) (parent horizontal-pane) (callback (λ (t e) (send connection set-train-speed! 'T-9 (send t get-value))))))
           (buttons-pane (new vertical-pane% (parent horizontal-pane))))
      (new button% (label "apply") (parent buttons-pane) (callback (λ (t e) (void))))
      (new button% (label "unlock") (parent buttons-pane) (callback (λ (t e) (send connection unlock! 'T-9))))
      (new button% (label "stop") (parent buttons-pane) (callback (λ (t e) (send connection emergency-stop! 'T-9) (send slider set-value 0)))))

    

    (new button% (label "test route") (parent manage-trains-panel) (callback (λ (t e) (send connection follow-route 'T-3 "1->2->3"))))


    #| </tab-trains> |#))



#| ; komt uit startup

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