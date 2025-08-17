;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                          >>> railway/gui-tab-panels/tab-trains.rkt <<<                         ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/gui

(provide tab-trains%)

(define tab-trains%
  (class panel%
    (init-field parent connection add-to-log add-to-update)
    (super-new (parent parent)
               (style '(auto-vscroll)))
    (define tab-panel (new vertical-panel% (parent this)))

    (define log-event (add-to-log "Trains Tab")) ; curryied

    (define train-panels '())
    
    ;(define (remove-train ) )
    #|
    (define (add-train current-segment next-segment)
      (let* ((gbp (new group-box-panel% (parent this) (label "T-1"))))
        (new button% (parent gbp) (label "Remove Train"))

        ))
|#
    (for-each
     (λ (train-id)
       (let* ((horizontal-pane (new horizontal-pane% (parent tab-panel)))
              (vertical-pane (new vertical-pane% (parent horizontal-pane)))
              (train-label (new message%
                                (label (string-append "Train '" (symbol->string train-id)))
                                (parent vertical-pane)))
              (location-label (new message% (label "______________") (parent vertical-pane)))
              (route-input (new text-field% (label "Follow Route") (parent vertical-pane)))
              (slider (new slider% (label "Change Speed") (min-value -40) (max-value 40)
                           (init-value 0) (parent vertical-pane)
                           (callback (λ (t e) (send connection set-train-speed! train-id
                                                    (send t get-value))))))
              (buttons-pane (new vertical-pane% (parent horizontal-pane))))
         (add-to-update (λ ()
                          (send location-label set-label
                                (string-append "location: "
                                               (symbol->string
                                                (send connection get-train-location train-id))))
                          (send slider set-value (send connection get-train-speed train-id))))
         (new button% (label "add-conductor") (parent buttons-pane)
              (callback (λ (t e) (send connection add-conductor-to-train "NMBS" train-id))))
         (new button% (label "to-prev") (parent buttons-pane)
              (callback (λ (t e) (send connection instruct-conductor-go-to-prev train-id))))
         (new button% (label "to-next") (parent buttons-pane)
              (callback (λ (t e) (send connection instruct-conductor-go-to-next train-id))))
         (new button% (label "follow") (parent buttons-pane)
              (callback (λ (t e) (void))))
         (new button% (label "unlock") (parent buttons-pane)
              (callback (λ (t e) (send connection unlock! train-id))))
         (new button% (label "stop") (parent buttons-pane)
              (callback (λ (t e) (send connection manual-stop! train-id))))))
     (send connection get-train-ids))
    
    #|tab-trains|#))
