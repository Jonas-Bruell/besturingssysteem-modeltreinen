;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                         >>> railway/gui-tab-panels/tab-signals.rkt <<<                         ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/gui

(provide tab-signals%)

(define tab-signals%
  (class panel%
    (init-field parent connection add-to-log add-to-update)
    (super-new (parent parent)
               (style '(auto-vscroll)))
    (define tab-panel (new vertical-panel% (parent this)))

    (define log-event (add-to-log "Signals Tab")) ; curryied

    ;; CROSSINGS
    (define crossing-panel (new vertical-panel% (parent tab-panel) (alignment '(left top))))
    (define cgbp (new group-box-panel% (label "Crossings") (parent crossing-panel)
                      (stretchable-width #f) (stretchable-height #f) (border 3)))
    (for-each
     (λ (crossing-id)
       (let* ((gbp (new group-box-panel% (label (symbol->string crossing-id)) (horiz-margin 9)
                        (parent cgbp) (stretchable-width #f) (stretchable-height #f)))
              (hzp (new horizontal-panel% (parent gbp))))
         (for-each
          (λ (crossing-position)
            (new button% (label (symbol->string crossing-position)) (parent hzp)
                 (callback
                  (λ (t e) (log-event "Crossing Button Pressed"
                                      (string-append "change crossing '"
                                                     (symbol->string crossing-id)
                                                     " to position '"
                                                     (symbol->string crossing-position)))
                    (send connection set-crossing-position! crossing-id crossing-position)))))
          (send connection get-crossing-positions))))
     (send connection get-crossing-ids))

    ;; LIGHTS
    (define lights-panel (new vertical-panel% (parent tab-panel) (alignment '(left top))))
    (define lgbp (new group-box-panel% (label "Lights") (parent lights-panel)
                      (stretchable-width #f) (stretchable-height #f) (border 3)))
    (for-each
     (λ (light-id)
       (let* ((gbp (new group-box-panel% (label (symbol->string light-id)) (horiz-margin 9)
                        (parent lgbp) (stretchable-width #f) (stretchable-height #f)))
              (hzp (new horizontal-panel% (parent gbp))))
         (for-each
          (λ (light-signals)
            (new button% (label (symbol->string light-signals)) (parent hzp)
                 (callback
                  (λ (t e) (log-event "Lights Button Pressed"
                                      (string-append "change light '"
                                                     (symbol->string light-id)
                                                     " to signal '"
                                                     (symbol->string light-signals)))
                    (send connection set-light-signal! light-id light-signals)))))
          (send connection get-light-signals))))
     (send connection get-light-ids))

    #| </tab-crossings-lights > |#))
