;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                         >>> provider/gui-tab-panels/tab-routes.rkt <<<                         ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/gui

(provide tab-routes%)

(define tab-routes%
  (class panel%
    (init-field parent connection add-to-log add-to-update)
    (super-new (parent parent)
               (style '(auto-vscroll)))
    (define tab-panel (new vertical-panel% (parent this)))

    (define log-event (add-to-log "Routes Tab")) ; curried

    (new button% (label "button") (parent tab-panel)
         (callback (λ (t e) (log-event "Button" "button Pressed"))))

    ))