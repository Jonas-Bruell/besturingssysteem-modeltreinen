;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                    >>> provider/gui.rkt <<<                                    ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/gui

(require "config.rkt")

(provide provider-gui%)

(define provider-gui%
  (class frame%
    (init-field provider-name stop-provider)
    (super-new (label (string-append provider-name APPLICATION_NAME))
               (width APPLICATION_WIDTH)
               (height APPLICATION_HEIGHT))
    (define/augment (on-close) (stop-provider))

    ;; Menu Bar
    (define menu-bar (new menu-bar% (parent this)))
    (let* ((menu-bar-menu (new menu% (label "Menu") (parent menu-bar)))
           (menu-bar-view (new menu% (label "View") (parent menu-bar)))
           (menu-bar-help (new menu% (label "Help") (parent menu-bar)))
           (menu-bar-debug (when DEBUG_TOOLS? (new menu% (label "DEBUG") (parent menu-bar)))))
      (new menu-item% (label "Stop") (parent menu-bar-menu)
           (callback (λ (t e) (void))))
      (when DEBUG_TOOLS? 
        (new menu-item% (label "Disconnect") (parent menu-bar-debug)
             (callback (λ (t e) (void))))
        (new menu-item% (label "Reconnect") (parent menu-bar-debug)
             (callback (λ (t e) (void))))


        (define global-pane (new horizontal-pane% (parent this)))
        (new button% (label "button") (parent global-pane) (callback (λ (t e) (send menu-bar-view set-label "View - live"))))
        ))

    

    

    #| </provider-gui%> |#))