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
    (init-field provider-name
                provider
                railway-tab-panels-list
                logs-callback
                add-to-log
                add-to-update
                stop-provider)
    (super-new (label (string-append provider-name APPLICATION_NAME))
               (width APPLICATION_WIDTH)
               (height APPLICATION_HEIGHT))
    (define/augment (on-close) (stop-provider))

    ;; Menu Bar
    (define menu-bar (new menu-bar% (parent this)))
    (let* ((menu-bar-menu (new menu% (label "Menu") (parent menu-bar)))
           (menu-bar-view (new menu% (label "View") (parent menu-bar)))
           (menu-bar-debug (when DEBUG_TOOLS? (new menu% (label "DEBUG") (parent menu-bar)))))
      (new menu-item% (label "Stop") (parent menu-bar-menu)
           (callback (λ (t e) (send provider stop))))
      (when DEBUG_TOOLS? 
        (new menu-item% (label "Disconnect") (parent menu-bar-debug)
             (callback (λ (t e) (void))))
        (new menu-item% (label "Reconnect") (parent menu-bar-debug)
             (callback (λ (t e) (void))))
        ))
    
    (define global-panel (new horizontal-panel% (parent this)))
    (define input-panel (new vertical-panel% (parent global-panel)))
    (define log-event (add-to-log provider-name)) ; curryied

    ;; Tab Panel 
    (define tabs (map car railway-tab-panels-list))
    (define tab-panes-list '())
    
    (define (fill-tab tab-panel nr)
      (send tab-panel change-children
            (lambda (c*)
              (list (list-ref tab-panes-list nr)))))
    
    (define (change-tab tab-panel event)
      (when (eq? (send event get-event-type) 'tab-panel)
        (fill-tab tab-panel (send tab-panel get-selection))))
   
    (define tab-panel (new tab-panel% (parent input-panel) (min-height 880)
                           (choices tabs) (style '(can-reorder can-close))
                           (min-width (- APPLICATION_WIDTH TRACK_WIDTH))
                           (vert-margin 7) (horiz-margin 10) (callback change-tab)))
    (for-each
     (λ (tab-pane)
       (set! tab-panes-list
             (append tab-panes-list (list (new tab-pane
                                               (parent tab-panel)
                                               (connection provider)
                                               (add-to-log log-event)
                                               (add-to-update add-to-update))))))
     (map cdr railway-tab-panels-list))
    (fill-tab tab-panel 0)

    (define info-panel (new vertical-panel% (parent global-panel) (min-width TRACK_WIDTH)))
    
    ;; Log Panel
    (define log-panel
      (new panel% (parent info-panel) (min-height (- APPLICATION_HEIGHT TRACK_HEIGHT))))
    (new editor-canvas%
         (parent log-panel)
         (editor logs-callback)
         (style '(transparent no-focus))
         (vert-margin 9)
         (horiz-margin 5)
         )

    #|provider-gui%|#))