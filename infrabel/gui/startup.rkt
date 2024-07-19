#lang racket/gui

(require "../../config.rkt")
(provide startup)

(define (startup simulator-types startup-callback status-callback)
  (send (make-object gui% simulator-types startup-callback status-callback)
        show #t)
  )

(define gui%
  (class frame%
    (init-field simulator-types
                startup-callback
                status-callback)
    
    (super-new
     (label "INFRABEL startup manager")
     (width SERVER_STARTUP_WIDTH)
     (height SERVER_STARTUP_HEIGHT)
     (style '(no-resize-border #|float|#)))

    (let* ((global-pane (new horizontal-pane% (parent this)))
           (config-pane
            (new vertical-pane%
                 (parent global-pane)
                 (min-width (exact-floor (* SERVER_STARTUP_WIDTH .38)))))
           (status-pane
            (new vertical-pane%
                 (parent global-pane)
                 (min-width (exact-floor (* SERVER_STARTUP_WIDTH .62)))))
           (choice-pane (new object%))
           (type 0)
           (sim 0)
           (host 0)
           (hostname "")
           (port 0)
           (portnumber "")
           (admin&debugger? DEFAULT_A&D_CHECKBOX))

      ;; start server
      (define (start-server)
        (startup-callback
         (if (zero? type) 'simulator 'hardware)
         (list-ref simulator-types sim)
         (if (zero? host) DEFAULT_HOST hostname)
         (if (zero? port) DEFAULT_PORT portnumber)
         admin&debugger?))

      ;; railway type
      (let ((group-box-panel
             (new group-box-panel%
                  (label "Railway type")
                  (parent config-pane)
                  (alignment '(left center)))))
        (new radio-box%
             (label " ")
             (choices '("   Simulator        " "    Hardware"))
             (parent group-box-panel)
             (callback
              (λ (t e)
                (set! type (send t get-selection))
                (if (zero? type)
                    (send choice-pane enable #t)
                    (begin (send choice-pane enable #f)
                           (set! sim 0)
                           (send choice-pane set-selection 0)))))
             (style '(horizontal))))

      ;; simulator type
      (let ((group-box-panel
             (new group-box-panel%
                  (label "Simulator type")
                  (parent config-pane)
                  (alignment '(left center)))))
        (set! choice-pane
              (new choice%
                   (label "  ")
                   (parent group-box-panel)
                   (choices simulator-types)
                   (stretchable-width #t)
                   (callback (λ (t e) (set! sim (send t get-selection))))
                   )))

      ;; hostname
      (let ((group-box-panel
             (new group-box-panel%
                  (label "Hostname")
                  (parent config-pane))))
        (let ((horizontal-pane
               (new horizontal-pane%
                    (parent group-box-panel)
                    (alignment '(left center)))))
          (new radio-box%
               (label " ")
               (choices `(,(string-join `("  " ,DEFAULT_HOST "        ")) ""))
               (parent horizontal-pane)
               (callback (λ (t e) (set! host (send t get-selection))))
               (style '(horizontal)))
          (new text-field%
               (label "")
               (parent horizontal-pane)
               (callback (λ (t e) (set! hostname (send t get-value)))))
          ))

      ;; portnumber
      (let ((group-box-panel
             (new group-box-panel%
                  (label "Portnumber")
                  (parent config-pane))))
        (let ((horizontal-pane
               (new horizontal-pane%
                    (parent group-box-panel)
                    (alignment '(left center)))))
          (new radio-box%
               (label " ")
               (choices
                `(,(string-join `("  " ,DEFAULT_PORT "                "))
                  ""))
               (callback (λ (t e) (set! port (send t get-selection))))
               (parent horizontal-pane)
               (style '(horizontal)))
          (new text-field%
               (label "")
               (parent horizontal-pane)
               (callback (λ (t e) (set! portnumber (send t get-value)))))
          ))

      ;; checkbox and start button
      (let ((vertical-pane
             (new vertical-pane%
                  (parent config-pane)
                  (alignment '(center center)))))
        (new check-box%
             (label "   Start with admin panel and debugger ")
             (parent vertical-pane)
             (callback (λ (t e) (set! admin&debugger? (send t get-value))))
             (value DEFAULT_A&D_CHECKBOX)
             (vert-margin 8))
        (new button%
             (label "start")
             (parent vertical-pane)
             (callback (λ (t e) (start-server)))
             ))

      ;; status panel
      (let* ((text (new text%))
             (editor 
              (new editor-canvas%
                   (parent status-pane)
                   (editor text)
                   (style '(no-hscroll transparent no-focus))
                   (vert-margin 9)
                   (horiz-margin 5))))
        (status-callback text))
      
      )))