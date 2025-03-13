#lang racket/gui

(require try-catch 
         "config.rkt"
         "interface.rkt"


         ;(prefix-in server: "../tcp/server.rkt")


         )

(provide start-infrabel)

(define server (void))
(define infrabel 'dummy)
(define gui 'dummy)

(define status-callback (λ (callback) (set! status-callback callback)))

(define (startup-callback track)
  (λ (architecture version host port control-panel?)
    (let/cc return

      ;; starting message
      (send status-callback insert
            "Startup of the INFRABEL control software for modal railways.\n\n")

      ;; Connection to track
      (send status-callback insert "Connecting to the modal railway ... : ")
      (try
       ((send track config! architecture version)
        (send track start))
       (catch (exn:fail?
               (send status-callback insert
                     "\nERROR: could not connect to modal railway\n\n")
               (send track stop) (return #f) e)))
      (send status-callback insert "SUCCES\n")

      ;; server connection
      (send status-callback insert "Setting up INFRABEL server ... : ")
      (try
       ((set! infrabel (new infrabel% (track track)))) ;;;;;;;;;;;;;;;;;;;;;;;;;
       (catch
        (exn:fail?     
         (send
          status-callback insert
          (string-append*
           `("\nERROR: could not setup server on " ,host ":" ,port "\n\n")))
         (send track stop) (return #f) e)))
      (send status-callback insert "SUCCES\n")

      ;; starting control-panel
      (when control-panel?
        (send status-callback insert "Setting up control panel ... : ")
        (try
         ((send infrabel start-control-panel))
         (catch (exn:fail?
                 (send status-callback insert
                       "\nERROR: could not setup admin and debugger panel \n\n")
                 (send track stop) (display e) (return #f) e)))
        (send status-callback insert "SUCCES\n"))
   
      ;; ending message 
      (send status-callback insert
            (string-append*
             `("\nStartup succesful, server on  >>> " ,host ":" ,port " <<<")))
      (send status-callback insert "\nYou may close this window now.\n\n"))))

(define (start-infrabel track%)
  (define track (new track%))
  (define sim-versions (send track get-versions))
  (send (make-object gui%
          sim-versions
          (startup-callback track)
          status-callback
          ) show #t))

(define gui%
  (class frame%
    (init-field simulator-versions startup-callback status-callback)
    (super-new
     (label "INFRABEL startup manager")
     (width 0)
     (height 0)
     (style '(no-resize-border)))

    (let* ((global-pane (new horizontal-pane% (parent this)))
           (config-pane (new vertical-pane% (parent global-pane)))
           (status-pane (new vertical-pane% (parent global-pane)))
           (choice-pane (new object%))
           ; Choosable elements in gui
           (architecture 0)
           (sim 0)
           (host 0)
           (hostname "")
           (port 0)
           (portnumber "")
           (admin&debugger? DEFAULT_A&D_CHECKBOX))

      ;; start server
      (define (start)
        (startup-callback
         (if (zero? architecture) 'sim 'hw)
         (list-ref simulator-versions sim)
         (if (zero? host) DEFAULT_HOST hostname)
         (if (zero? port) DEFAULT_PORT portnumber)
         admin&debugger?))

      ;; railway architecture
      (let ((group-box-panel
             (new group-box-panel%
                  (label "Railway architecture")
                  (parent config-pane)
                  (alignment '(left center)))))
        (new radio-box%
             (label " ")
             (choices '("   simulator        " "    hardware"))
             (parent group-box-panel)
             (callback
              (λ (t e)
                (set! architecture (send t get-selection))
                (if (zero? architecture)
                    (send choice-pane enable #t)
                    (begin (send choice-pane enable #f)
                           (set! sim 0)
                           (send choice-pane set-selection 0)))))
             (style '(horizontal))))

      ;; simulator version
      (let ((group-box-panel
             (new group-box-panel%
                  (label "Simulator version")
                  (parent config-pane)
                  (alignment '(left center)))))
        (set! choice-pane
              (new choice%
                   (label "  ")
                   (parent group-box-panel)
                   (choices simulator-versions)
                   (stretchable-width #t)
                   (callback (λ (t e) (set! sim (send t get-selection))))
                   )))

      ;; trains
      (let ((group-box-panel
             (new group-box-panel%
                  (label "Trains on railway")
                  (parent config-pane)
                  (alignment '(left center)))))
        (new button%
             (label "Add Train")
             (parent group-box-panel)
             (callback (λ (t e) (displayln "new train")))))
      
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
             (callback (λ (t e) (start)))
             ))

      ;; status panel
      (let* ((text (new text%))
             (editor 
              (new editor-canvas%
                   (parent status-pane)
                   (editor text)
                   (style '(no-hscroll transparent no-focus))
                   (vert-margin 9)
                   (horiz-margin 5)
                   (min-width 400))))
        (status-callback text))
      
      )))