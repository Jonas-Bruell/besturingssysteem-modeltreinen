;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                  >>> infrabel/startup.rkt <<<                                  ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require try-catch racket/gui racket/exn racket/date
         "config.rkt"
         "interface.rkt"
         "server.rkt"
         "gui.rkt"
         )

(provide start-infrabel)
(define infrabel #f)

;;
;; Logging
;;
(date-display-format 'rfc2822)
(define logs-callback (λ (callback) (set! logs-callback callback)))
(define add-to-log
  (curry (λ (service origin action event)
           (send logs-callback insert
                 (string-append (date->string (current-date) #t)
                                "  ---  " service " > " origin " > " action " : " event "\n")))))

;;
;; Automatic Updating
;;
(define to-update-list '())
(define (add-to-update lambda-wrapped-function)
  (set! to-update-list (append to-update-list (list lambda-wrapped-function))))
(thread (λ () (let loop () (for-each (λ (x) (x)) to-update-list) (sleep 1) (loop))))

;;
;; startup-callback
;;
(define status-callback (λ (callback) (set! status-callback callback)))
(define (startup-callback track tab-panels sim-graphics)
  (define (print-new-setup s) (send status-callback insert (string-append s " ... : ")))
  (define (print-succes) (send status-callback insert "SUCCES\n"))
  (define (print-error e i)
    (send status-callback insert (string-append "\n\n>>> ERROR: " i "\n"))
    (send status-callback insert (string-append (exn->string e) "\n\n")))
  (λ (architecture version host port control-panel?)
    (let/cc return
      (let ((server #f)
            (gui #f))

        ;; starting message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (send status-callback insert (string-append "\nStartup of the " APPLICATION_NAME ".\n\n"))

        ;; connect to track ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (print-new-setup "Setting up modal railway")
        (try
         ((send track config! architecture version))
         (catch (exn:fail? (print-error e "could not connect to modal railway")
                           (return #f) e)))
        (print-succes)

        ;; connect to server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (print-new-setup "Setting up INFRABEL server")
        (try
         ((set! server (new infrabel-server%
                            (host host)
                            (port port)
                            (add-to-log add-to-log)
                            (add-to-update add-to-update))))
         (catch (exn:fail? (print-error e (string-append "could not setup on " host ":" port))
                           (send track stop) (return #f) e)))
        (print-succes)

        ;; startup INFRABEL with track & server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (print-new-setup "Initialising INFRABEL Control Software")
        (try
         ((set! infrabel (new infrabel%
                              (connection track)
                              (server server)
                              (add-to-log add-to-log)
                              (add-to-update add-to-update)))
          (send server init! infrabel))
         (catch (exn:fail? (print-error e "could not initialise INFRABEL Control Software")
                           (send server stop) (send track stop) (return #f) e)))
        (print-succes)

        ;; starting control-panel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (when control-panel?
          (print-new-setup "Initialising INFRABEL Control Panel")
          (try
           ((set! gui (new infrabel-gui%
                           (infrabel infrabel)
                           (railway-tab-panels-list tab-panels)
                           (logs-callback logs-callback)
                           (add-to-log add-to-log)
                           (add-to-update add-to-update)
                           (set-simulator-panel (car sim-graphics))
                           (simulator-panel (cdr sim-graphics))))
            (send gui show #t))
           (catch (exn:fail? (print-error e "could not initialise INFRABEL Control Panel")
                             (send server stop) (send track stop) (return #f) e)))
          (print-succes))

        ;; start track ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (print-new-setup "Starting modal railway")
        (try
         ((send track start))
         (catch (exn:fail? (print-error e "could not start the modal railway")
                           (send server stop) (send track stop) (return #f) e)))
        (print-succes)
   
        ;; ending message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (send status-callback insert
              (string-append "\nStartup succesful, server on  >>> " host ":" port " <<<"))
        (send status-callback insert "\nYou may close this window now.\n\n")

        #| </startup-callback> |#))))

;;
;; gui%
;;
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
           (control-panel? DEFAULT_CONTROL_PANEL_CHECKBOX))

      ;; start server
      (define (start)
        (startup-callback
         (if (zero? architecture) 'sim 'hw)
         (list-ref simulator-versions sim)
         (if (zero? host) DEFAULT_HOST hostname)
         (if (zero? port) DEFAULT_PORT portnumber)
         control-panel?))

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
      (let ((vertical-pane (new vertical-pane% (parent config-pane) (alignment '(center center)))))
        (new check-box%
             (label "   Start with Control Panel ")
             (parent vertical-pane)
             (callback (λ (t e) (set! control-panel? (send t get-value))))
             (value DEFAULT_CONTROL_PANEL_CHECKBOX)
             (vert-margin 8))
        (new button% (label "start") (parent vertical-pane) (callback (λ (t e) (start)))))

      ;; status panel
      (let* ((text (new text%))
             (editor (new editor-canvas%
                          (parent status-pane)
                          (editor text)
                          (style '(transparent no-focus))
                          (vert-margin 9)
                          (horiz-margin 5)
                          (min-width 400))))
        (status-callback text))
      
      #| </gui%>|#)))

;;
;; start-infrabel
;;
(define (start-infrabel track% tab-panels sim-graphics)
  (define track (new track%))
  (define sim-versions (send track get-versions))
  (send (make-object gui%
          sim-versions
          (startup-callback track tab-panels sim-graphics)
          status-callback) show #t)
  (λ () infrabel))