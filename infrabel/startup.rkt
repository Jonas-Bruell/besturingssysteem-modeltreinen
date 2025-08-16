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

(define logs-callback (new text%))
(define infrabel #f)
(define server #f)
(define startup-gui #f)
(define gui #f)

;;
;; Logging
;
(date-display-format 'iso-8601)
(define (date) (date->string (current-date) #t))
(define log-file-path
  (string-append "infrabel/logs/"
                 (string-replace (string-replace (date) "T" ", ") ":" "-")
                 ".log.txt"))
(date-display-format 'rfc2822)
(define (save-to-log-file log-string)
  (call-with-output-file* log-file-path (λ (out) (writeln log-string out)) #:exists 'append))
(define add-to-log
  (curry (λ (service origin action event)
           (let ((log-line (string-append (date) "  ---  "
                                          service " > " origin " > " action " : " event)))
             (save-to-log-file log-line)
             (send logs-callback insert (string-append log-line "\n"))))))

;;
;; Automatic Updating
;;
(define to-update-list '())
(define (add-to-update lambda-wrapped-function)
  (set! to-update-list (append to-update-list (list lambda-wrapped-function))))
(define auto-update-loop
  (thread (λ () (let loop () (for-each (λ (x) (x)) to-update-list) (sleep 1) (loop)))))

;;
;; Stop Infrabel
;;
(define (stop-infrabel)
  (kill-thread auto-update-loop)
  (send gui show #f))

;;
;; startup-callback
;;
(define (startup-callback track tab-panels sim-graphics)
  (define log-event (add-to-log "INFRABEL" "Startup" "Startup Manager"))
  (define (print-new-setup s) (log-event (string-append s " ... : ")))
  (define (print-succes) (log-event "SUCCES"))
  (define (print-error e i)
    (log-event (string-append "ERROR: " i
                              ", internal error: " (exn->string e))))
  (λ (architecture version host port control-panel?)
    (let/cc return

      ;; starting message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (log-event (string-append "Startup of the " APPLICATION_NAME))

      ;; connect to track ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (print-new-setup "Setting up modal railway")
      (try
       ((send track config! architecture version))
       (catch (exn:fail? (print-error e "could not connect to modal railway")
                         (send track stop) (return #f) e)))
      (print-succes)

      ;; start track (hardware) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (eq? architecture 'hw) 
        (print-new-setup "Starting modal railway hardware")
        (try
         ((send track start))
         (catch (exn:fail? (print-error e "could not start the modal railway")
                           (send track stop) (return #f) e)))
        (print-succes))

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
                            (add-to-update add-to-update)
                            (stop-infrabel stop-infrabel)))
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
                         (stop-infrabel stop-infrabel)
                         (add-to-log add-to-log)
                         (add-to-update add-to-update)
                         (set-simulator-panel (car sim-graphics))
                         (simulator-panel (cdr sim-graphics))))
          (send gui show #t)
          (send startup-gui show #f))
         (catch (exn:fail? (print-error e "could not initialise INFRABEL Control Panel")
                           (send server stop) (send track stop) (return #f) e)))
        (print-succes))

      ;; start track (simulator) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (when (eq? architecture 'sim) 
        (print-new-setup "Starting modal railway simulator")
        (try
         ((send track start))
         (catch (exn:fail? (print-error e "could not start the modal railway")
                           (send server stop) (send track stop) (send gui show #f)
                           (send startup-gui show #t) (return #f) e)))
        (print-succes))
   
      ;; ending message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (log-event (string-append "Startup succesful, server on  >>> " host ":" port " <<<"))

      #|startup-callback|#)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                           GUI                                                  ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define gui%
  (class frame%
    (init-field simulator-versions startup-callback)
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
           (architecture DEFAULT_ARCHITECTURE_RADIO_BOX)
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
             (selection DEFAULT_ARCHITECTURE_RADIO_BOX)
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

      ;; resolve startup side effect from default simulator/hardware radio box
      (if (zero? architecture)
          (send choice-pane enable #t)
          (begin (send choice-pane enable #f)
                 (set! sim 0)
                 (send choice-pane set-selection 0)))

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
      (new editor-canvas%
           (parent status-pane)
           (editor logs-callback)
           (style '(transparent no-focus))
           (vert-margin 9)
           (horiz-margin 5)
           (min-width 600))
      
      #|gui%|#)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                         START-INFRABEL                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (start-infrabel track% tab-panels sim-graphics)
  (define track (new track%))
  (define sim-versions (send track get-versions))
  (set! startup-gui (make-object gui%
                      sim-versions
                      (startup-callback track tab-panels sim-graphics)))
  (send startup-gui show #t)
  (λ () infrabel))