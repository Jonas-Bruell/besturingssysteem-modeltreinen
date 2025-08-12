;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                  >>> provider/startup.rkt <<<                                  ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require try-catch racket/gui racket/exn racket/date
         "config.rkt"
         "interface.rkt"
         "client.rkt"
         "gui.rkt"
         )

(provide start-provider)

(define logs-callback (new text%))
(define provider #f)
(define client #f)
(define startup-gui #f)
(define gui #f)

;;
;; Logging
;;
(date-display-format 'iso-8601)
(define (date) (date->string (current-date) #t))
(define log-file-path (string-append "provider/logs/"
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
;; Stop Provider
;;
(define (stop-provider)
  (kill-thread auto-update-loop)
  (send gui show #f))

;;
;; startup-callback
;;
(define (startup-callback provider-name tab-panels)
  (define log-event (add-to-log provider-name "Startup" "Startup Manager"))
  (define (print-new-setup s) (log-event (string-append s " ... : ")))
  (define (print-succes) (log-event "SUCCES"))
  (define (print-error e s1 s2)
    (log-event (string-append "ERROR: " s1 " " provider-name " " s2
                              ", internal error: "(exn->string e))))
  (λ (host port)
    (let/cc return

      ;; starting Message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (log-event (string-append "Startup of the " provider-name APPLICATION_NAME))

      ;; connect to infrabel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (print-new-setup "Making Client and connecting to Infrabel server")
      (try
       ((set! client (new provider-client%
                          (name provider-name)
                          (host host)
                          (port port)
                          (add-to-log add-to-log)
                          (add-to-update add-to-update))))
       (catch (exn:fail? (print-error e "could not connect" (string-append "to " host ":" port))
                         (return #f) e)))
      (print-succes)
        
      ;; starting control-panel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (print-new-setup (string-append "Initialising " provider-name " Command & Control."))
      (try
       ((set! gui (new provider-gui%
                       (provider-name provider-name)
                       (logs-callback logs-callback)
                       (stop-provider stop-provider)))
        (send gui show #t))
       (catch (exn:fail? (print-error e "could not start" "Command & Control.")
                         (return #f) e)))
      (print-succes)

      ;; starting application
      (print-new-setup (string-append "Starting " provider-name))
      (try
       ((set! provider (new provider%
                            (connection client)
                            (add-to-log add-to-log)
                            (add-to-update add-to-update)
                            (stop-provider stop-provider)))
        (send client init! provider))
       (catch (exn:fail? (print-error e "could not start" "")
                         (send client stop) (return #f) e)))
      (print-succes)
        
      #| </startup-callback> |#)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                           GUI                                                  ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define gui%
  (class frame%
    (init-field provider-name startup-callback)
    (super-new
     (label (string-append provider-name " startup manager"))
     (width 0)
     (height 0)
     (style '(no-resize-border)))

    (let* ((global-pane (new horizontal-pane% (parent this)))
           (config-pane (new vertical-pane% (parent global-pane)))
           (status-pane (new vertical-pane% (parent global-pane)))
           ; Choosable elements in gui
           (host 0)
           (hostname "")
           (port 0)
           (portnumber ""))

      ;; start server
      (define (start)
        (startup-callback
         (if (zero? host) DEFAULT_HOST hostname)
         (if (zero? port) DEFAULT_PORT portnumber)))
      
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
        (new button% (label "start") (parent vertical-pane) (callback (λ (t e) (start)))))

      ;; status panel
      (new editor-canvas%
           (parent status-pane)
           (editor logs-callback)
           (style '(transparent no-focus))
           (vert-margin 9)
           (horiz-margin 5)
           (min-width 600))
      
      #| </gui%> |#)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                         START-PROVIDER                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (start-provider name tab-panels)
  ;background-color
  ;text-color
  (set! startup-gui (make-object gui%
                      name
                      (startup-callback name tab-panels)))
  (send startup-gui show #t)
  (λ () provider))