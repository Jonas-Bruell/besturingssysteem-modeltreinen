;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                  >>> provider/startup.rkt <<<                                  ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/gui

(require try-catch racket/exn racket/date
         "config.rkt"
         "interface.rkt"
         "client.rkt"
         "gui.rkt"
         )

(provide start-provider)

(define provider #f)

;;
;; Logging
;;
(date-display-format 'rfc2822)
(define (date) (date->string (current-date) #t))
(define log-file-path (string-append "provider/logs/"
                                     (string-replace (string-replace (date) "T" ", ") ":" "-")
                                     ".log.txt"))
(define (save-to-log-file log-string)
  (call-with-output-file* log-file-path (λ (out) (writeln log-string out)) #:exists 'append))
(define logs-callback
  (λ (callback) (set! logs-callback callback)))
(define add-to-log
  (curry (λ (service origin action event)
           (let ((log-line (string-append (date) "  ---  "
                                          service " > " origin " > " action " : " event)))
             (save-to-log-file log-line)
             (send logs-callback insert (string-append log-line "\n"))))))

;;
;; startup-callback
;;
(define status-callback (λ (callback) (set! status-callback callback)))
(define (startup-callback provider-name tab-panels)
  (define (print-new-setup s) (send status-callback insert (string-append s " ... : ")))
  (define (print-succes) (send status-callback insert "SUCCES\n"))
  (define (print-error e str1 str2)
    (send status-callback insert (string-append "\n\n>>> ERROR: " str1 provider-name str2 "\n"))
    (send status-callback insert (string-append (exn->string e) "\n\n")))
  (λ ()
    (let/cc return
      (let ((client #f)
            (gui #f))

        ;; starting Message ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (send status-callback insert
              (string-append "\nStartup of the " provider-name APPLICATION_NAME ".\n\n"))

        ;; starting control-panel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (print-new-setup (string-append "Initialising " provider-name " Command & Control."))
        (try
         ((set! gui (new provider-gui%
                         (provider-name provider-name)))
          (send gui show #t))
         (catch (exn:fail? (print-error e "could not start " " Command & Control.")
                           (return #f) e)))
        (print-succes)




        

        #|
        ; Connection to infrabel
        (send status-callback insert "Connecting to infrabel ... : ")
        (try
         ((void))
         (catch (exn:fail?
                 (send status-callback insert
                       ("\nERROR: could not connect to infrabel.\n\n")
                       (return #f)
                       e))))
        (send status-callback insert "SUCCESS\n")

        ; ending message
        (send status-callback insert
              (string-append* `("\nStartup succesful")))
        (send status-callback insert "\nYou may close this window now.\n\n")
      |#







        
        #| </startup-callback> |#))))

;;
;; gui%
;;
(define gui%
  (class frame%
    (init-field provider-name startup-callback status-callback)
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
        (startup-callback))
      
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
      (let* ((text (new text%))
             (editor (new editor-canvas%
                          (parent status-pane)
                          (editor text)
                          (style '(transparent no-focus))
                          (vert-margin 9)
                          (horiz-margin 5)
                          (min-width 400))))
        (status-callback text))
      
      #| </gui%> |#)))

;;
;; start-provider
;;
(define (start-provider name tab-panels)
  (send (make-object gui%
          name
          (startup-callback name tab-panels)
          status-callback
          ) show #t)
  (λ () provider))