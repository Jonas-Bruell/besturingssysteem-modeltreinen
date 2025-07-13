;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        >>> infrabel/startup.rkt <<<                        ;;
;;                      programmeerproject 2,  2023-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 6 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket/gui

(require try-catch
         "config.rkt"
         "interface.rkt"
         "client.rkt"
         )

(provide start-provider)

(define client (void))
(define nmbs 'dummy)
(define gui 'dummy)

(define status-callback (λ (callback) (set! status-callback callback)))

(define (startup-callback)
  (λ (provider-name)
    (let/cc return

      (send status-callback insert (string-append "Startup of the "
                                                  provider-name
                                                  " control software for modal railways.\n\n"))

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
      (send status-callback insert "\nYou may close this window now.\n\n"))))

;
; GUI
;
(define gui%
  (class frame%
    (init-field provider startup-callback status-callback)
    (super-new
     (label (string-append provider " startup manager"))
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
      )))

;
; start-provider
;
(define (start-provider)
  (send (make-object gui%
          "NMBS"
          (startup-callback)
          status-callback
          ) show #t))