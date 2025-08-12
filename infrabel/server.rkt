;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                   >>> infrabel/server.rkt <<<                                  ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(provide infrabel-server%)

(define infrabel-server%
  (class object%
    (init-field host port add-to-log add-to-update)
    (super-new)
    
    (define infrabel #f)
    (define clients-list '())
    (define the-listener (tcp-listen (string->number port) 4 #t host))

    (define log-event (add-to-log "INFRABEL" "Server"))

    ;;
    ;; init! :: initialises the server
    ;;
    ;; @param i :: infrabel
    ;;
    (define/public (init! i)
      (log-event "Server initialisation requested" "the server is initialising")
      (set! infrabel i))
    
    ;;
    ;; stop :: stops the server
    ;;
    (define/public (stop)
      (log-event "Server shutdown requested" "the server is shutting down")
      (send-all-providers "infrabel-closed")
      (kill-thread server)
      (tcp-close the-listener))

    ;;
    ;; send-provider
    ;;
    ;; @param provider-name
    ;; @param header
    ;; @param body
    ;;
    (define/public (send-provider provider-name header . body)
      (let ((output-port (cadr (assoc provider-name clients-list))))
        (write (append (list header) body) output-port) (flush-output output-port)))

    ;;
    ;; send-all-providers
    ;;
    ;; @param header
    ;; @param body
    ;;
    (define/public (send-all-providers header . body)
      (for-each (λ (output-port) (write (append (list header) body) output-port)
                  (flush-output output-port))
                (map cadr clients-list)))


    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                                                                                        ;;
    ;;                                        INTERFACE                                       ;;
    ;;                                                                                        ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



    ;;
    ;; ping : pings all providers
    ;;
    (define/public (ping) (send-all-providers "ping"))





    





    #|
                (let/ec return
                  (let listen-for-message-loop ()
                    (define msg (read input-port))
                    (displayln msg)
                    (cond ((string=? msg "Ping")
                           (write "Pong" output-port) (flush-output output-port))
                          ((string=? msg "Hello?")
                           (write "World!" output-port) (flush-output output-port))
                          ((string=? msg "Test")
                           (write "Performing test :)" output-port) (flush-output output-port)
                           (send infrabel set-crossing-position! 'C-1 'closed)
                           (log-event "set-crossing-position!"))
                          ((string=? msg "provider-closed")
                           (write "closed" output-port) (flush-output output-port) (return))
                          ;;;;;;;;;
                          ((string=? msg "get-track-info")
                           (write (send infrabel get-track-info) output-port)
(flush-output output-port))
                          (else
                           (displayln "no equal msg")))
                    (listen-for-message-loop)))
|#


    
    

    

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                                                                                        ;;
    ;;                                         SERVER                                         ;;
    ;;                                                                                        ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    
    (define (header=? msg hdr) (string=? (car msg) hdr))
    (define (body m) (unless (null? (cdr m)) (cadr m)))
    (define (listen-for-messages provider input-port)
      (let/ec return
        (let listen-for-message-loop ((m (read input-port)))
          (displayln m)
          (let ((bdy (body m)))
            (cond
              ((header=? m "ping") (send-provider provider "pong"))
              ;; setup
              ((header=? m "get-track-info")
               (send-provider provider "track-info"
                              (send infrabel get-track-info)))
              ((header=? m "get-train-info")
               (send-provider provider "train-info"
                              (send infrabel get-train-info)))
              ;; segments
              ((header=? m "get-segment-ids")
               (send-provider provider "segment-ids"
                              (send infrabel get-segment-ids)))
              ((header=? m "get-segment-states")
               (send-provider provider "segment-states"
                              (send infrabel get-segment-states))) 
              ((header=? m "get-segment-next")
               (send-provider provider "segment-next"
                              (list bdy (send infrabel get-segment-next bdy))))
              ((header=? m "get-segment-prev")
               (send-provider provider "segment-prev"
                              (list bdy (send infrabel get-segment-next bdy))))
              ((header=? m "get-segment-state")
               (send-provider provider "segment-state"
                              (list bdy (send infrabel get-segment-state bdy))))
              ((header=? m "set-segment-state!") void) #|TODO|#
              ;; detection-blocks
              ((header=? m "get-detection-block-ids")
               (send-provider provider "detection-block-ids"
                              (send infrabel get-detection-block-ids)))
              ((header=? m "get-detection-block-states")
               (send-provider provider "detection-block-states"
                              (send infrabel get-detection-block-states)))
              ((header=? m "get-detection-block-next")
               (send-provider provider "detection-block-next"
                              (list bdy (send infrabel get-detection-block-next bdy))))
              ((header=? m "get-detection-block-prev")
               (send-provider provider "detection-block-prev"
                              (list bdy (send infrabel get-detection-block-prev bdy))))
              ((header=? m "get-detection-block-state")
               (send-provider provider "detection-block-state"
                              (list bdy (send infrabel get-detection-block-state bdy))))
              ((header=? m "set-detection-block-state!") void) #|TODO|#
              ;; switches
              ((header=? m "get-switch-ids")
               (send-provider provider "switch-ids"
                              (send infrabel get-switch-ids)))
              ((header=? m "get-switch-positions")
               (send-provider provider "switch-positions"
                              (send infrabel get-switch-positions)))
              ((header=? m "get-switch-states")
               (send-provider provider "switch-states"
                              (send infrabel get-switch-states)))
              ((header=? m "get-switch-next")
               (send-provider provider "switch-next"
                              (list bdy (send infrabel get-switch-next bdy))))
              ((header=? m "get-switch-next-left")
               (send-provider provider "switch-next-left"
                              (list bdy (send infrabel get-switch-next-left bdy))))
              ((header=? m "get-switch-next-right")
               (send-provider provider "switch-next-right"
                              (list bdy (send infrabel get-switch-next-right bdy))))
              ((header=? m "get-switch-prev")
               (send-provider provider "switch-prev"
                              (list bdy (send infrabel get-switch-prev bdy))))
              ((header=? m "get-switch-state")
               (send-provider provider "switch-state"
                              (list bdy (send infrabel get-switch-state bdy))))
              ((header=? m "set-switch-state!") void) #|TODO|#
              ((header=? m "get-switch-position")
               (send-provider provider "switch-position"
                              (list bdy (send infrabel get-switch-position bdy))))
              ((header=? m "set-switch-position!") void) #|TODO|#
              ;; crossings
              ((header=? m "get-crossing-ids")
               (send-provider provider "crossing-ids"
                              (send infrabel get-crossing-ids)))
              ((header=? m "get-crossing-positions")
               (send-provider provider "crossing-positions"
                              (send infrabel get-crossing-positions)))
              ((header=? m "get-crossing-segments")
               (send-provider provider "crossing-segments"
                              (list bdy (send infrabel get-crossing-segments bdy))))
              ((header=? m "get-crossing-position")
               (send-provider provider "crossing-position"
                              (list bdy (send infrabel get-crossing-position bdy))))
              ((header=? m "set-crossing-position!") void) #|TODO|#
              ;; lights
              ((header=? m "get-light-ids")
               (send-provider provider "light-ids"
                              (send infrabel get-light-ids)))
              ((header=? m "get-light-signals")
               (send-provider provider "light-signals"
                              (send infrabel get-light-signals)))
              ((header=? m "get-light-segment")
               (send-provider provider "light-segment"
                              (list bdy (send infrabel get-light-segment bdy))))
              ((header=? m "get-light-signal")
               (send-provider provider "light-signal"
                              (list bdy (send infrabel get-light-signal bdy))))
              ((header=? m "set-light-signal!") void) #|TODO|#
              ;; trains
              ((header=? m "get-train-ids")
               (send-provider provider "train-ids"
                              (send infrabel get-train-ids)))
              ((header=? m "unlock!") void) #|TODO|#
              ((header=? m "get-train-location")
               (send-provider provider "train-location"
                              (list bdy (send infrabel get-train-location bdy))))
              ((header=? m "get-train-speed")
               (send-provider provider "train-speed"
                              (list bdy (send infrabel get-train-speed bdy))))
              ((header=? m "set-train-speed!") void) #|TODO|#
              ((header=? m "emergency-stop!") void) #|TODO|#
              ((header=? m "follow-route") void) #|TODO|#
              ;; closing
              ((header=? m "provider-closed") (return))
              (else (displayln (string-append "Infrabel > Server : No such message : " m))))
            (listen-for-message-loop (read input-port))))))

    (define server
      (thread
       (lambda ()
         (let listen-for-new-clients-loop ()
           
           (define-values (input-port output-port) (tcp-accept the-listener))
           (define provider-name (car (read input-port)))

           (define new-client
             (thread
              (lambda ()
                
                (log-event "New client connected" "new client listener created")
                (listen-for-messages provider-name input-port)

                ;; only continues after closed new client
                (log-event (string-append "Client " provider-name " disconnected")
                           (string-append provider-name " listener removed"))
                (close-input-port input-port) (close-output-port output-port))))

           ;; continues on startup - add thread to clients-list
           (set! clients-list
                 (append clients-list (list (list provider-name output-port new-client))))
           
           (listen-for-new-clients-loop)))))

    #|infrabel-server%|#))