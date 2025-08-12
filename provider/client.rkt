;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                   >>> provider/client.rkt <<<                                  ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(provide provider-client%)

(define provider-client%
  (class object%
    (init-field name host port add-to-log add-to-update)
    (super-new)

    (define provider #f)
    (define track-info #f)
    (define train-info #f)
    (define-values (input-port output-port) (tcp-connect host (string->number port)))
    
    (define log-event (add-to-log name "Client"))

    ;;
    ;; init! :: initialises the server
    ;;
    ;; @param i :: infrabel
    ;;
    (define/public (init! p)
      (log-event "Client initialisation requested" "the client is initialising")
      (set! provider p))
    
    ;;
    ;; stop :: stops the server
    ;;
    (define/public (stop)
      (log-event "Client shutdown requested" "the client is shutting down")
      (send-infrabel "provider-closed")
      (kill-thread client)
      (close-input-port input-port) (close-output-port output-port))

    ;;
    ;; send-infrabel
    ;;
    ;; @param header
    ;; @param body
    ;;
    (define/public (send-infrabel header . body)
      (write (append (list header) body) output-port) (flush-output output-port))

    ;;
    ;; get-train-info
    ;;
    ;; @returns list
    ;;
    (define/public (get-track-info) track-info)

    ;;
    ;; get-train-info
    ;;
    ;; @returns list
    ;;
    (define/public (get-train-info) train-info)

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                                                                                        ;;
    ;;                                        INTERFACE                                       ;;
    ;;                                                                                        ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;
    ; TODO :: What are these for? Why are they in track/interface? Why are they so broad?
    ;
    (define/public (get-state id) (void))
    (define/public (set-state! id new-state) (void))
    (define/public (get-position id) (void))
    (define/public (add-loco id prev curr) (void))
    

    ;;
    ;; ping :: pings infrabel
    ;;
    (define/public (ping) (send-infrabel "ping"))

    ;;
    ;; segments
    ;;
    (define/public (get-segment-ids)
      (send-infrabel "get-segment-ids"))
    (define/public (get-segment-states)
      (send-infrabel "get-segment-states"))
    (define/public (get-segment-next segment)
      (send-infrabel "get-segment-next" segment))
    (define/public (get-segment-prev segment)
      (send-infrabel "get-segment-prev" segment))
    (define/public (get-segment-state segment)
      (send-infrabel "get-segment-state" segment))
    (define/public (set-segment-state! segment new-state)
      (send-infrabel "set-segment-state!" (list segment new-state)))
    ;;
    ;; detection-blocks
    ;;
    (define/public (get-detection-block-ids)
      (send-infrabel "get-detection-block-ids"))
    (define/public (get-detection-block-states)
      (send-infrabel "get-detection-block-states"))
    (define/public (get-detection-block-next detection-block)
      (send-infrabel "get-detection-block-next" detection-block))
    (define/public (get-detection-block-prev detection-block)
      (send-infrabel "get-detection-block-prev" detection-block))
    (define/public (get-detection-block-state detection-block)
      (send-infrabel "get-detection-block-state" detection-block))
    (define/public (set-detection-block-state! detection-block new-state)
      (send-infrabel "set-detection-block-state!" (list detection-block new-state)))
    ;;
    ;; switches
    ;;
    (define/public (get-switch-ids)
      (send-infrabel "get-switch-ids"))
    (define/public (get-switch-positions)
      (send-infrabel "get-switch-positions"))
    (define/public (get-switch-states)
      (send-infrabel "get-switch-states"))
    (define/public (get-switch-next switch)
      (send-infrabel "get-switch-next" switch))
    (define/public (get-switch-next-left switch)
      (send-infrabel "get-switch-next-left" switch))
    (define/public (get-switch-next-right switch)
      (send-infrabel "get-switch-next-right" switch))
    (define/public (get-switch-prev switch)
      (send-infrabel "get-switch-prev" switch))
    (define/public (get-switch-state switch)
      (send-infrabel "get-switch-state" switch))
    (define/public (set-switch-state! switch new-state)
      (send-infrabel "set-switch-state!" (list switch new-state)))
    (define/public (get-switch-position switch)
      (send-infrabel "get-switch-position" switch))
    (define/public (set-switch-position! switch new-position)
      (send-infrabel "set-switch-position!" (list switch new-position)))
    ;;
    ;; crossings
    ;;
    (define/public (get-crossing-ids)
      (send-infrabel "get-crossing-ids"))
    (define/public (get-crossing-positions)
      (send-infrabel "get-crossing-positions"))
    (define/public (get-crossing-segments crossing)
      (send-infrabel "get-crossing-segments" crossing))
    (define/public (get-crossing-position crossing)
      (send-infrabel "get-crossing-position" crossing))
    (define/public (set-crossing-position! crossing new-position)
      (send-infrabel "set-crossing-position!" (list crossing new-position)))
    ;;
    ;; lights
    ;;
    (define/public (get-light-ids)
      (send-infrabel "get-light-ids"))
    (define/public (get-light-signals)
      (send-infrabel "get-light-signals"))
    (define/public (get-light-segment light)
      (send-infrabel "get-light-segment" light))
    (define/public (get-light-signal light)
      (send-infrabel "get-light-signal" light))
    (define/public (set-light-signal! light new-signal)
      (send-infrabel "set-light-signal!" (list light new-signal)))
    ;;
    ;; trains
    ;;
    (define/public (get-train-ids)
      (send-infrabel "get-train-ids"))
    (define/public (unlock! train)
      (send-infrabel "unlock!" train))
    (define/public (get-train-location train)
      (send-infrabel "get-train-location" train))
    (define/public (get-train-speed train)
      (send-infrabel "get-train-speed" train))
    (define/public (set-train-speed! train new-speed)
      (send-infrabel "set-train-speed!" (list train new-speed)))
    (define/public (emergency-stop! train)
      (send-infrabel "emergency-stop!" train))
    (define/public (follow-route train route)
      (send-infrabel "follow-route" (list train route)))


    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;                                                                                        ;;
    ;;                                         CLIENT                                         ;;
    ;;                                                                                        ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    
    (define (header=? msg hdr) (string=? (car msg) hdr))
    (define body cadr) 
    (define (listen-for-messages)
      (let/ec return
        (let listen-for-message-loop ((m (read input-port)))
          (displayln m)
          (let ((bdy (body m)))
          (cond
            ((header=? m "ping") (send-infrabel "pong"))
            ;; setup
            ((header=? m "track-info") (set! track-info bdy))
            ((header=? m "train-info") (set! train-info bdy))
            ;; switches
            ((header=? m "set-switch-position!")
             (send provider set-switch-position! (first bdy) (second bdy)))
            ;; crossings
            ((header=? m "set-crossing-position!")
             (send provider set-crossing-position! (first bdy) (second bdy)))
            ;; lights
            ((header=? m "set-light-signal!")
             (send provider set-light-signal! (first bdy) (second bdy)))
            ;; trains
            (else (displayln (string-append "Provider > Client : No such message : " m))))
          (listen-for-message-loop (read input-port))))))

    (define client
      (thread
       (lambda ()
         (listen-for-messages)
         (displayln "disconnected"))))
    
    (send-infrabel name)
    (send-infrabel "get-track-info")
    (send-infrabel "get-train-info")
    
    #|provider-client£|#))