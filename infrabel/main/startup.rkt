#lang racket

(require try-catch
         "../../modules/string-contains.rkt"
         (prefix-in gui: "../gui/startup.rkt")
         (prefix-in udp: "../udp/socket.rkt")
         (prefix-in server: "../tcp/server.rkt")
         (prefix-in adm&dbg: "../gui/admin-debugger.rkt")
         (prefix-in infrabel: "main.rkt"))

(provide startup)

(define socket (udp:make-socket))
(define server (void))
(define infra 'dummy)
(define gui 'dummy)

(define simulator-types
  (map (λ (s) (let ((l (string-length s))) (substring s 0 (- l 4))))
       (filter (λ (str) (string-contains? str ".rkt"))
               (map path->string (directory-list "infrabel/routes")))))
(define startup-callback
  (λ (type version host port a&d?)
    (startup-server type version host port a&d?)))
(define status-callback
  (λ (callback) (set! status-callback callback)))

(define (startup-server type version host port a&d?)
  (displayln version)
  (let/cc return
    ;; starting message
    (send status-callback insert
          "Startup of the INFRABEL control software for modal railways.\n\n")
    ;; socket connection
    (send status-callback insert "Connecting to the modal railway ... : ")
    (try
     ((send socket config 'sim (string->symbol version))
      (send socket open-connection))
     (catch (exn:fail? (send status-callback insert
                             "\nERROR: could not connect to modal railway\n\n")
                       (send socket close-connection) (return #f) e)))
    (send status-callback insert "SUCCES\n")
    ;; server connection
    (send status-callback insert "Setting up server ... : ")
    (try
     ;##########################################################################
     (;(server:start-server host port) ; NOT CONNECTED TO INFRA!
      (set! infra (infrabel:start-infrabel socket version)))
     (catch (exn:fail?
             (send
              status-callback insert
              (string-append*
               `("\nERROR: could not setup server on " ,host ":" ,port "\n\n")))
             (send socket close-connection) (return #f) e)))
    (send
     status-callback insert "SUCCES\n")

    ;; starting admin & debugger
    (send status-callback insert "Setting up admin and debugger panel ... : ")
    (try
     ((when a&d?
        (set! gui (adm&dbg:make-adm&dbg version infra))
        (send gui show #t)))
     (catch (exn:fail?
             (send status-callback insert
                   "\nERROR: could not setup admin and debugger panel \n\n")
             (send socket close-connection) (display e) (return #f) e)))
    (send status-callback insert "SUCCES\n")
    
    ;; ending message 
    (send status-callback insert
          (string-append*
           `("\nStartup succesful, server on  >>> " ,host ":" ,port " <<<")))
    (send status-callback insert "\nYou may close this window now.\n\n")))

(define (startup)
  (gui:startup simulator-types startup-callback status-callback)
  )