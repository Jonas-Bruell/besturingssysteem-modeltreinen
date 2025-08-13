;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                   >>> infrabel/config.rkt <<<                                  ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(provide (all-defined-out))

;; APPLICATION WINDOW
(define APPLICATION_WIDTH 1900)
(define APPLICATION_HEIGHT 1000)
(define APPLICATION_NAME "INFRABEL Control Software for modal railways")

;; TRACK WINDOW
(define TRACK_WIDTH 1150)
(define TRACK_HEIGHT 600)

;; SERVER STARTUP
(define DEFAULT_HOST "localhost")
(define DEFAULT_PORT "2020")
(define DEFAULT_ARCHITECTURE_RADIO_BOX 1) ; 0 = simulator, 1 = hardware
(define DEFAULT_CONTROL_PANEL_CHECKBOX #t) ; #t = with gui, #f = no gui

(define PROVIDERS '("DB" "NMBS" "NS" "SNCF"))