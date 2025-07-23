;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                   >>> provider/config.rkt <<<                                  ;;
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
(define APPLICATION_NAME "Command and Control")

;; CLIENT STARTUP
(define DEFAULT_HOST "localhost")
(define DEFAULT_PORT "2020")

;; DEBUG?
(define DEBUG_TOOLS? #t)