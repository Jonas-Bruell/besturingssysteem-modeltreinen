;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                          >>> infrabel/main.rkt <<<                         ;;
;;                      programmeerproject 2,  2023-2024                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 1 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "config.rkt"
         "logic/startup.rkt")

(provide start-infrabel
         )

; startup
(define (start-infrabel)
  (startup TRACKS_LIST)
  )