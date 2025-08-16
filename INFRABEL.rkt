;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                      >>> INFRABEL.rkt <<<                                      ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "infrabel/startup.rkt"
         "railway/gui-tab-panels/tab-signals.rkt"
         "railway/gui-tab-panels/tab-states.rkt"
         "railway/gui-tab-panels/tab-switches.rkt"
         "railway/gui-tab-panels/tab-trains.rkt"
         "track/interface.rkt"
         "track/simulator/graphics.rkt"
         "track/simulator/simulator.rkt"
         )

(define tab-panels (list (cons " Trains  " tab-trains%)
                         (cons "Switches " tab-switches%)
                         (cons "  States  " tab-states%)
                         (cons " Signals " tab-signals%)))
(define sim-graphics (cons set-panel window%))

(define infrabel (start-infrabel track% tab-panels sim-graphics))

;; Run this file to startup Infrabel
