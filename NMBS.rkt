;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                ;;
;;                                        >>> NMBS.rkt <<<                                        ;;
;;                                programmeerproject 2,  2023-2025                                ;;
;;                                written by: Jonas Brüll, 0587194                                ;;
;;                                          > version 8 <                                         ;;
;;                                                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require "provider/startup.rkt"
         "railway/gui-tab-panels/tab-signals.rkt"
         "railway/gui-tab-panels/tab-states.rkt"
         "railway/gui-tab-panels/tab-switches.rkt"
         "railway/gui-tab-panels/tab-trains.rkt"
         "provider/gui-tab-panels/tab-routes.rkt")

(define tab-panels (list (cons " Trains  " tab-trains%)
                         (cons " Routes  " tab-routes%)
                         (cons "Switches " tab-switches%)
                         (cons "  States  " tab-states%)
                         (cons " Signals " tab-signals%)))

(define provider (start-provider "NMBS" tab-panels))

;; Run this file to startup NMBS
;; !!! This module is not complete yet !!!