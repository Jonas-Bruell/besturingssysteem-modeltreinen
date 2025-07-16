#lang racket
(require "infrabel/startup.rkt"
         "railway/gui-tab-panels/tab-crossings-lights.rkt"
         "railway/gui-tab-panels/tab-segments-detection-blocks.rkt"
         "railway/gui-tab-panels/tab-switches.rkt"
         "railway/gui-tab-panels/tab-trains.rkt"
         "track/interface.rkt"
         "track/simulator/graphics.rkt"
         "track/simulator/simulator.rkt")

(define tab-panels (list (cons " Trains  " tab-trains%)
                         (cons "Switches " tab-switches%)
                         (cons "  Rails  " tab-segments-detection-blocks%)
                         (cons " Signals " tab-crossings-lights%)))
(define sim-graphics (cons set-panel window%))

(define infrabel (start-infrabel track% tab-panels sim-graphics))

;; Run this file to startup Infrabel
