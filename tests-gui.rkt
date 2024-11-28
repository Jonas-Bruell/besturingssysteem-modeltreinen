;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                            >>> tests-gui.rkt <<<                           ;;
;;                      programmeerproject 2,  2024-2025                      ;;
;;                      written by: Jonas Brüll, 0587194                      ;;
;;                                > version 4 <                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket

(require rackunit/gui "tests.rkt")
(test/gui all-tests)

;
; TO DO IN SWITCH-3WAY & SWITCH-CROSS
; -> id's als lists implementeren om zo de 2 sub-wissels te representeren
; -> deze 2 sub-wissels implementeren
; -> set-position! aanpassen om deze 2 sub-wissels aan te spreken
;

;; What about de twee wissels in een lijst steken als connections??
;; -> ook voor testing accessible??