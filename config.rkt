#lang racket

(provide (all-defined-out))

;; SERVER STARTUP
(define DEFAULT_HOST "localhost")
(define DEFAULT_PORT "2020")
(define DEFAULT_A&D_CHECKBOX #t)

;; ADMIN & DEBUGGER
(define SERVER_A&D_WIDTH 700)
(define SERVER_A&D_HEIGHT 1052)
(define SERVER_TERMINAL_WIDTH 1200)
(define SERVER_TERMINAL_HEIGHT 340)
(define COLOR_FREE "green")
(define COLOR_RESERVED "blue")
(define COLOR_OCCUPIED "red")