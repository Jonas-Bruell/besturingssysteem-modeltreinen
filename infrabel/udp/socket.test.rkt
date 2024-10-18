#lang racket
(require rackunit
         rackunit/gui
         (prefix-in udp: "udp.rkt"))

; aliasses
(define start                         udp:start)
(define stop                          udp:stop)
(define add-loco                      udp:add-loco)
(define get-loco-speed                udp:get-loco-speed)
(define set-loco-speed!               udp:set-loco-speed!)
(define get-detection-block-ids       udp:get-detection-block-ids)
(define get-occupied-detection-blocks udp:get-occupied-detection-blocks)
(define get-switch-ids                udp:get-switch-ids)
(define get-switch-position           udp:get-switch-position)
(define set-switch-position!          udp:set-switch-position!)
(define open-crossing!                udp:open-crossing!)
(define close-crossing!               udp:close-crossing!)
(define set-sign-code!                udp:set-sign-code!)

; setting up all individual test suites
(define test-udp-start
  (test-suite "testing udp start"
              (test-case "check 'start' exists"
                         (check-not-exn (λ () start)))))
(define test-udp-stop
  (test-suite "testing udp stop"
              (test-case "check 'stop' exists"
                         (check-not-exn (λ () stop)))))
(define test-udp-add-loco
  (test-suite "testing udp add-loco"
              (test-case "check 'add-loco' exists"
                         (check-not-exn (λ () add-loco)))))
(define test-udp-get-loco-speed
  (test-suite "testing udp get-loco-speed"
              (test-case "check 'get-loco-speed' exists"
                         (check-not-exn (λ () get-loco-speed)))))
(define test-udp-set-loco-speed!
  (test-suite "testing udp set-loco-speed!"
              (test-case "check 'set-loco-speed!' exists"
                         (check-not-exn (λ () set-loco-speed!)))))
(define test-udp-get-detection-block-ids
  (test-suite "testing udp get-detection-blocks-ids"
              (test-case "check 'get-detection-blocks-ids' exists"
                         (check-not-exn (λ () get-detection-block-ids)))))
(define test-udp-get-occupied-detection-blocks
  (test-suite "testing udp get-occupied-detection-blocks"
              (test-case "check 'get-occupied-detection-blocks' exists"
                         (check-not-exn (λ () get-occupied-detection-blocks)))))
(define test-udp-get-switch-ids
  (test-suite "testing udp get-switch-ids"
              (test-case "check 'get-switch-ids' exists"
                         (check-not-exn (λ () get-switch-ids)))))
(define test-udp-get-switch-position
  (test-suite "testing udp get-switch-position"
              (test-case "check 'get-switch-position' exists"
                         (check-not-exn (λ () get-switch-position)))))
(define test-udp-set-switch-position!
  (test-suite "testing udp set-switch-position!"
              (test-case "check 'set-switch-position!' exists"
                         (check-not-exn (λ () set-switch-position!)))))
(define test-udp-open-crossing!
  (test-suite "testing udp open-crossing!"
              (test-case "check 'open-crossing!' exists"
                         (check-not-exn (λ () open-crossing!)))))
(define test-udp-close-crossing!
  (test-suite "testing udp close-crossing!"
              (test-case "check 'close-crossing!' exists"
                         (check-not-exn (λ () close-crossing!)))))
(define test-udp-set-sign-code!
  (test-suite "testing udp set-sign-code!"
              (test-case "check 'set-sign-code!' exists"
                         (check-not-exn (λ () set-sign-code!)))))

; testing all test suites
(define test-all-suites
  (test-suite
   "udp tests"
   test-udp-start
   test-udp-stop
   test-udp-add-loco
   test-udp-get-loco-speed
   test-udp-set-loco-speed!
   test-udp-get-detection-block-ids
   test-udp-get-occupied-detection-blocks
   test-udp-get-switch-ids
   test-udp-get-switch-position
   test-udp-set-switch-position!
   test-udp-open-crossing!
   test-udp-close-crossing!
   test-udp-set-sign-code!
   ))
(test/gui test-all-suites)