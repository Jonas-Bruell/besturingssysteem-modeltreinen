#lang racket

(provide TRACK)

(define TRACK
  '((detection-block
    ;(name clockwise-in   clockwise-out )
     (D1   (segment . T2) (dblock  . D2))
     (D2   (dblock  . D1) (dblock  . D3))
     (D3   (dblock  . D2) (dblock  . D4))
     (D4   (dblock  . D3) (dblock  . D5))
     (D5   (dblock  . D4) (segment . T1))
     (D6   (segment . T1) (dblock  . D7))
     (D7   (dblock  . D6) (dblock  . D8))
     (D8   (dblock  . D7) (segment . T2))
     )
    (segment
    ;(name clockwise-in   clockwise-out )
     (T1   (dblock .  D5) (dblock .  D6))
     (T2   (dblock .  D8) (dblock .  D1))
     )
    (switch
    ;(name in    out1 out2 )
     )
    (switch*
     (threeway
     ;(name s1  s2  in  out1 out2 out3)
      )
     (cross
     ;(name s1  s2  cl-in1 cl-in2 cl-out1 cl-out2)
      )
     )
    (crossing
    ;(name)
     )
    (light
    ;(name)
     )
    ))