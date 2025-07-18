#lang racket

(provide TRACK)

(define TRACK
  '((detection-block
    ;(name clockwise-in   clockwise-out )
     (D1   NIL            (dblock  . D2))
     (D2   (dblock  . D1) (segment . T ))
     (D3   (segment . T ) (dblock  . D4))
     (D4   (dblock  . D3) NIL           )
     )
    (segment
    ;(name clockwise-in  clockwise-out)
     (T    (dblock . D2) (dblock . D3))
     )
    (switch
    ;(name in out1 out2 )
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