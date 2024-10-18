#lang racket

(provide TRACK)

(define TRACK
  '((block
     (detection-block
     ;(name cl-in cl-out)
      (D1   NIL   D2    )
      (D2   D1    T     )
      (D3   T     D4    )
      (D4   D3    NIL   )
      )
     (segment
     ;(name cl-in cl-out)
      (T    D2    D3    )
      )
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