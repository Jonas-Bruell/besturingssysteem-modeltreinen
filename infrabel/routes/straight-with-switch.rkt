#lang racket

(provide TRACK)

(define TRACK
  '((block
     (detection-block
     ;(name cl-in cl-out)
      (D1   NIL   S1    )
      (D4   S1    D5    )
      (D5   D4    NIL   )
      (D6   S1    D7    )
      (D7   D6    NIL   ) 
      )
     (segment
     ;(name cl-in cl-out)
      )
     )
    (switch
    ;(name in out1 out2 )
     (S-1  D1 D4   D6   )
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