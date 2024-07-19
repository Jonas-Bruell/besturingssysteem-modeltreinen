#lang racket/gui

(require "../../config.rkt")

(provide (all-defined-out))

(define horizontal-group-box-panel%
  (class horizontal-pane%
    (init label parent)
    (super-new
     (parent (new group-box-panel% (label label) (parent parent)))
     (alignment '(center center)))))

(define status%
  (class pane%
    (init-field parent
                name
                callback
                )
    (super-new
     (parent parent)
     )
    
    (define group-box (new group-box-panel% (label name) (parent this)))
           
    (define message
      (new message% (label (callback)) (parent group-box) (auto-resize #t)))

    (define/public (set-label msg)
      (send message set-label msg))
    (define/public (set-color clr)
      (send message set-color clr))
    ))

(define crossing-panel%
  (class horizontal-group-box-panel%
    (init-field parent name)
    (super-new (label name) (parent parent))

    (new button% (label "open") (parent this))
    (new message% (label "opened") (parent this))
    (new button% (label "close") (parent this))
    
    ))

(define light-panel%
  (class group-box-panel%
    (init-field parent name)
    (super-new (label name) (parent parent))

    ))

(define (wrap-callback callback change-colors)
  (λ (t e)
    (callback t e)
    (change-colors)))

(define switch-panel%
  (class horizontal-group-box-panel%
    (init-field parent
                name
                in
                check
                button1 button2
                out1 out2)
    (super-new (label name) (parent parent))
    ;; color active track
    (define (change-colors)
      (let ((postion (check)))
        (cond ((eq? postion 1)
               (send out-label1 set-color COLOR_FREE)
               (send out-label2 set-color COLOR_OCCUPIED))
              ((eq? postion 2)
               (send out-label1 set-color COLOR_OCCUPIED)
               (send out-label2 set-color COLOR_FREE)))))
    ;; panes
    (define in-pane
      (new pane% (parent this)))
    (define button-pane
      (new vertical-pane% (parent this) (alignment '(center center))))
    (define out-pane
      (new vertical-pane% (parent this) (alignment '(center center))))
    ;; content
    (define in-label
      (new message% (label in) (parent in-pane)))
    (define out-label1
      (new message% (label out1) (parent out-pane)))
    (define out-label2
      (new message% (label out2) (parent out-pane)))
    (define the-button1
      (new button% (label "pos-1") (parent button-pane)
           (callback (wrap-callback button1 change-colors))))
    (define the-button2
      (new button% (label "pos-2") (parent button-pane)
           (callback (wrap-callback button2 change-colors))))
    (send in-label set-color COLOR_FREE)
    (change-colors)))

(define three-way-switch-panel%
  (class horizontal-group-box-panel%
    (init-field parent
                name
                in
                check1 check2
                button1 button2 button3
                out1 out2 out3)
    (super-new (label name) (parent parent))
    ;; color active track
    (define (change-colors)
      (let ((position1 (check1))
            (position2 (check2)))
        (cond ((eq? position1 1)
               (send out-label1 set-color COLOR_FREE)
               (send out-label2 set-color COLOR_OCCUPIED)
               (send out-label3 set-color COLOR_OCCUPIED))
              ((and (eq? position2 1))
               (send out-label1 set-color COLOR_OCCUPIED)
               (send out-label2 set-color COLOR_FREE)
               (send out-label3 set-color COLOR_OCCUPIED))
              ((and (eq? position2 2))
               (send out-label1 set-color COLOR_OCCUPIED)
               (send out-label2 set-color COLOR_OCCUPIED)
               (send out-label3 set-color COLOR_FREE)))))
    ;; panes
    (define in-pane
      (new pane% (parent this)))
    (define button-pane
      (new vertical-pane% (parent this) (alignment '(center center))))
    (define out-pane
      (new vertical-pane% (parent this) (alignment '(center center))))
    ;; content
    (define in-label
      (new message% (label in) (parent in-pane)))
    (define out-label1
      (new message% (label out1) (parent out-pane)))
    (define out-label2
      (new message% (label out2) (parent out-pane)))
    (define out-label3
      (new message% (label out3) (parent out-pane)))
    (define the-button1
      (new button% (label "pos-1") (parent button-pane)
           (callback (wrap-callback button1 change-colors))))
    (define the-button2
      (new button% (label "pos-2") (parent button-pane)
           (callback (wrap-callback button2 change-colors))))
    (define the-button3
      (new button% (label "pos-3") (parent button-pane)
           (callback (wrap-callback button3 change-colors))))
    (send in-label set-color COLOR_FREE)
    (change-colors)))

(define cross-switch-panel%
  (class horizontal-group-box-panel%
    (init-field parent
                name
                in1 in2
                check1 check2
                button1 button2 button3 button4
                out1 out2)
    (super-new (label name) (parent parent))
    ;; color active track
    (define (change-colors)
      (let ((position1 (check1))
            (position2 (check2)))
        (cond ((eq? position1 1)
               (send in-label1 set-color COLOR_FREE)
               (send in-label2 set-color COLOR_OCCUPIED))
              ((eq? position1 2)
               (send in-label1 set-color COLOR_OCCUPIED)
               (send in-label2 set-color COLOR_FREE)))
        (cond ((eq? position2 1)
               (send out-label1 set-color COLOR_FREE)
               (send out-label2 set-color COLOR_OCCUPIED))
              ((eq? position2 2)
               (send out-label1 set-color COLOR_OCCUPIED)
               (send out-label2 set-color COLOR_FREE)))))
    ;; panes
    (define in-pane
      (new vertical-pane% (parent this) (alignment '(center center))))
    (define button-pane
      (new horizontal-pane% (parent this) (alignment '(center center))))
    (define button-pane-L
      (new vertical-pane% (parent button-pane) (alignment '(center center))))
    (define button-pane-R
      (new vertical-pane% (parent button-pane) (alignment '(center center))))
    (define out-pane
      (new vertical-pane% (parent this) (alignment '(center center))))
    ;; content
    (define in-label1
      (new message% (label in1) (parent in-pane)))
    (define in-label2
      (new message% (label in2) (parent in-pane)))
    (define out-label1
      (new message% (label out1) (parent out-pane)))
    (define out-label2
      (new message% (label out2) (parent out-pane)))
    (define the-button1
      (new button% (label "pos-1") (parent button-pane-L)
           (callback (wrap-callback button1 change-colors))))
    (define the-button2
      (new button% (label "pos-2") (parent button-pane-L)
           (callback (wrap-callback button2 change-colors))))
    (define the-button3
      (new button% (label "pos-1") (parent button-pane-R)
           (callback (wrap-callback button3 change-colors))))
    (define the-button4
      (new button% (label "pos-2") (parent button-pane-R)
           (callback (wrap-callback button4 change-colors))))
    (change-colors)))
