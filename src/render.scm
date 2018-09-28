(use ncurses)
(use srfi-13)

(load "state.scm")

; parent menu occupies left third of screen
(define (parent-left-bound)   0)
(define (parent-right-bound)  (quotient (COLS) 3))
; working menu occupies middle third of screen
(define (working-left-bound)  (add1 (parent-right-bound)))
(define (working-right-bound) (* 2 (parent-right-bound)))
; child menu occupies right third of screen
(define (child-left-bound)    (add1 (working-right-bound)))
(define (child-right-bound)   (sub1 (COLS)))
; each menu is offset a bit from the top and bottom
(define (menu-top-bound)      2)
(define (menu-bottom-bound)   (sub1 (LINES)))
; the point at which menu should start scrolling
(define (scroll-point)        (quotient (LINES) 2))

(define (render-all win state)
  (render-top-bar win state)
  (render-parent-menu win (parent-left-bound) (parent-right-bound) state)
  (render-working-menu win (working-left-bound) (working-right-bound) state)
  (render-child-menu win (child-left-bound) (child-right-bound) state))

(define (render-top-bar win state)
  (mvwaddstr win 0 0 (free-range-working-directory state)))

(define (render-working-menu win lbound rbound state)
  (let ((wd (free-range-working-directory state))
        (wc (free-range-working-contents state))
        (wi (free-range-working-index state)))

    (do ((offset (menu-top-bound) (add1 offset))
         (index  0 (add1 index)))
        ((stop-rendering? wc index (menu-bottom-bound) (menu-top-bound)))

        (render-item
          win
          offset lbound rbound
          (path-append wd (list-ref wc index))
          wi index))))

(define (render-parent-menu win lbound rbound state)
  (let ((wd (free-range-working-directory state))
        (wc (free-range-working-contents state))
        (wi (free-range-working-index state))
        (pd (free-range-parent-directory state))
        (pc (free-range-parent-contents state)))

    (unless (equal? wd "/")
      (do ((offset (menu-top-bound) (add1 offset))
           (index  0 (add1 index)))
          ((stop-rendering? pc index (menu-bottom-bound) (menu-top-bound)))

          (render-item
            win
            offset lbound rbound
            (path-append pd (list-ref pc index))
            wi -1)))))

(define (render-child-menu win lbound rbound state)
  (let ((wd (free-range-working-directory state))
        (wc (free-range-working-contents state))
        (wi (free-range-working-index state))
        (c  (free-range-child state))
        (cc (free-range-child-contents state)))

    (unless (equal? wc '(""))
      (do ((offset (menu-top-bound) (add1 offset))
           (index  0 (add1 index)))
          ((stop-rendering? cc index (menu-bottom-bound) (menu-top-bound)))

          (render-item
            win
            offset lbound rbound
            (path-append c (list-ref cc index))
            wi -1)))))

(define (render-item win y x-min x-max path wi index) 
  (let ((item (car (reverse (string-split path "/"))))
        (info ""))

    (if (= wi index)
        (begin
          (mvwaddnstr win y (add1 x-min) item (- x-max x-min))
          (mvwaddstr win y x-min "*"))
        (mvwaddnstr win y x-min item (- x-max x-min)))

    (when (symbolic-link? path)
      (set! info (format #f "~A~A" "-> " info)))

    (when (can-enter? path)
      (set! info (format #f "~A~A" info (length (directory path)))))

    (mvwaddstr win y (- x-max (string-length info)) info)
  ))

(define (stop-rendering? contents index bbound tbound)
  (or (> index (sub1 (length contents)))
      (>= index (- bbound tbound))))

