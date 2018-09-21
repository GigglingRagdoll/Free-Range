(use srfi-13)
(use ncurses)
(use posix)

; parent menu bounds
(define (PARENT_MIN_X) 0)
(define (PARENT_MAX_X) (quotient (COLS) 3))
(define (PARENT_MIN_Y) 1)
(define (PARENT_MAX_Y) (sub1 (LINES)))

; current menu bounds
(define (CURRENT_MIN_X) (add1 (PARENT_MAX_X)))
(define (CURRENT_MAX_X) (* 2 (sub1 (CURRENT_MIN_X))))
(define (CURRENT_MIN_Y) 1)
(define (CURRENT_MAX_Y) (sub1 (LINES)))

; child menu bounds
(define (CHILD_MIN_X) (add1 (CURRENT_MAX_X)))
(define (CHILD_MAX_X) (sub1 (COLS)))
(define (CHILD_MIN_Y) 1)
(define (CHILD_MAX_Y) (sub1 (LINES)))

(define (draw-menu contents left right bottom top)
  (do ((y bottom (add1 y)))
      ((>= y top))

      (let ((idx (- y bottom)))
        (unless (>= idx (length contents))

          (let ((item (list-ref contents idx)))
            (if (= idx MENU_POS)
                (mvwaddnstr (stdscr) y left (string-append "*" item) (- right left))
                (mvwaddnstr (stdscr) y left item (- right left))))))))

(define (handle-key key)
  (cond
    ((equal? #\j key)
     (set! MENU_POS (min (add1 MENU_POS)
                         (sub1 (length (directory (current-directory)))))))
    ((equal? #\k key)
     (set! MENU_POS (max 0 (sub1 MENU_POS))))
    ((equal? #\h key)
     (set! MENU_POS 0)
     (change-directory ".."))
    ((equal? #\l key)
     (enter))))

(define (enter)
  (let* ((curr-dir (current-directory))
         (item (list-ref (directory curr-dir) MENU_POS))
         (path (string-append curr-dir "/" item)))

    (if (and (directory? path) (file-read-access? path))
        (begin 
          (change-directory path)
          (set! MENU_POS 0))
        (change-directory curr-dir))))

(define (child-contents)
  (let* ((curr-dir (current-directory))
         (item (list-ref (directory curr-dir) MENU_POS))
         (path (string-append curr-dir "/" item)))

    (if (and (directory? path) (file-read-access? path))
        (directory path)
        (preview path))))

(define (preview path)
  (list "just kidding"))

(set! MENU_POS 0)
;(set! PARENT_POS (get-parent-idx))

(initscr)
(noecho)
(cbreak)
(curs_set 0)

(let loop ((win (stdscr)))
  (wclear win)
  (draw-menu (directory (current-directory)) 
             (CURRENT_MIN_X) 
             (CURRENT_MAX_X) 
             (CURRENT_MIN_Y) 
             (CURRENT_MAX_Y))

  (unless (equal? "/" (current-directory))
    (draw-menu (directory "..")
               (PARENT_MIN_X) 
               (PARENT_MAX_X) 
               (PARENT_MIN_Y) 
               (PARENT_MAX_Y)))

  (draw-menu (child-contents)
             (CHILD_MIN_X) 
             (CHILD_MAX_X) 
             (CHILD_MIN_Y) 
             (CHILD_MAX_Y))

  (mvwaddnstr win 0 0 (current-directory) (CHILD_MAX_X))
  (wrefresh win)

  (let ((key (getch)))
    (if (not (equal? #\q key))
        (begin 
          (handle-key key)
          (loop win))
        (begin
          (echo)
          (nocbreak)
          (curs_set 1)
          (endwin)))))

