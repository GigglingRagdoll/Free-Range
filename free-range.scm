(use srfi-13)
(use ncurses)
(use posix)

; parent menu bounds
(define (PARENT_MIN_X) 0)
(define (PARENT_MAX_X) (quotient (COLS) 3))

; current menu bounds
(define (CURRENT_MIN_X) (add1 (PARENT_MAX_X)))
(define (CURRENT_MAX_X) (* 2 (sub1 (CURRENT_MIN_X))))

; child menu bounds
(define (CHILD_MIN_X) (add1 (CURRENT_MAX_X)))
(define (CHILD_MAX_X) (sub1 (COLS)))

; all menus share height
(define (MENU_MIN_Y) 0)
(define (MENU_MAX_Y) (sub1 (LINES)))

(define (draw-menu contents left right bottom top)
  ; renders directory content to screen
  ; needs improvement
  (do ((y bottom (add1 y)))
      ((>= y top))

      (let ((idx (- y bottom)))
        (unless (>= idx (length contents))

          (let ((item (list-ref contents idx)))
            (if (= idx MENU_POS)
                ; place marker on element at MENU_POS
                (mvwaddnstr (stdscr) y left (string-append "*" item) (- right left))
                (mvwaddnstr (stdscr) y left item (- right left))))))))

(define (handle-key key)
  (cond
    ; move marker down
    ((equal? #\j key)
     ; don't let marker go further than the contents of the directory
     (set! MENU_POS (min (add1 MENU_POS)
                         (sub1 (length (directory (current-directory)))))))
    ; move marker up
    ((equal? #\k key)
     ; don't let marker go higher than the first element
     (set! MENU_POS (max 0 (sub1 MENU_POS))))
    ; switch to parent directory
    ((equal? #\h key)
     (set! MENU_POS 0)
     (change-directory ".."))
    ; attempt to enter child
    ((equal? #\l key)
     (enter))))

(define (enter)
  ; changes to child if it's a directory
  (let* ((curr-dir (current-directory))
         (item (list-ref (directory curr-dir) MENU_POS))
         (path (string-append curr-dir "/" item)))

    (if (and (directory? path) (file-read-access? path))
        (begin 
          (change-directory path)
          (set! MENU_POS 0))
        (change-directory curr-dir))))

(define (child-contents)
  ; show the contents of the child
  ; whether it's a directory or a file
  (let* ((curr-dir (current-directory))
         (item (list-ref (directory curr-dir) MENU_POS))
         (path (string-append curr-dir "/" item)))

    (if (and (directory? path) (file-read-access? path))
        ; show files in child directory
        (directory path)
        ; preview file
        (preview path))))

(define (preview path)
  ; will eventually preview text files
  (list "just kidding"))

(set! MENU_POS 0)

(initscr)
(noecho)
(cbreak)
(curs_set 0)

(let loop ((win (stdscr)))
  (wclear win)
  (draw-menu (directory (current-directory)) 
             (CURRENT_MIN_X) 
             (CURRENT_MAX_X) 
             (MENU_MIN_Y) 
             (MENU_MAX_Y))

  ; don't render parent if we are at the root
  (unless (equal? "/" (current-directory))
    (draw-menu (directory "..")
               (PARENT_MIN_X) 
               (PARENT_MAX_X) 
               (MENU_MIN_Y) 
               (MENU_MAX_Y))

  (draw-menu (child-contents)
             (CHILD_MIN_X) 
             (CHILD_MAX_X) 
             (MENU_MIN_Y) 
             (MENU_MAX_Y))

  (mvwaddnstr win 0 0 (current-directory) (CHILD_MAX_X))
  (wrefresh win)

  (let ((key (getch)))
    ; quit when 'q' is pressed
    (if (not (equal? #\q key))
        (begin 
          (handle-key key)
          (loop win))
        (begin
          (echo)
          (nocbreak)
          (curs_set 1)
          (endwin)))))

