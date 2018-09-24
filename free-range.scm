(use ncurses)
(use posix)
(use section-combinators)
(use srfi-1 srfi-13)

;;; Directory Sorting Functions

(define (sort-by sort lst)
  (cond
    ((equal? sort 'NAME) (sort-names lst))))

(define (sort-names lst)
  (cond
    ((<= (length lst) 1) 
     lst)
    (#t
     (let* ((pivot (car lst))
            (lt (filter (left-section string-ci> pivot) lst))
            (gt (filter (left-section string-ci< pivot) lst)))
       (append (sort-names lt) (list pivot) (sort-names gt))))))

;;; Child awareness functions

(define (child-of dir contents position)
  (path-append dir (list-ref contents position)))

(define (path-append path item)
  (if (equal? path "/")
      (string-append path item)
      (string-append path "/" item)))

(define (peek item)
  (if (can-enter? item)
      (directory item)
      (preview item)))

(define (preview file)
  (if (file-read-access? file)
      (list "not implemented")
      (list "permission denied")))

(define (can-enter? file)
  (and (directory? file) 
       (file-read-access? file)))

;;; All of the state in one place 

(set! SORT 'NAME)

(set! WORKING (current-directory))
(set! WORKING_CONTENTS (sort-by SORT (directory WORKING)))
(set! WORKING_POSITION 0)

(set! CHILD (child-of WORKING WORKING_CONTENTS WORKING_POSITION))
(set! CHILD_CONTENTS (peek CHILD))

(define (update-state sort new-dir new-position)
  (begin
    ; setting is likely cheap so no need to never do it
    (set! SORT sort)
    (cond 
      ; changed to parent directory
      ((equal? new-dir "..")
       (begin
         (change-directory "..")

         (set! WORKING (current-directory))
         (set! WORKING_CONTENTS (sort-by SORT (directory WORKING)))
         (set! WORKING_POSITION 0)

         (set! CHILD (child-of WORKING WORKING_CONTENTS WORKING_POSITION))
         (set! CHILD_CONTENTS (peek CHILD))))
      
      ; changed to child directory
      ((equal? new-dir CHILD)
       (unless (not (can-enter? CHILD))
         (begin
           (change-directory new-dir)

           (set! WORKING new-dir)
           (set! WORKING_CONTENTS (sort-by SORT (directory WORKING)))
           (set! WORKING_POSITION 0)

           (set! CHILD (child-of WORKING WORKING_CONTENTS WORKING_POSITION))
           (set! CHILD_CONTENTS (peek CHILD)))))

      ; move cursor up
      ((< new-position WORKING_POSITION)
       (set! WORKING_POSITION (max 0 (sub1 WORKING_POSITION)))
       (set! CHILD (child-of WORKING WORKING_CONTENTS WORKING_POSITION))
       (set! CHILD_CONTENTS (peek CHILD)))

      ; move cursor down
      ((> new-position WORKING_POSITION)
       (set! WORKING_POSITION (min (sub1 (length WORKING_CONTENTS))
                                   (add1 WORKING_POSITION)))
       (set! CHILD (child-of WORKING WORKING_CONTENTS WORKING_POSITION))
       (set! CHILD_CONTENTS (peek CHILD)))
    )))

(define (handle-key key)
  (cond
    ((equal? key #\j)
     (update-state SORT WORKING (add1 WORKING_POSITION)))
    ((equal? key #\k)
     (update-state SORT WORKING (sub1 WORKING_POSITION)))
    ((equal? key #\h)
     (update-state SORT ".." #f))
    ((equal? key #\l)
     (update-state SORT CHILD #f))))

;;; Functions to get menu dimensions

; parent menu bounds first third of the screen
(define (PARENT_MIN_X) 0)
(define (PARENT_MAX_X) (quotient (COLS) 3))

; current menu bounds second third of the screen
(define (WORKING_MIN_X) (add1 (PARENT_MAX_X)))
(define (WORKING_MAX_X) (* 2 (sub1 (WORKING_MIN_X))))

; child menu bounds final third of the screen
(define (CHILD_MIN_X) (add1 (WORKING_MAX_X)))
(define (CHILD_MAX_X) (sub1 (COLS)))

; all menus share height and offset
(define (MENU_MIN_Y) 1)
(define (MENU_MAX_Y) (sub1 (LINES)))

(define (SCROLL_POINT)
  (- (quotient (- (MENU_MAX_Y) (MENU_MIN_Y)) 2) (MENU_MIN_Y)))

;;; Rendering functions

(define (render-status-bar win)
  (mvwaddstr win 0 0 WORKING))

(define (render-main-menu win)
  (let ((min-x (WORKING_MIN_X))
        (max-x (WORKING_MAX_X))
        (min-y (MENU_MIN_Y))
        (max-y (MENU_MAX_Y)))

    (do ((offset min-y (add1 offset))
         (idx 0 (add1 idx)))
        ((>= idx (min (length WORKING_CONTENTS)
                      (- max-y min-y))))

        (let* ((drop-num (cond
                           ;((> WORKING_POSITION (- max-y min-y))
                           ; (- WORKING_POSITION (- max-y min-y)))
                           ((> WORKING_POSITION (SCROLL_POINT))
                            (- WORKING_POSITION (SCROLL_POINT)))
                           (#t 0)))
               (item (list-ref (drop WORKING_CONTENTS drop-num) idx)))
          (cond
            ((= idx (- WORKING_POSITION drop-num))
             (mvwaddstr win offset min-x "*")
             (mvwaddnstr win offset (add1 min-x) item (- max-x min-x 1)))
            (#t
             (mvwaddnstr win offset min-x item (- max-x min-x))))

          (let* ((path (path-append WORKING item))
                 (size 
                   (unless (not (can-enter? path))
                     (length (directory path)))))
            (cond
              ((symbolic-link? path)
               (let ((output (format #f "-> ~A" size)))
                 (mvwaddstr win offset (- max-x (string-length output)) output)))
              ((directory? path)
               (let ((output (format #f " ~A" size)))
                 (mvwaddstr win offset (- max-x (string-length output)) output)))
              (#t
               (let ((output (format #f " ~A" (format-file-size (file-size path)))))
                 (mvwaddstr win offset (- max-x (string-length output)) output)))
               ))))))

(define (render-child-menu win)
  (do ((offset (MENU_MIN_Y) (add1 offset))
       (idx 0 (add1 idx)))
      ((>= idx (min (length CHILD_CONTENTS)
                    (- (MENU_MAX_Y) (MENU_MIN_Y)))))
      (mvwaddnstr win offset 
                  (CHILD_MIN_X) 
                  (list-ref CHILD_CONTENTS idx) 
                  (- (CHILD_MAX_X) (CHILD_MIN_X)))))

(define (render-debug win)
  (mvwaddstr win (sub1 (LINES)) 0 
             (format #f 
                     "pos: ~A lines: ~A scroll: ~A" 
                     WORKING_POSITION
                     (- (MENU_MAX_Y) (MENU_MIN_Y))
                     (SCROLL_POINT)
                     )))

(define (format-file-size size)
  (let loop ((conversion size)
             (suffix '("B" "K" "M" "G" "T" "P")))
    (if (< conversion 1024)
        (format #f "~A ~A" conversion (car suffix))
        (loop (/ conversion 1024) (cdr suffix)))))

;;; Run program

(define (setup)
  (initscr)
  (noecho)
  (cbreak)
  (curs_set 0))

(define (cleanup)
  (echo)
  (nocbreak)
  (curs_set 1)
  (endwin))

(define (mainloop)
  (let loop ((win (stdscr)))
    (wclear win)
    (render-status-bar win)
    (render-main-menu win)
    (render-child-menu win)
    (render-debug win)
    (wrefresh win)

    (let ((key (getch)))
      (if (equal? key #\q)
          (cleanup)
          (begin
            (handle-key key)
            (loop win))))))

(define (main)
  (setup)
  (mainloop))

(main)

