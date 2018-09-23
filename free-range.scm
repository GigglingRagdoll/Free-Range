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
  (if (and (directory? item) (file-read-access? item))
      (directory item)
      (preview item)))

(define (preview file)
  (list "not implemented"))

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
       (unless (not (directory? CHILD))
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

;;; Rendering functions

(define (render-status-bar win)
  (mvwaddstr win 0 0 WORKING))

(define (render-main-menu win)
  (do ((i (MENU_MIN_Y) (add1 i))
       (idx 0 (add1 idx)))
      ((>= idx (min (length WORKING_CONTENTS)
                    (- (MENU_MAX_Y) (MENU_MIN_Y)))))
      (if (= idx WORKING_POSITION)
          (mvwaddstr win i (WORKING_MIN_X) (string-append "*" (list-ref WORKING_CONTENTS idx)))
          (mvwaddstr win i (WORKING_MIN_X) (list-ref WORKING_CONTENTS idx)))))

(define (render-child-menu win)
  (do ((offset (MENU_MIN_Y) (add1 offset))
       (idx 0 (add1 idx)))
      ((>= idx (min (length CHILD_CONTENTS)
                    (- (MENU_MAX_Y) (MENU_MIN_Y)))))
      (mvwaddstr win offset (CHILD_MIN_X) (list-ref CHILD_CONTENTS idx))))


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

