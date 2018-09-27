(use ncurses)

(load "state.scm")
(load "render.scm")
(load "traverse.scm")

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
  (let ((state (init-state))) ; initialize state
    (let loop ((win (stdscr)))
      (wclear win)           ; clear screen
      (render-all win state) ; draw everything to screen
      (wrefresh win)         ; update screen

      (let ((key (getch)))   ; grab input
        (if (equal? key #\q) ; quit if q is detected
            (cleanup)

            (begin
              (handle-key key state) ; otherwise call handle-key
              (loop win)))))))       ; before looping 

(define (main)
  (setup)
  (mainloop))

(main)

