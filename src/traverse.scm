(use posix)

(load "state.scm")
(load "directory.scm")

(define (move-vertically by state)
  ; function for moving within a directory
  (let ((wd (free-range-working-directory state))
        (wc (free-range-working-contents state))
        (wi (free-range-working-index state)))

    ; bound move within 0 and length of contents
    (let ((move (if (< by 0)
                    (max 0 (+ wi by))
                    (min (sub1 (length wc)) (+ wi by)))))

    ; alter working index
    (free-range-working-index-set! 
      state move)

    ; set child with respect to new index
    (free-range-child-set!
      state (child-of wd wc move))

    ; same for the contents of the child
    (free-range-child-contents-set!
      state (peek (free-range-child state))))))

(define (move-horizontally by state)
  ; function for changing directories
  (if (< by 0)
      ; only go up a directory if we aren't at the root directory
      (unless (equal? "/" (free-range-working-directory state))
        (change-directory ".."))

      ; don't attempt to enter files or unreadable directories
      (unless (not (can-enter? (free-range-child state)))
        (change-directory (free-range-child state))))

  ; change working directory to current directory
  (free-range-working-directory-set!
    state (current-directory))

  ; get all contents of the working directory
  (free-range-working-contents-set!
    state 
    (directory* (free-range-working-directory state)))

  ; reset index
  (free-range-working-index-set!
    state 0)

  ; get parent directory
  (free-range-parent-directory-set!
    state (parent-of (free-range-working-directory state)))

  ; get parent contents
  (free-range-parent-contents-set!
    state (directory* (free-range-parent-directory state)))

  ; reset the child with respect to the index
  (free-range-child-set!
    state
    (child-of (free-range-working-directory state)
              (free-range-working-contents state)
              (free-range-working-index state)))

  ; get the contents of the child
  (free-range-child-contents-set!
    state (peek (free-range-child state))))

;;; input handling

(define (handle-key key state)
  (cond
    ; go to the next element
    ((equal? key #\j)
     (move-vertically 1 state))

    ; go to the previous element
    ((equal? key #\k)
     (move-vertically -1 state))

    ; go up a directory
    ((equal? key #\h)
     (move-horizontally -1 state))

    ; go down a directory
    ((equal? key #\l)
     (move-horizontally 1 state))))

