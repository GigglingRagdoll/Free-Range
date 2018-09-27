(use defstruct)

(load "directory.scm")

(defstruct free-range
  ; keeps track of neighboring directories
  ; while traversing the filesystem
  working-directory
  working-contents
  working-index
  parent-directory
  parent-contents
  child
  child-contents)

(define (init-state)
  (let* ((wd (current-directory)) ; working directory
         (wc (directory* wd))     ; working contents
         (wi 0)                   ; working index
         (pd (parent-of wd))      ; parent directory
         (pc (directory* pd))     ; parent contents
         (c  (child-of wd wc wi)) ; child
         (cc (peek c)))           ; child contents

    (let ((state (make-free-range
                   working-directory: wd
                   working-contents:  wc
                   working-index:     wi
                   parent-directory:  pd
                   parent-contents:   pc
                   child:             c
                   child-contents:    cc)))
      ; return new state
      state)))

