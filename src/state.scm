(use defstruct)

(defstruct free-range
  ; keeps track of neighboring directories
  ; while traversing the filesystem
  working-directory
  working-contents
  working-index
  child
  child-contents)

(define (init-state)
  (let* ((wd (current-directory)) ; working directory
         (wc (directory wd))      ; working contents
         (wi 0)                   ; working index
         (c  (child-of wd wc wi)) ; child
         (cc (peek c)))           ; child contents

    (let ((state (make-free-range
                   working-directory: wd
                   working-contents:  wc
                   working-index:     wi
                   child:             c
                   child-contents:    cc)))
      ; return new state
      state)))

