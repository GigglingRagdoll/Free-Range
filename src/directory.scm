(use posix)

;;; child directory functions

(define (child-of wd wc wi)
  ; returns child at working index
  (path-append wd (list-ref wc wi)))

(define (peek path)
  ; returns directory contents
  ; or previews file
  (cond
    ((can-enter? path)
     (directory* path))
    (#t 
     (preview path))))

(define (can-enter? path)
  ; checks if a file if both a directory
  ; and that we have read access to it
  (and (directory? path)
       (file-read-access? path)))

(define (preview file)
  ; shows the contents of a file
  ; currently unimplemented
  (list "just kidding"))

;;; parent directory functions

(define (parent-of wd)
  ; returns parent of working directory
  (cond
    ; returns root even if it is the working directory
    ; since it's easier to deal with that scenario
    ((equal? wd "/") "/")
    ; make sure the root directory is not tampered with
    (#t (save-root (trim-to #\/ wd)))))

;;; helper functions

(define (directory* path)
  ; returns a list with a single empty string
  ; if directory is empty
  (let ((contents (directory path)))
    (if (null? contents)
        '("")
        contents)))

(define (trim-to chr str)
  ; removes characters from the right of the string
  ; until we reach the given character
  (let loop ((remaining (reverse (string->list str))))
    (if (equal? chr (car remaining))
        (list->string (reverse (cdr remaining)))
        (loop (cdr remaining)))))

(define (path-append path item)
  ; convenience function so we don't need a separate
  ; case for normal directories and the root directory
  ; every time we append paths together
  (if (equal? path "/")
      (string-append path item)
      (string-append path "/" item)))

(define (save-root str)
  ; returns "/" if we would get an empty string
  (if (equal? str "")
      "/"
      str))

