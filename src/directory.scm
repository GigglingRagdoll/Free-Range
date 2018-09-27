(use posix)

;;; child directory functions

(define (child-of wd wc wi)
  (path-append wd (list-ref wc wi)))

(define (peek path)
  (cond
    ((can-enter? path)
     (directory* path))
    (#t 
     (preview path))))

(define (can-enter? path)
  (and (directory? path)
       (file-read-access? path)))

(define (preview file)
  (list "just kidding"))

;;; parent directory functions

(define (parent-of wd)
  (cond
    ((equal? wd "/") "/")
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
  (let loop ((remaining (reverse (string->list str))))
    (if (equal? chr (car remaining))
        (list->string (reverse (cdr remaining)))
        (loop (cdr remaining)))))

(define (path-append path item)
  (if (equal? path "/")
      (string-append path item)
      (string-append path "/" item)))

(define (save-root str)
  (if (equal? str "")
      "/"
      str))

