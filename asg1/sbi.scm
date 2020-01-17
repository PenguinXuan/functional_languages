#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.12 2020-01-08 17:13:13-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))


(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key))
(define (symbol-put! key value)
         (hash-set! *symbol-table* key value))

(for-each
    (lambda (pair) (hash-set! *function-table* (car pair) (cadr pair)))
    `(
        (abs       ,abs)
        (acos      ,acos)
        (asin      ,asin )
        (atan      ,atan)
        (ceil      ,ceiling)
        (cos       ,cos)
        (exp       ,exp)
        (floor     ,floor)
        (log       ,log )
        (round     ,round)
        (sin       ,sin)
        (sqrt      ,sqrt)
        (tan       ,tan)
        (truncate  ,truncate)
        (dim       ,interpret-dim)
        (let       ,interpret-let)
        (goto      ,interpret-goto)
        (if        ,interpret-if)
        (print     ,interpret-print)
        (input     ,interpret-input)
     ))


(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key `(0)))
(define (symbol-put! key value)
         (hash-set! *symbol-table* key value))

(for-each
    (lambda (varval)
        (hash-set! *variable-table* (car varval) (cadr varval)))
    `(
        (eof 0.0)
        (pi  ,(acos -1.0))
        (e   ,(exp 1.0))
     ))

(define NAN (/ 0.0 0.0))

(define *array-table* (make-hash))



(define *label-table* (make-hash))
;(define label
    ;(lambda (program)
  ;when program is not null
    ;if there is a label
     ; insert table with key:label, value:program 
    ;call helper on (cdr program)




    ;)



(define (interpret-program program)
    (printf "interp-prog: len program: ~a ~n" (len program))
    ;;(cond (not (null? )
           ;;function-get 



     ;;)
}



















