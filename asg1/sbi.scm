#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;;#!/Applications/racket/bin/mzscheme -qr
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

(define *function-table* (make-hash))
(define *variable-table* (make-hash))
(define *array-table* (make-hash))
(define *label-table* (make-hash))

(define NAN (/ 0.0 0.0))

(define (evaluate-expression expr)
  (cond ((number? expr) (+ expr 0.0))
    ((symbol? expr) (hash-ref *variable-table* expr NAN))
    ((pair? expr)
      (if (hash-has-key? *function-table* (car expr))
        (let ((func (hash-ref *function-table* (car expr) NAN)) 
          (opnds (map evaluate-expression (cdr expr)))) 
        (apply func (map evaluate-expression opnds))) 
      (vector-ref (hash-ref *array-table* (cadr expr)) 
        (exact-round (hash-ref *variable-table* (caddr expr))))))
    (else NAN)))

(define (interpret-print statement)
  (if (not (null? statement))
    (begin
      (cond ((string? (car statement)) 
        (display (car statement)))
      (else 
        ;;(display (car statement))
        (printf "~a " (evaluate-expression (car statement)))))
      (interpret-print (cdr statement)))
    (newline)))

(define (interpret-let statement)
  ;;(display (car statement))
  (if (pair? (car statement))
    (begin
      (vector-set! (hash-ref *array-table* (cadar statement)) 
        (exact-round (hash-ref *variable-table* (caddar statement)))
        (evaluate-expression (cadr statement))))
    (hash-set! *variable-table* (car statement) 
      (evaluate-expression (cadr statement))))
  ;;(hash-for-each *variable-table* 
  ;;(lambda (key value) (show key value)))
  ;;(newline)
  )

(define (interpret-dim statement)
  ;;(display (evaluate-expression (caddar statement)))
  (hash-set! *array-table* (cadar statement) 
    (make-vector 
      (exact-round (evaluate-expression (caddar statement))) 0.0)))
  ;;(hash-for-each *array-table* 
  ;;(lambda (key value) (show key value)))
  ;;(newline))

(define (interpret-goto program statement)
  ;;(display (- (hash-ref *label-table* (cadr statement)) 1))
  (interpret-program program 
    (- (hash-ref *label-table* (cadr statement)) 1)))

(define (interpret-if program statement)
  ;;(display (- (hash-ref *label-table* (caddr statement)) 1))
  (interpret-program program 
    (- (hash-ref *label-table* (caddr statement)) 1)))

(define (readnumber)
  (let ((object (read)))
    (cond ((eof-object? object) object)
                   ((number? object) (+ object 0.0))
                   (else (begin (printf "invalid number: ~a~n" object)
                                (readnumber))))))

(define (interpret-input statement) 
  ;;(display statement)
  (unless (null? statement)
    (let ((number (readnumber)))
      (if (eof-object? number)
        (begin
          (printf "*EOF* ~a~n" number) 
          (hash-set! *variable-table* 'eof 1)
          ;(hash-for-each *variable-table* 
            ;(lambda (key value) (show key value))) 
          ;(newline)
          )
        (begin
          ;;(printf "number = ~a~n" number)
          (hash-set! *variable-table* (car statement) number)
          (interpret-input (cdr statement)))))))                

(for-each
  (lambda (pair) 
    (hash-set! *function-table* (car pair) (cadr pair)))
  `(
    (+         ,+)
    (-         ,-)
    (*         ,*)
    (/         ,/)
    (^         ,expt)
    (sqrt      ,sqrt)
    (abs       ,abs)
    (acos      ,acos)
    (asin      ,asin )
    (atan      ,atan)
    (ceiling   ,ceiling)
    (cos       ,cos)
    (exp       ,exp)
    (floor     ,floor)
    (log       ,log )
    (round     ,round)
    (sin       ,sin)
    (sqrt      ,sqrt)
    (tan       ,tan)
    (truncate  ,truncate)
    (<=        ,(lambda (x y) (<= x y)))
    (>=        ,(lambda (x y) (>= x y)))
    (<         ,(lambda (x y) (< x y)))
    (>         ,(lambda (x y) (> x y)))
    (=         ,(lambda (x y) (eqv? x y)))
    (!=        ,(lambda (x y) 
      (not (eqv? (evaluate-expression x) (evaluate-expression y)))))
    (dim       ,interpret-dim)
    (let       ,interpret-let)
    (goto      ,interpret-goto)
    (if        ,interpret-if)
    (print     ,interpret-print)
    (input     ,interpret-input)))

(for-each
  (lambda (varval)
    (hash-set! *variable-table* (car varval) (cadr varval)))
  `(
    (pi  ,(acos -1.0))
    (e   ,(exp 1.0))
    (eof   0)
    ))

(define (evaluate-labels program)
  (unless (null? program)
    (let ((n (caar program)))
      (when (number? n)
        (if (not (null? (cdar program)))
          (if (symbol? (cadar program))
            (hash-set! 
              *label-table* (cadar program) (caar program))
            (void))
          (void))))
    (evaluate-labels (cdr program))))

(define (show label it)
  (display label)
  (display " = ")
  (display it)
  (newline)
)

(define (interpret-statement statement program line-num)
  ;;(display statement)  
  (if (not (null? statement))
    (begin
      (cond
        ((eqv? (car statement) 'goto) 
          (interpret-goto program statement))
        ((eqv? (car statement) 'if) 
          (if (evaluate-expression (cadr statement))
            (interpret-if program statement)
            (interpret-program program (+ line-num 1))))
        (else
          ((hash-ref 
            *function-table* (car statement)) (cdr statement))
          (interpret-program program (+ line-num 1)))))
    (exit 1)))

(define (interpret-program program line-num)
  (when (< line-num (length program))
    (let ((line (list-ref program line-num)))
      (cond
        ((= (length line) 3) 
          (interpret-statement (caddr line) program line-num))
        ((and (= (length line) 2) (pair? (cadr line)))
          (interpret-statement (cadr line) program line-num))
        (else
          (interpret-program program (+ line-num 1)))))))

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
              (write-program-by-line sbprogfile program)
              (evaluate-labels program)
              (interpret-program program 0))))

;;(if (terminal-port? *stdin*)
    ;;(main (vector->list (current-command-line-arguments)))
    ;;(printf "sbi.scm: interactive mode~n"))
(main (vector->list (current-command-line-arguments)))
