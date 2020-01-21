#!/Applications/racket/bin/mzscheme -qr
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


(define *function-table* (make-hash))
(define *variable-table* (make-hash))
(define *array-table* (make-hash))
(define *label-table* (make-hash))

(define NAN (/ 0.0 0.0))

(define (evaluate-expression expr)
  ;;(display expr)
  (cond ((number? expr) (+ expr 0.0))
    ((symbol? expr) (hash-ref *variable-table* expr NAN))
    ;;((and (pair? expr) (vector? (hash-ref *variable-table* (caddr expr))))
      ;;(vector-ref (hash-ref *array-table* (cadr expr)) 
       ;;(- (exact-round (hash-ref *variable-table* (caddr expr))) 1)))

    ((pair? expr)
      (let ((func (hash-ref *function-table* (car expr) NAN))
        (opnds (map evaluate-expression (cdr expr))))
      (if (null? func) NAN
        (apply func (map evaluate-expression opnds)))))
    (else NAN)))

(define (interpret-print statement)
  (if (not (null? statement))
    (begin
      (if (string? (car statement))
        (display (car statement))
        (display (evaluate-expression (car statement))))
      (interpret-print (cdr statement)))
    (newline)))

(define (interpret-let statement)
  (if (pair? (car statement))
    (begin
      ;;(display (hash-ref *array-table* (cadar statement)))
      ;;(display (- (exact-round (hash-ref *variable-table* (caddar statement))) 1))
      ;;(display (evaluate-expression (cadr statement))))
      (vector-set! (hash-ref *array-table* (cadar statement)) 
        (- (exact-round (hash-ref *variable-table* (caddar statement))) 1)
        (evaluate-expression (cadr statement))))
    (hash-set! *variable-table* (car statement) (evaluate-expression (cadr statement))))
  ;;(hash-for-each *array-table* (lambda (key value) (show key value)))
  ;;(newline)
  )

(define (interpret-dim statement)
  ;;(display (cadar statement)))
  ;;(display (caddar statement)))
  (hash-set! *array-table* (cadar statement) (make-vector (exact-round (caddar statement)) 0.0)))
  ;;(hash-for-each *array-table* (lambda (key value) (show key value)))
  ;;(newline))
  ;;(hash-set! *function-table* (caar expr) 
    ;;(lambda(x) (vector-ref (variable-get (caar expr)) (- x 1)))))

(define (interpret-goto program statement)
  ;;(display (- (hash-ref *label-table* (cadr statement)) 1))
  (interpret-program program (- (hash-ref *label-table* (cadr statement)) 1)))

(define (interpret-if program statement)
  ;;(display (- (hash-ref *label-table* (caddr statement)) 1))
  (interpret-program program (- (hash-ref *label-table* (caddr statement)) 1)))

(define (interpret-input statement)
  (variable-put! 'inputcount 0)
  (define (get-input expr)
     (when (not (null? (car expr)))
        (variable-put! (car expr) (void))
        (let ((object (read)))
      (cond [(eof-object? object)(variable-put! 'inputcount -1)]
       [(number? object)(variable-put! (car expr) object)
        (variable-put! 'inputcount (+ (variable-get 'inputcount) 1))]
          [else (begin (printf "invalid number: ~a~n" object)
                                )] )) 
         (when (not (null? (cdr expr)))
     (get-input (cdr expr)))
   )
  )
  (get-input expr)
)







(for-each
  (lambda (pair) (hash-set! *function-table* (car pair) (cadr pair)))
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
    (<=        ,(lambda (x y) (<= x y)))
    (>=        ,(lambda (x y) (>= x y)))
    (<         ,(lambda (x y) (< x y)))
    (>         ,(lambda (x y) (> x y)))
    (=         ,(lambda (x y) (eqv? x y)))
    (dim       ,interpret-dim)
    (let       ,interpret-let)
    (goto      ,interpret-goto)
    (if        ,interpret-if)
    (print     ,interpret-print)
    (input     ,interpret-input)))

(for-each
  (lambda (varval) (hash-set! *variable-table* (car varval) (cadr varval)))
  `(
    (pi  ,(acos -1.0))
    (e   ,(exp 1.0))
    ))

(define (evaluate-labels program)
  (unless (null? program)
    (let ((n (caar program)))
      (when (number? n)
        (if (not (null? (cdar program)))
          (if (not (symbol? (cadar program)))
            (void)
            (begin
              (hash-set! *label-table* (cadar program) (caar program))))
          (void))))
    (evaluate-labels (cdr program))))


(define (show label it)
  (display label)
  (display " = ")
  (display it)
  (newline)
)

(define (interpret-statement instr program line-num)
  (if (null? instr)
    (interpret-program program (+ (line-num 1)))
    (begin
      (unless (hash-has-key? *function-table* (car instr))
        (display (car instr)) (display " is not valid") (newline)
        (usage-exit))
      (cond
        ((eqv? (car instr) 'goto)
          (interpret-goto program instr))
        ((eqv? (car instr) 'if)
          ;;(display (cadr instr))
          (if (equal? #t (evaluate-expression (cadr instr)))
            (interpret-if program instr)
            (interpret-program program (+ line-num 1))))
        (else
          ((hash-ref *function-table* (car instr)) (cdr instr))
          (interpret-program program (+ line-num 1))
        )))))

(define (interpret-program program line-num)
  (when (< line-num (length program))
    (let ((line (list-ref program line-num)))
      (cond
        ((= (length line) 3) 
          ;;(display (caddr line))
          (interpret-statement (caddr line) program line-num))
        ((and (= (length line) 2) (list? (cadr line)))
          ;;(display (cadr line))
          (interpret-statement (cadr line) program line-num))
        (else
          (interpret-program program (+ line-num 1)))))))


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
    (evaluate-labels program)
    (interpret-program program 0)
    )


(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))



#|(define *stderr* (current-error-port))

;; run-file: finds file in root and reads it
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath)))

;; die: kills off any program with nonexistent file
;; and displays error message on screen
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1))

;; usage-exit: displays message to stderr
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename")))

;; read-list-from-inputfile: reads input sbir files
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;; function table: holds all the functions
;; and the variables
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key '(no such key in
                                         function-table)))
(define (function-put! key value)
        (hash-set! *function-table* key value))

;; label table: hold addresses of each line
;; and the labels for each program
(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key '()))
(define (label-put! key value)
        (hash-set! *label-table* key value ))

;; variable table: holds all the value of
;; the variables
(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key '(no such key in 
                                          variable-table)))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

;; length: returns the length of the list
(define len (lambda (l)
    (define length (lambda (l n)
        (if (null? l)n
          (length (cdr l) (+ n 1)))))
        (length l 0)))

;; value: get the value from the list
(define (value-of expr)
  ;(printf "value-of: cond number? expr ~a ~n" expr)
  (if (pair? expr)
      (apply (function-get (car expr)) 
          (map value-of (cdr expr)))
        (cond ((number? expr) expr)
            (else (variable-get expr)))))

;; display-list: for debugging purposes only
;;(define (display-list lst)
;;  (let loop ((lst lst))
;;    (when (pair? lst) 
;;      (printf "display-list: list is: ~a ~n" lst)
;;      (printf "display-list: car of list: ~a ~n" (car lst))
;;      (printf "display-list: cdr of list: ~a ~n" (cdr lst))
;;      (loop (cdr lst))))
;;  (newline))

;; interp-print: deals with print statments
(define (interp-print expr) 
   ;(printf"in interp-print: expr: ~a ~n" expr)
   (if (not (null? expr))
     (begin
          (if (string? (car expr))
            (display (car expr))
            (display (value-of (car expr))))         
            (interp-print (cdr expr)))
            (newline)))

;; interp-input: deals with input statements
(define (interp-input expr)
    ;(printf "interp-input: expr: ~a ~n" (display-list expr))
    (variable-put! 'inputcount 0)
        (define (get-input expr)
            (when (not (null? (car expr)))
                ;(printf "interp-input: case null? 
                ;(car expr) car expr: ~a ~n" (car expr))
            (variable-put! (car expr) (void))
            (let ((data (read)))
            ;(printf "interp-input: data: ~a ~n" data)
                (cond [(eof-object? data)(variable-put! 'inputcount -1)]
                    [(number? data)(variable-put! (car expr) data)
                        (variable-put! 'inputcount 
                            (+ (variable-get 'inputcount) 1))]
                [else (begin (printf "invalid number: ~a~n" data))]))
         (when (not (null? (cdr expr)))
             ;(printf "interp-input: case null? 
             ;(cdr expr) cdr expr: ~n" (cdr expr))
             (get-input (cdr expr)))))
  (get-input expr))

;; interp-dim: deals with dim statements
(define (interp-dim expr)
    ;(printf "interp-dim: expr: ~a ~n" expr)
    (variable-put! (caar expr) (make-vector (value-of (cadar expr))) )
    ;(printf "interp-dim: (caar expr): ~a, (cadar expr): ~a ~n"
    ;    (caar expr) (cadar expr))
    (function-put! (caar expr)
        (lambda(x) (vector-ref (variable-get (caar expr)) (- x 1)))))

;; interp-let: deals with let statments
(define (interp-let expr)
    ;(printf "interp-let: expr: ~a ~n" expr)
    (if (pair? (car expr))
        (begin
            ;(printf "interp-let: got in case if pair? (car expr) ~n")
            (vector-set! (variable-get
                (caar expr)) (- (value-of (cadar expr)) 1) 
                 (value-of (cadr expr))))
    (begin
        ;(printf "interp-let: in case else ~n")
        (let ((result (value-of (cadr expr))))
           (variable-put! (car expr) result)
           ;(printf "interp-let: result: ~a, 
           ;(car expr): ~a ~n" result (car expr))
           ;(printf "interp-let: result: ~a, 
           ;(cadr expr): ~a ~n" result (cadr expr))
        ))))

;; interp-if deals with if statments
(define (interp-if program expr)
    (interp-prog program (- (label-get (caddr expr)) 1)))

;; interp-goto: deals with goto statements
(define (interp-goto program expr)
    (interp-prog program (- (label-get (cadr expr)) 1)))

;; for-each for operator expressions, code to
;; evaluate function calls and arrays from
;; the symbol table
(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(
        (+ ,+) 
        (- ,-) 
        (* ,*) 
        (/ ,(lambda (x y)  (/ x (if (equal? y 0) 0.0 y))))
        (% ,(lambda (x y) (- x (* (div x y) y))))
        (^ ,expt)
        (abs ,abs) 
        (ceil ,ceiling) 
        (floor ,floor) 
        (exp ,exp)
        (sqrt ,sqrt) 
        (sin ,sin)  
        (cos ,cos) 
        (tan ,tan)
        (asin ,asin) 
        (acos ,acos) 
        (atan ,atan)
        (round ,round)
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (<=      ,(lambda (x y) (<= x y)))
        (>=      ,(lambda (x y) (>= x y)))
        (<       ,(lambda (x y) (< x y)))
        (>       ,(lambda (x y) (> x y)))
        (=       ,(lambda (x y) (eqv? x y)))
        (<>      ,(lambda (x y) (not (equal? x y))))
        (log     ,(lambda(x)(log (if (equal? x 0) 0.0 x))))
        (print   ,interp-print)
        (input   ,interp-input)
        (dim     ,interp-dim)
        (let     ,interp-let)
        (if      ,interp-if)
        (goto    ,interp-goto)
    ))

(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(
        (inputcount 0)
        (pi 3.141592653589793238462643383279502884197169399)
        (e 2.718281828459045235360287471352662497757247093)))

;; interp-stmt: deals with statements
(define (interp-stmt instr program line-nr)
  ;(printf "interp-stmt: instr is: ~a ~n" instr)
    (if (null? instr)
      (interp-prog program (+ line-nr 1))
      (begin
      (when (not (hash-has-key? *function-table* (car instr )))
          (print (car instr))
          (printf " is not valid ~n")
          (usage-exit))
      (cond
        ((eqv? (car instr) 'goto)
          ;(printf "got in case goto ~n")
          (interp-goto program instr))
        ((eqv? (car instr) 'if)
          (if (equal? #t (value-of (cadr instr)))
            (interp-if program instr)
            (interp-prog program (+ line-nr 1))))  
          (else
            ((function-get (car instr)) (cdr instr))
            (interp-prog program (+ line-nr 1)))))))

;; label: deals with labels
(define (build-label-table list)
    (when (not (null? list))
        (let ((first (caar list)))
        (when (number? first)
            (if (not (null? (cdar list)))
                (if (not (symbol? (cadar list)))
                    (void)
                    (begin
                        (label-put! (cadar list) (caar list))))
                            (void))))
                        ;(display-list list)
    (build-label-table (cdr list))))

;; interp-prog: deals with executed lines
(define (interp-prog program line-nr)
    ;(printf "interp-prog: program is: ~a ~n" program)
    (when (< line-nr (len program))
        ;(printf "interp-prog: len program: ~a ~n" (len program))
        (let ((line (list-ref program line-nr)))
            (cond
                ((= (len line) 3) 
                    ;(printf "interp-prog: case 
                    ;= (len line) 3, line is: ~a ~n"
                    ;    (len line))
                    ;(newline)
                    (set! line (cddr line))
                    (interp-stmt (car line) program line-nr))
                    ((and (= (len line) 2) (list? (cadr line)))
                        ;(printf "interp-prog: case 
                        ;= (len line) 2: len line is: ~a ~n"
                        ;    (len line))
                        ;(printf "interp-prog case 
                        ;= (len line) 2: line is: ~a ~n"
                        ;    line)
                        (set! line (cdr line))
                        ;(printf "interp-prog: line is: ~a ~n" line)
                        (interp-stmt 
                            (car line) program line-nr))           
                    (else
                        ;(printf "interp-prog: case else: 
                        ;len line: ~a ~n" (len line))
                        (interp-prog program (+ line-nr 1)))))))

;; main: calls readlist-from-input-file and then process
;; the sbir program
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               (begin (build-label-table program)
                      (interp-prog program 0)))))

(main (vector->list 
    (current-command-line-arguments))) |#;; stores results to list






