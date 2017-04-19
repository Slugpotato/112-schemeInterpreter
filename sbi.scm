#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm 00-hello-world.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;


; Define standard error
(define *stderr* (current-error-port))

; Find the name of the file being executed
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

; Exit and print out the error that caused exit
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

; Usage info and exit
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

; Read in the file
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; My functions

;; Function table
; Definitions
(define *function-table* (make-hash))

; Function for retrieving values for keys
(define (function-get key)
        (hash-ref *function-table* key))

; Function for inserting values for keys
(define (function-put! key value)
        (hash-set! *function-table* key value))

;;;;;;;;; Function calls

(define (functionEval expr)
  (cond

    ; Check for numbers
    ((number? expr)
      expr)

    ; Check for strings
    ((string? expr)
      expr)



  )

)

(define (printThis val)
  (map (lambda (line) (display (functionEval line))) val)
  ; (map (lambda (line) (display val)) val)
  ; (printf "is a string and made it to printThis!!!!")
  (newline)


)

; Entering functios into the hash table
(for-each
  (lambda (pair)
          (function-put! (car pair) (cadr pair))
          )
  `(      ; This hash table translates SB functions to our functions.
      (print   ,printThis)
      ; (dim   ,sb_dim)
      ; (let   ,sb_let)
      ; (input ,sb_input)
      ; (if    (void))
      ; (goto  (void)))
  )

)








;; Label table
; Definitions
(define *label-table* (make-hash))

; Function for retrieving values for keys
(define (label-get key)
        (hash-ref *label-table* key))

; Function for inserting values for keys
(define (label-put! key value)
        (hash-set! *label-table* key value))

;; Variable table
; Definitions
(define *variable-table* (make-hash))

; Function for retrieving values for keys
(define (variable-get key)
        (hash-ref *variable-table* key))

; Function for inserting values for keys
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (<>      ,(lambda (x y) (not (= x y))))
        (-       ,-)
        (*       ,*)
        (/       ,/)
        (abs     ,abs)
        (<=      ,<=)
        (>=      ,>=)
        (=       ,=)
        (>       ,>)
        (tan     ,tan)
        (<       ,<)
        (atan    ,atan)
        (sin     ,sin)
        (cos     ,cos)
        (asin    ,asin)
        (acos    ,acos)
        (log2    ,(lambda (x) (/ (log x) (log 2.0))))
        (round   ,round)
        (trunc   ,truncate)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)

     ))

; Show contents of a hashtable
(define (show label it)
    (display label)
    (display " = ")
    (display it)
    (newline)
)


; (define (endofstring linestosearch)
;   (printf "~s~n" linestosearch)
;   (if (null? (cdr linestosearch))
;     (car  linestosearch)
    

;     (endofstring (cdr linestosearch))))

(define (foundList linestosearch)
  ; (printf "linesearch is ~s~n"   linestosearch)
  ; (when (< 1 (length linestosearch))

    (when (pair? linestosearch)

      ((hash-ref *function-table* (car linestosearch)) (cdr linestosearch))
    ; ((label-get! (car linestosearch)) (cadr linestosearch))
    ; (printf "Found a list and is pair car: ~s~n"   (car linestosearch))
    ; (printf "Found a list and is not pair1: ~s~n"   linestosearch)
    )
    ; (if (pair? linestosearch)

    ; (printf "Found a list and is pair cdr: ~s~n"   (cdr linestosearch))
    ; (printf "Found a list and is not pair2: ~s~n"   linestosearch)
  ; )
  

  )

(define (endofstring linestosearch)
  ; (printf "Top: ~s~n" linestosearch)

  ; (when (< 1 (length linestosearch))
  ;   ;   (printf "~s~n" linestosearch)
  ;   ; )
  ;   (if (list? (cdr linestosearch))

      
  ;     (printf "was list, cadr is: ~s~n" (cdr linestosearch))
  ;     (printf "wasn't list, car is ~s~n" (car linestosearch))


  ;   )
  ; )
  ; (else (printf "~s~n" linestosearch))

  (if (null? (cdr linestosearch))
    ; (when (> 1 (length line))
    ;   (printf "~s~n" linestosearch)
    ; )

    ; (when (> 1 (length line))
    ;   (printf "Made it to here ~s~n" linestosearch)
    ; (printf "cdr was null, car is: ~s~n" (car  linestosearch))
    ; (car  linestosearch)
    (foundList (car linestosearch))
    ; (endofstring (cdr linestosearch))
    (endofstring (cdr linestosearch))

    ; (printf "cdr is: ~s~n" (cdr linestosearch))


    )

  ; (if (null? (cdr linestosearch))
    
  ;   (car  linestosearch)
  ;   (endofstring (cdr linestosearch))

  ;   ; (printf "cdr is: ~s~n" (cdr linestosearch))


  ;   )

  )

    


; Actually run the program
(define (runprog filename program)

  (map 
    (lambda (line) 

      ; one-armed if statement in scheme is when
      (when (not (null? line))
        (endofstring line)
        

            ; (printf "~s~n" (cadr line))
            ; (printf "~s~n" (- (car line) 1) )

          
        

      )
    )
      program
  )
    
;     (printf ")~n"))
)

; Search for labels
(define (search-for-labels filename program)

    (map (lambda (line) 

      ; one-armed if statement in scheme is when
      (when (not (null? line))
        (when (< 1 (length line))
          (when (not (list? (cadr line)))

            ; (printf "~s~n" (cadr line))
            ; (printf "~s~n" (- (car line) 1) )
            (label-put! (cadr line) (- (car line) 1))

          )
        )

      )
      ) program
    )
      
    (runprog filename program)
;     (printf ")~n"))
)
; Main function
(define (main arglist)

    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              ; (write-program-by-line sbprogfile program))))
              (search-for-labels sbprogfile program)
              (hash-for-each *label-table* (lambda (key value) (show key value)))
              )))
(main (vector->list (current-command-line-arguments)))

