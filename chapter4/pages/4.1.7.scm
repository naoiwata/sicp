;;
;; @author naoiwata
;; SICP Chapter4
;; 4.1.7 Separating Syntactic Analysis from Execution
;;

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond
   ((self-evaluation? exp)
    (analyze-self-evaluating exp))
   ((quoted? exp)
    (analyze-quoted exp))
   ((variable? exp)
    (analyze-variable exp))
   ((assingnment? exp)
    (analyze-assingment exp))
   ((definition? exp)
    (analyze-definition exp))
   ((if? exp)
    (analyze-if exp))
   ((lambda? exp)
    (analyze-lambda exp))
   ((begin? exp)
    (analyze-begin exp))
   ((cond? exp)
    (analyze (cond->if exp)))
   ((application? exp)
    (analyze-application exp))
   (else
    (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lamnda (env) (loolup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exp)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentally first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((pproc (analyze (operatior exp)))
        (aprocs (map analyze (operands exp))))
    (lamnda (env)
            (execute-application (pproc env)
                                 (map (lambda (aproc) (aproc env))
                                      aprocs)))))

(define (execute-appliaction proc args)
  (cond
   ((primitive-procedure? proc)
    (apply-primitive-procedure proc args))
   ((compound-procedure? proc)
    (extend-environment (procedure-parameters proc)
                        args
                        (procedure-environment proc)))
   (else
    (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))
