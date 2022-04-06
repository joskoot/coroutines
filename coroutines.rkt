#lang racket

;-----------------------------------------------------------------------------------------------------

; By Jacob J. A. Koot

(provide coroutine-constr coroutine-constr? coroutine? coroutine-state co-lambda)

;-----------------------------------------------------------------------------------------------------

(define (coroutine-constr proc-maker (terminator values))
        
 (define (constr)
         
  (define (entry . args)
   (call-with-values (λ () (apply (proc-maker return finish constr) args)) finish))
  
  (define exit 'disabled-exit)
  
  (define state 'inactive)
  
  (define (coroutine . resume-values)
   (let/cc cc
    (toggle-control entry resume-values disabled-entry cc 'inactive 'active 'coroutine-call)))
  
  (define (return . return-values)
   (let/cc cc
    (toggle-control exit return-values cc disabled-exit 'active 'inactive 'return-call)))
  
  (define (finish . last-values)
   (let ((last-values (call-with-values (lambda () (apply terminator last-values)) list)))
    (toggle-control exit last-values expired-entry expired-exit 'active 'expired 'finish-call)))
  
  (define (toggle-control entry/exit values new-entry new-exit required-state new-state call-type)
   (unless (eq? state required-state) (wrong-entry state required-state values))
   (set! state new-state) (set! entry new-entry) (set! exit new-exit)
   (apply entry/exit values))
  
  (make-coroutine coroutine (lambda () state)))
 
 (make-coroutine-constr constr))

;-----------------------------------------------------------------------------------------------------

(define (coroutine-state coroutine) ((coroutine-ref coroutine 1)))

(define-values (dummy-1 make-coroutine-constr coroutine-constr? dummy-2 dummy-3)
 (make-struct-type
  'coroutine-constr
  #f     ; no supertype
  1      ; 1 field
  0      ; no auto fields
  #f     ; auto-field value does not apply
  (list (cons prop:custom-write (λ (obj port mode) (display "#<coroutine-constr>" port))))
  (make-sibling-inspector)
  0      ; the struct is a procedure as contained in field 0.
  '()))  ; the only field 0 already is immutable because of the previous argument.

(define-values (dummy-4 make-coroutine coroutine? coroutine-ref dummy-5)
 (make-struct-type
  'coroutine
  #f     ; no supertype
  2      ; 2 fields
  0      ; no auto fields
  #f     ; auto-value not applicable.
  (list (cons prop:custom-write (λ (obj port mode) (display "#<coroutine>" port))))
  (make-sibling-inspector)
  0      ; the struct is a procedure as containned in field 0.
  '(1)))

(define (disabled-entry . args) (wrong-entry 'disabled 'inactive args))
(define (disabled-exit  . args) (wrong-entry 'inactive 'active   args))
(define (expired-entry  . args) (wrong-entry 'expired  'inactive args))
(define (expired-exit   . args) (wrong-entry 'expired  'active   args))

(define (wrong-entry state required-state values)
 (error 'coroutine "required state: ~s, actual state: ~s, args: ~s" required-state state values))

;-----------------------------------------------------------------------------------------------------

(define-syntax (co-lambda stxx)
 
 (define idents '(return finish terminator constr))
 
 (define (parse-bindings stx)
  
  (define (parse-bindings hash stx)
   (syntax-case stx ()
    (() (apply values (map (λ (id) (hash-ref hash id (datum->syntax stxx id))) idents)))
    ((#:return id . rest)
     (and
      (identifier? #'id)
      (not (hash-has-key? hash 'return)))
     (parse-bindings (hash-set hash 'return #'id) #'rest))
    ((#:finish id fun . rest)
     (and
      (identifier? #'id)
      (not (hash-has-key? hash 'finish))
      (not (keyword? (syntax-e #'fun))))
     (parse-bindings (hash-set* hash 'finish #'id 'terminator #'fun) #'rest))
    ((#:finish id . rest)
     (and
      (identifier? #'id)
      (not (hash-has-key? hash 'finish)))
     (parse-bindings (hash-set* hash 'finish #'id 'terminator #'values) #'rest))
    ((#:constr id . rest)
     (and
      (identifier? #'id)
      (not (hash-has-key? hash 'constr)))
     (parse-bindings (hash-set hash 'constr #'id) #'rest))))
  
  (define (generate-temp id) (car (generate-temporaries (list id))))
  (parse-bindings (make-immutable-hash (list (cons 'terminator #'values))) stx))
 
 (syntax-case stxx ()
  ((_ (binding ...) (name . args) . body)
   (let-values
    (((return finish terminator constr)
      (parse-bindings #'(binding ...))))
    (with-syntax
     ((return return)
      (finish finish)
      (terminator terminator)
      (constr constr))
   #'(coroutine-constr
      (λ (return finish constr)
       (define (name . args) . body)
       name)
      terminator))))))

;-----------------------------------------------------------------------------------------------------

