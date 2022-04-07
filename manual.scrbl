#lang scribble/manual
@(require "scribble-utensils.rkt" "coroutines.rkt" (for-label "coroutines.rkt" racket))

@title{Coroutines}
@author{Jacob J. A. Koot}
@(defmodule coroutines/coroutines #:packages ())

@section{Flow control}

A coroutine is a procedure
that can return values before it has finished and
thereafter can be called again such as to continue from the point from where it returned.
For this purpose a coroutine has a return procedure, say @itt{return}.

@inset{@image[#:scale 0.75 "control-flow.gif"]}

Proceed as follows:

@inset{
@itemlist[#:style 'ordered
 @item{Make a coroutine-constructor with procedure @nbr[coroutine-constr]
       @nb{or macro @nbr[co-lambda].}}
 @item{Use the constructor to make a coroutine.@(lb)
       The constructor accepts no arguments.}
 @item{A coroutine receives arguments when called for the first time.}
 @item{It can return by calling the @itt{return} procedure:@(lb)
       @(hspace 3)@itt{(return value ...)}@(lb)
      This returns the multiple value @itt{(@nbr[values] value ...)}}
 @item{The @itt{coroutine} can be called again:@(lb)
       @(hspace 3)@itt{(coroutine arg ...)}@(lb)
       The continuation of item 4 is called with multiple value @itt{(@nbr[values] arg ...)}}
 @item{After a coroutine has returned normally, it cannot be called again.}
 @item{A coroutine-constructor can be called repeatedly for multiple instances of the coroutine.
       It can be called by the coroutine itself too for nested instances.}]}

@defproc[
(coroutine-constr
 (proc-maker
  ((-> any/c ... any)
   (-> any/c ... any)
   coroutine-constr?
   . -> .
   (-> any/c ... any)))
 (terminator (-> any/c ... any) values))
(and/c coroutine-constr? (-> coroutine?))]{
                                           
The arguments given to the @nbr[proc-maker] are:
                           
@inset{
 @itemlist[#:style 'ordered
 @item{@itt{return}@(lb)For return and allowing the coroutine to be called again.}
 @item{@itt{finish}@(lb)For return and prohibiting the coroutine to be called again.}
 @item{@itt{constr}@(lb)The constructor as returned by procedure @nbr[coroutine-constr].@(lb)
       Can be used to make nested coroutines.}]}

The @nbr[proc-maker] is supposed to return a procedure. It is not yet called.
When the produced coroutine-constructor is called, a coroutine is returned,
but the @itt{proc-maker} still is not called.
The first time the coroutine is called
the @racket[proc-maker] is called with the arguments
@itt{return}, @itt{finish} and @itt{constr}.
Let @itt{proc} be the procedure returned by the @racket[proc-maker].
Now @itt{proc} is called with the arguments that where given to the coroutine.
The @itt{proc} can use @itt{return} in which case the coroutine returns with the values
given to @itt{return}.
When the coroutine is called again, it continues with the continuation of the @itt{return}-form.
This continuation receives the arguments given to the new coroutine-call.
The @itt{proc} can use @itt{finish} in which case the coroutine calls the @racket[terminator]
with the arguments given to @itt{finish}. The coroutine returns the values returned by the
@racket[terminator]. The coroutine expires and can no longer be called.
Returning normally from @itt{proc} has the same effect as @itt{finish}
called with the values returned normally from the @itt{proc}.
The @itt{proc} is not required to halt by calling @itt{finish} or by returning normally.
In this case returns and repeated calls can be made ad infinitum.
Argument @itt{constr} is given the coroutine-constructor and can be used to prepare
other instances of the coroutine to be used within @itt{proc}.}

@defform[(co-lambda (binding ...) header body ...)
#:grammar
((binding (#:return return-id)
         (#:finish finish-id)
         (#:finish finish-id terminator)
         (#:constr constr-id))
 (header (name arg ...)
         (name arg ...+ . rest-arg)
         rest-arg)
 (name id)
 (arg id (id default-expr) #,(itt "keyword id") #,(itt "keyword (id default-expr)"))
 (rest-arg id))
#:contracts
((terminator (-> any/c ... any)))]{
Defaults:
@inset{@Tabular[
((@nbr[return-id] @tt{return})
 (@nbr[finish-id] @tt{finish})
 (@nbr[constr-id] @tt{constr})
 (@nbr[terminator] @nbr[values]))
 #:sep (hspace 2)]}

Expanded to:

@inset{
@racketblock[
(coroutine-constr
 (λ (return-id finish-id constr-id)
  (define (name arg ... [· rest-id]) body ...)
  name)
 terminator)]}

@nbr[name], @nbr[return-id], @nbr[finish-id] and @nbr[constr-id]
are bound within procedure @nbr[name].}

@defproc[#:kind "predicate" (coroutine-constr? (arg any/c)) boolean?]

@defproc[#:kind "predicate" (coroutine? (arg any/c)) boolean?]

@defproc[(coroutine-state (coroutine coroutine?)) (or/c 'active 'inactive 'expired)]{
                                                                                     
The state of a coroutine is active, inactive or expired.
A coroutine-constructor returns an inactive coroutine.
A coroutine can be called when it is inactive.
If called while its state is not inactive, an error is raised.
Calling a coroutine activates it.
A return-procedure can be called while the coroutine is active.
If called while the coroutine is not active, an error is raised.
A return-procedure sets the state to inactive.
A normal return from the coroutine or return by means of a finish-procedure
sets the state to expired.
@inset{@image["states.gif" #:scale 0.75]}

The following example results in an error because @itt{return} is called
while the coroutine is inactive.

@Interaction[
((((co-lambda () (wrong) (return return)))))]

A coroutine that never expires.
It runs in constant space because the coroutine calls itself in tail position.

@Interaction[
(define fibonacci
 ((co-lambda () (fibonacci #:first (n 0) #:second (m 1))
  (return n)
  (fibonacci #:first m #:second (+ n m)))))
(for/list ((k (in-range 10))) (fibonacci))]}

@section{Examples}

@subsection{Conversation}

@Interaction[
(define make-boby
 (coroutine-constr
  (λ (return finish constr)
   (λ (arg)
    (displayln (return "Hoe are you?"))
    arg))))
(define boby (make-boby))
(boby "That's nice")
(boby "I am fine")]

@subsection{Flattener}

A flattener (not protected against circular lists):

@note{There are much faster and less memory consuming ways to make a flattener.}

@Interaction*[
(define make-flattener
 (coroutine-constr
  (lambda (return finish constr)
   (define (flatten x)
    (cond
     ((null? x) (values))
     ((pair? x) (flatten (car x)) (flatten (cdr x)))
     (else (return x))))
   flatten)))

(define flattener (make-flattener))

(flattener '(a ((b) . c) () . d))
(flattener)
(flattener)
(flattener)
(code:comment "The flattener has no more elements to return and returns no value.")
(flattener)
(code:comment "The coroutine has expired and calling it again yields an error.")
(flattener)]

A flattener made with @nbr[co-lambda] (not protected against circular lists):

@Interaction[
(define make-flatten
 (co-lambda () (flatten x)
  (cond
   ((null? x))
   ((pair? x) (flatten (car x)) (flatten (cdr x)))
   (else (return x)))
  'end))
(define flatten (make-flatten))
(flatten '((a (b c) . d) e () (()) . f))
(flatten)
(flatten)
(flatten)
(flatten)
(flatten)
(flatten)]

@subsection{Implied finish-call}

@Interaction[
(((coroutine-constr
   (lambda (return finish constr)
    (lambda () "I do nothing"))
   (lambda (msg)
    (string-append msg " and therefore finish immediately.")))))]

@subsection{Tower of Hanoi}
Tower-of-hanoi step by step:

@Interaction[
(define tower-of-hanoi
 ((coroutine-constr
   (lambda (return finish constr)
    (define (tower-of-hanoi n from thrd onto)
     (when (> n 0) (tower-of-hanoi (sub1 n) from onto thrd))
                   (return               n  from      onto)
     (when (> n 0) (tower-of-hanoi (sub1 n) thrd from onto)))
    tower-of-hanoi)
   (lambda ignore (values 'end 'end 'end)))))
(code:line)
(for ((n (in-naturals 1)))
 (define-values (disk from onto) (tower-of-hanoi 3 'a 'b 'c))
 #:break (and (equal? disk 'end) (printf "End~n"))
 (printf "Move ~a: disk ~s from ~a to ~a.~n"
  (~s n #:align 'right #:min-width 2) disk from onto))]

@subsection{Equal-fringe}

Procedure @tt{equal-fringe?} accepts two arguments and returns @nbr[#t]
if and only if the flattened arguments are equal.
For the flatteners coroutines are used. This can speed up procedure @tt{equal-fringe?}
when comparing long fringes that differ in early stage.
@nb{A counter} is added in order to check that no more iterations are made than necessary.

@Interaction[
(code:line)
(define (make-flatten counter)
 (co-lambda () (flatten x)
  (set-box! counter (add1 (unbox counter)))
  (cond
   ((null? x))
   ((pair? x) (flatten (car x)) (flatten (cdr x)))
   (else (return x)))
  (code:comment "Return empty list when no more atoms can be found.")
  '()))
(code:line)
(define count-x (box 0))
(define count-y (box 0))
(code:line)
(define (equal-fringe? x y)
 (define flatten-x ((make-flatten count-x)))
 (define flatten-y ((make-flatten count-y)))
 (let loop ((x (flatten-x x)) (y (flatten-y y)))
  (cond
   ((and (null? x) (null? y)))
   ((or (null? x) (null? y)) #f)
   ((eq? x y) (loop (flatten-x) (flatten-y)))
   (else #f))))
(code:line)
(equal-fringe? '(a b c) '((a) (b) (c)))
(values (unbox count-x) (unbox count-y))
(code:line)
(set-box! count-x 0)
(set-box! count-y 0)
(equal-fringe? '(a b c) '(x (a) (b) (c)))
(values (unbox count-x) (unbox count-y))]

@bold{@larger{@larger{The end}}}