#lang scribble/manual
@(require "scribble-utensils.rkt" "coroutines.rkt" (for-label "coroutines.rkt" racket))

@title{Coroutines}
@author{Jacob J. A. Koot}
@(defmodule coroutines/coroutines #:packages ())

Coroutines resemble generators as found in module
@seclink["Generators" #:doc '(lib "scribblings/reference/reference.scrbl")]{racket/generator}.

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
 @item{Use the constructor to make a coroutine, say @itt{coroutine}.@(lb)
       The constructor accepts no arguments.}
 @item{A coroutine receives arguments when called for the first time.}
 @item{It can return by calling the @itt{return} procedure:@(lb)
       @(hspace 3)@itt{(return value ...)}@(lb)
      This returns the multiple value @itt{(@nbr[values] value ...)}}
 @item{The @itt{coroutine} can be called again:@(lb)
       @(hspace 3)@itt{(coroutine arg ...)}@(lb)
       The continuation of item 4 is called with the arguments @itt{arg ...}}
 @item{After a coroutine has returned normally it cannot be called again.
       A coroutine can also finish by means of a @itt{finish}-procedure.
       A coroutine that always returns by means of its @itt{return}-procedure and
       never returns normally and never calls its @itt{finish}-procedure
       can be called ad infinitum.}
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
 @item{@(list @itt{return}" : "(nbr (-> any/c ... any)))@(lb)
       For return and allowing the coroutine to be called again.}
 @item{@(list @itt{finish}" : "(nbr (-> any/c ... any)))@(lb)
       For return and prohibiting the coroutine to be called again.}
 @item{@(list @itt{constr}" : "@(nbr coroutine?))@(lb)
       The same constructor as returned by procedure @nbr[coroutine-constr].@(lb)
       Can be used to make nested coroutines.}]}

Procedure @nbr[coroutine-constr] returns a coroutine-constructor.
It does not yet call the @nbr[proc-maker].
The latter is called when the produced coroutine-constructor is called and
receives the arguments @itt{return}, @itt{finish} and @itt{constr}.
It is supposed to return a procedure, say @itt{proc}.
The constructor returns an instance of a coroutine.
The first time the coroutine is called
the @itt{proc} is called with the arguments given to the coroutine.
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
In this case the coroutine can be called ad infinitum.
Argument @itt{constr} is given the coroutine-constructor and can be used to prepare
other instances of the coroutine to be used within @itt{proc}.}

@(require (only-in scribble/core element))

@defform[(co-lambda (binding ...) header body ...)
#:grammar
((binding (code:line #:return return-id)
          (code:line #:finish finish-id)
          (code:line #:finish finish-id terminator)
          (code:line #:finish _ terminator)
          (code:line #:constr constr-id))
 (header (name arg ...)
         (name arg ... . rest-arg))
 (name id _)
 (arg id (id default-expr) (code:line keyword id) (code:line keyword (id default-expr)))
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
  (define (name arg ... [· rest-arg]) body ...)
  name)
 terminator)]}

If @nbr[name] is an underscore @nbr[free-identifier=?] with @nbrl[_]{syntax underscore}
a hidden hygienic identifier is chosen.
The same holds for the @nbr[finish-id].
The identifiers @nbr[name], @nbr[return-id], @nbr[finish-id] and @nbr[constr-id]
are bound within procedure @nbr[name].
@nb{The @nbr[define]-form} ensures that procedure @nbr[name]
can call itself recursively (if the @nbr[name] is not hidden)}

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

@note{
In the above example the following happens in order@(lb)
from the inner to the outer level of the nested expression:@(lb)
4: The @nbr[co-lambda]-form produces a coroutine-constructor.@(lb)
3: The coroutine-constructor is called returning a coroutine.@(lb)
2: The coroutine is called. It immediately returns the return-procedure.@(lb)
1: The returned return-procedure is called outside the dynamic extent of the coroutine.}

Calling a coroutine after it has expired yields an error too:

@Interaction[
(define c ((co-lambda (#:finish f +) (c) (values 1 2 3))))
(coroutine-state c)
(c)
(coroutine-state c)
(c)]

Example of a coroutine that never expires.
Disregarding the memory required for ever increasing natural numbers,
the coroutine runs in constant space because procedure @tt{fibonacci} calls itself in tail position.

@Interaction[
(define fibonacci
 ((co-lambda (#:return deliver) (fibonacci FIRST SECOND)
  (deliver FIRST)
  (fibonacci SECOND (+ FIRST SECOND)))))
(cons (fibonacci 0 1) (for/list ((k (in-range 20))) (fibonacci)))]

When a coroutine exits from its dynamic content by calling a continuation
located outside its dynamic extent, it remains active.

@Interaction[
(define coroutine #f)
(let/cc cc
 (set! coroutine ((co-lambda () (co) (cc 'monkey))))
 (coroutine))
(coroutine-state coroutine)]

There are hacks to reenter a coroutine that exits to a continuation located outside its
dynamic extent. I don't think such hacks have a purpose, though.}

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
   (define (FLATTEN x)
    (cond
     ((null? x) (values))
     ((pair? x) (FLATTEN (car x)) (FLATTEN (cdr x)))
     (else (return x))))
   FLATTEN)))

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
 (co-lambda () (FLATTEN x)
  (cond
   ((null? x))
   ((pair? x) (FLATTEN (car x)) (FLATTEN (cdr x)))
   (else (return x)))
  'end))
(define FLATTEN (make-flatten))
(FLATTEN '((a (b c) . d) e () (()) . f))
(FLATTEN)
(FLATTEN)
(FLATTEN)
(FLATTEN)
(FLATTEN)
(FLATTEN)]

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
 (code:comment "counter is a box used as a call by reference argument.")
 (co-lambda () (FLATTEN x)
  (set-box! counter (add1 (unbox counter)))
  (cond
   ((null? x))
   ((pair? x) (FLATTEN (car x)) (FLATTEN (cdr x)))
   (else (return x)))
  (code:comment "Return empty list when no more atoms can be found.")
  '()))
(code:line)
(define count-x (box 'yet-to-be-assigned))
(define count-y (box 'yet-to-be-assigned))
(code:line)
(define (equal-fringe? x y)
 (define flatten-x ((make-flatten count-x)))
 (define flatten-y ((make-flatten count-y)))
 (let loop ((x (flatten-x x)) (y (flatten-y y)))
  (cond
   ((and (null? x) (null? y)))
   ((or (null? x) (null? y)) #f)
   ((equal? x y) (loop (flatten-x) (flatten-y)))
   (else #f))))
(code:line)
(define (call-equal-fringe? x y)
 (set-box! count-x 0)
 (set-box! count-y 0)
 (values (equal-fringe? x y) (unbox count-x) (unbox count-y)))
(code:line)
(code:line (call-equal-fringe? '(a b c) '((a) ((b) ((c))))) (code:comment #,(green "ok")))
(code:line (call-equal-fringe? '(a b c p q r s) '(a b c p q r S)) (code:comment #,(red "no")))
(code:line (call-equal-fringe? '(a b c p q r s) '(a b c P q r s)) (code:comment #,(red "no")))
(code:line (call-equal-fringe? '(a b c d e f) '((A) b c d e f)) (code:comment #,(red "no")))]

@bold{@larger{@larger{The end}}}