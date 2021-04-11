#! /usr/bin/guile
!#

;; The concept of event handling is different from transitioning.

;; When you enter a state, it is possible for the state to have an initial
;; state ... the initial state only points to direct children, not to
;; grandchildren.
;; The "init" event is only fired when the state was addressed DIRECTLY.

;; Events can take arguments - yours ... don't.

;; Guard conditions could be done with closures on the event procedures?
;; (I'm not so sure that this is going to work out)
;; With guard conditions, if the guard prevents the event from running - the
;; event has to percolate up somewhere else. This seems to mean that the event
;; handler has to return a boolean?

;; Don't know how to do "transition to history" (yet).
;; Transition-to-history comes in two types:
;; 1) Shallow transition to history, which can probably be implemented with a "set!" on the parent object.
;; 2) Deep transition to history ... not so sure, probably a rewrite?
;;    Perhaps figuring out a way to do this in the hsm class.
;;    You would either need to use a (find-root) function, or store a reference to the root.
;;    You could then use a hashtable there to do the lookups?
;;    You might also need labels on the states to store as tags.
;; Should each HSM run in its own thread? (or be able to? there's a lot of side effecting going on)
;; XXX: Since 'handle-event' handles(?) the case of no parent ... do you need an HSM class?
(use-modules (oop goops)
	     (ice-9 hash-table))


;; Class definitions and helper methods.
;; XXX: seems like all this does is track the active state?
(define-class <hsm> ()
  active-state)


(define-class <state> ()
  (parent #:init-form #f)
  (events #:init-form (events-initialize))
  (contained-states #:init-form (make-hash-table)))


(define (events-initialize)
  "Ensures that the default 'entry and 'exit event handlers are installed."
  (alist->hash-table
    `((entry . ,do-nothing)
      (init  . ,do-nothing)
      (exit  . ,do-nothing))))


(define (event-handled? e) e)
(define event-handled #t)
(define event-not-handled #f)
(define (do-nothing) event-handled)


(define-method (add-handlers (state <state>) event-handlers)
  "When supplied with an alist of event handlers, add them to the state."
  (let ([handlers (slot-ref state 'events)])
    (for-each
      (lambda (h)
	(let ([event (car h)]
	      [proc (cdr h)])
	  (hash-set! handlers event proc)))
      event-handlers)))


(define-method (add-contained (parent <state>) (child <state>))
  "Maintains the parent link and mapping of contained and recursively contained states."
  (slot-set! child 'parent parent)

  (let ([parent-container (slot-ref parent 'contained-states)])
    ;; Add the child state to the container, with the next step being itself.
    (let ([grandchildren (slot-ref child 'contained-states)])
      (hash-set! parent-container child child)

      ;; Iterate over grandchildren, adding child name as the next step.
      ;; The mapping is: target -> next step to target.
      ;; You ignore the 'value', because it is the next step for the child.
      ;; In the case here, your next step is actually the child.
      (hash-for-each
	(lambda (target next-step)
	  (hash-set! parent-container target child))
	grandchildren))))


(define-method (complete-transition (machine <hsm>) (target <state>))
  (slot-set! machine 'active-state target)
  event-handled)


;;----------------------------------------------------------------------------
;; FIXME I think that this has to return a boolean indicating that the event
;; was handled or not. There are a couple of special situations that I need to
;; check:
;; 1) initial transition (default thunk that returns #t?) DONE
;; 2) guarded event (return the guard?)
;;   a) guard prevent: return #f
;;   b) guard allow: return transition, or return #t (if internal)?
;;   c) at any rate, guards should not be special as far as this function is concerned.

(define (handle-event state event)
  "Look inside of the state for a handler of 'event'."
  ;; The 'when' condition causes events that make it up this far to be dropped.
  ;; This is because the root state has no parent.
  (when state
    (display (format #f "handle-event event: \"~s\"\n" event)) ;; DEBUG
    (let* ([table (slot-ref state 'events)]
	   [thunk (hash-ref table event)])
      (when (list? thunk) (display "\nlist\n")) ;; DEBUG
      (when (procedure? thunk) (display "procedure\n")) ;; DEBUG
      thunk)))


(define (get-contained state target-state)
  (hash-ref (slot-ref state 'contained-states) target-state))


(define (get-parent state)
  (slot-ref state 'parent))


(define (transition hsm start target)
  (display "transitioning\n") ;; DEBUG
  (let loop ([s start])
    (let ([c (get-contained start target)]
          [p (get-parent start)])
      (cond [(equal? s target)
             ; EITHER an init transition OR this is your new target state.
             (if (hash-get-handle 'init)
                 (handle-event s 'init)
                 (complete-transition hsm target))]
            [c
	      (handle-event c 'entry)
	      (loop c)]
	    [p
	      (handle-event s 'exit)
	      (loop p)]))))


(define-method (initialize (hsm <hsm>) (start <state>))
  (slot-set! hsm 'active-state start))


(define-method (dispatch (hsm <hsm>) event)
  (display (format #f "dispatch: event: \"~s\"\n" event)) ;; DEBUG
  (let loop ([s (slot-ref hsm 'active-state)])
    (let ([result (handle-event s event)])
      (when (not (event-handled? result))
	(loop (get-parent s))))))


;;------;;
;; test ;;
;;------;;
;; First, you define the HSM.
(define m (make <hsm>))

;; Next, you define the states.
(define stop (make <state>))
(define first-digit (make <state>))
(define second-digit (make <state>))
(define third-digit (make <state>))

;; Then you add the event tables.
;; FIXME: should HSM be able to track the temporary state pointers?
(add-handlers first-digit 
  `((1 . ,(lambda ()
	   (display "event 1: transitioning from first-digit to second-digit\n")
	   (transition m first-digit second-digit)))))

(add-handlers second-digit
  `((2 . ,(lambda ()
	   (display "event 2: transitioning from second-digit to third-digit\n")
	   (transition m second-digit third-digit)))))

(add-handlers third-digit
  `((3 . ,(lambda ()
	   (display "event 3: transitioning from third-digit to stop\n")
	   (transition m third-digit stop)))))

;; Add the states as substates.
;; perhaps "contained-by"?
(add-contained third-digit stop)
(add-contained second-digit third-digit)
(add-contained first-digit second-digit)

;; FIXME The problem here is that transition is never being called.
(initialize m first-digit)
(dispatch m 'hi)
(dispatch m 1)
(dispatch m 'hi)
(dispatch m 2)
(dispatch m 1)
