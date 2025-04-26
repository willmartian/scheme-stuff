(define current-computation #f)

(define (make-signal initial-value)
  (let ((value initial-value)
        (observers '())) ; List of observer functions
    (lambda args
      (cond
       ;; If no arguments are provided, return the current value
       ((null? args)
        ;; If there's an active computation, register it as an observer
        (when current-computation
          (set! observers (cons current-computation observers)))
        value)
       ;; If the first argument is 'set, update the value and notify observers
       ((and (= (length args) 2) (eq? (car args) 'set))
        (let ((new-value (cadr args)))
          (if (not (equal? value new-value)) ; Only update if the value changes
              (begin
                (set! value new-value)
                ;; Notify all observers
                (for-each (lambda (observer) (observer)) observers)))))
       ;; Otherwise, raise an error
       (else (error "Invalid usage of signal"))))))

(define (make-computed-signal compute-fn)
  (let ((value #f)
        (observers '())
        (dirty #t))
    (define (recompute)
      (when dirty
        (let ((previous-computation current-computation))
          (set! current-computation recompute) ; Track dependencies
          (let ((new-value (compute-fn)))
            (if (not (equal? value new-value))
                (begin
                  (set! value new-value)
                  (for-each (lambda (observer) (observer)) observers))))
          (set! current-computation previous-computation)
          (set! dirty #f))))
    (lambda args
      (cond
       ((null? args)
        (recompute)
        (when current-computation
          (set! observers (cons current-computation observers)))
        value)
       (else (error "Invalid number of arguments to computed signal"))))))

(define (make-effect effect-fn)
  (define (run-effect)
    (let ((previous-computation current-computation))
      ;; Set the current computation to track dependencies
      (set! current-computation run-effect)
      ;; Execute the effect function
      (effect-fn)
      ;; Restore the previous computation
      (set! current-computation previous-computation)))
  ;; Run the effect initially to register dependencies
  (run-effect))