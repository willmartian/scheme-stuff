(use-modules (srfi srfi-64))

(load "lib.scm")

(test-begin "signal-tests")

(test-assert "Signal returns initial value"
  (let ((counter (make-signal 1)))
    (= (counter) 1)))

(test-assert "Signal updates value"
  (let ((counter (make-signal 1)))
    (counter 'set 2) ; Update the signal
    (= (counter) 2))) ; Check the updated value

(test-assert "Computed signal updates correctly"
  (let* ((count (make-signal 0))
         (double-count (make-computed-signal (lambda () (* 2 (count))))))
    (count 'set 1) ; Update the base signal
    (= (double-count) 2))) ; Check the computed signal

(test-assert "Effect runs on dependency change"
  (let ((count (make-signal 0))
        (effect-ran #f))
    (make-effect (lambda () (set! effect-ran #t)))
    (count 'set 1) ; Update the signal
    effect-ran)) ; Check if the effect ran

(test-assert "Multiple computed signals update correctly"
  (let* ((count (make-signal 0))
         (double-count (make-computed-signal (lambda () (* 2 (count)))))
         (triple-count (make-computed-signal (lambda () (* 3 (count))))))
    (count 'set 2) ; Update the base signal
    (and (= (double-count) 4) (= (triple-count) 6)))) ; Check both computed signals

(test-assert "Chained computed signals update correctly"
  (let* ((count (make-signal 1))
         (double-count (make-computed-signal (lambda () (* 2 (count)))))
         (quadruple-count (make-computed-signal (lambda () (* 2 (double-count))))))
    (count 'set 3) ; Update the base signal
    (= (quadruple-count) 12))) ; Check the chained computed signal

(test-assert "Effect reacts to computed signal"
  (let* ((count (make-signal 1))
         (double-count (make-computed-signal (lambda () (* 2 (count)))))
         (effect-output '()))
    (make-effect (lambda ()
                   (set! effect-output (cons (double-count) effect-output))))
    (count 'set 2) ; Update the base signal
    (count 'set 3) ; Update the base signal again
    (equal? effect-output '(6 4)))) ; Check if the effect ran with correct values

(test-end "signal-tests")