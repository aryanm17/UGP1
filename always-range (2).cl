(define (requirements)
  (:requirements :always-range))
  
(define (always-range ?predicate - predicate ?lower - float ?upper - float))

; Define time-points function to get all time points in the plan
; This function returns a list of integers representing the time points
(define (time-points)
  (map #'+ (domain-range) (range (total-time))))

; Define probability function to get the probability that a predicate holds at a given time point
; This function takes as input the predicate and the time point, and returns a floating-point number between 0 and 1
(define (probability ?predicate - proposition ?time - integer)
  (if (holds ?predicate ?time)
      1.0
      0.0))

; Define check-always-range function to check if a predicate holds with probability in the specified range
; This function takes as input the current state, the predicate, and the lower and upper bounds of the probability range
; It returns true if the predicate holds with probability in the specified range, and false otherwise
(define (check-always-range state ?predicate - predicate ?lower - float ?upper - float)
  (let ((count 0)
        (total 0))
    (dolist (?t (time-points))
      (let ((p (probability ?predicate ?t)))
        (when p
          (setq count (+ count 1))
          (setq total (+ total p)))))
    (let ((prob (if (= count 0) 0 (/ total count))))
      (and (>= prob ?lower) (<= prob ?upper)))))

; Define constraint that uses the check-always-range function to ensure that the predicate holds with probability in the specified range
(define (constraint always-range-constraint ?predicate - predicate ?lower - float ?upper - float)
  (always ?predicate)
  (check-satisfiability)
  (check-always-range state ?predicate ?lower ?upper))
