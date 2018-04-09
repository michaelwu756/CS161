;;; PAD takes an integer N and returns the Nth padovan number using recursion
;;; This is defined by the recurrence relation PAD(N) = PAD(N - 2) + PAD(N - 3)
;;; The base case is PAD(0) = PAD(1) = PAD(2) = 1

;;; Parameters:
;;;   N - Integer greater than or equal to 0
;;; Return Value:
;;;   An integer which is the Nth padovan number
(defun PAD (N)
  (cond ((<= N 2) 1)
        (t (+ (PAD (- N 2)) (PAD (- N 3))))))

;;; SUMS takes an integer N and returns the number of summations used in PAD(N)
;;; This uses recursion and is defined by the relation SUMS(N) = SUMS(N - 2) + SUMS(N -3) + 1
;;; The base case is SUMS(0) = SUMS(1) = SUMS(2) = 0

;;; Parameters:
;;;   N - Integer greater than or equal to 0
;;; Return Value:
;;;   An integer which is the number of sums in PAD(N)
(defun SUMS (N)
  (cond ((<= N 2) 0)
        (t (+ (SUMS (- N 2)) (SUMS (- N 3)) 1))))

;;; ANON takes a TREE object represented as a nested list and replaces every atom
;;; in the list with the atom ?. If TREE is an atom, ANON will return ?. If TREE
;;; is nil, ANON will return nil.

;;; Parameters:
;;;   TREE - Nested list, atom, or nil that represents a tree.
;;; Return Value:
;;;   Either a nested list that has the same structure as TREE with all atoms
;;;   replaced by ?, ? itself, or nil.
(defun ANON (TREE)
  (cond ((null TREE) nil)
        ((atom TREE) '?)
        (t (cons (ANON (car TREE)) (ANON (cdr TREE))))))

;;; Test cases for PAD
(PAD 0) ; 1
(PAD 1) ; 1
(PAD 2) ; 1
(PAD 3) ; 2
(PAD 4) ; 2
(PAD 5) ; 3
(PAD 6) ; 4
(PAD 7) ; 5
(PAD 8) ; 7
(PAD 9) ; 9

;;; Test cases for SUMS
(SUMS 0) ; 0
(SUMS 1) ; 0
(SUMS 2) ; 0
(SUMS 3) ; 1
(SUMS 4) ; 1
(SUMS 5) ; 2
(SUMS 6) ; 3
(SUMS 7) ; 4
(SUMS 8) ; 6
(SUMS 9) ; 8

;;; Test cases for ANON
(ANON '42)                             ; ?
(ANON 'FOO)                            ; ?
(ANON '(((L E) F) T))                  ; (((? ?) ?) ?)
(ANON '(5 FOO 3.1 -0.2))               ; (? ? ? ?)
(ANON '(1 (FOO 3.1) -0.2))             ; (? (? ?) ?)
(ANON '(((1 2) (FOO 3.1)) (BAR -0.2))) ; (((? ?) (? ?)) (? ?))
(ANON '(R (I (G (H T)))))              ; (? (? (? (? ?))))
