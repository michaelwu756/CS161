;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;;; BFS takes a list representation of a tree FRINGE and returns a list of leaf
;;; nodes in FRINGE as they are visited in leftmost first breadth-first search.

;;; Parameters:
;;;   FRINGE - list representation of tree
;;; Return Value:
;;;   A list of leaf nodes in FRINGE as they are visited in BFS.
(defun BFS (FRINGE)
  (cond ((null FRINGE) NIL)
        ((atom FRINGE) FRINGE)
        ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
        ((BFS (append (cdr FRINGE) (car FRINGE))))))

;;; Test cases
(BFS '(ROOT))
(BFS '((((L E) F) T)))
(BFS '((R (I (G (H T))))))
(BFS '(((A (B)) C (D))))
(BFS '((T (H R E) E)))
(BFS '((A ((C ((E) D)) B))))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call
; (DFS '(NIL NIL NIL NIL) NIL)
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
  (equal S '(T T T T)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer
; with dog, and p for homer with poison).
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poison and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
  (cond ((and (equal S '(NIL NIL NIL NIL)) (equal A 'b)) '((T T NIL NIL)))
        ((and (equal S '(NIL NIL NIL T)) (equal A 'b)) '((T T NIL T)))
        ((and (equal S '(NIL NIL NIL T)) (equal A 'd)) '((T NIL T T)))
        ((and (equal S '(NIL NIL T NIL)) (equal A 'b)) '((T T T NIL)))
        ((and (equal S '(NIL NIL T NIL)) (equal A 'p)) '((T NIL T T)))
        ((and (equal S '(NIL NIL T T)) (equal A 'h)) '((T NIL T T)))
        ((and (equal S '(NIL NIL T T)) (equal A 'b)) '((T T T T)))
        ((and (equal S '(NIL T NIL NIL)) (equal A 'h)) '((T T NIL NIL)))
        ((and (equal S '(NIL T NIL NIL)) (equal A 'd)) '((T T T NIL)))
        ((and (equal S '(NIL T NIL NIL)) (equal A 'p)) '((T T NIL T)))
        ((and (equal S '(T NIL T T)) (equal A 'h)) '((NIL NIL T T)))
        ((and (equal S '(T NIL T T)) (equal A 'd)) '((NIL NIL NIL T)))
        ((and (equal S '(T NIL T T)) (equal A 'p)) '((NIL NIL T NIL)))
        ((and (equal S '(T T NIL NIL)) (equal A 'h)) '((NIL T NIL NIL)))
        ((and (equal S '(T T NIL NIL)) (equal A 'b)) '((NIL NIL NIL NIL)))
        ((and (equal S '(T T NIL T)) (equal A 'b)) '((NIL NIL NIL T)))
        ((and (equal S '(T T NIL T)) (equal A 'p)) '((NIL T NIL NIL)))
        ((and (equal S '(T T T NIL)) (equal A 'b)) '((NIL NIL T NIL)))
        ((and (equal S '(T T T NIL)) (equal A 'd)) '((NIL T NIL NIL)))
        ((and (equal S '(T T T T)) (equal A 'b)) '((NIL NIL T T)))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
  (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
  (cond ((null STATES) NIL)
        ((equal S (car STATES)) T)
        ((ON-PATH S (cdr STATES)))))

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
  (cond ((null STATES) NIL)
        ((FINAL-STATE (car STATES)) (append PATH (list (car STATES))))
        ((ON-PATH (car STATES) PATH) (MULT-DFS (cdr STATES) PATH))
        ((MULT-DFS (SUCC-FN (car STATES)) (append PATH (list (car STATES)))))
        ((MULT-DFS (cdr STATES) PATH))))


; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
  (cond ((null PATH) (DFS S (List S)))
        ((FINAL-STATE s) path)
        ((MULT-DFS (SUCC-FN S) PATH))))

;;; Test cases
(DFS '(NIL NIL NIL NIL) NIL) ; original problem
(DFS '(T NIL NIL NIL) NIL) ; starting in invalid state
(DFS '(T T T T) NIL) ; starting in final state
(DFS '(NIL NIL NIL T) NIL) ; starting in unused state
(DFS '(NIL NIL NIL NIL) '((T T NIL NIL) (NIL NIL NIL NIL))) ; check prev state
