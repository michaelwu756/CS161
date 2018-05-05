;
; CS161 Hw3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy
; Allegro).
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
;
; In either case, this limitation should not significantly affect your grade.
;
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
;
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
;
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

;;; goal-test (s)
;;;
;;; Return true (t) if and only if s is a goal state of a Sokoban game. Works by
;;; looping through s and checking that there are no boxes not on a
;;; star. Recurses by first iterating through the elements of (car s), then
;;; checks (cdr s).
(defun goal-test (s)
  (cond ((null s))
        ((null (car s)) (goal-test (cdr s)))
        ((isBox (car (car s))) nil)
        ((goal-test (cons (cdr (car s)) (cdr s))))))

;;; next-states (s)
;;;
;;; Returns a list of next states possible from the state s. This works by
;;; creating a list of size four from the result of try-move in all four
;;; directions. It then removes nil entries with cleanUpList.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s x y 0 -1)
                       (try-move s x y 0 1)
                       (try-move s x y -1 0)
                       (try-move s x y 1 0))))
    (cleanUpList result)))

;;; get-square (s x y)
;;;
;;; Takes a state s and coordinates x and y. Returns the number at the square
;;; given by x and y. The x axis begins at 0 and increases going to the
;;; right. The y axis begins at 0 and increases going downwards. This uses
;;; recursion by first recursing along the y axis, then the x axis. The base
;;; case is at (0,0), where this returns (car (car s)).
(defun get-square (s x y)
  (cond ((null s) wall)
        ((null (car s)) wall)
        ((< y 0) wall)
        ((< x 0) wall)
        ((> y 0) (get-square (cdr s) x (- y 1)))
        ((> x 0) (get-square (list (cdr (car s))) (- x 1) y))
        ((car (car s)))))

;;; set-square (s x y v)
;;;
;;; Takes a state s, coordinates x and y, and a value v. Returns the state s
;;; with the element at coordinate x and y replaced by value v. The x axis
;;; begins at 0 and increases goingto the right. The y axis begins at 0 and
;;; increases going downwards. Recurses in a similar way to get-square,
;;; appending to the result of the recursive call so that the output is correct.
(defun set-square (s x y v)
  (cond ((null s) s)
        ((null (car s)) s)
        ((< y 0) s)
        ((< x 0) s)
        ((> y 0) (cons (car s) (set-square (cdr s) x (- y 1) v)))
        ((> x 0) (cons (cons (car (car s))
                             (car (set-square (list (cdr (car s))) (- x 1) y v)))
                       (cdr s)))
        ((cons (cons v (cdr (car s))) (cdr s)))))

;;; try-move (s x y dx dy)
;;;
;;; Takes a state s, keeper coordinates x and y, and a direction specified by dx
;;; and dy. Returns the state of the game after a valid move in the direction
;;; given by dx and dy, or nil if no valid move exists. Also returns nil if the
;;; keeper is not at (x,y). Valid directions for (dx, dy) are (0,1), (0,-1),
;;; (1,0), and (-1,0). Uses get-state and set-state to check the values at given
;;; targets. If the square in front of the keeper is a star or blank, then a
;;; move can take place. If a box is blocking the square in front of the keeper,
;;; a further check must be made to the square past the target.
(defun try-move (s x y dx dy)
  (let ((origin (get-square s x y))
        (target (get-square s (+ x dx) (+ y dy)))
        (pastTarget (get-square s (+ x (+ dx dx)) (+ y (+ dy dy))))
        (targetX (+ x dx))
        (targetY (+ y dy))
        (pastTargetX (+ x (+ dx dx)))
        (pastTargetY (+ y (+ dy dy))))
    (cond ((or (> dx 1) (> dy 1) (< dx -1) (< dy -1) (= dx dy) (= dx (- dy))) nil)
          ((not (or (isKeeper origin) (isKeeperStar origin))) nil)
          ((isBlank target)
           (if (isKeeper origin)
               (set-square (set-square s x y blank) targetX targetY keeper)
             (set-square (set-square s x y star) targetX targetY keeper)))
          ((isStar target)
           (if (isKeeper origin)
               (set-square (set-square s x y blank) targetX targetY keeperstar)
             (set-square (set-square s x y star) targetX targetY keeperstar)))
          ((isBox target)
           (cond ((and (isKeeper origin) (isBlank pastTarget))
                  (set-square (set-square (set-square s x y blank) targetX targetY keeper) pastTargetX pastTargetY box))
                 ((and (isKeeper origin) (isStar pastTarget))
                  (set-square (set-square (set-square s x y blank) targetX targetY keeper) pastTargetX pastTargetY boxstar))
                 ((and (isKeeperStar origin) (isBlank pastTarget))
                  (set-square (set-square (set-square s x y star) targetX targetY keeper) pastTargetX pastTargetY box))
                 ((and (isKeeperStar origin) (isStar pastTarget))
                  (set-square (set-square (set-square s x y star) targetX targetY keeper) pastTargetX pastTargetY boxstar))))
          ((isBoxStar target)
           (cond ((and (isKeeper origin) (isBlank pastTarget))
                  (set-square (set-square (set-square s x y blank) targetX targetY keeperstar) pastTargetX pastTargetY box))
                 ((and (isKeeper origin) (isStar pastTarget))
                  (set-square (set-square (set-square s x y blank) targetX targetY keeperstar) pastTargetX pastTargetY boxstar))
                 ((and (isKeeperStar origin) (isBlank pastTarget))
                  (set-square (set-square (set-square s x y star) targetX targetY keeperstar) pastTargetX pastTargetY box))
                 ((and (isKeeperStar origin) (isStar pastTarget))
                  (set-square (set-square (set-square s x y star) targetX targetY keeperstar) pastTargetX pastTargetY boxstar)))))))

;;; h0 (s)
;;;
;;; Takes a state s and returns the trivial admissible heuristic, 0.
(defun h0 (s)
  0)

;;; h1-helper (s c)
;;;
;;; Takes a state s and a count c, and returns c plus the number of misplaced
;;; boxes in s. Recurses in a similar way to goal-test.
(defun h1-helper (s c)
  (cond ((null s) c)
        ((null (car s)) (h1-helper (cdr s) c))
        ((isBox (car (car s))) (h1-helper (cons (cdr (car s)) (cdr s)) (+ c 1)))
        ((h1-helper (cons (cdr (car s)) (cdr s)) c))))

;;; h1 (s)
;;;
;;; Takes a state s and returns the number of misplaced boxes in s by calling
;;; the helper function. This is an admissible heuristic, since each box
;;; unplaced box requires at least one move to finish.
(defun h1 (s)
  (h1-helper s 0))

(defun smaller (x y)
  (if (< x y) x y))

(defun absdiff (x y)
  (if (> (- x y) 0)
      (- x y)
    (- y x)))

;;;
(defun getStarListFirstRow (s r c)
  (cond ((null (car s)) nil)
        ((isStar (car (car s)))
         (cons (list r c) (getStarListFirstRow (cons (cdr (car s)) (cdr s)) r (+ c 1))))
        ((getStarListFirstRow (cons (cdr (car s)) (cdr s)) r (+ c 1)))))

(defun getStarList (s r)
  (cond ((null s) nil)
        ((append (getStarListFirstRow s r 0) (getStarList (cdr s) (+ r 1))))))

(defun getDistanceToClosestStar (starList x y)
  (cond ((null starList) -1)
          ((= (getDistanceToClosestStar (cdr starList) x y) -1)
           (+ (absdiff (car (car starList)) y) (absdiff (car (cdr (car starList))) x)))
          ((smaller
            (+ (absdiff (car (car starList)) y) (absdiff (car (cdr (car starList))) x))
            (getDistanceToClosestStar (cdr starList) x y)))))

(defun getBoxListFirstRow (s r c)
  (cond ((null (car s)) nil)
        ((isBox (car (car s)))
         (cons (list r c) (getBoxListFirstRow (cons (cdr (car s)) (cdr s)) r (+ c 1))))
        ((getBoxListFirstRow (cons (cdr (car s)) (cdr s)) r (+ c 1)))))

(defun getBoxList (s r)
  (cond ((null s) nil)
        ((append (getBoxListFirstRow s r 0) (getBoxList (cdr s) (+ r 1))))))

(defun sumMinDistances (boxList starList)
  (cond ((null boxList) 0)
        ((+ (getDistanceToClosestStar starList (car (car boxList)) (car (cdr (car boxList))))
            (sumMinDistances (cdr boxList) starList)))))
;;; h404751542 (s)
;;;
;;; Heuristic function that just calls h1.
(defun h404751542 (s)
  (sumMinDistances (getBoxList s 0) (getStarList s 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 2 1 4 1)
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1)
	   (1 1 1 0 0 1 1 1 1)
	   (1 0 0 0 0 0 2 0 1)
	   (1 0 1 0 0 1 2 0 1)
	   (1 0 4 0 4 1 3 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
