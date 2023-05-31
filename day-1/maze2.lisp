(in-package #:calm)
(setq *random-state* (make-random-state t))
;; https://rosettacode.org/wiki/Knuth_shuffle#Common_Lisp
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defparameter *maze-width* 13)
(defparameter *maze-height* 13)

(defparameter maze nil)

(defun reset-maze ()
  ;; in the beginning, there was a piece of flat land, each plot has the initial value 1, means it's solid.
  (setf maze (make-array (list *maze-width* *maze-height*) :initial-element 1))
  ;; then, we dig an entrance in the upper left corner,
  ;; because we need the left wall to exist, so we start from (1 0) instead of (0 0)
  (setf (aref maze 1 0) 0)
  ;; and then, we dig one step further
  (setf (aref maze 1 1) 0)
  ;; add the escape
  (setf (aref maze (- *maze-width* 1) (- *maze-height* 2)) 0)
  )

(reset-maze)



;; now, from this step, let's release the maze digger
;; the digger will randomly dig through this piece of flat land until no plot to dig
;; how to determine if a plot is digable?
;;    - if a plot has already been dug, i.e. it's value is 0
;;    - if a plot is the outer wall, i.e. (or (= x *maze-width*) (= y *maze-height*) (= x 0) (= y 0))
;; how to determine the next digging direction?
;;    - we randomly pick it
;; how to guarantee that we will dig all the digable plots?
;;    - at the beginning, we release 4 maze digger for four directions, their priority is randomly set
;;    - this will guarantee that all four directions are taken cared of,
;;    ---- then we recursively release 4 maze digger for four directions for every subsequent plot,
;;    ---- their priority is also randomly set
;;    -------- ......
;;    - since we are not using multiple-threads, there will be no conflicts, only some later diggers will find no plot to dig
;; how to guarantee that we don't dig through the wall?
;;    - for one same direction, we dig twice at a time
;;    - if both of them are digable, then we do it,
;;    - otherwise we abandon this direction

(defun dig-maze (x y)
  ;; randomize
  ;; we don't mess with the current plot,
  ;; only we should dig around all four plots near the current plot
  ;; note that the directions are randomized, so random direction will be dug first
  (let ((directions (nshuffle '(1 2 3 4))))
    ;; (format t "directions: ~A~%" directions)
    (loop
      for direction in directions
      for x-direction = (case direction (1 0) (2 0) (3 -1) (4 1))
      for y-direction = (case direction (1 -1) (2 1) (3 0) (4 0))
      for x1 = (+ x x-direction)
      for y1 = (+ y y-direction)
      for x2 = (+ x1 x-direction)
      for y2 = (+ y1 y-direction)
      do
         ;; test if all these two plots are digable
         (when (and
                (> x2 0) (> y2 0) ;; don't dig the outer wall
                (< x2 *maze-width*) (< y2 *maze-height*) ;; don't dig the outer wall
                (= (aref maze x1 y1) 1) ;; must be solid
                (= (aref maze x2 y2) 1)) ;; must be solid
           ;; now dig
           (setf (aref maze x1 y1) 0)
           (setf (aref maze x2 y2) 0)
           ;; and set new diggers for the next plots
           (dig-maze x2 y2)
           )
      ))
  )

;; (trace dig-maze)

;; let's do it
(dig-maze 1 1)

;; now show the maze
(defun show-maze-ascii ()
  (loop
    for x from 0 to (1- *maze-width*)
    do
       (loop
         for y from 0 to (1- *maze-height*)
;;         do (format t "~A,~A" x y)
         if (= (aref maze x y) 1)
           do (princ "[]")
         else do
           (princ "  "))
       (terpri)))
