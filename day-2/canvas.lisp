(in-package #:calm)

;;
;; CALM version check
;; version check won't work on JSCL, since the lack of ASDF
;;
#-jscl
(let ((required-version "0.1.2")
      (calm-version (slot-value (asdf:find-system 'calm) 'asdf:version)))
  (when (uiop:version< calm-version required-version)
    (format t
            "Sorry, this is built on CALM ~A, older version (current: ~A) of CALM won't work.~%"
            required-version calm-version)
    (uiop:quit)))


;;
;; the swank server is for debugging, for usage please check
;; Emacs:
;;        https://slime.common-lisp.dev/
;; Visual Studio Code
;;        https://lispcookbook.github.io/cl-cookbook/vscode-alive.html
;;
;; uncomment the following line to enable SWANK Server
(unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (swank:create-server))

;;
;; by default, the screensaver is disabled,
;; if you want to enable screensaver,
;; please uncomment the following line
;;
(setf (uiop:getenv "SDL_VIDEO_ALLOW_SCREENSAVER") "1")

;;
;; setting window properties, for more of this, please check
;;      https://github.com/VitoVan/calm/blob/main/src/config.lisp
;;
(setf *calm-window-width* 600)
(setf *calm-window-height* 600)
(setf *calm-window-title* "Save Kitties")

(u:load-from-app "maze-data.lisp")

;; https://rosettacode.org/wiki/Knuth_shuffle#Common_Lisp
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defparameter *maze-index* nil)
(defparameter *maze-data* nil)
(defparameter *maze* nil)
(defparameter *player-position* nil)
(defparameter *kitty-position* nil)
(defparameter *kitty-collected* nil)

(defparameter *player-color* '(0 0.35 0.59))
(defparameter *kitty-color* '(0.94 0.87 0.47))
(defparameter *player-with-kitty-color* '(0.89 0.12 0.17))
(defparameter *maze-color-solid* '(0.83 0.82 0.84))
(defparameter *maze-color-border* '(0.6 0.6 0.6))
(defparameter *win* 0)
(defparameter *win-pause* nil)
(defparameter *steps* 0)
(defparameter *game-over* nil)

(defun init-maze (&optional (maze-index 0))
  (when (> maze-index 3)
    (nshuffle *maze-data-list*))
  (setf
   *maze-index* (if (> maze-index 3) 0 maze-index)
   *maze-data* (nth *maze-index* *maze-data-list*)
   *maze* (nth 0 *maze-data*)
   *player-position* (nth 1 *maze-data*)
   *kitty-position* (nth 2 *maze-data*)
   *kitty-collected* nil))

(init-maze)

(defun on-keydown (key)
  (when (c:keq key :scancode-q :scancode-escape)
    (setf *game-over* t))

  (when (and *game-over* (c:keq key :scancode-return))
    (setf
     *game-over* nil
     *win* 0
     *steps* 0)
    (init-maze 0))

  (when (and *win-pause* (c:keq key :scancode-return))
    (init-maze (1+ *maze-index*))
    (setf *win-pause* nil))

  (unless *win-pause*
    (let ((next-player-position (copy-list *player-position*)))
      (cond
        ((c:keq key :scancode-left :scancode-h)
         (decf (car next-player-position)))
        ((c:keq key :scancode-right :scancode-l)
         (incf (car next-player-position)))
        ((c:keq key :scancode-up :scancode-k)
         (decf (cadr next-player-position)))
        ((c:keq key :scancode-down :scancode-j)
         (incf (cadr next-player-position))))
      (ignore-errors
       (when (= (aref *maze* (car next-player-position) (cadr next-player-position)) 0)
         (setf *player-position* next-player-position)
         (incf *steps*))))

    (when (equalp *player-position* '(12 11))
      (incf *win*)
      (setf *win-pause* t)))

  (format t "current player position: ~A~%" *player-position*))

(defun draw-kitty ()
  (c:save)
  (c:set-line-width 4)
  (c:set-line-cap :round)
  ;; body
  (c:move-to 0 0)
  (c:curve-to 10 10 20 10 30 0)
  (c:curve-to 60 50 30 45 30 45)
  (c:line-to 0 45)
  (c:curve-to -10 45 -20 35 0 0)
  (c:stroke)
  ;; eyes
  (c:arc 8 20 3 0 (* 2 pi))
  (c:arc 24 20 3 0 (* 2 pi))
  (c:fill-path)
  ;; smile
  (when *kitty-collected*
    (c:move-to 8 34)
    (c:curve-to 14 38 20 38 24 34)
    (c:stroke))
  ;; tail
  (c:move-to 0 45)
  (c:curve-to -10 45 -15 45 -20 40)
  (c:stroke)
  (c:restore))

(defun draw-player ()
  (c:save)
  (c:set-line-width 4)
  (c:set-line-cap :round)
  ;; body
  (c:arc 16 20 28 0 (* 2 pi))
  (c:stroke)
  ;; eyes
  (c:arc 8 14 3 0 (* 2 pi))
  (c:arc 24 14 3 0 (* 2 pi))
  (c:fill-path)
  ;; nose
  (c:move-to 8 34)
  (c:line-to 24 34)
  (c:stroke)
  (c:restore))

(defun draw-result ()
  (c:set-line-width 4)
  (apply #'c:set-source-rgba (append *player-color* (list (if *game-over* 1 0.5))))
  (c:paint)
  (apply #'c:set-source-rgb *player-with-kitty-color*)
  (c:rrectangle 200 380 200 30 :radius 15)
  (c:stroke-preserve)
  (c:set-source-rgb 1 1 1)
  (c:fill-path)
  (apply #'c:set-source-rgb *player-color*)
  (c:select-font-face "Open Sans" :normal :normal)
  (c:set-font-size 24)
  (c:move-to 274 404)
  (c:show-text (str:pad-left 4 (write-to-string *win*) :pad-char #\0))
  (c:fill-path)
  (c:save)
  (apply #'c:set-source-rgb *player-with-kitty-color*)
  (c:arc 300 300 50 0 (* 2 pi))
  (c:stroke-preserve)
  (c:set-source-rgb 1 1 1)
  (c:fill-path)
  (apply #'c:set-source-rgb *player-with-kitty-color*)
  (c:new-path)
  ;; (apply #'c:set-source-rgb *player-with-kitty-color*)
  (c:translate 285 280)
  (draw-kitty)
  (c:restore)
  )

(defun draw-maze ()
  (c:set-line-width 1)
  (let* ((empty-plot-index 0)
         (maze-dimensions (array-dimensions *maze*))
         (maze-width (car maze-dimensions))
         (maze-height (cadr maze-dimensions))
         (scale-factor (/ *calm-window-width* (+ maze-width 2))))
    (loop
      for x from 0 to (1- maze-width)
      do
         (loop
           for y from 0 to (1- maze-height)
           for cx =  (* (+ x 1) scale-factor)
           for cy = (* (+ y 1) scale-factor)
           do
              (apply #'c:set-source-rgb *maze-color-border*)
              (c:rectangle cx cy scale-factor scale-factor)
           if (= (aref *maze* x y) 1) ;; wall
             do
                (apply #'c:set-source-rgb *maze-color-solid*)
                (c:fill-preserve)
                (apply #'c:set-source-rgb *maze-color-border*)
                (c:stroke)
           else
             ;; empty plot
             do
                (c:new-path)
                (cond
                  ;; player with kitty
                  ((and ;; Kitty and the player overlapped
                    *player-position*
                    *kitty-position*
                    (= x (car *player-position*)) (= y (cadr *player-position*))
                    (= x (car *kitty-position*)) (= y (cadr *kitty-position*)))
                   (apply #'c:set-source-rgb *player-with-kitty-color*)
                   ;; (c:arc (+ cx (/ scale-factor 2)) (+ cy (/ scale-factor 2))  (- (/ scale-factor 2) 4) 0 (* 2 pi))
                   ;; (c:fill-path)
                   (c:save)
                   (c:translate (+ cx 13) (+ cy 10))
                   (c:scale 0.5 0.5)
                   (draw-kitty)
                   (c:restore)
                   (setf *kitty-position* nil
                         *kitty-collected* t))
                  ;; player
                  ((and *player-position* (= x (car *player-position*)) (= y (cadr *player-position*)))
                   (apply #'c:set-source-rgb (if *kitty-collected* *player-with-kitty-color* *player-color*))

                   (if *kitty-collected*
                       (progn
                         (c:save)
                         (c:translate (+ cx 13) (+ cy 10))
                         (c:scale 0.5 0.5)
                         (draw-kitty)
                         (c:restore))
                       (progn
                         ;; (c:arc (+ cx (/ scale-factor 2)) (+ cy (/ scale-factor 2))  (- (/ scale-factor 2) 4) 0 (* 2 pi))
                         ;; (c:fill-path)
                         (c:save)
                         (c:translate (+ cx 13) (+ cy 10))
                         (c:scale 0.5 0.5)
                         (draw-player)
                         (c:restore)
                         ))
                   )
                  ;; kitty
                  ((and (not *kitty-collected*) *kitty-position* (= x (car *kitty-position*)) (= y (cadr *kitty-position*)))
                   (apply #'c:set-source-rgb *kitty-color*)
                   ;; (c:arc (+ cx (/ scale-factor 2)) (+ cy (/ scale-factor 2))  (- (/ scale-factor 2) 4) 0 (* 2 pi))
                   ;; (c:fill-path)
                   (c:save)
                   (c:translate (+ cx 13) (+ cy 10))
                   (c:scale 0.5 0.5)
                   (draw-kitty)
                   (c:restore)
                   )
                  (t (apply #'c:set-source-rgb *maze-color-border*)
                     (c:stroke)))
                (incf empty-plot-index)
           )
      ))
  )

(defun draw-night ()
  (let* ((maze-dimensions (array-dimensions *maze*))
         (maze-width (car maze-dimensions))
         (maze-height (cadr maze-dimensions))
         (scale-factor (/ *calm-window-width* (+ maze-width 2)))
         (light-distance (round (max (* 20 (/ (- 1000 *steps*) 1000)) 0))))
    (loop
      for x from 0 to (1- maze-width)
      do
         (loop
           for y from 0 to (1- maze-height)
           for cx =  (* (+ x 1) scale-factor)
           for cy = (* (+ y 1) scale-factor)
           for x-distance = (max (abs (- x (car *player-position*))) 1)
           for y-distance = (max (abs (- y (cadr *player-position*))) 1)
           do
              (c:set-source-rgba 0 0 0
                                 (if (= 0 light-distance)
                                     1
                                     (max (/ x-distance light-distance) (/ y-distance light-distance))))
              (c:rectangle cx cy scale-factor scale-factor)
              (c:fill-path)
           )
      ))
  )

(defun draw ()
  (c:set-source-rgb 1 1 1)
  (c:paint)
  (draw-maze)
  (unless *win-pause*
    (draw-night))
  (when (or *win-pause* *game-over*)
    (draw-result))
  )
