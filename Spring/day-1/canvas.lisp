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
(setf *calm-window-width* 1024)
(setf *calm-window-height* 768)
(setf *calm-window-title* "Save Pupu")

(u:load-from-app "maze2.lisp")

(defparameter *scale-factor* (/ *calm-window-width* (+ *maze-width* 8)))

(defparameter *player-start-index* (random 20))
(defparameter *player-position* nil)
(defparameter *pupu-start-index* (+ *player-start-index* 5))
(defparameter *pupu-position* nil)

(defparameter *player-color* '(0 0.35 0.59))
(defparameter *pupu-color* '(0.94 0.87 0.47))

(defun on-keydown (key)
  (when (c:keq key :scancode-return)
    (reset-maze)
    (dig-maze 1 1)
    (setf
     *player-start-index* (random 20)
     *player-position* nil
     *pupu-start-index* (+ *player-start-index* 5)
     *pupu-position* nil
     ))
  (let ((next-player-position (copy-list *player-position*)))
    (cond
      ((c:keq key :scancode-left :scancode-h)
       (decf (cadr next-player-position)))
      ((c:keq key :scancode-right :scancode-l)
       (incf (cadr next-player-position)))
      ((c:keq key :scancode-up :scancode-k)
       (decf (car next-player-position)))
      ((c:keq key :scancode-down :scancode-j)
       (incf (car next-player-position))))
    (ignore-errors
     (when (= (aref maze (car next-player-position) (cadr next-player-position)) 0)
       (setf *player-position* next-player-position)))))

(defun draw-maze ()
  (show-maze-ascii)
  (c:set-source-rgb 0 0 0)
  (c:set-line-width 1)
  (let ((empty-plot-index 0))
    (loop
      for x from 0 to (1- *maze-width*)
      do
         (loop
           for y from 0 to (1- *maze-height*)
           ;; cairo has different coordinate system with Array, x and y are reversed
           for cx =  (* (+ y 4) *scale-factor*)
           for cy = (* (+ x 1) *scale-factor*)
           do
              (c:set-source-rgb 0 0 0)
              (c:rectangle cx cy *scale-factor* *scale-factor*)
              (c:stroke-preserve)
           if (= (aref maze x y) 1) ;; wall
             do
                (c:set-source-rgb 0 0 0)
                (c:fill-path)
           else ;; empty plot
           do
              (cond
                ((or
                  ;; this is the initial location for the player
                  (and (null *player-position*) (= empty-plot-index *player-start-index*))
                  ;; this is the current location for the player
                  (and *player-position* (= x (car *player-position*)) (= y (cadr *player-position*)))
                  )
                 (when (null *player-position*)
                   (setf *player-position* (list x y)))
                 (apply #'c:set-source-rgb *player-color*)
                 (c:new-path)
                 (c:arc (+ cx (/ *scale-factor* 2)) (+ cy (/ *scale-factor* 2))  (- (/ *scale-factor* 2) 4) 0 (* 2 pi))
                 (c:fill-path))
                ((or
                  ;; this is the initial location for pupu
                  (and (null *pupu-position*) (= empty-plot-index *pupu-start-index*))
                  ;; this is the current location for pupu
                  (and *pupu-position* (= x (car *pupu-position*)) (= y (cadr *pupu-position*)))
                  )
                 (apply #'c:set-source-rgb *pupu-color*)
                 (c:new-path)
                 (c:arc (+ cx (/ *scale-factor* 2)) (+ cy (/ *scale-factor* 2))  (- (/ *scale-factor* 2) 4) 0 (* 2 pi))
                 (c:fill-path))
                (t (c:set-source-rgb 0 0 0)
                   (c:stroke)))
              (incf empty-plot-index)
           )
      ))
  )

(defun draw-player ()
  (let ((player-plot-index (+ 5 (random 5)))
        (plot-index 0))
    (loop
      for x from 0 to (1- *maze-width*)
      do
         (loop
           for y from 0 to (1- *maze-height*)
           ;; cairo has different coordinate system with Array, x and y are reversed
           for xc = (* (+ y 4) *scale-factor*)
           for yc = (* (+ x 1) *scale-factor*)
           when (= (aref maze x y) 0)
             do
                (incf plot-index)
                (when (= plot-index player-plot-index)
                  ;; (c:save)
                  (c:set-source-rgb 255 0 0)
                  ;; (c:translate xc yc)
                  ;; (c:scale *scale-factor* *scale-factor*)
                  (format t "arc: ~A,~A : ~A,~A~%" xc yc x y)
                  (c:arc (- xc 20) (- yc 20) 20 0 (* 2 pi))
                  ;; (c:arc 0 0 1 0 (* 2 pi))
                  (c:fill-path)
                  ;; (c:restore)
                  (return))
           )
      )))

(defun draw ()
  (c:set-source-rgb 255 255 255)
  (c:paint)
  (draw-maze)
)
