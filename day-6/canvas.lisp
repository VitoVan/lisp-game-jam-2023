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
#-jscl
(unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (swank:create-server))

;;
;; by default, the screensaver is disabled,
;; if you want to enable screensaver,
;; please uncomment the following line
;;
#-jscl
(setf (uiop:getenv "SDL_VIDEO_ALLOW_SCREENSAVER") "1")

;;
;; setting window properties, for more of this, please check
;;      https://github.com/VitoVan/calm/blob/main/src/config.lisp
;;
#-jscl
(setf *calm-delay* 10)
#+jscl
(setf *calm-fps* 0)
(setf *calm-window-width* 600)
(setf *calm-window-height* 600)
(setf *calm-window-title* "The Maze and Lost Cat")

(defparameter *maze-1*
  (list
   '(
      (1 1 1 1 1 1 1 1 1 1 1 1 1)
      (0 0 1 0 0 0 1 0 0 0 1 0 1)
      (1 0 1 0 1 0 1 1 1 0 1 0 1)
      (1 0 1 0 1 0 0 0 0 0 1 0 1)
      (1 0 1 0 1 1 1 1 1 0 1 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1)
      (1 0 1 1 1 0 1 1 1 0 1 0 1)
      (1 0 0 0 1 0 0 0 1 0 1 0 1)
      (1 1 1 0 1 1 1 0 1 0 1 0 1)
      (1 0 0 0 1 0 0 0 1 0 1 0 1)
      (1 0 1 1 1 0 1 1 1 0 1 0 1)
      (1 0 0 0 0 0 1 0 0 0 1 0 1)
      (1 1 1 1 1 1 1 1 1 1 1 0 1)
      )
   ;; player position
   '(1 0)
   ;; kitty position
   '((5 7) (11 7) (1 7))
   )
  )

(defparameter *maze-2*
  (list
   '(
      (1 1 1 1 1 1 1 1 1 1 1 1 1)
      (0 0 1 0 0 0 0 0 0 0 0 0 1)
      (1 0 1 0 1 1 1 1 1 1 1 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1)
      (1 0 1 0 1 1 1 1 1 0 1 0 1)
      (1 0 1 0 1 0 0 0 1 0 1 0 1)
      (1 0 1 1 1 0 1 0 1 0 1 1 1)
      (1 0 1 0 0 0 1 0 1 0 0 0 1)
      (1 0 1 0 1 1 1 0 1 1 1 0 1)
      (1 0 0 0 1 0 1 0 0 0 0 0 1)
      (1 1 1 0 1 0 1 1 1 1 1 0 1)
      (1 0 0 0 1 0 0 0 0 0 0 0 1)
      (1 1 1 1 1 1 1 1 1 1 1 0 1)
      )
   ;; player position
   '(1 0)
   ;; kitty position
   '((5 3) (3 7) (11 1) (9 5))
   ))


(defparameter *maze-3*
  (list
   '(
      (1 1 1 1 1 1 1 1 1 1 1 1 1)
      (0 0 1 0 0 0 0 0 0 0 0 0 1)
      (1 0 1 1 1 0 1 1 1 1 1 0 1)
      (1 0 0 0 1 0 0 0 0 0 1 0 1)
      (1 1 1 0 1 0 1 1 1 0 1 0 1)
      (1 0 0 0 1 0 0 0 1 0 1 0 1)
      (1 0 1 1 1 1 1 1 1 0 1 0 1)
      (1 0 1 0 0 0 0 0 0 0 1 0 1)
      (1 0 1 0 1 0 1 1 1 1 1 0 1)
      (1 0 1 0 1 0 1 0 0 0 0 0 1)
      (1 0 1 1 1 0 1 0 1 1 1 1 1)
      (1 0 0 0 0 0 1 0 0 0 0 0 1)
      (1 1 1 1 1 1 1 1 1 1 1 0 1)
      )
   ;; player position
   '(1 0)
   ;; kitty position
   '((9 3) (5 7))
   ))

(defparameter *maze-4*
  (list
   '(
       (1 1 1 1 1 1 1 1 1 1 1 1 1)
       (0 0 1 0 0 0 0 0 0 0 0 0 1)
       (1 0 1 1 1 1 1 0 1 1 1 0 1)
       (1 0 0 0 0 0 1 0 1 0 1 0 1)
       (1 1 1 1 1 0 1 0 1 0 1 0 1)
       (1 0 0 0 1 0 0 0 1 0 0 0 1)
       (1 0 1 0 1 1 1 1 1 0 1 1 1)
       (1 0 1 0 0 0 0 0 1 0 1 0 1)
       (1 0 1 1 1 1 1 1 1 0 1 0 1)
       (1 0 1 0 0 0 0 0 1 0 1 0 1)
       (1 0 1 0 1 1 1 0 1 0 1 0 1)
       (1 0 0 0 0 0 1 0 0 0 0 0 1)
       (1 1 1 1 1 1 1 1 1 1 1 0 1)
       )
   ;; player position
   '(1 0)
   ;; kitty position
   '((11 5) (7 7) (3 9))
   ))

(defparameter *maze-data-list* (list *maze-1* *maze-2* *maze-3* *maze-4*))

;; https://rosettacode.org/wiki/Knuth_shuffle#Common_Lisp
#-jscl
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defparameter *maze-index* nil)
(defparameter *maze-data* nil)
(defparameter *maze* nil)
(defun maze-in-bounds-p (maze x y)
  (let ((height (length maze))
        (width (length (first maze))))
    (and (>= x 0) (>= y 0) (< x width) (< y height))
    ))
(defun maze-aref (maze x y)
  (nth y (nth x maze)))
(defun maze-dimensions (maze)
  (list (length maze) (length (first maze))))
(defun maze-position-equalp (p1 p2)
  (and (= (car p1) (car p2)) (= (cadr p1) (cadr p2))))

(defparameter *sight-distance* 0)
(defparameter *player-position* nil)
(defparameter *player-position-previous* nil)
(defparameter *kitty-position* nil)
(defparameter *kitty-collected* nil)

(defparameter *bgm-started* nil)

(defparameter *player-color* '(0 0.35 0.59))
;; (defparameter *kitty-color* '(0.94 0.87 0.47))
(defparameter *kitty-color* '(0.89 0.12 0.17))
(defparameter *player-with-kitty-color* '(0.89 0.12 0.17))
(defparameter *maze-color-solid* '(0.83 0.82 0.84))
;; (defparameter *maze-color-solid* '(0.4 1 0.4))
(defparameter *maze-color-border* '(0.6 0.6 0.6))
(defparameter *win* 0)
(defparameter *win-pause* nil)
(defparameter *max-steps* 512)
(defparameter *steps* 0)
(defparameter *getting-dark* t)
(defparameter *let-go* nil)

(defparameter *last-meow* nil)
(defun meow ()
  (when (> (- (get-universal-time) *last-meow*) (+ 10 (random 42)))
    (setf *last-meow* (get-universal-time))
    (let* ((kitty-x (car *kitty-position*))
           (kitty-y (cadr *kitty-position*))
           (player-x (car *player-position*))
           (player-y (cadr *player-position*))
           (distance (sqrt (+ (expt (abs (- kitty-x player-x)) 2) (expt (abs (- kitty-y player-y)) 2))))
           (volume (round (* 128 (/ (max (- 8 distance) 1) 8)))))
      #-jscl
      (c:volume-music volume)
      ;; (format t "distance: ~A, volume: ~A~%" distance volume)
      #-jscl
      (c:play-music "assets/meow.ogg")
      #+jscl
      (c:play-audio "assets/meow.ogg" :volume (/ volume 128))
      )))

(defparameter *already-purring* nil)
(defun purr ()
  (unless *already-purring*
    #-jscl
    (c:play-music "assets/purr.ogg" :loops -1)
    #+jscl
    (c:play-audio "assets/purr.ogg" :loop-audio-p t)
    (setf *already-purring* t)))

(defparameter *already-meow-with-purring* nil)
(defun meow-with-purr ()
  (unless *already-meow-with-purring*
    #-jscl
    (c:volume-music 128)
    #-jscl
    (c:play-music "assets/meow-with-purr.ogg")
    #+jscl
    (c:play-audio "assets/meow-with-purr.ogg")
    (setf *already-meow-with-purring* t)))

(defparameter *walk-index* 0)
(defun walk ()
  #-jscl
  (c:play-wav (concatenate 'string "assets/walk-" (write-to-string *walk-index*) ".ogg"))
  #+jscl
  (c:play-audio (concatenate 'string "assets/walk-" (write-to-string *walk-index*) ".ogg"))
  (if (< *walk-index* 7)
      (incf *walk-index*)
      (setf *walk-index* 0)))

(defparameter *maze-initialized* nil)

(defun init-maze (&optional (maze-index 0))
  #-jscl
  (c:halt-music)
  #+jscl
  (c:halt-audio "assets/purr.ogg")
  (setf *already-purring* nil
        *already-meow-with-purring* nil
        *last-meow* (get-universal-time))
  #-jscl
  (when (> maze-index 3)
    (nshuffle *maze-data-list*))
  (setf
   *maze-index* (if (> maze-index 3) 0 maze-index)
   *maze-data* (nth *maze-index* *maze-data-list*)
   *maze* (nth 0 *maze-data*)
   *player-position* (nth 1 *maze-data*)
   *kitty-position* (nth (random (length (nth 2 *maze-data*))) (nth 2 *maze-data*))
   *kitty-collected* nil))

(defparameter *last-move* 0)
(defun auto-move ()
  (unless (> (- (c:get-ticks) *last-move*) 160)
    (setf *calm-redraw* t))
  (when (and (not *win-pause*) *player-position-previous* (> (- (c:get-ticks) *last-move*) 160))
    (let* ((x-incf-factor (- (car *player-position*) (car *player-position-previous*)))
           (y-incf-factor (- (cadr *player-position*) (cadr *player-position-previous*)))
           (next-player-position (list (+ (car *player-position*) x-incf-factor) (+ (cadr *player-position*) y-incf-factor)))
           (all-possible-positions
             (list
              (list (+ (car *player-position*) 1) (+ (cadr *player-position*) 0))
              (list (+ (car *player-position*) 0) (+ (cadr *player-position*) 1))
              (list (- (car *player-position*) 1) (- (cadr *player-position*) 0))
              (list (- (car *player-position*) 0) (- (cadr *player-position*) 1)))))
      (when (and
             (maze-in-bounds-p *maze* (car next-player-position) (cadr next-player-position))
             (not (maze-position-equalp next-player-position *player-position*)))
;;        (format t "all-possible: ~A~%" all-possible-positions)
        (when
            (and
            ;; next plot is empty
            (= (maze-aref *maze* (car next-player-position) (cadr next-player-position)) 0)
            ;; no more than 2 possible routes
            (<= (count-if #'(lambda (x) (and (maze-in-bounds-p *maze* (car x) (cadr x)) (= (maze-aref *maze* (car x) (cadr x)) 0))) all-possible-positions) 2)
            )
          (when *kitty-collected*
            (setf *kitty-position* *player-position*))
          (setf *player-position-previous* *player-position*)
          (setf *player-position* next-player-position)
          (when (> *steps* *max-steps*)
            (setf *getting-dark* (not *getting-dark*)))
          (if *getting-dark* (incf *steps*) (decf *steps*))
          (setf *last-move* (c:get-ticks))
          (setf *calm-redraw* t)
          ;; (format t "cur: ~A, nxt: ~A~%" *player-position* next-player-position)
          )
        ))
    (when (and *kitty-collected* (maze-position-equalp *player-position* '(12 11)))
      (purr)
      (incf *win*)
      (setf *win-pause* t))))

(defun on-keydown (key)
  (unless *bgm-started*
    #-jscl
    (c:play-wav "assets/bgm.ogg" :loops -1)
    #+jscl
    (c:play-audio "assets/bgm.ogg" :loop-audio-p t)
    (setf *bgm-started* t))

  (when (c:keq key :scancode-q :scancode-escape)
    (setf *let-go* t))

  (when (and *let-go* (c:keq key :scancode-return :scancode-space :scancode-lalt :scancode-ralt :scancode-lctrl :scancode-rctrl :scancode-delete :scancode-backspace))
    (setf
     *let-go* nil
     *win* 0
     *steps* 0)
    (init-maze 0))

  ;; let go
  (when (and *win-pause* (= *win* 10) (c:keq key :scancode-g))
    (setf *let-go* t)
    #-jscl
    (c:halt-music)
    #+jscl
    (c:halt-audio))

  ;; trap kitty
  (when (and *win-pause* (= *win* 11) (c:keq key :scancode-t))
    (init-maze 0)
    (setf
     *win-pause* nil
     *win* 0))

  (when (and *win-pause* (c:keq key  :scancode-return :scancode-space :scancode-lalt :scancode-ralt :scancode-lctrl :scancode-rctrl :scancode-delete :scancode-backspace))
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
      (when (and
             (maze-in-bounds-p *maze* (car next-player-position) (cadr next-player-position))
             (not (maze-position-equalp next-player-position *player-position*)))
        (when (= (maze-aref *maze* (car next-player-position) (cadr next-player-position)) 0)
          (when *kitty-collected*
            (setf *kitty-position* *player-position*))
          (setf *player-position-previous* *player-position*)
          (setf *player-position* next-player-position)
          (when (or (> *steps* *max-steps*) (< *steps* 0))
            (setf *getting-dark* (not *getting-dark*)))
          (if *getting-dark* (incf *steps*) (decf *steps*))
          (setf *last-move* (c:get-ticks))
          (walk))
        ))

    (when (and *kitty-collected* (maze-position-equalp *player-position* '(12 11)))
      (purr)
      (incf *win*)
      (setf *win-pause* t)))

  ;;(format t "current player position: ~A~%" *player-position*)
  )

(defun on-fingerup (&key x  y dx dy pressure finger-id)
  (declare (ignore x y dx dy pressure finger-id))

  (when *win-pause*
    (init-maze (1+ *maze-index*))
    (setf *win-pause* nil))

  (unless *win-pause*
    (let* ((dimensions (maze-dimensions *maze*))
           (maze-width (car dimensions))
           (scale-factor (/ *calm-window-width* (+ maze-width 2)))
           (m-x (1- (round (/ (- *calm-state-mouse-x* (/ scale-factor 2)) scale-factor))))
           (m-y (1- (round (/ (- *calm-state-mouse-y* (/ scale-factor 2)) scale-factor))))
           (p-x (car *player-position*))
           (p-y (cadr *player-position*))
           (distance-x (- m-x p-x))
           (distance-y (- m-y p-y))
           (abs-x (abs distance-x))
           (abs-y (abs distance-y)))

      (format t "x: ~A, y: ~A~%m-x: ~A, m-y: ~A~%"
              *calm-state-mouse-x* *calm-state-mouse-y*
              m-x m-y)

      (when (maze-in-bounds-p *maze* m-x m-y)
        (if (> abs-x abs-y)
            ;; go horizontally
            (if (> distance-x 0)
                (on-keydown :scancode-right)
                (on-keydown :scancode-left))
            ;; go vertically
            (if (> distance-y 0)
                (on-keydown :scancode-down)
                (on-keydown :scancode-up))
            ))))
  )

(defun draw-kitty ()
  (when (or *kitty-collected* (< (- (get-universal-time) *last-meow*) 1))
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
    (c:restore)))

(defun draw-player ()
  (c:save)
  (c:set-line-width 4)
  (c:set-line-cap :round)
  (when (or (> *sight-distance* 1) *win-pause*)
    ;; body
    (c:arc 16 20 28 0 (* 2 pi))
    (c:stroke))
  ;; eyes
  (c:arc 8 14 3 0 (* 2 pi))
  (c:arc 24 14 3 0 (* 2 pi))
  (c:fill-path)
  ;; smile
  (c:move-to 8 34)
  (if *kitty-collected*
      (c:curve-to 12 38 20 38 24 34)
      (c:line-to 24 34))
  (c:stroke)
  (c:restore))

(defun draw-result ()
  (c:set-line-width 4)
  (c:set-source-rgba 0 0 0 0.7)
  (c:paint)
  (c:save)
  (apply #'c:set-source-rgb *player-with-kitty-color*)
  (c:arc 300 200 50 0 (* 2 pi))
  (c:stroke-preserve)
  (c:set-source-rgb 1 1 1)
  (c:fill-path)
  (apply #'c:set-source-rgb *player-with-kitty-color*)
  (c:new-path)
  ;; (apply #'c:set-source-rgb *player-with-kitty-color*)
  (c:translate 285 176)
  (draw-kitty)
  (c:restore)
  (c:set-source-rgb 1 1 1)
  (c:set-font-size 22)
  (c:select-font-family "Special Elite" :normal :normal)
  (case *win*
    (1
     (c:move-to 90 300)
     (c:show-text "The Owl and the Pussy-cat went to sea")
     )
    (2
     (c:move-to 60 300)
     (c:show-text "In a beautiful pea-green boat,")
     (c:move-to 60 340)
     (c:show-text "They took some honey, and plenty of money,")
     (c:move-to 60 380)
     (c:show-text "Wrapped up in a five-pound note."))
    (3
     (c:move-to 100 300)
     (c:show-text "The Owl looked up to the stars above,")
     (c:move-to 100 340)
     (c:show-text "And sang to a small guitar,")
     (c:move-to 100 380)
     (c:show-text "‘O lovely Pussy! O Pussy, my love,")
     (c:move-to 100 420)
     (c:show-text "What a beautiful Pussy you are,")
     (c:move-to 100 460)
     (c:show-text "You are,")
     (c:move-to 100 500)
     (c:show-text "You are!")
     (c:move-to 100 540)
     (c:show-text "What a beautiful Pussy you are!’"))
    (4
     (c:move-to 44 300)
     (c:show-text "Pussy said to the Owl, ‘You elegant fowl!")
     (c:move-to 44 340)
     (c:show-text "How charmingly sweet you sing!")
     (c:move-to 44 380)
     (c:show-text "O let us be married! too long we have tarried:")
     (c:move-to 44 420)
     (c:show-text "But what shall we do for a ring?’"))
    (5
     (c:move-to 94 300)
     (c:show-text "They sailed away, for a year and a day,")
     (c:move-to 94 340)
     (c:show-text "To the land where the Bong-Tree grows")
     (c:move-to 94 380)
     (c:show-text "And there in a wood a Piggy-wig stood")
     (c:move-to 94 420)
     (c:show-text "With a ring at the end of his nose,")
     (c:move-to 94 460)
     (c:show-text "His nose,")
     (c:move-to 94 500)
     (c:show-text "His nose,")
     (c:move-to 94 540)
     (c:show-text "With a ring at the end of his nose."))
    (6
     (c:move-to 10 300)
     (c:show-text "‘Dear Pig, are you willing to sell for one shilling")
     (c:move-to 10 340)
     (c:show-text "Your ring?’ Said the Piggy, ‘I will.’"))
    (7
     (c:move-to 34 300)
     (c:show-text "So they took it away, and were married next day")
     (c:move-to 34 340)
     (c:show-text "By the Turkey who lives on the hill."))
    (8
     (c:move-to 64 300)
     (c:show-text "They dinèd on mince, and slices of quince,")
     (c:move-to 64 340)
     (c:show-text "Which they ate with a runcible spoon;"))
    (9
     (c:move-to 64 300)
     (c:show-text "And hand in hand, on the edge of the sand,")
     (c:move-to 64 340)
     (c:show-text "They danced by the light of the moon,")
     (c:move-to 64 380)
     (c:show-text "The moon,")
     (c:move-to 64 420)
     (c:show-text "The moon,")
     (c:move-to 64 460)
     (c:show-text "They danced by the light of the moon."))
    (10
     (c:move-to 120 300)
     (c:show-text "Are you holding the cat,")
     (c:move-to 120 340)
     (c:show-text "That you just met?")
     (c:move-to 120 380)
     (c:show-text "Are you going to let it ") (apply #'c:set-source-rgb *kitty-color*) (c:show-text "g") (c:set-source-rgb 1 1 1) (c:show-text "o,")
     (c:move-to 120 420)
     (c:show-text "Go to its love, the elegant Owl?"))
    (11
     (c:move-to 70 300)
     (c:show-text "Or you feel lonely,")
     (c:move-to 70 340)
     (c:show-text "In this dark cold maze, you want a kitty,")
     (c:move-to 70 380)
     (c:show-text "To be ")
     (apply #'c:set-source-rgb *kitty-color*) (c:show-text "t") (c:set-source-rgb 1 1 1)
     (c:show-text "rapped with you, endlessly?")
     )
    (t
     (case (mod *win* 4)
       (0
        (c:move-to 16 300)
        (c:show-text "There was a Young Lady whose bonnet")
        (c:move-to 16 340)
        (c:show-text "Came untied when birds sate upon it;")
        (c:move-to 16 380)
        (c:show-text "But she said, ‘I don’t care! all the birds in the air")
        (c:move-to 16 420)
        (c:show-text "Are welcome to sit on my bonnet.’"))
       (1
        (c:move-to 42 300)
        (c:show-text "There was an Old Man on a hill,")
        (c:move-to 42 340)
        (c:show-text "Who seldom, if ever, stood still;")
        (c:move-to 42 380)
        (c:show-text "He ran up and down in his Grandmothers gown,")
        (c:move-to 42 420)
        (c:show-text "Which adorned that Old Man on a hill."))
       (2
        (c:move-to 60 300)
        (c:show-text "There was a Young Lady of Poole,")
        (c:move-to 60 340)
        (c:show-text "Whose soup was excessively cool;")
        (c:move-to 60 380)
        (c:show-text "So she put it to boil by the aid of some oil,")
        (c:move-to 60 420)
        (c:show-text "That ingenious Young Lady of Poole."))
       (3
        (c:move-to 80 300)
        (c:show-text "There was an Old Man with a beard,")
        (c:move-to 80 340)
        (c:show-text "Who said, ‘It is just as I feared! —")
        (c:move-to 80 380)
        (c:show-text "Two Owls and a Hen, four Larks and Wren,")
        (c:move-to 80 420)
        (c:show-text "Have all built their nests in my beard.’")))
     )
    )
  (when (>= *win* 9)
    ;; (c:move-to 160 200)
    ;; (c:show-text "shall the kitty be sailed away?")
    ;; (c:move-to 110 200)
    ;; (c:show-text "do we deserve the dark maze, lonely?")
    )
  )

(defun draw-maze ()
  (c:set-line-width 1)
  (let* ((dimensions (maze-dimensions *maze*))
         (maze-width (car dimensions))
         (maze-height (cadr dimensions))
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
           if (= (maze-aref *maze* x y) 1) ;; wall
             do
                (apply #'c:set-source-rgb *maze-color-solid*)
                (c:fill-preserve)
                (apply #'c:set-source-rgb *maze-color-border*)
                (c:stroke)
           else
             ;; empty plot
             do
                (c:new-path)
                (apply #'c:set-source-rgb *maze-color-border*)
                (c:stroke)
           )
      ))
  )

(defun draw-night ()
  (let* ((dimensions (maze-dimensions *maze*))
         (maze-width (car dimensions))
         (maze-height (cadr dimensions))
         (scale-factor (/ *calm-window-width* (+ maze-width 2))))
    (setf *sight-distance* (max (* 24 (/ (- *max-steps* *steps*) *max-steps*)) 1))
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
                                 (if (= 0 *sight-distance*)
                                     1
                                     (max (/ x-distance *sight-distance*) (/ y-distance *sight-distance*))))
              (c:rectangle cx cy scale-factor scale-factor)
              (c:fill-path)
           )
      ))
  )

(defun draw-player-and-kitty ()
  (let* ((dimensions (maze-dimensions *maze*))
         (maze-width (car dimensions))
         (maze-height (cadr dimensions))
         (scale-factor (/ *calm-window-width* (+ maze-width 2))))
    (loop
      for x from 0 to (1- maze-width)
      do
         (loop
           for y from 0 to (1- maze-height)
           for cx =  (* (+ x 1) scale-factor)
           for cy = (* (+ y 1) scale-factor)
           do
              (cond
                ;; player with kitty
                ((and ;; Kitty and the player overlapped
                  *player-position*
                  *kitty-position*
                  (maze-position-equalp (list x y) *player-position*)
                  (maze-position-equalp *kitty-position* *player-position*))
                 (apply #'c:set-source-rgb *player-with-kitty-color*)
                 (c:save)
                 (c:translate (+ cx 13) (+ cy 10))
                 (c:scale 0.5 0.5)
                 (draw-kitty)
                 (c:restore)
                 (setf *kitty-collected* t))
                ;; player
                ((and *player-position* (maze-position-equalp (list x y) *player-position*))
                 (apply #'c:set-source-rgb *player-color*)
                 (c:save)
                 (c:translate (+ cx 13) (+ cy 10))
                 (c:scale 0.5 0.5)
                 (draw-player)
                 (c:restore)
                 )
                ;; kitty
                ((and *kitty-position* (maze-position-equalp (list x y) *kitty-position*))
                 (apply #'c:set-source-rgb *kitty-color*)
                 (c:save)
                 (c:translate (+ cx 13) (+ cy 10))
                 (c:scale 0.5 0.5)
                 (draw-kitty)
                 (c:restore)
                 )
                (t (apply #'c:set-source-rgb *maze-color-border*)
                   (c:stroke)))
           )
      ))
  )

(defun draw-forever ()
  (unless *maze-initialized*
    (init-maze)
    (setf *maze-initialized* t))
  (c:set-source-rgb 1 1 1)
  (c:paint)
  (when *let-go*
    (setf *calm-redraw* nil)
    (c:set-source-rgb 1 1 1)
    (c:select-font-family "Special Elite" :normal :normal)
    (apply #'c:set-source-rgb *player-color*)
    (c:set-font-size 40)
    (c:move-to 80 70)
    (c:show-text "The Maze and Lost Cat")
    (c:set-font-size 22)
    (c:set-source-rgb 0 0 0)
    (c:move-to 100 140)
    (c:show-text "The cat has gone, gone for its fowl.")
    (c:move-to 100 200)
    (c:show-text "Text")
    (c:move-to 200 200)
    (c:show-text "by Edward Lear")
    (c:move-to 100 240)
    (c:show-text "Sound")
    (c:move-to 200 240)
    (c:show-text "by eZZin")
    (c:move-to 200 280)
    (c:show-text "     Mafon2")
    (c:move-to 200 320)
    (c:show-text "     skymary")
    (c:move-to 200 360)
    (c:show-text "     xoiziox")
    (c:move-to 200 400)
    (c:show-text "on freesound.org")
    (c:move-to 100 440)
    (c:show-text "Font")
    (c:move-to 200 440)
    (c:show-text "by Astigmatic (Special Elite)")
    (c:move-to 100 480)
    (c:show-text "Code")
    (c:move-to 200 480)
    (c:show-text "by Vito Van"))
  (unless *let-go*
    (draw-maze)

    (unless *win-pause* (draw-night))
    (draw-player-and-kitty)
    (when *win-pause* (draw-result))
    (setf *calm-redraw* nil)
    (auto-move)
    (unless
        (or *win-pause* *kitty-collected*)
      (meow)
      (setf *calm-redraw* t))
    (when
        (maze-position-equalp *player-position* *kitty-position*)
      (meow-with-purr)
      (setf *calm-redraw* t))
    )
  )
