
(defpackage :tetris
  (:use :cl))

(in-package #:tetris)


(defvar *default-map-size* '(15 30))
(defvar *default-interval* 2)

;; Import from cletris project
(defparameter +blocks+ 
  '(((#(0  -1) #(0  0) #(0  1) #(1  1)) (255 255 0)) ;; J
    ((#(0  -1) #(0  0) #(0  1) #(-1  1)) (255 0 255))  ;; L
    ((#(0  -1) #(0  0) #(-1  0) #(1  0)) (178 109 57)) ;; T
    ((#(0  -2) #(0  -1) #(0  0) #(0  1)) (255 0 0))	   ;; I
    ((#(0  -1) #(1  -1) #(1  0) #(0  0)) (0 0 255))	   ;; O
    ((#(-1  -1) #(-1  0) #(0  0) #(0  1)) (0 255 0))   ;; S
    ((#(1  -1) #(1  0) #(0  0) #(0  1)) (0 255 255)))) ;; Z  


(defclass moving-block ()
  ((blocks 
    :initform (get-random-shape)
    :accessor blocks)
   (block-position
    :initarg :block-position
    :initform #(0 0)
    :accessor block-position)))

(defclass tetris-game ()
  ((block-map
    :initform (create-map)
    :accessor block-map)
   (moving-block
    :initform (create-mb)
    :accessor moving-block)))



(defun create-map () (make-array (reverse *default-map-size*) :initial-element nil))

(defun create-mb () 
  (let ((mb (make-instance 'moving-block)))
    (setf (block-position mb) 
	  (vector (floor (/ (car *default-map-size*) 2)) (cadr *default-map-size*)))
    mb))

(defun valid-map-p (x y)
  (and (>= x 0)
       (>= y 0)
       (< x (first *default-map-size*))
       (< y (second *default-map-size*))))

(defun valid-point-p (map x y)
  (and (>= x 0)
       (>= y 0)
       (< x (first *default-map-size*))
       ; Do not check upper bound
       (or (>= y (second *default-map-size*)) 
	   (null (aref map y x)))))

(defun valid-points-p (map points)
  (every #'(lambda (p) (valid-point-p map (elt p 0) (elt p 1))) points))

(defun valid-line-num-p (line-num)
  (and (>= line-num 0)
       (< line-num (cadr *default-map-size*))))

(defun check-line (map line-num)
  (loop for i below (first *default-map-size*) do
       (if (valid-point-p map i line-num) (return-from check-line nil)))
  line-num)

(defun check-lines (map)
  (loop for i below (second *default-map-size*) when (check-line map i) collect i))

(defun remove-line (map line-num)
  (let ((h (cadr *default-map-size*))
	(w (car *default-map-size*)))
    (if (valid-line-num-p line-num)
	(loop for y from line-num below h do
	     (loop for x below w do
		  (let ((new-val (if (valid-line-num-p (1+ y))
				     (aref map (1+ y) x)
				     nil)))
		    (setf (aref map y x) new-val)))))))

(defun remove-lines (map)
  (let ((lines (check-lines map)))
    (loop for i in lines do
	 (remove-line map i))))


(defun get-random-shape ()
  ;; return random shape
  (let* ((l (length +blocks+))
	 (i (random l)))
    (nth i +blocks+)))




(defmacro points+ (&rest points)
  `(map 'vector #'+ ,@points))

(defmacro points-offset (point-list &rest offsets)
  `(map 'list #'(lambda (p) (map 'vector #'+ p ,@offsets)) ,point-list))


(defun get-block-position (mb &optional (offset #(0 0)))
  (with-slots (blocks block-position) mb
    (points-offset (car blocks) block-position offset)))

(defun get-block-color (mb)
  (cadr (blocks mb)))




(defun collision-check (map mb &optional (offset #(0 0)))
  (let ((positions (get-block-position mb offset)))
    (loop for p in positions do 
	 (unless (valid-point-p map (elt p 0) (elt p 1))
	   (return-from collision-check T)))))



(defun mark (map position value)
  (let ((x (elt position 0))
	(y (elt position 1)))
    (when (valid-map-p x y) 
      (setf (aref map y x) value))))
    
(defun mark-moving-block (map mb)
  (let ((positions (get-block-position mb))
	(color (get-block-color mb)))
    (loop for p in positions do
	 (mark map p color))))
    

(defun move-moving-block (map mb offset)
  (if (valid-points-p map (get-block-position mb offset)) 
      (with-slots (block-position) mb 
	(setf block-position (points+ offset block-position)))))

(defun rotate-points (points)
  (mapcar #'(lambda (p) (let ((x (- (elt p 1))) (y (elt p 0))) (vector x y))) 
	  points))
       

(defun rotate-moving-block (map mb)
  "Rotate the moving block."
  (let* ((points (car (blocks mb)))
	 (rotated-points (rotate-points points))
	 (rotated-position (points-offset rotated-points (block-position mb))))
    (when (valid-points-p map rotated-position)
	(setf (blocks mb) (list rotated-points (cadr (blocks mb)))))))

(defun collision-offset (map mb)
  (loop for i to (cadr *default-map-size*) do
       (when (collision-check map mb (vector 0 (- i)))
	 (return-from collision-offset (vector 0 (1+ (- i)))))))

(defun fast-put (map mb)
  (let ((offset (collision-offset map mb)))
    (move-moving-block map mb offset)))

;;; High level functions.




(defvar *game* nil)

(defun game-start ()
  (setf *game* (make-instance 'tetris-game))
  (setf (moving-block *game*) (create-mb)))

(defun game-over ()
  (game-start))
	     
(defun each-tick ()
  (let ((game *game*))
    (with-slots ((map block-map) (mb moving-block)) game
      (if (collision-check map mb #(0 -1))
	
	  ;; Collision.
	  (progn (mark-moving-block map mb)
		 (remove-lines map)
		 (setf mb (create-mb))
		 (when (collision-check map mb) (game-over)))

	  ;; No collision. keep going.
	  (progn (move-moving-block map mb #(0 -1)))))))
  

(defun game-key-handler (game key)
  (with-slots ((map block-map) (mb moving-block)) game
    (cond ((eq key 'UP) (rotate-moving-block map mb))
	  ((eq key 'LEFT) (move-moving-block map mb #(-1 0)))
	  ((eq key 'RIGHT) (move-moving-block map mb #(1 0)))
	  ((eq key 'DOWN) (move-moving-block map mb #(0 -1)))
	  ((eq key 'SPACE) (fast-put map mb) (each-tick))
	  ((eq key 'ESCAPE) (sdl:push-quit-event)))))

;;; Drawing functions.

(defparameter *window-size* '(320 480))
(defparameter *game-frame-margin* 10)
(defparameter *screen-rect* (append '(0 0) *window-size*))
(defparameter *out-rect* *screen-rect*)



(defun find-min-block-size (max-w max-h map-w map-h)
  (floor (min (/ max-w map-w) (/ max-h map-h))))

(defun center-position (out-size in-size)
  (mapcar #'(lambda (s1 s2) (/ (- s1 s2) 2)) out-size in-size))

(defun find-block-size (out-rect)
  (find-min-block-size (nth 2 out-rect)
		       (nth 3 out-rect)
		       (car *default-map-size*)
		       (cadr *default-map-size*)))

(defparameter *block-size* (find-block-size *out-rect*))

(defun find-game-frame (out-rect)
  (let* ((s (mapcar #'* *default-map-size*
				  (list *block-size* *block-size*)))
	 (p (center-position (cddr out-rect) s)))
    (append p s)))
   
(defun rect-margin (rect margin)
  (list (+ (nth 0 rect) margin)
	(+ (nth 1 rect) margin)
	(- (nth 2 rect) (* 2 margin))
	(- (nth 3 rect) (* 2 margin))))

(defun edges (rect)
  (list (nth 0 rect)
	(nth 1 rect)
	(+ (nth 0 rect) (nth 2 rect))
	(+ (nth 1 rect) (nth 3 rect))))


(defparameter *game-frame-rect* (find-game-frame *out-rect*))
(defparameter *game-frame-edges* (edges *game-frame-rect*))


(defmacro sdl-point (point)
  `(sdl:point :x (nth 0 ,point)
	      :y (- (second *window-size*) (nth 1 ,point))))

(defmacro sdl-rect (rect)
  `(sdl:rectangle :x (nth 0 ,rect)
		  :y (- (second *window-size*) (nth 1 ,rect) (nth 3 ,rect))
		  :w (nth 2 ,rect)
		  :h (nth 3 ,rect)))

(defmacro sdl-color (c)
  `(sdl:color :r (nth 0 ,c)
	      :g (nth 1 ,c)
	      :b (nth 2 ,c)))


(defun b-rect (x y)
  (list (+ (first *game-frame-rect*) (* x *block-size*))
	(+ (second *game-frame-rect*) (* y *block-size*))
	*block-size*
	*block-size*))


(defun shrink-rect (rect amount)
  (append (mapcar #'(lambda (x) (+ x amount)) (subseq rect 0 2))
	  (mapcar #'(lambda (x) (- x (* 2 amount))) (subseq rect 2 4))))
	
(defun rect-points (rect)
  (destructuring-bind (x y w h) rect
    (list (list x y)
	  (list (+ x w) y)
	  (list (+ x w) (+ y h))
	  (list x (+ y h)))))

(defun right-rotate (list)
  (append (last list) (butlast list)))

(defun circular-lines-from-points (points)
  (mapcar #'(lambda (p1 p2) (list p1 p2)) points (right-rotate points)))
	      

(defun draw-background ()
  (sdl:draw-box (sdl-rect (find-game-frame *out-rect*))
		:color sdl:*white*))


(defun draw-block (x y color)
  (let* ((rect (b-rect x y))
	 (lines (circular-lines-from-points (rect-points rect))))
    (sdl:draw-box (sdl-rect rect) :color (sdl-color color))
    (loop for l in lines do
	 (let ((p1 (sdl-point (first l)))
	       (p2 (sdl-point (second l))))
	   (sdl:draw-line p1 p2 :color sdl:*black*)))))


(defun draw-map (map)
  (loop for y below (cadr *default-map-size*) do
       (loop for x below (car *default-map-size*) do
	    (let ((v (aref map y x)))
	      (when v (draw-block x y v))))))


(defun draw-moving-block (b)
  (let ((ps (get-block-position b))
	(c (get-block-color b)))
    (loop for p in ps do (draw-block (elt p 0) (elt p 1) c))))



(defun draw-end-game ())  



;;; SDL functions.

(defparameter *default-asset-path*
  (make-pathname
   :host (pathname-host #.(or *compile-file-truename*
                              *load-truename*))
   :directory (pathname-directory #.(or *compile-file-truename*
                                        *load-truename*))))

(defparameter *sheet-image* nil)
(defvar *sheet-cell* 0)

(defun get-cells (w h)
  (loop for x below w append
       (loop for y below h collect
	    (list (* x 100) (* y 50) 100 50))))

(defun init-resource ()
  #+nil(setf *sheet-image* 
	(sdl:load-image 
	 (merge-pathnames "obj-sheet1.png" *default-asset-path*)
	 :alpha 255))
  #+nil(setf (sdl:cells *sheet-image*) (get-cells 10 5)))




(defun draw () 
  (sdl:clear-display sdl:*black*)
  (draw-background)
  (if (null *game*) 
      ;; End of Game
      (progn (draw-end-game))

      ;; Drawing Game
      (progn (draw-map (block-map *game*))
	     (draw-moving-block (moving-block *game*))))
;  (sdl:draw-surface-at *sheet-image* #(0 0) :cell *sheet-cell*))
)
  

  

(defun mouse-handler (button) button)

(defun intern-keyword-symbol (name)
  (intern name 'keyword))

(defun sdl-key-symbol (sym)
  (intern-keyword-symbol (concatenate 'string "SDL-KEY-" (symbol-name sym))))


(defmacro key-map-keywords (input &rest keys)
  `(cond 
     ,@(loop for k in keys collect
	    `((sdl:key= ,input (sdl-key-symbol ,k)) (game-key-handler *game* ,k)))))
	

(defun key-handler (key)
  (key-map-keywords key 'UP 'RIGHT 'LEFT 'DOWN 'SPACE 'ESCAPE))



(defun repeat-call (func duration &rest arg)
  (let ((elapsed 0.0))
    #'(lambda (dt)
	(setf elapsed (+ elapsed dt))
	(when (>= elapsed duration)
	  (setf elapsed 0.0)
	  (apply func arg)))))


(defun init-opengl ()
  (gl:clear-color 0 0 0 0)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))

(defun draw-opengl ()
  (gl:clear :color-buffer-bit)
  (gl:color 1 1 1)
  (gl:with-primitive :polygon
    (gl:vertex 0.25 0.25 0)
    (gl:vertex 0.75 0.25 0)
    (gl:vertex 0.75 0.75 0)
    (gl:vertex 0.25 0.75 0))
  (gl:flush))
	  
(defun main () 
  "Entry point"
  (sdl:with-init ()
    (sdl:window (car *window-size*) 
		(cadr *window-size*) 
		:title-caption "Tetris Game In CL"
		;:opengl t
		:opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (sdl:update-display)

    ;; Enable key repeat
    (sdl:enable-key-repeat nil nil)

    (init-resource)
    ;(init-opengl)

    (game-start)
    
    (let ((tick (repeat-call #'each-tick 1.0)))

      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:key-down-event (:key key) (key-handler key))
	(:mouse-button-down-event (:button button) (mouse-handler button))
	(:idle ()
	      
	       (funcall tick (sdl:dt))
	       (draw)
	       ;(draw-opengl)
	       (sdl:update-display)
	       
	       )))))


