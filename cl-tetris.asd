
(asdf:defsystem #:cl-tetris
		:serial t
		:components ((:file "tetris"))
		:depends-on (:lispbuilder-sdl
			     :cl-opengl))