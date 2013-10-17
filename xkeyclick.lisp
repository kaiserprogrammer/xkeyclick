(defpackage :xkeyclick
  (:use :cl :xlib :lisp-unit))

(in-package :xkeyclick)

(defclass octotree ()
  ((xpos :accessor xpos
         :initarg :xpos
         :initform 0)
   (ypos :accessor ypos
         :initarg :ypos
         :initform 0)
   (width :accessor width
          :initarg :width)
   (height :accessor height
           :initarg :height)))

(defun create-octotree (width height)
  (make-instance 'octotree :width width :height height))

(defun selections (octotree)
  (let ((selections (list)))
    (let ((htimes 1)
          (wtimes 8))
      (with-slots (xpos ypos width height) octotree
        (when (< width 30)
          (rotatef htimes wtimes))
        (let ((new-width (truncate (/ width wtimes)))
              (new-height (truncate (/ height htimes))))
          (dotimes (height-times htimes)
            (dotimes (width-times wtimes)
              (push (list (+ xpos (* new-width width-times))
                          (+ ypos (* new-height height-times))
                          new-width
                          new-height)
                    selections))))))
    (nreverse selections)))

(defun select (tree nth)
  (destructuring-bind (xpos ypos width height)
      (nth nth (selections tree))
    (setf (xpos tree) xpos)
    (setf (ypos tree) ypos)
    (setf (width tree) width)
    (setf (height tree) height)))


(defparameter *keys* #(38 39 40 41 44 45 46 47))
(defparameter *keys-asdf* "asdfjkl;")
(defvar *canvas*)
(defvar *gcontext*)

(defun start ()
  (let* ((display (xlib:open-default-display))
         (screen (first (xlib:display-roots display)))
         (root (xlib:screen-root screen))
         (colormap (xlib:screen-default-colormap screen))
         (blue (xlib:alloc-color colormap :blue))
         (*canvas* (xlib:create-window :x 0 :y 0 :width (xlib:screen-width screen) :height (xlib:screen-height screen)
                                       :parent root
                                       :background :none
                                       :event-mask (make-event-mask :button-press)
                                       :class :input-output))
         (*gcontext* (xlib:create-gcontext :drawable *canvas* :foreground blue :subwindow-mode :include-inferiors)))
    (setf (xlib:window-override-redirect *canvas*) :on)
    (xlib:change-property *canvas*
                          :wm_name
                          "Pick two numbers"
                          :string
                          8
                          :transform #'char-code)

    (xlib:grab-keyboard root)
    (xlib:map-window *canvas*)
    (unwind-protect
         (progn (let ((tree (create-octotree (xlib:screen-width screen) (xlib:screen-height screen))))
                  (draw-octotree tree)
                  (xlib:display-force-output display)
                  (handler-case
                      (xlib:process-event display :handler
                                          (lambda (&rest args)
                                            (when (eq (getf args :event-key) :key-press)
                                              (let ((key (getf args :code)))
                                                (case key
                                                  ((nil) t)
                                                  (t (when (find key *keys*)
                                                       (select tree (position key *keys*))
                                                       (with-slots (xpos ypos width height) tree
                                                         (if (and (< width 30)
                                                                  (< height 20))
                                                             (progn
                                                               (unmap-window *canvas*)
                                                               (xtest:fake-motion-event display xpos ypos)
                                                               (xtest:fake-button-event display 1 t)
                                                               (display-finish-output display)
                                                               (xtest:fake-button-event display 1 nil :delay 1)
                                                               (display-finish-output display)
                                                               t)
                                                             (progn
                                                               (unmap-window *canvas*)
                                                               (display-finish-output display)
                                                               (map-window *canvas*)
                                                               (draw-octotree tree)
                                                               (display-force-output display)
                                                               nil))))))))))
                    (t (e) (print e)))))
      (xlib:free-colors colormap (list blue))
      (xlib:free-gcontext *gcontext*)
      (xlib:destroy-window *canvas*)
      (xlib:close-display display))))

(defun draw-window (char x y width height)
  (xlib:draw-rectangle *canvas* *gcontext* x y width height)
  (xlib:draw-glyph *canvas* *gcontext*
                   (+ x (truncate (/ width 2)))
                   (+ y (truncate (/ height 2)))
                   char))

(defun draw-octotree (tree)
  (loop for selection in (selections tree)
     for key across *keys-asdf*
     do (destructuring-bind (x y width height) selection
          (draw-window key x y width height))))
