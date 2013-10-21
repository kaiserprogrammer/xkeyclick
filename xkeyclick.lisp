(defpackage :xkeyclick
  (:use :cl :xlib :lisp-unit))

(in-package :xkeyclick)

(defvar *keys* #(38 39 40 41 44 45 46 47))
(defvar *keys-asdf*)
(defvar *canvas*)
(defvar *gcontext*)

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
        (when (< width height)
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


(defun start (&rest args)
  (declare (ignore args))
  (let* ((display (xlib:open-default-display))
         (screen (first (xlib:display-roots display)))
         (root (xlib:screen-root screen))
         (colormap (xlib:screen-default-colormap screen))
         (blue (xlib:alloc-color colormap :blue))
         (*canvas* (xlib:create-window :x 0 :y 0 :width (xlib:screen-width screen) :height (xlib:screen-height screen)
                                       :parent root
                                       :background :none
                                       :event-mask (make-event-mask :key-press)
                                       :class :input-output))
         (*gcontext* (xlib:create-gcontext :drawable *canvas* :foreground blue :subwindow-mode :include-inferiors)))
    (setf (xlib:window-override-redirect *canvas*) :on)
    (xlib:enable-xkeyboard display)
    (xlib:map-window *canvas*)
    (loop
       until (eq :success (xlib:grab-keyboard root :owner-p t)))
    (unwind-protect
         (progn (let ((tree (create-octotree (xlib:screen-width screen) (xlib:screen-height screen)))
                      (*keys-asdf* (map 'vector (lambda (code) (xlib:keycode->character display code 0)) *keys*)))
                  (draw-octotree tree)
                  (xlib:display-force-output display)
                  (handler-case
                      (xlib:process-event display :handler
                                          (lambda (&rest args)
                                            (when (eq (getf args :event-key) :key-press)
                                              (let* ((key (getf args :code))
                                                     (char (xlib:xkb/keysym->character
                                                            (xlib::keyevent->keysym
                                                             (xlib::transform-xkb-keymap-to-client-mapping
                                                              (xlib::get-map display xlib::+use-core-kbd+
                                                                             (logior xlib::+KeyTypes+
                                                                                     xlib::+KeySyms+)))
                                                             key
                                                             (getf args :state))
                                                            xlib:+xkbkeysymdb+)))
                                                (if (eq key 9)
                                                    t
                                                    (if (find char '(#\Esc))
                                                        t
                                                        (when (find key *keys*)
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
                                                                  (clear-area root :width (screen-width screen)
                                                                              :height (screen-height screen)
                                                                              :exposures-p t)
                                                                  (display-finish-output display)
                                                                  (sleep 0.01)
                                                                  (map-window *canvas*)
                                                                  (draw-octotree tree)
                                                                  (display-force-output display)
                                                                  nil))))))))))
                    (t (e) (with-open-file (out "/home/coder/.xkeyclick_errors"
                                                :direction :output
                                                :if-exists :append
                                                :if-does-not-exist :create)
                             (print e out))))))
      (xlib:free-colors colormap (list blue))
      (xlib:free-gcontext *gcontext*)
      (xlib:destroy-window *canvas*)
      (xlib:close-display display))))

