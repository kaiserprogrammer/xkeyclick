(in-package :xkeyclick)

(remove-tests :all)

(define-test no-selection
  (let* ((width 1920)
         (height 1080)
         (tree (create-octotree width height)))
    (assert-equal 0 (xpos tree))
    (assert-equal 0 (ypos tree))
    (assert-equal 1920 (width tree))
    (assert-equal 1080 (height tree))))

(define-test list-selections
  (let* ((width 1920)
         (height 1080)
         (tree (create-octotree width height)))
    (assert-equal 8 (length (selections tree)))
    (assert-equalp (list (list (* (/ width 4) 0) (* (/ height 2) 0) (/ width 4) (/ height 2))
                         (list (* (/ width 4) 1) (* (/ height 2) 0) (/ width 4) (/ height 2))
                         (list (* (/ width 4) 2) (* (/ height 2) 0) (/ width 4) (/ height 2))
                         (list (* (/ width 4) 3) (* (/ height 2) 0) (/ width 4) (/ height 2))
                         (list (* (/ width 4) 0) (* (/ height 2) 1) (/ width 4) (/ height 2))
                         (list (* (/ width 4) 1) (* (/ height 2) 1) (/ width 4) (/ height 2))
                         (list (* (/ width 4) 2) (* (/ height 2) 1) (/ width 4) (/ height 2))
                         (list (* (/ width 4) 3) (* (/ height 2) 1) (/ width 4) (/ height 2)))
                   (selections tree))))

(define-test list-selections-after-select
  (let* ((width 1920)
         (height 1080)
         (tree (create-octotree width height)))
    (select tree 5)
    (assert-equal 8 (length (selections tree)))
    (with-slots (xpos ypos width height) tree
      (assert-equalp (list (list (+ xpos (* (/ width 4) 0)) (+ ypos (* (/ height 2) 0)) (/ width 4) (/ height 2))
                           (list (+ xpos (* (/ width 4) 1)) (+ ypos (* (/ height 2) 0)) (/ width 4) (/ height 2))
                           (list (+ xpos (* (/ width 4) 2)) (+ ypos (* (/ height 2) 0)) (/ width 4) (/ height 2))
                           (list (+ xpos (* (/ width 4) 3)) (+ ypos (* (/ height 2) 0)) (/ width 4) (/ height 2))
                           (list (+ xpos (* (/ width 4) 0)) (+ ypos (* (/ height 2) 1)) (/ width 4) (/ height 2))
                           (list (+ xpos (* (/ width 4) 1)) (+ ypos (* (/ height 2) 1)) (/ width 4) (/ height 2))
                           (list (+ xpos (* (/ width 4) 2)) (+ ypos (* (/ height 2) 1)) (/ width 4) (/ height 2))
                           (list (+ xpos (* (/ width 4) 3)) (+ ypos (* (/ height 2) 1)) (/ width 4) (/ height 2)))
                     (selections tree)))))

(define-test select
  (let* ((width 1920)
         (height 1080)
         (tree (create-octotree width height)))
    (select tree 0)
    (assert-equal 0 (xpos tree))
    (assert-equal 0 (ypos tree))
    (assert-equal (/ width 4) (width tree))
    (assert-equal (/ height 2) (height tree))))

(define-test select-other
  (let* ((width 1920)
         (height 1080)
         (tree (create-octotree width height)))
    (select tree 6)
    (assert-equal (* (/ width 4) 2) (xpos tree))
    (assert-equal (* (/ height 2) 1) (ypos tree))
    (assert-equal (/ width 4) (width tree))
    (assert-equal (/ height 2) (height tree))))



(let ((*print-failures* t)
      (*print-errors* t))
  (run-tests :all))
