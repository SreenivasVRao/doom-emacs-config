;;; ~/.doom.d/lisp/misc.el -*- lexical-binding: t; -*-

;; Ivy Posframe has trouble with the size of the frame.
;;; Alternatively, disable ivy-rich-mode
;; replace the frame width multiplier with something slightly higher.
(defun my-ivy-posframe-get-size ()
  "The default functon used by `ivy-posframe-size-function'."
  (list
   :height ivy-posframe-height
   :width ivy-posframe-width
   :min-height (or ivy-posframe-min-height
                   (let ((height (+ ivy-height 1)))
                     (min height (or ivy-posframe-height height))))
   :min-width (or ivy-posframe-min-width
                  (let ((width (round (* (frame-width) 0.5))))
                    (min width (or ivy-posframe-width width))))))



;; Using C-r within swiper similar to how C-s works.
;; Modification of swiper-C-s function.
(defun swiper-C-r (&optional arg)
    "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
    (interactive "p")
    (if (string= ivy-text "")
        (ivy-previous-history-element 1)
      (ivy-previous-line arg)))
