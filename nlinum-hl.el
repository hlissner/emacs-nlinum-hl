;;; nlinum-hl.el --- highlight current line number for nlinum
;;
;; Copyright (C) 2017 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: Jun 03, 2017
;; Modified: Jun 03, 2017
;; Version: 1.0.0
;; Keywords: nlinum highlight current line
;; Homepage: https://github.com/hlissner/emacs-nlinum-hl
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Extends nlinum to provide current-line-number highlighting, as well as
;; various other fixes.
;;
;;; Installation:
;;
;; M-x package-install RET nlinum-hl
;;
;;   (require 'nlinum-hl)
;;   (add-hook 'nlinum-mode #'nlinum-hl-mode)
;;
;;; Code:

(require 'cl-lib)

(defgroup nlinum-hl nil
  "Options for nlinum-hl."
  :group 'faces) ; FIXME :group

(defvar nlinum-hl--overlay nil)
(defvar nlinum-hl--line 0)

(defface nlinum-hl-face
  '(((background dark)  (:inherit linum :foreground "white"))
    ((background white) (:inherit linum :foreground "black")))
  "Face for the highlighted line number."
  :group 'nlinum-hl)

(defun nlinum-hl-overlay-p (ov)
  (overlay-get ov 'nlinum))

(defun nlinum-hl-line (&rest _)
  "Highlight the current line number with nlinum."
  (while-no-input
    (let* ((pbol (line-beginning-position))
           (peol (1+ pbol))
           (max (point-max))
           (lineno (string-to-number (format-mode-line "%l"))))
      (unless (= nlinum-hl--line lineno)
        (setq nlinum-hl--line lineno)
        ;; Handle EOF case
        (when (>= peol max)
          (setq peol max))
        (jit-lock-fontify-now pbol peol)
        ;; Unhighlight previous highlight
        (when nlinum-hl--overlay
          (let* ((disp (get-text-property 0 'display (overlay-get nlinum-hl--overlay 'before-string)))
                 (str (nth 1 disp)))
            (put-text-property 0 (length str) 'face 'linum str)
            (setq nlinum-hl--overlay nil)
            disp))
        (let ((ov (cl-find-if #'nlinum-hl-overlay-p (overlays-in pbol peol))))
          ;; Try to deal with evaporating line numbers (a known nlinum bug)
          (unless (or ov (eobp))
            (nlinum--region pbol peol)
            (setq ov (cl-find-if #'nlinum-hl-overlay-p (overlays-in pbol peol))))
          (when ov
            (overlay-put ov 'nlinum-hl t)
            (let ((str (nth 1 (get-text-property 0 'display (overlay-get ov 'before-string)))))
              (put-text-property 0 (length str) 'face 'nlinum-hl-face str)
              (setq nlinum-hl--overlay ov))))))))

;; (defun nlinum-hl-unhl-line (&optional pt)
;;   "Removes the hanging overlay hl-line sometimes leaves behind."
;;   (ignore-errors
;;     (dolist (overlay (overlays-at (or pt (point))))
;;       (when (eq (overlay-get overlay 'face) 'hl-line)
;;         (delete-overlay overlay)))))

(defun nlinum-hl-flush-window (&optional window)
  (let ((window (or window (selected-window)))
        (orig-win (selected-window)))
    (with-selected-window window
      (when nlinum-mode
        (if (not (eq window orig-win))
            (nlinum--flush)
          ;; done in two steps to leave current line number highlighting alone
          (nlinum--region (point-min) (max 1 (1- (line-beginning-position))))
          (nlinum--region (min (point-max) (1+ (line-end-position))) (point-max)))))))

;; nlinum has a tendency to lose line numbers over time; a known issue. These
;; hooks/advisors attempt to stave off these glitches.
(defun nlinum-hl-flush-all-windows (&rest _)
  "Fix nlinum margins after major UI changes (like a change of font)."
  (mapc #'nlinum-hl-flush-window (window-list))
  nil)
(advice-add #'set-frame-font :after #'nlinum-hl-flush-all-windows)

(defun nlinum-hl-flush (&optional _ norecord)
  ;; norecord check is necessary to prevent infinite recursion in
  ;; `select-window'
  (when (not norecord) (nlinum-hl-flush-window)))
(advice-add #'select-window :before #'nlinum-hl-flush)
(advice-add #'select-window :after  #'nlinum-hl-flush)
(add-hook 'focus-in-hook  #'nlinum-hl-flush-all-windows)
(add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)

;;;###autoload
(define-minor-mode nlinum-hl-mode
  "Highlight current line in current buffer, using nlinum-mode."
  :lighter "" ; should be obvious it's on
  :init-value nil
  ;; hl-line-mode forces the current line number to redraw.
  ;; TODO manual redraw without hl-line
  (when (featurep 'hl-line)
    (remove-overlays (point-min) (point-max) 'hl-line t)
    (hl-line-mode (if nlinum-hl-mode +1 -1)))
  (if nlinum-hl-mode
      (add-hook 'post-command-hook #'nlinum-hl-line nil t)
    (remove-hook 'post-command-hook #'nlinum-hl-line t)))

;; Folding in web-mode can cause nlinum glitching. This attempts to resolve it.
(eval-after-load "web-mode"
  (lambda ()
    (advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-flush)))

(provide 'nlinum-hl)
;;; nlinum-hl.el ends here
