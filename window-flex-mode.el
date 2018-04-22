;;; window-flex-mode.el --- automatically grow and balance windows as needed

;; Copyright (C) 2018 Michael Schuldt

;; Author: Michael Schuldt <mbschuldt@gmail.com>
;; Maintainer: Michael Schuldt <mbschuldt@gmail.com>
;; Keywords: tools
;; Version: 0.1
;; URL: https://github.com/mschuldt/window-flex-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; window-flex-mode grows the horizontal width of the selected window to
;; accommodate the width of the longest visable line.
;;
;; This is intended to make it easy to work with many vertically
;; split windows when the average window width is less then the
;; possible length of text lines.
;;
;; The width of the selected window is only expanded, ever reduced.
;;
;; By default it will try to prevent large differences in window width
;; by balancing all windows before attempting to increase the width.
;;
;;; Code:

(defgroup window-flex nil
  "Automatically grow and balance windows."
  :version "0.1"
  :group 'tools)

(defcustom window-flex-min-width 40
  "Minimum allowed window width."
  :group 'window-flex
  :type 'integer)

(defcustom window-flex-max-width 150
  "Minimum allowed window width for window extensions.
A nil value means unlimited"
  :group 'window-flex
  :type 'integer)

(defcustom window-flex-balance-windows t
  "If non-nil balance windows before growing the current window."
  :group 'window-flex
  :type 'boolean)

(defvar window-flex--selected-window nil)
(defvar window-flex--window-start nil)
(defvar window-flex--window-end nil)
(defvar window-flex--min-window-width nil)
(make-variable-buffer-local 'window-flex--window-start)
(make-variable-buffer-local 'window-flex--window-end)
(make-variable-buffer-local 'window-flex--min-window-width)

(defun window-flex--fit-window-to-buffer ()
  (let ((fit-window-to-buffer-horizontally 'only)
        (fit-frame-to-buffer nil))
    (fit-window-to-buffer (selected-window) nil nil
                          window-flex-max-width
                          (min window-flex--min-window-width window-flex-max-width))))

(defun window-flex--fit-current-line ()
  "Horizontally enlarge window to fit current line"
  (let ((len (save-excursion (end-of-line)
                             (current-column)))
        (width (window-width)))
    (when (and window-flex-min-width
               (< width
                  window-flex-min-width))
      (setq len window-flex-min-width))
    (when window-flex-max-width
      (setq len (min len window-flex-max-width)))
    (when (> len width)
      ;;TODO: window-resizable
      (enlarge-window (- len width) t))))

(defun window-flex--save-position ()
  (setq window-flex--selected-window (selected-window))
  (unless (markerp window-flex--window-start)
    (setq window-flex--window-start (make-marker)))
  (unless (markerp window-flex--window-end)
    (setq window-flex--window-end (make-marker)))
  (set-marker window-flex--window-start (window-start (selected-window)))
  (set-marker window-flex--window-end (window-end (selected-window))))

;;TODO: problems when switching in dired buffers - does not enlarge window enough
;;  when entering subdirs the window shrinks sometimes

(defun window-flex--maybe-fit-window ()
  "Horizontally extend the selected window if needed to fit the buffer."
  (let ((start (window-start (selected-window)))
        (end (window-end (selected-window))))
    (unless (eq window-flex--selected-window (selected-window))
      (setq window-flex--min-window-width (max window-flex-min-width
                                               (window-total-width)))) ;;don't allow shrinking the window
    (if (and (eq window-flex--selected-window (selected-window))
             (markerp window-flex--window-start) ;;TODO: make default value a marker
             (eq (marker-position window-flex--window-start) start)
             (markerp window-flex--window-end)
             (eq (marker-position window-flex--window-end) end))
        ;; The window and visable region has not changed.
        ;; Check if the current line has grown to long.
        (window-flex--fit-current-line)
      (window-flex--fit-window-to-buffer)
      (window-flex--save-position))))

(progn (setq window-flex--count 0)
       (setq window-flex--total-time (time-subtract (current-time) (current-time))))
;; (float-time window-flex--total-time)
;; (/ (float-time window-flex--total-time) window-flex--count)

(defun window-flex--balance-other-windows (window)
  ;;TODO: fix case when multiplw windows are displaying the same buffer
  ;;  -- locking a window should not lock all the windows visiting that buffer
  (with-current-buffer (window-buffer window) ;; ok to use (current-buffer)?
    (let ((_window-size-fixed window-size-fixed))
      (setq window-size-fixed t)
      (unwind-protect
          (balance-windows-area)
        (setq window-size-fixed _window-size-fixed)))))

(defun window-flex-update()
  "Grow the size of the selected window if needed.
Balance windows before resize if `window-flex-balance-windows' is set"
  (interactive)
  (let ((t0 (current-time))
        (w (window-width))
        delta)
    (window-flex--maybe-fit-window)
    (when (and window-flex-balance-windows
               (> (window-width) w))
      (window-flex--balance-other-windows (selected-window)))
    (setq window-flex--min-window-width (max window-flex-min-width
                                             (window-total-width)))
    (setq window-flex--total-time (time-add window-flex--total-time
                                            (time-subtract (current-time)
                                                           t0))
          window-flex--count (1+ window-flex--count))))

;;;###autoload
(define-minor-mode window-flex-mode
  (if window-flex-mode
      (remove-hook 'post-command-hook 'window-flex-update)
    (add-hook 'post-command-hook 'window-flex-update)))

(provide 'window-flex-mode)

;; TODO: don't immediately resize windows if they are manually adjusted

;;; window-flex-mode.el ends here
