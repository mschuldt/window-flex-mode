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

;; window-flex-mode grows the horizontal width of the current window to
;; accommodate the width of the line at point.
;; 
;; This is intended to make it easy to work with many vertically
;; split windows when the average window width is less then the
;; possible length of text lines.
;;
;; By default it will try to prevent large differences in window width
;; by balancing all windows before attempting to increase the width.
;;
;;; Code:

(defgroup window-flex nil
  "Automatically grow and balance windows."
  :version "0.1"
  :group 'tools)

(defcustom window-flex-min-width 80
  "Minimum allowed window width."
  :group 'window-flex
  :type 'integer)

(defcustom window-flex-balance-windows t
  "If non-nil balance windows before growing the current window."
  :group 'window-flex
  :type 'boolean)

(defun window-flex--calc-delta ()
  "Calculate the delta by which to grow the current window."
  (let ((len (save-excursion (end-of-line)
                             (current-column)))
        (width (window-width)))
    (when (and window-flex-min-width
               (< width
                  window-flex-min-width))
      (setq len window-flex-min-width))
    (if (> len width)
        (- len width)
      0)))

(defun window-flex-update()
  "Grow the size of the current window if needed.
Balance windows before resize if `window-flex-balance-windows' is set"
  (interactive)
  (let ((delta (window-flex--calc-delta)))
    (when (and window-flex-balance-windows
               (> delta 0))
      (balance-windows)
      (setq delta (window-flex--calc-delta)))
    (when (> delta 0)
      (enlarge-window delta t))))

;;;###autoload
(define-minor-mode window-flex-mode
  (if window-flex-mode
      (remove-hook 'post-command-hook 'window-flex-update)
    (add-hook 'post-command-hook 'window-flex-update)))

(provide 'window-flex-mode)

;;; window-flex-mode.el ends here
