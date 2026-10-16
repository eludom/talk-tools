;;; talk-tools.el --- Simple talk timer with warnings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 George Jones 

;; Author: George Jones <gmj@pobox.com>
;; Maintainer: George Jones <gmj@pobox.com>
;; URL: https://github.com/eludom/talk-tools
;; Version: 0.2
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, timer

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A simple presentation timer.
;;
;; Note: For the best experience during presentations, consider installing the
;; `posframe` package (for visual popup warnings) and the `keycast` package
;; (for displaying keys pressed in the tab bar).
;;
;; Enable `talk-tools-mode' to get the keybindings:
;;   C-c t  `talk-tools-start'
;;   C-c T  `talk-tools-stop'
;;
;; When the timer is running, the elapsed time appears in the mode line,
;; near the beginning (before the file/buffer name).  At configurable
;; times, a warning is shown.  If `posframe' is installed, warnings appear
;; in a popup; otherwise they use the echo area.

;;; Code:

(defgroup talk-tools nil
  "Talk timer with warnings for presentations."
  :group 'tools)

;; Sample timing setting for a 20 minute demo talk 
(defcustom talk-tools-warnings
  '((5 . "Move on from intro.")
    (10 . "Demo half done.")
    (15 . "Finish up demo.")
    (18 . "Two minute warning.")
    (20 . "Finis. Questions?"))
  "Alist of (MINUTES . MESSAGE) warnings.
When the elapsed time reaches MINUTES, MESSAGE is displayed."
  :type '(alist :key-type number :value-type string)
  :group 'talk-tools)

(defcustom talk-tools-warning-popup-seconds 8
  "How long warning popups remain visible, in seconds."
  :type 'integer
  :group 'talk-tools)

(defcustom talk-tools-warning-font "DejaVu Sans Mono-32"
  "Font used for warning popups."
  :type 'string
  :group 'talk-tools)

;; Try to load optional packages if available.
(require 'posframe nil t)
(require 'keycast nil t)

(defvar talk-tools--start-time nil
  "Float time when the timer was started, or nil.")

(defvar talk-tools--timer nil
  "Timer object for updating the mode line and checking warnings.")

(defvar talk-tools--warnings-issued nil
  "List of warning times (in seconds) already shown.")

(defvar talk-tools--saved-tool-bar-mode nil
  "Saved state of `tool-bar-mode' prior to enabling `talk-tools-mode'.")

(defvar talk-tools--saved-menu-bar-mode nil
  "Saved state of `menu-bar-mode' prior to enabling `talk-tools-mode'.")

(defvar talk-tools--saved-keycast-tab-bar-mode nil
  "Saved state of `keycast-tab-bar-mode' prior to enabling `talk-tools-mode'.")

(defvar talk-tools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") #'talk-tools-start)
    (define-key map (kbd "C-c T") #'talk-tools-stop)
    map)
  "Keymap for `talk-tools-mode'.")

(declare-function keycast-tab-bar-mode "keycast")

;;;###autoload
(define-minor-mode talk-tools-mode
  "Toggle Talk Tools mode.

When enabled, provides keybindings to start/stop a talk timer.
The timer display is added near the beginning of the mode line.
\\{talk-tools-mode-map}"
  :lighter " Talk"
  :keymap talk-tools-mode-map
  :global t
  (if talk-tools-mode
      (progn
        (setq talk-tools--saved-tool-bar-mode (bound-and-true-p tool-bar-mode)
              talk-tools--saved-menu-bar-mode (bound-and-true-p menu-bar-mode)
              talk-tools--saved-keycast-tab-bar-mode (bound-and-true-p keycast-tab-bar-mode))
        (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
        (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
        (when (fboundp 'keycast-tab-bar-mode) (keycast-tab-bar-mode 1))
        (talk-tools--add-mode-line-element))
    (when (fboundp 'tool-bar-mode)
      (tool-bar-mode (if talk-tools--saved-tool-bar-mode 1 -1)))
    (when (fboundp 'menu-bar-mode)
      (menu-bar-mode (if talk-tools--saved-menu-bar-mode 1 -1)))
    (when (fboundp 'keycast-tab-bar-mode)
      (keycast-tab-bar-mode (if talk-tools--saved-keycast-tab-bar-mode 1 -1)))
    (talk-tools--remove-mode-line-element)))

;; Mode line element for the timer.
(defvar talk-tools--mode-line-el
  '(:eval (talk-tools--mode-line-string))
  "Mode line element shown when talk-tools-mode is enabled.")

(defun talk-tools--add-mode-line-element ()
  "Add the timer element to the front of `mode-line-format'."
  (unless (memq talk-tools--mode-line-el mode-line-format)
    (setq-default mode-line-format
                  (cons talk-tools--mode-line-el mode-line-format))))

(defun talk-tools--remove-mode-line-element ()
  "Remove the timer element from `mode-line-format'."
  (setq-default mode-line-format
                (delq talk-tools--mode-line-el mode-line-format)))

;; Optional posframe support.
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")

(defun talk-tools--elapsed ()
  "Return seconds since timer start, or nil."
  (when talk-tools--start-time
    (- (float-time) talk-tools--start-time)))

(defun talk-tools--format-seconds (seconds)
  "Format SECONDS as \"[M:SS]\" or \"[H:MM:SS]\"."
  (let* ((total (floor seconds))
         (hours (/ total 3600))
         (minutes (% (/ total 60) 60))
         (secs (% total 60)))
    (if (> hours 0)
        (format "[%d:%02d:%02d]" hours minutes secs)
      (format "[%d:%02d]" minutes secs))))

(defun talk-tools--mode-line-string ()
  "Return mode line string for current timer state."
  (let ((elapsed (talk-tools--elapsed)))
    (if elapsed
        (talk-tools--format-seconds elapsed)
      "")))

(defun talk-tools--show-warning (message)
  "Display MESSAGE using posframe if available, else echo area."
  (if (and (fboundp 'posframe-show) (fboundp 'posframe-hide))
      (progn
        (posframe-hide "*talk-tools-warning*")
        (posframe-show "*talk-tools-warning*"
                       :string message
                       :poshandler #'posframe-poshandler-frame-bottom-right-corner
                       :font talk-tools-warning-font
                       :background-color "grey"
                       :foreground-color "black"
                       :timeout talk-tools-warning-popup-seconds))
    (message "Talk Tools: %s" message)))

(defun talk-tools--tick ()
  "Check for due warnings and refresh mode line."
  (let ((elapsed (talk-tools--elapsed)))
    (when elapsed
      (dolist (warning talk-tools-warnings)
        (let ((when (* (car warning) 60)))
          (when (and (>= elapsed when)
                     (not (member when talk-tools--warnings-issued)))
            (push when talk-tools--warnings-issued)
            (talk-tools--show-warning (cdr warning)))))
      (force-mode-line-update t))))

;;;###autoload
(defun talk-tools-start ()
  "Start the talk timer."
  (interactive)
  (when talk-tools--timer
    (cancel-timer talk-tools--timer)
    (setq talk-tools--timer nil))
  (setq talk-tools--start-time (float-time)
        talk-tools--warnings-issued nil)
  (setq talk-tools--timer (run-with-timer 0 1 #'talk-tools--tick))
  (force-mode-line-update t)
  (message "Talk timer started."))

;;;###autoload
(defun talk-tools-stop ()
  "Stop the talk timer and hide any warning popup."
  (interactive)
  (when talk-tools--timer
    (cancel-timer talk-tools--timer)
    (setq talk-tools--timer nil))
  (setq talk-tools--start-time nil
        talk-tools--warnings-issued nil)
  (when (fboundp 'posframe-hide)
    (ignore-errors (posframe-hide "*talk-tools-warning*")))
  (force-mode-line-update t)
  (message "Talk timer stopped."))

(provide 'talk-tools)

;;; talk-tools.el ends here
