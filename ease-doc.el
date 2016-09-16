;;; ease-doc.el --- show keyword descriptions in echo area

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created: 11 April 1998
;; Version:  0.6.5
;; Keywords: EASE, keywords, feff, feffit, atoms, autobk

;; $Id: ease-doc.el,v 1.1.1.1 2000/05/21 05:46:16 bruce Exp $

;; Copyright (C) 1998 Bruce Ravel
;;
;; This file is not part of GNU Emacs.

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; This program shows the description of the keyword under the cursor in the
;; minibuffer. It acts as a kind of "emacs background process", by regularly
;; checking the word under the cursor and matching it against the list of
;; keywords for the current input file type
;;
;; This program was essentially swiped from the `eldoc.el' package by
;; Noah Friedman
;;
;;; Installation:
;;
;; This is disabled by default in EASE, but putting the following in
;; your .emacs or .ease:
;;    (add-hook 'input-mode-hook 'turn-on-ease-doc-mode)
;; turns it on whenever you start editing an input file.  It can also
;; be turned on by hand: `M-x ease-doc-mode' or `C-c C-b t' or in the
;; Input pull-down menu.
;;
;;; ToDo:
;;
;;; Bugs:
;;
;;; Changelog:
;;

;;; Code:

;; Use idle timers if available in the version of emacs running.
;; Please don't change this to use `require'; this package works as-is in
;; XEmacs (which doesn't have timer.el as of 19.14), and I would like to
;; maintain compatibility with that since I must use it sometimes.  --Noah
(or (featurep 'timer)
    (load "timer" t))


(defgroup ease-doc nil
  "Real-time keyword documentation in EASE."
  :prefix "ease-doc-"
  :group 'ease)

(defcustom ease-doc-load-hook nil
 "Hook invoked when loading ease-doc.el."
 :group 'ease-doc
 :type 'hook)
(defcustom ease-doc-mode-hook nil
 "Hook invoked when entering `ease-doc-mode'."
 :group 'ease-doc
 :type 'hook)

(defcustom ease-doc-idle-delay 0.50
  "*Number of seconds of idle time to wait before printing.
If user input arrives before this interval of time has elapsed after
the last input, no documentation will be printed.  If this variable is
set to 0, no idle time is required."
  :group 'ease-doc
  :type 'number)
(defcustom ease-doc-identifier-string "*"
  "*String appended to `input-mode' mode-name by `ease-doc-mode'.
Typically the mode name for input mode is \"Input\" and this string
is concatenated to the end of that, forming \"Input*\", when
`ease-doc-mode' is started and removed when it is ended.  This is done
rather than adding an entry to `minor-mode-alist' as an effort to
conserve modeline real estate.  To conserve even more modeline real
estate, set this variable to \"\"."
  :group 'ease-doc
  :type 'string)

;;;###autoload
(defcustom ease-doc-mode nil
  "*If non-nil, show the defined parameters for the elisp function near point.
Show a brief description in the echo area of the keyword underneath point.
This variable is buffer-local."
  :type 'boolean
  :group 'ease-doc)
(make-variable-buffer-local 'ease-doc-mode)


;; Commands after which it is appropriate to print in the echo area.
;; Ease-Doc does not try to print function arglists, etc. after just any command,
;; because some commands print their own messages in the echo area and these
;; functions would instantly overwrite them.  But self-insert-command as well
;; as most motion commands are good candidates.
;; This variable contains an obarray of symbols; do not manipulate it
;; directly.  Instead, use `ease-doc-add-command' and `ease-doc-remove-command'.
(defvar ease-doc-message-commands nil)

;; Bookkeeping; the car contains the last symbol read from the buffer.
;; The cdr contains the string last displayed in the echo area, so it can
;; be printed again if necessary without reconsing.
(defvar ease-doc-last-data (cons nil nil))
(defvar ease-doc-last-message nil)

;; Idle timers are supported in Emacs 19.31 and later.
(defvar ease-doc-use-idle-timer-p (fboundp 'run-with-idle-timer))

;; ease-doc's timer object, if using idle timers
(defvar ease-doc-timer nil)

;; idle time delay currently in use by timer.
;; This is used to determine if ease-doc-idle-delay is changed by the user.
(defvar ease-doc-current-idle-delay ease-doc-idle-delay)


(defun ease-doc-modify-mode-string (on)
  "Mark the major mode name when `ease-doc-mode' is enabled.
The value of `ease-doc-identifier-string' is appended to
the mode-name when `ease-doc-mode' is turned on and
removed when `ease-doc-mode' is turned off.  ON is t when
`ease-doc-mode' is enabled and nil when disabled."
  (if on
      (setq mode-name (concat input-mode-name ease-doc-identifier-string))
    (setq mode-name input-mode-name)) )

;;;###autoload
(defun ease-doc-mode (&optional prefix)
  "*Enable or disable ease-doc mode.
See documentation for the variable of the same name for more details.

If called interactively with no prefix argument, toggle current condition
of the mode.
If called with a positive or negative prefix argument, enable or disable
the mode, respectively."
  (interactive "P")
  (cond ((string-match "Input" mode-name)
	 (setq ease-doc-last-message nil)
	 (cond (ease-doc-use-idle-timer-p
		(add-hook 'post-command-hook 'ease-doc-schedule-timer)
		(add-hook 'pre-command-hook
			  'ease-doc-pre-command-refresh-echo-area))
	       (t
		;; Use post-command-idle-hook if defined, otherwise
		;; use post-command-hook.  The former is only proper
		;; to use in Emacs 19.30; that is the first version in
		;; which it appeared, but it was obsolesced by idle
		;; timers in Emacs 19.31.
		(add-hook (if (boundp 'post-command-idle-hook)
			      'post-command-idle-hook
			    'post-command-hook)
			  'ease-doc-print-description)
		;; quick and dirty hack for seeing if this is XEmacs
		(and (fboundp 'display-message)
		     (add-hook 'pre-command-hook
			       'ease-doc-pre-command-refresh-echo-area))))
	 (setq ease-doc-mode (if prefix
				 (>= (prefix-numeric-value prefix) 0)
			       (not ease-doc-mode)))
	 (ease-doc-modify-mode-string ease-doc-mode)
	 (and (interactive-p)
	      (if ease-doc-mode
		  (message (concat "Turning on ease-doc-mode -- keyword "
				   "descriptions will be shown in the echo area"))
		(message "Turning off ease-doc-mode")))
	 ease-doc-mode)
	(t
	 (message "Ease-doc-mode is only for use with Input major mode"))) )


;;;###autoload
(defun turn-on-ease-doc-mode ()
  "Unequivocally turn on ease-doc-mode."
  (interactive)
  (ease-doc-mode 1))

;; Idle timers are part of Emacs 19.31 and later.
(defun ease-doc-schedule-timer ()
  (or (and ease-doc-timer
           (memq ease-doc-timer timer-idle-list))
      (setq ease-doc-timer
            (run-with-idle-timer ease-doc-idle-delay t
                                 'ease-doc-print-description)))

  ;; If user has changed the idle delay, update the timer.
  (cond ((not (= ease-doc-idle-delay ease-doc-current-idle-delay))
         (setq ease-doc-current-idle-delay ease-doc-idle-delay)
         (timer-set-idle-time ease-doc-timer ease-doc-idle-delay t))))

;; This function goes on pre-command-hook for XEmacs or when using idle
;; timers in Emacs.  Motion commands clear the echo area for some reason,
;; which make ease-doc messages flicker or disappear just before motion
;; begins.  This function reprints the last ease-doc message immediately
;; before the next command executes, which does away with the flicker.
;; This doesn't seem to be required for Emacs 19.28 and earlier.
(defun ease-doc-pre-command-refresh-echo-area ()
  (and ease-doc-last-message
       (if (ease-doc-display-message-no-interference-p)
           (ease-doc-message ease-doc-last-message)
         (setq ease-doc-last-message nil))))

(defun ease-doc-message (&rest args)
  (let ((omessage ease-doc-last-message))
    (cond ((eq (car args) ease-doc-last-message))
          ((or (null args)
               (null (car args)))
           (setq ease-doc-last-message nil))
          (t
           (setq ease-doc-last-message (apply 'format args))))
    ;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages
    ;; are recorded in a log.  Do not put ease-doc messages in that log since
    ;; they are Legion.
    (if (fboundp 'display-message)
        ;; XEmacs 19.13 way of preventing log messages.
        (if ease-doc-last-message
            (display-message 'no-log ease-doc-last-message)
          (and omessage
               (clear-message 'no-log)))
      (let ((message-log-max nil) (log-message-max-size nil))
        (if ease-doc-last-message
            (message "%s" ease-doc-last-message)
          (and omessage
               (message nil))))))
  ease-doc-last-message)


(defun ease-doc-print-description ()
 "Print the type of the symbol under the cursor.
This function is hooked into the `post-command-idle-hook' to print the type
automatically if `ease-doc-mode' is turned on. It can also be called
directly to ask for the type of a function."
  (interactive)
  (and ease-doc-mode
       (not executing-kbd-macro)
       (not (eq (selected-window) (minibuffer-window)))
       (not (input-comment-p))
       (sit-for ease-doc-idle-delay)  ;; pause, then
       (ease-doc-describe-keyword (save-excursion (input-this-word))) ))


;;;###autoload
(defun ease-doc-describe-keyword (&optional keyword)
  "Show the description of the KEYWORD near point."
  (interactive)
  (let* ((key  (or keyword (save-excursion (input-this-word))))
	 (desc (input-get-from-keyword-alist (downcase key) 2)))
    (and desc
	 (let (message-log-max)
	   (message "%S takes %s" key desc)))))


;; Decide whether now is a good time to display a message.
(defun ease-doc-display-message-p ()
  (and (ease-doc-display-message-no-interference-p)
       (cond (ease-doc-use-idle-timer-p
              ;; If this-command is non-nil while running via an idle
              ;; timer, we're still in the middle of executing a command,
              ;; e.g. a query-replace where it would be annoying to
              ;; overwrite the echo area.
              (and (not this-command)
                   (symbolp last-command)
                   (intern-soft (symbol-name last-command)
                                ease-doc-message-commands)))
             (t
              ;; If we don't have idle timers, this function is
              ;; running on post-command-hook directly; that means the
              ;; user's last command is still on `this-command', and we
              ;; must wait briefly for input to see whether to do display.
              (and (symbolp this-command)
                   (intern-soft (symbol-name this-command)
                                ease-doc-message-commands)
                   (sit-for ease-doc-idle-delay))))))

(defun ease-doc-display-message-no-interference-p ()
  (and ease-doc-mode
       (not executing-kbd-macro)
       ;; Having this mode operate in an active minibuffer/echo area causes
       ;; interference with what's going on there.
       (not cursor-in-echo-area)
       (not (eq (selected-window) (minibuffer-window)))))


;;; Run Hook ------------------------------------------------------------------

(provide 'ease-doc)
(run-hooks 'ease-doc-load-hook)

;;;============================================================================
;;;
;;; ease-doc.el ends here
