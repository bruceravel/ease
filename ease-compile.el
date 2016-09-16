;;; ease-compile.el --- emacs lisp script for compiling the EASE package

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  16 May 1998
;; Version:  0.6.5
;; Keywords:  feff, input files, atoms, autobk, feffit

;; $Id: ease-compile.el,v 1.1.1.1 2000/05/21 05:46:16 bruce Exp $

;; Copyright (C) 1998 Bruce Ravel

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file is used to compile and install EASE
;; -- ease-compile-files was inspired by dgnushack.el from the
;;    gnus package
;; -- the custom hack is taken from the custom web-page

;;; History:

;;; Bugs:

;;; Code:

;;(load "/home/bruce/desktop/xlisp/set-paths")
(setq load-path (cons "." load-path))

(require 'cl)
(require 'time-stamp)
(require 'comint)

(eval-when-compile (defvar imenu-generic-expression nil))

(eval-when-compile
  (condition-case ()
      (require 'hilit19)
    (error nil))
  (defun hilit-repaint-command (arg)
    arg)
  (defun hilit-set-mode-patterns (arg1 arg2 &optional arg3 arg4)
    (or arg1 arg2 arg3 arg4)))

(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
	   ;; David Hughes 2nd April 1998
	   ;;(or (not speedbar-xemacsp) speedbar-xemacs20p))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      (` (progn
	   (defvar (, var) (quote (, var)))
	   ;; To make colors for your faces you need to set your .Xdefaults
	   ;; or set them up ahead of time in your .emacs file.
	   (make-face (, var))
	   )))
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defvar ease-xanes-p nil)

(defun ease-compile-files ()
  (interactive)
  (let ((files  '("comment.el"       "info-look.el"    "tempo.el"
		  "gnuplot.el"       "input.el"        "ease.el"
		  "ease-gnuplot.el"  "ease-dired.el"   "ease-doc.el"
		  "ease-atoms.el"    "ease-autobk.el"  "ease-feff.el"
		  "ease-feffit.el"   "ease-normal.el"  "ease-phit.el"
		  "ease-diffkk.el"   "ease-fluo.el"
		  "ease-generic.el"  "gnuplot-gui.el"))
	(xfiles '("ease-toolbar.el"  "ease-icons.el"))
	;;(xanes '("ease-xanes.el" "ease-correct.el"))
	(xemacs (string-match "XEmacs" emacs-version))
	(byte-compile-verbose nil)
	(byte-compile-warnings nil) )
	;;(byte-compile-warnings
	;; '(free-vars unresolved callargs redefine obsolete)))
    (mapc 'ease-compile-file files)     ; compile the essential files
    (if xemacs (mapc 'ease-compile-file xfiles)) ; compile toolbar code
    ;;(if ease-xanes-p (mapc 'ease-compile-file xanes)) ; compile xanes+correct
    (ease-compile-file "ease-math.el")  ; compile math expressions code
    ))					; should I check for calc??

;; byte-compile-verbose	        Whether to report the function currently being
;;				compiled in the minibuffer;
;; byte-compile-warnings	List of warnings to issue, or t.  May contain
;;				'free-vars (references to variables not in the
;;					    current lexical scope)
;;				'unresolved (calls to unknown functions)
;;				'callargs  (lambda calls with args that don't
;;					    match the lambda's definition)
;;				'redefine  (function cell redefined from
;;					    a macro to a lambda or vice versa,
;;					    or redefined to take other args)
;;				'obsolete  (obsolete variables and functions)

(defun ease-compile-file (file)
  (interactive)
  (let ((elc (concat file "c")))
    (cond ((file-newer-than-file-p elc file)
	   (message "%-15s is up to date" file))
	  ((file-newer-than-file-p file elc)
	   (if (or byte-compile-warnings byte-compile-verbose)
	       (message "\n%s Byte compiling %s:\n" (make-string 7 ?=) file))
	   (byte-compile-file file))
	  ((not (file-exists-p file))
	   (message "%-15s does not exist" file)))))


;;; ease-compile.el ends here
