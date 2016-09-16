;;; ease.el --- the EXAFS Analysis System for Emacs

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  30 January 1998
;; Version:  0.6.5
;; Keywords:  feff, input files, atoms, autobk, feffit

;; $Id: ease.el,v 1.1.1.1 2000/05/21 05:46:16 bruce Exp $

;; Copyright (C) 1998 Bruce Ravel

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;; Everyone is granted permission to copy, modify and redistribute this
;; and related files provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.

;;; History:
;;  current package version            comment
;; -----------------------------------------------------------------
;;  0.4.3     first stab: keyboard + menu + mouse interaction with
;;            commands; define commands for editing, documentation,
;;            configuration, bug report, quit, exit;  faces;
;;            toolbar for XEmacs (BR Jan 31 1998)

;;; Commentary:
;;   here is a good way to start ease:
;;     alias ease 'emacs -f ease-mode -name EASE -T EASE'

;;; Bugs: (well...known bugs)

;;; Code:

(require 'input)

(defvar ease-mode-hook nil
  "*Hooks run when EASE is loaded.")

;; a gross hack to be sure that the ease buffor gets pretty colors
(cond ((and (featurep 'custom) (fboundp 'custom-declare-variable))
       (defface ease-highlight-face '((((class color) (background light))
				       (:foreground "black"
				      	:background "green"))
				      (((class color) (background dark))
				       (:foreground "black"
				        :background "green")))
	 "Face used to highlight ease-mode commands."
	 :group 'ease-programs)
       (defface ease-title-face     '((((class color) (background light))
				       (:foreground "blue"))
				      (((class color) (background dark))
				       (:foreground "cadetblue")))
	 "Face used for EASE titles."
	 :group 'ease-programs)
       (defface ease-command-face   '((((class color) (background light))
				       (:foreground "firebrick3"))
				      (((class color) (background dark))
				       (:foreground "pink")))
	 "Face used for EASE command keys."
	 :group 'ease-programs))
      (t
       (copy-face 'highlight 'ease-highlight-face)
       (set-face-background 'ease-highlight-face "green")

       (copy-face 'bold 'ease-title-face)
       (set-face-foreground 'ease-title-face "blue")

       (copy-face 'bold 'ease-command-face)
       (set-face-foreground 'ease-command-face "firebrick3")))

(defvar ease-mode-map nil
  "Local keymap for EASE-mode buffer.")
(if ease-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\C-m" 'ease-under-point)
    (define-key map "c"    'input-customize)
    (define-key map "d"    'input-ease-document)
    (define-key map "e"    'ease-find-file)
    (define-key map "h"    'ease-mini-help)
    (define-key map "p"    'input-document)
    (define-key map "q"    'ease-quit)
    (define-key map "s"    'input-submit-feedback)
    (define-key map "t"    'ease-tutorial)
    (define-key map "v"    'input-show-version)
    (define-key map "x"    'save-buffers-kill-emacs)
    (cond ((string-match "XEmacs" emacs-version)
	   (define-key map '(button2) 'ease-under-mouse))
	  (t
	   (define-key map [mouse-2] 'ease-under-mouse)))
    (setq ease-mode-map map)))



(defvar ease-mode-menu nil)
(easy-menu-define
 ease-mode-menu ease-mode-map
 "Menu used in EASE"
 '("EASE"
   ["Open an input file"          'ease-find-file t]
   ["Read EASE document"          'input-ease-document t]
   ["Read program documentation"  'input-document t]
   ["Read EASE tutorial"          'ease-tutorial t]
   ["Customize EASE"              'input-customize :active (featurep 'custom)]
   ["Submit a EASE bug report"    'input-submit-feedback t]
   ["A bit of help with Emacs"    'ease-mini-help t]
   ["Quit EASE"                   'ease-quit t]
   ["Exit Emacs"                  'save-buffers-kill-emacs t]
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for ease-mode

(defun ease-quit ()
  "Quit EASE."
  (interactive) (kill-buffer (current-buffer)))

(defun ease-find-file ()
  "Prompt for an input filename."
  (interactive) (find-file (read-file-name "Name of input file > ")))

(defun ease-under-mouse (event)
  "Fetch the command under the mouse from the EASE screen display.
EVENT is a mouse event."
  (interactive "@e")
  (mouse-set-point event)
  (ease-under-point)    )

(defun ease-under-point ()
  "Fetch the command under point from the EASE screen display."
  (interactive)
  (let (start letter (orig (point-marker)))
    (back-to-indentation)
    (cond ((looking-at "`")
	   (forward-char 1)
	   (setq start (point-marker))
	   (forward-char 1)
	   (setq letter (buffer-substring-no-properties start (point)))
	   (forward-char 1)
	   (funcall (key-binding letter)))
	  (t
	   (goto-char orig)))    ))

(defun ease-identify-line (pt)
  "Identify the current line.
Return the completion to the sentence \"Mouse-2 to \".  PT is the
current value of point."
  (interactive)
  (let (match (alist '(("c" . "customize EASE")
		       ("d" . "read the EASE document")
		       ("e" . "edit a new input file")
		       ("h" . "get a bit of help")
		       ("p" . "read program documentation")
		       ("q" . "quit EASE")
		       ("s" . "submit a bug report about EASE by email")
		       ("t" . "read the EASE tutorial")
		       ("x" . "exit Emacs") )))
    (save-excursion
      (goto-char pt)
      (back-to-indentation)
      (when (looking-at "`\\([cdehpqstx]\\)")
	(cdr (assoc (match-string 1) alist))))))

(defun ease-mini-help ()
  "Write a short message to the echo area for the Emacs challenged."
  (interactive)
  (message (substitute-command-keys
	    "C-x C-c to exit Emacs, \\[keyboard-quit] to abort anything, \\[delete-other-windows] for one window")))

(defun ease-tutorial ()
  "Look at on-line tutorial for *EASE* using info or the w3 package."
  (interactive)  (input-document "ease-tutorial"))


(defun ease-set-properties ()
  "Set properties on the text in the EASE screen display."
  (save-excursion
    (let (start extent1 extent2)
      (goto-char (point-min))
      (search-forward ">" (point-max) t)
      (set-text-properties (point-min) (point) '(face ease-title-face))
      (while (not (eobp))
	(cond ((search-forward "`" (point-max) "to-limit")
	       (back-to-indentation)
	       (setq start (point-marker))
	       (forward-char 3)
	       (cond ((string-match "XEmacs" emacs-version)
		      (setq extent1 (make-extent start (point-marker)))
		      (set-extent-endpoints extent1 start (point-marker))
		      (set-extent-property  extent1 'face 'ease-command-face))
		     (t
		      (set-text-properties start (point)
					   '(face ease-command-face))))
	       (end-of-line)
	       (cond ((string-match "XEmacs" emacs-version)
		      (setq extent2 (make-extent start (point-marker)))
		      (set-extent-endpoints extent2 start (point-marker))
		      (set-extent-property extent2 'mouse-face
					   'ease-highlight-face)
		      (set-extent-property extent2 'pointer
					   'toolbar-pointer-glyph)
		      (set-extent-property extent2 'balloon-help
					   (concat "Mouse-2 to "
						   (ease-identify-line (point)))))
		     (t
		      (add-text-properties start (point)
				    '(mouse-face ease-highlight-face))))
	       ))))))

;;(defvar ease-toolbar-location "left")

(defun ease-display ()
  "Generate the text of the EASE screen display."
  (let (display)
    (setq display
	  (concat
	   "Welcome to EASE\n\n"
	   "EASE is the EXAFS Analysis System for Emacs\n"
	   "an interface to FEFF, FEFFIT, and other programs.\n"
	   "version " input-mode-version "\n" input-author
	   " <" input-author-email ">\n\n"
	   "You may begin by typing one of the one-letter commands listed\n"
	   "below, by hitting the middle mouse button on one of the commands\n"
	   "listed below, or (in XEmacs) hitting the left mouse button on\n"
	   "the toolbar.\n\n"

	   "Users of \"vi\", \"crisp\", and \"edt\": \n"
	   "Emacs can be made to emulate these editors.  See the EASE \n"
	   "document for details on customizing EASE to emulate the editor\n"
	   "of your choice.\n\n"

	   "`e' = Edit an input file\n"
	   "`d' = Read the EASE document\n"
	   "`p' = Read program documentation\n"
	   "`t' = Read the EASE tutorial\n"
	   "`s' = Submit a bug report about EASE\n"
	   "`h' = Don't panic!\n"
	   ))
    (if (featurep 'custom)
	(setq display (concat display "`c' = Customize EASE\n")))
    (setq display
	  (concat
	   display
	   "----------------------------------------\n"
	   "`q' = Quit EASE\n"
	   "`x' = Kill Emacs\n"))
    display    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for ease-mode

(defvar ease-mode nil
  "Determines if ease minor mode is active.")
(make-variable-buffer-local 'ease-mode)

;;;###autoload
(defun ease-mode ()
  "Define the mode of the preliminary interaction screen in EASE.
This screen is used a launching point for editing input files
using the EASE package.

Defined keys in EASE:\n\\{ease-mode-map}"
  (interactive)
  (switch-to-buffer ">> EASE <<")
  (delete-other-windows)
  (if ease-mode nil
    (fundamental-mode)
    (kill-all-local-variables)
    (require 'input)
    (use-local-map ease-mode-map)
    (easy-menu-add ease-mode-menu ease-mode-map)
    (insert (ease-display))
    (goto-char (point-min))
    (indent-region (point-min) (point-max) 7)
    (ease-set-properties)
    ;;(auto-fill-mode -1)
    (if abbrev-mode (abbrev-mode -1))
    (setq ease-mode t
	  major-mode 'ease-mode
	  mode-name "EASE"
	  buffer-read-only t)
    (and (string-match "XEmacs" emacs-version)
	 (require 'ease-toolbar))
    (input-read-init-file)
    (run-hooks 'ease-mode-hook)  ))

;;; Run Hook ------------------------------------------------------------------

(provide 'ease)
(run-hooks 'ease-load-hook)

;;============================================================================
;;
;; ease.el ends here
