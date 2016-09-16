;;; ease-toolbar.el --- toolbar support for EASE

;; Author:  Bruce Ravel <ravel@phys.washington.edu>
;; Maintainer:  Bruce Ravel <ravel@phys.washington.edu>
;; Created:  16 February 1998
;; Version:  0.6.5
;; Keywords:  feff, input files, atoms, autobk, feffit

;; $Id: ease-toolbar.el,v 1.1.1.1 2000/05/21 05:46:16 bruce Exp $

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

;;; Commentary:

;;; Change-log:

;;; Code:


(eval-and-compile        ; for Atoms-math
  (condition-case ()
      (require 'calc)
    (error nil)))
(require 'cl)

(require 'ease-icons)    ; pixmap definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define toolbars for the various modes

(defvar ease-toolbar
  '([ease-toolbar-new-file-xpm         ease-toolbar-new-file
				       t "Edit an input file"]
    [ease-toolbar-document-xpm         ease-toolbar-document
				       t "Read the EASE document"]
    [ease-toolbar-tutorial-xpm         ease-toolbar-tutorial
				       t "Read the EASE tutorial"]
    [ease-toolbar-program-document-xpm ease-toolbar-program-document
				       t "Read program documentation"]
    [ease-toolbar-helper-xpm           ease-toolbar-helper
				       t "A bit of help"]
    [ease-toolbar-bug-xpm              ease-toolbar-bug
				       t "Report a bug"]
    [:style 3d :size 8]
    [ease-toolbar-quit-xpm             ease-toolbar-quit
				       t "Quit EASE"]
    [ease-toolbar-exit-xpm             ease-toolbar-exit
				       t "Quit Emacs"]
    )
  "The EASE toolbar.")

(add-hook 'ease-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (message "EASE: Setting up ease toolbar")
		  (ease-make-toolbar-function ease-toolbar))))

(defvar Atoms-toolbar ()
  "The Atoms toolbar.")
(setq Atoms-toolbar
      '([ease-toolbar-template-xpm Atoms-toolbar-template
				   t "Write an Atoms template"]
	[ease-toolbar-run-xpm      Atoms-toolbar-run
				   t "Run Atoms"]
	[ease-toolbar-log-xpm      Atoms-toolbar-log
				   t "Look at the Feff input file"]))
(if (featurep 'calc)
    (setq Atoms-toolbar
	  (append Atoms-toolbar
		  '([:style 3d :size 8]
		    [Atoms-toolbar-eval-line-xpm
		     Atoms-toolbar-eval-line
		     t
		     "Expand evaluation line under point"]
		    [Atoms-toolbar-eval-buffer-xpm
		     Atoms-toolbar-eval-buffer
		     t
		     "Expand all evaluatation lines in this buffer"]
		    [:style 3d :size 8]))))
(setq Atoms-toolbar
      (append Atoms-toolbar
	      '([ease-toolbar-helper-xpm   Atoms-toolbar-helper
					   t "Display Atoms keywords"]
		[ease-toolbar-document-xpm Atoms-toolbar-document
					   t "Read the Atoms document"]
		[ease-toolbar-quit-xpm     Atoms-toolbar-quit
					   t "Quit editing this input file"])))

(add-hook 'Atoms-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (message "EASE: Setting up Atoms toolbar")
		  (ease-make-toolbar-function Atoms-toolbar))))

(defvar Feff-toolbar
  '([ease-toolbar-template-xpm Feff-toolbar-template
			       t "Write a Feff template"]
    [ease-toolbar-run-xpm      Feff-toolbar-run
			       t "Run Feff"]
    [ease-toolbar-log-xpm      Feff-toolbar-log
			       t "Look at output from intrp"]
    [:style 3d :size 8]
    [Feff-toolbar-chi-xpm      Feff-toolbar-chi
			       t "Plot chi(k)"]
    [Feff-toolbar-xmu-xpm      Feff-toolbar-xmu
			       t "Plot mu(E) and mu0(E)"]
    [:style 3d :size 8]
    [ease-toolbar-helper-xpm   Feff-toolbar-helper
			       t "Display Feff keywords"]
    [ease-toolbar-document-xpm Feff-toolbar-document
			       t "Read the Feff document"]
    [ease-toolbar-quit-xpm     Feff-toolbar-quit
			       t "Quit editing this input file"]
    )
  "The Feff toolbar.")

(add-hook 'Feff-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (message "EASE: Setting up Feff toolbar")
		  (ease-make-toolbar-function Feff-toolbar))))

(defvar Autobk-toolbar
  '([ease-toolbar-template-xpm   Autobk-toolbar-template
				 t "Write an Autobk template"]
    [ease-toolbar-run-xpm        Autobk-toolbar-run
				 t "Run Autobk"]
    [ease-toolbar-log-xpm        Autobk-toolbar-log
				 t "Look at Autobk log file"]
    [:style 3d :size 8]
    [Autobk-toolbar-chi-xpm      Autobk-toolbar-chi
				 t "Plot chi(k) from current stanza"]
    [Autobk-toolbar-xmu-xpm      Autobk-toolbar-xmu
				 t "Plot mu(E) and mu0(E) from current stanza"]
    [Autobk-toolbar-all-xpm      Autobk-toolbar-all
				 t "Plot all chi(k) in file"]
    [:style 3d :size 8]
    [ease-toolbar-helper-xpm     Autobk-toolbar-helper
				 t "Display Autobk keywords"]
    ;;[Autobk-toolbar-document Autobk-toolbar-document
    ;;		   t "Read the Autobk document"]
    [ease-toolbar-quit-xpm       Autobk-toolbar-quit
				 t "Quit editing this input file"]
    )
  "The Autobk toolbar.")

(add-hook 'Autobk-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (message "EASE: Setting up Autobk toolbar")
		  (ease-make-toolbar-function Autobk-toolbar))))

(defvar Feffit-toolbar
  '([ease-toolbar-template-xpm Feffit-toolbar-template
			       t "Write a Feffit template"]
    [ease-toolbar-run-xpm      Feffit-toolbar-run
			       t "Run Feffit"]
    [Feffit-toolbar-log-xpm      Feffit-toolbar-log
				 t "Look at Feffit log file"]
    [Feffit-toolbar-prm-xpm      Feffit-toolbar-prm
				 t "Look at Feffit prm file"]
    [:style 3d :size 8]
    [Feffit-toolbar-k-xpm        Feffit-toolbar-k
				 t "Plot chi(k) and fit"]
    [Feffit-toolbar-r-xpm        Feffit-toolbar-r
				 t "Plot chi(R) and fit"]
    [Feffit-toolbar-q-xpm        Feffit-toolbar-q
				 t "Plot back-transformed chi(k) and fit"]
    [:style 3d :size 8]
    [ease-toolbar-helper-xpm   Feffit-toolbar-helper
			       t "Display Feffit keywords"]
    [ease-toolbar-quit-xpm     Feffit-toolbar-quit
			       t "Quit editing this input file"]
    )
  "The Feffit toolbar.")
    ;;    [Feffit-toolbar-document Feffit-toolbar-document
    ;;			     t "Read the Feffit document"]

(add-hook 'Feffit-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (message "EASE: Setting up Feffit toolbar")
		  (ease-make-toolbar-function Feffit-toolbar))))

(defvar Normal-toolbar
  '([ease-toolbar-template-xpm   Normal-toolbar-template
				 t "Write an Normal template"]
    [ease-toolbar-run-xpm        Normal-toolbar-run
				 t "Run Normal"]
    [:style 3d :size 8]
    [Normal-toolbar-this-nor-xpm Normal-toolbar-this-nor
				 t "Plot un-normalized mu(E) under point"]
    [Normal-toolbar-all-nor-xpm  Normal-toolbar-all-nor
				 t "Plot all un-normalized mu(E) in file"]
    [Normal-toolbar-this-xmu-xpm Normal-toolbar-this-xmu
				 t "Plot un-normalized mu(E) under point"]
    [Normal-toolbar-all-xmu-xpm  Normal-toolbar-all-xmu
				 t "Plot all un-normalized mu(E) in file"]
    [:style 3d :size 8]
    [ease-toolbar-helper-xpm     Normal-toolbar-helper
				 t "Display Normal keywords"]
    [ease-toolbar-document-xpm   Normal-toolbar-document
				 t "Read the Normal document"]
    [ease-toolbar-quit-xpm       Normal-toolbar-quit
				 t "Quit editing this input file"]
    )
  "The Normal toolbar.")

(add-hook 'Normal-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (message "EASE: Setting up Normal toolbar")
		  (ease-make-toolbar-function Normal-toolbar))))

(defvar Phit-toolbar
  '([ease-toolbar-template-xpm Phit-toolbar-template
			       t "Write an Phit template"]
    [ease-toolbar-run-xpm      Phit-toolbar-run
			       t "Run Phit"]
    [ease-toolbar-log-xpm      Phit-toolbar-log
			       t "Look at Phit log file"]
    [:style 3d :size 8]
    [Phit-toolbar-plot-xpm     Phit-toolbar-plot
			       t "Plot data and fit"]
    [:style 3d :size 8]
    [ease-toolbar-helper-xpm   Phit-toolbar-helper
			       t "Display Phit keywords"]
    [ease-toolbar-document-xpm Phit-toolbar-document
			       t "Read the Phit document"]
    [ease-toolbar-quit-xpm     Phit-toolbar-quit
			       t "Quit editing this input file"]
    )
  "The Phit toolbar.")

(add-hook 'Phit-mode-hook
	  '(lambda ()
	     (and (featurep 'toolbar)
		  (message "EASE: Setting up Phit toolbar")
		  (ease-make-toolbar-function Phit-toolbar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toolbar utilities

(defun ease-make-toolbar-function (toolbar)
  ;;(if gnuplot-all-buttons-defined
  ;;    (progn
	;;(remove-specifier gnuplot-use-toolbar (current-buffer))
  (ease-toolbar-setup-toolbar toolbar)
  (add-spec-to-specifier (symbol-value ease-use-toolbar)
			 toolbar (current-buffer)))

(defvar ease-toolbar-location "")

(defun ease-toolbar-setup-toolbar (toolbar)
  (let ((width 46) (height 46)
	(frame (selected-frame))
	(buffer (current-buffer))
	(tag-set '(win)))
    ;;(and ease-use-toolbar
	;; (ease-setup-toolbar toolbar nil)
	;; (set-specifier (symbol-value ease-use-toolbar)
	;;		(cons (current-buffer) toolbar)))
    (cond ((eq (symbol-value ease-use-toolbar) right-toolbar)
	   ;;(if myframe
	   ;;    (set-specifier right-toolbar toolbar frame tag-set))
	   (setq ease-toolbar-location          "right")
	   (set-specifier right-toolbar         toolbar buffer)
	   (set-specifier right-toolbar-width   width frame  tag-set))
	  ((eq (symbol-value ease-use-toolbar) left-toolbar)
	   (setq ease-toolbar-location          "left")
	   (set-specifier left-toolbar          toolbar buffer)
	   (set-specifier left-toolbar-width    width frame  tag-set))
	  ((eq (symbol-value ease-use-toolbar) bottom-toolbar)
	   (setq ease-toolbar-location          "bottom")
	   (set-specifier bottom-toolbar        toolbar buffer)
	   (set-specifier bottom-toolbar-height height frame tag-set))
	  ((eq (symbol-value ease-use-toolbar) top-toolbar)
	   (setq ease-toolbar-location          "top")
	   (set-specifier top-toolbar           toolbar buffer)
	   (set-specifier top-toolbar-height    height frame tag-set))) ))

;; (defun ease-setup-toolbar (bar &optional force)  ;;package)
;;   (let ((dir ease-glyph-directory)
;; 	(xpm (if (featurep 'xpm) "xpm" "xbm"))
;; 	icon up down disabled name)
;;     ;;(unless package
;;     ;;  (setq message-xmas-glyph-directory dir))
;;     (when dir
;;       (while bar
;; 	(setq icon (aref (car bar) 0)
;; 	      name (symbol-name icon)
;; 	      bar (cdr bar))
;; 	(when (or force
;; 		  (not (boundp icon)))
;; 	  (setq up (concat dir name "-up." xpm))
;; 	  (setq down (concat dir name "-down." xpm))
;; 	  (setq disabled (concat dir name "-disabled." xpm))
;; 	  (if (not (file-exists-p up))
;; 	      (setq bar nil
;; 		    dir nil)
;; 	    (set icon (toolbar-make-button-list
;; 		       up (and (file-exists-p down) down)
;; 		       (and (file-exists-p disabled) disabled)))))))
;;     dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fset toolbar names to actual functions

(fset 'ease-toolbar-new-file         'ease-find-file)
(fset 'ease-toolbar-document         'input-ease-document)
(fset 'ease-toolbar-tutorial         'ease-tutorial)
(fset 'ease-toolbar-program-document 'input-document)
(fset 'ease-toolbar-helper           'ease-mini-help)
(fset 'ease-toolbar-bug              'input-submit-feedback)
(fset 'ease-toolbar-quit             'ease-quit)
(fset 'ease-toolbar-exit             'save-buffers-kill-emacs)

(fset 'Atoms-toolbar-template    'Atoms-make-template)
(fset 'Atoms-toolbar-run         'input-run-this-program-this-file)
(fset 'Atoms-toolbar-log         'input-jump-to-log-file)
(fset 'Atoms-toolbar-eval-line   'Atoms-evaluate-line)
(fset 'Atoms-toolbar-eval-buffer 'Atoms-evaluate-buffer)
(fset 'Atoms-toolbar-helper      'input-display-keywords)
(fset 'Atoms-toolbar-document    'input-atoms-document)
(fset 'Atoms-toolbar-quit        'ease-quit)

(fset 'Feff-toolbar-template 'Feff-make-template)
(fset 'Feff-toolbar-run      'input-run-this-program-this-file)
(fset 'Feff-toolbar-log      'Feff-display-log)
(fset 'Feff-toolbar-chi      'Feff-plot-chi)
(fset 'Feff-toolbar-xmu      'Feff-plot-xmu)
(fset 'Feff-toolbar-helper   'input-display-keywords)
(fset 'Feff-toolbar-document 'input-feff-document)
(fset 'Feff-toolbar-quit     'ease-quit)

(fset 'Autobk-toolbar-template 'Autobk-make-template)
(fset 'Autobk-toolbar-run      'input-run-this-program-this-file)
(fset 'Autobk-toolbar-log      'input-jump-to-log-file)
(fset 'Autobk-toolbar-chi      'Autobk-plot-ksp)
(fset 'Autobk-toolbar-xmu      'Autobk-plot-bkg)
(fset 'Autobk-toolbar-all      'Autobk-plot-all-chi)
(fset 'Autobk-toolbar-helper   'input-display-keywords)
;;(fset 'Autobk-toolbar-document 'input-autobk-document)
(fset 'Autobk-toolbar-quit     'ease-quit)

(fset 'Feffit-toolbar-template 'ease-Feffit-choose-template)
(fset 'Feffit-toolbar-run      'input-run-this-program-this-file)
(fset 'Feffit-toolbar-log      'input-jump-to-log-file)
(fset 'Feffit-toolbar-prm      'Feffit-jump-to-prm-file)
(fset 'Feffit-toolbar-k        'Feffit-plot-k)
(fset 'Feffit-toolbar-r        'Feffit-plot-r)
(fset 'Feffit-toolbar-q        'Feffit-plot-q)
(fset 'Feffit-toolbar-helper   'input-display-keywords)
;;(fset 'Feffit-toolbar-document 'input-feffit-document)
(fset 'Feffit-toolbar-quit     'ease-quit)

(fset 'gnuplot-toolbar-line    'gnuplot-send-line-to-gnuplot)
(fset 'gnuplot-toolbar-region  'gnuplot-send-region-to-gnuplot)
(fset 'gnuplot-toolbar-buffer  'gnuplot-send-buffer-to-gnuplot)
;;(fset 'gnuplot-toolbar-quit    'ease-quit-gnuplot)
(fset 'gnuplot-toolbar-help    'gnuplot-show-gnuplot-buffer)
(fset 'gnuplot-toolbar-uphist  'ease-gnuplot-previous-script)
(fset 'gnuplot-toolbar-dnhist  'ease-gnuplot-next-script)

(fset 'Normal-toolbar-template 'Normal-make-template)
(fset 'Normal-toolbar-run      'input-run-this-program-this-file)
(fset 'Normal-toolbar-this-nor 'Normal-plot-this-norm)
(fset 'Normal-toolbar-all-nor  'Normal-plot-all)
(fset 'Normal-toolbar-this-xmu 'Normal-plot-this-xmu)
(fset 'Normal-toolbar-all-xmu  'Normal-plot-xmu)
(fset 'Normal-toolbar-helper   'input-display-keywords)
(fset 'Normal-toolbar-document 'input-normal-document)
(fset 'Normal-toolbar-quit     'ease-quit)

(fset 'Phit-toolbar-template 'ease-Phit-choose-template)
(fset 'Phit-toolbar-run      'input-run-this-program-this-file)
(fset 'Phit-toolbar-log      'input-jump-to-log-file)
(fset 'Phit-toolbar-plot     'Phit-plot-fit)
(fset 'Phit-toolbar-helper   'input-display-keywords)
(fset 'Phit-toolbar-document 'input-phit-document)
(fset 'Phit-toolbar-quit     'ease-quit)

;;; That's it! ----------------------------------------------------------------

;; any final chores before leaving
(provide 'ease-toolbar)

;;;============================================================================
;;;
;;; ease-toolbar.el end here
