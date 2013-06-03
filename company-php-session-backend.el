;;; company-php-session-backend.el --- use php internals to find completions

;; Copyright (C) 2013 Tom Regner

;;
;; Author: Tom Regner
;; Maintainer: Tom Regner <tom@goochesa.de>
;;
;; Version: 0.0.1
;;          See `company-php-session-backend-version'
;; Keywords: php, company, backend, completion, boris
;; Requires: company, deferred
;;
;;  This file is NOT part of GNU Emacs

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is alpha quality code, currently only php internal functions
;; are determined and used to find candidates.
;;
;; This company backend uses a running boris php repl process to load
;; code and uses internals like `get_defined_functions()` and
;; reflection to produce completion candidates for company
;
;; Put this file in your Emacs lisp path (e.g. ~/.emacs.d/site-lisp)
;; and add the following line to your .emacs:
;;
;;    (require 'company-php-session-backend)
;;    (add-to-list 'company-backends '(company-dabbrev-code company-php-session-backend))
;; 
;; You have to have boris, the php repl, installed and in your $PATH.
;;
;;
(require 'company)
(require 'comint)

(defvar cpsb/boris-buffer-name "php-completion"
  "Clear text part of the buffer used for the boris session.")

(defvar cpsb/boris-program "/home/tom/bin/boris"
  "The boris executable")

(defvar cpsb/php-libpath 
  (concat 
   (file-name-directory (or load-file-name ".")) "php") 
  "directory for the driver program.")

(defvar cpsb/internal-function-list '() 
  "Holds the list of php internal function names")

(defvar cpsb/at-tags '("@author"  "@param"  "@return")
"php-doc tags in comments")

(defun cpsb/get-boris-buffer ()
  "Return the buffer used to hold the boris comint session,
creating it if it does not exist."
  (get-buffer-create (concat " *" cpsb/boris-buffer-name "*") ))

(defun cpsb/get-comint ()
  "Return the comint buffer attached to the boris process,
creating it -- and starting boris --  if it does not exist."
  (interactive)
  (setq comint-eol-on-send t)
  (setq comint-prompt-regexp "\\[[^]]+\\] boris> ?")
  (setq comint-prompt-read-only nil)
  (setq comint-use-prompt-regexp nil)
  (setq comint-process-echoes nil)
  (apply 'make-comint-in-buffer 
		 (list "php-completion" 
			   (cpsb/get-boris-buffer) 
			   cpsb/boris-program 
			   nil 
			   "-r" (concat cpsb/php-libpath "/elisp-info.php"))))

(defun cpsb/get-comint-process ()
  "Return the comint process attached to the comint buffer."
  (get-buffer-process (cpsb/get-comint)))

(defun cpsb/php-functions ()
  "Return the names of all defined php funparctions as list"
  (if (and (boundp 'cpsb/internal-function-list)
		   cpsb/internal-function-list)
	  cpsb/internal-function-list
	(cpsb/fetch-internal-function-list)))

(defmacro cpsb/boris-command  (command &rest body)
  "Send COMMAND to boris, execute BODY after boris finished In
BODY you can access the output of COMMAND in
`cpsb/redirect-string`"
  `(save-excursion 
	 (with-current-buffer (cpsb/get-comint) (goto-char (point-max)))
	 ;; clear old output
	 (unwind-protect	
		 (let (cpsb/redirect-strings cpsb/redirect-string)
		   (setq cpsb/redirect-strings 
				 (comint-redirect-results-list-from-process
				  (cpsb/get-comint-process)
				  ,command
				  ";; -- php completion begin ;;\\([\"(].+[\")]\\);; -- php completion end ;;"
				  1))
		   (setq cpsb/redirect-string
				 (car cpsb/redirect-strings))
		   ,@body)
	   ;; clear boris buffer
	   (with-current-buffer (cpsb/get-boris-buffer)
		 (goto-char 0)
		 (comint-bol)
		 (kill-region (point) (point-max))))))
(def-edebug-spec cpsb/boris-command (command body)) 

(defun cpsb/php-classes ()
  "Ask boris for all declared classes, return their names as list"
  (cpsb/boris-command
   "declared_classes();"
   (if cpsb/redirect-string
	   (read cpsb/redirect-string))))

(defun cpsb/fetch-internal-function-list ()
  "Ask boris for all defined internal php-functions, return their
  names as list; set cpsb/internal-function-list"
  (cpsb/boris-command 
   "defined_internal_functions();" 
   (setq cpsb/internal-function-list
		(if cpsb/redirect-string
			(read cpsb/redirect-string)
		  ()))))

(defun cpsb/get-short-doc (name)
  "Return the first 80 characters of the documenting comment of
  PHP elelemnt NAME;"
  (ignore-errors 
	(cpsb/boris-command 
	 (concat "get_doc_string(\"" name "\", true);")
	 (if cpsb/redirect-string
		 (progn 
		   (let ((doc (read cpsb/redirect-string)))
			 (message (concat "DOc " doc))
			 doc
			 ))))))

;;;###autoload
(defun company-php-session-backend (command &optional arg &rest ignored)
  "Looks arround at point and produces more or less fitting
  completion candidates for php"
  (case command
	('init (and (cpsb/get-comint) (sleep-for .5)))
	('prefix (and (eq major-mode 'php-mode)
				  (or (company-grab-symbol) 'stop)))
	('sorted t)
	('candidates (cond
				  ((company-in-string-or-comment)
				   (all-completions arg cpsb/at-tags))
				  ((save-excursion
					 (and (re-search-backward "\\s-+" nil t)
						  (looking-back "new" 4)))
				   (all-completions arg (cpsb/php-classes)))
				  
				  (t (all-completions arg  (cpsb/php-functions)))))
	('meta (cpsb/get-short-doc arg))))

(provide 'company-php-session-backend)

;;; company-php-session-backend.el ends here
