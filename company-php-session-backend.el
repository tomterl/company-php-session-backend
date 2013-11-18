;;; company-php-session-backend.el --- use php internals to find completions

;; Copyright (C) 2013 Tom Regner

;;
;; Author: Tom Regner
;; Maintainer: Tom Regner <tom@goochesa.de>
;;
;; Version: 0.0.4
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
;;    (add-to-list 'company-backends 'company-php-session-backend)
;; 
;; You have to have boris, the php repl, installed and in your $PATH.
;;
;;
(require 'company)
(require 'comint)

;; customizations

(defcustom cpsb/boris-program "/home/tom/bin/boris"
  "The boris executable" :group 'company-php )

;; internal variables
(defvar company-php-session-backend-version "0.0.4"
  "Version of this backend")

(defvar cpsb/boris-buffer-name "php-completion"
  "Clear text part of the buffer used for the boris session.")


(defvar cpsb/php-libpath 
  (concat 
   (file-name-directory (or load-file-name ".")) "php") 
  "directory for the driver program.")

(defvar cpsb/internal-function-list '() 
  "Holds the list of php internal function names")

(defvar cpsb/at-tags '("@abstract" "@access"
"@author" "@category" "@copyright" "@deprecated" "@example"
"@final" "@filesource" "@global" "@ignore" "@internal"
"@license" "@link" "@method" "@name" "@package" "@param"
"@property" "@return" "@see" "@since" "@static" "@staticvar"
"@subpackage" "@todo" "@tutorial" "@uses" "@var" "@version")
"php-doc tags in comments")

(defvar cpsb/--coi nil 
  "Holds class or instance in prefix match")

(defvar cpsb/--acc nil 
  "Holds accessor in prefix match (->|;;)")

(defvar cpsb/--typ nil
  "Holds the type found for cpsb/--coi")

;; macros
(defmacro cpsb/boris-command  (command &rest body)
  "Send php COMMAND to boris, execute BODY after boris finished.

 In BODY you can access the output of COMMAND in
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

;; internal functions
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
  "Return the names of all defined php functions as list"
  (if (and (boundp 'cpsb/internal-function-list)
		   cpsb/internal-function-list)
	  cpsb/internal-function-list
	(cpsb/fetch-internal-function-list)))

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
   "defined_functions();" 
   (setq cpsb/internal-function-list
		(if cpsb/redirect-string
			(read cpsb/redirect-string)
		  ()))))

(defun cpsb/fetch-short-doc (name)
  "Return a short documentation of the PHP element NAME

  - function: signature
  - class: name and constructor signature"
  (ignore-errors 
	(cpsb/boris-command 
	 (format "doc_string(\"%s\", true, \"%s\");" name cpsb/--typ)
	 (if cpsb/redirect-string
		 (read cpsb/redirect-string)))))

(defun cpsb/fetch-doc (name)
  "Return the complete doc comment of the PHP element NAME"
  (cpsb/boris-command 
   (format "doc_string(\"%s\", false, \"%s\");" name cpsb/--typ)
   (when cpsb/redirect-string
	 (company-doc-buffer (read cpsb/redirect-string)))))

(defun cpsb/class-members (class-or-instance acc)
  "Retun a list of all members of CLASS-OR-INSTANCE. Return only static
members if ACC is `::`"
  (let ((type (cpsb/find-type class-or-instance)))
	(ignore-errors
	  (cpsb/boris-command
	   (format "class_members(\"%s\",\"%s\");" type acc)
	   (if cpsb/redirect-string
		   (read cpsb/redirect-string))))))

(defun cpsb/find-type (class-or-instance)
  "Try to find the type of CLASS-OR-INSTANCE.

- CLASS-OR-INSTANCE if it doesn't start with $
- else try
  - CLASS-OR-INSTANCE = new TYPE
  - function blah(..., TYPE CLASS-OR-INSTANCE,...)
  - @param TYPE CLASS-OR-INSTANCE"
  (let (result)
	(save-excursion 
	  (if (re-search-backward
		   (format "[[:space:]]*%s[[:space:]]*=[[:space:]]*new[[:space:]]+\\(.*\\)(" 
				   (regexp-quote class-or-instance))
		   nil t)
		  (setq result (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
	(save-excursion 
	  (if (and (not result)
			   (re-search-backward
				(format "[[:space:]]*function[[:space:]]+[^(]+([^)]*[,]?\\([^)]+\\)[[:space:]]+%s[[:space:]]*[,)]"
						(regexp-quote (concat "&?" class-or-instance)))
				nil t)) 
		  (setq result (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
	(save-excursion 
	  (if (and (not result)
			   (re-search-backward
				(format "@param[[:space:]]+\\(.*\\)[[:space:]]+%s" 
						(regexp-quote class-or-instance))
				nil t)) 
		  (setq result (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
	(setq cpsb/--typ result)))

(defun cpsb/candidates (prefix)
  "Looks arround at point and produces more or less fitting
completion candidates for PREFIX."
  (cond 
   ((company-in-string-or-comment)
	(all-completions prefix cpsb/at-tags))
   ((save-excursion (looking-back "new .*" (- (point) (+ (length prefix) 5))))
	(all-completions prefix (cpsb/php-classes)))
   ((and 
	 (save-excursion (looking-back "\\([^:[:space:];()-]+\\)\\(->\\|::\\)" (point-at-bol) t))
	 (setq cpsb/--coi (match-string 1)
		   cpsb/--acc (match-string 2)))
	(all-completions prefix (cpsb/class-members cpsb/--coi cpsb/--acc)))
   (t (all-completions prefix  (cpsb/php-functions)))))

(defun cpsb/prefix ()
  "Return the current prefix.

In case of instance or class access (->|::), complete immediatly."
  (let ((symbol (company-grab-symbol)))
	(setq cpsb/--coi nil
		  cpsb/--acc nil
		  cpsb/--typ nil)
	(cond 
	 ((save-excursion
		(forward-char (- (length symbol)))
		(looking-back "->\\|::" (- (point) 2)))
	  (progn
		(save-excursion
		  (if (looking-back
			   (format "[[:space:]]+[^:[:space:]-]+\\(?:->\\|::\\)%s" 
					   (regexp-quote symbol)) (point-at-bol)) 
			  (cons 
			   symbol
			   t)
			symbol))))
	 ((save-excursion 
		(forward-char (- (length symbol)))
		(looking-back "@" (- (point) 1)))
	  (concat "@" symbol))
	 (t (or symbol 'stop)))))

;;;###autoload
(defun company-php-session-backend (command &optional arg &rest ignored)
  "Implementing company backend commands for php code.

See the documentation for the customization `company-backends`
for details.
"
  (case command
	('init (and (cpsb/get-comint) (sleep-for .5)))
	('prefix (if (eq major-mode 'php-mode)
				 (cpsb/prefix)))
	('sorted t)
	('candidates (cpsb/candidates arg))
	('meta (cpsb/fetch-short-doc arg))
	('doc-buffer (cpsb/fetch-doc arg))))


(provide 'company-php-session-backend)

;;; company-php-session-backend.el ends here
