<?php
/* ;; Copyright (C) 2013 Tom Regner

;;
;; Author: Tom Regner
;; Maintainer: Tom Regner <tom@goochesa.de>
;;
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
;;
;;; Commentary:
;;  This file provides php functions that emacs can call in a boris comint process
*/
if(!defined("ELISP_INFO_LOADED")) {
    define("ELISP_INFO_LOADED", TRUE);
    define("RESULT_MARK_BEGIN", ";; -- php completion begin ;;");
    define("RESULT_MARK_END", ";; -- php completion end ;;");

    function defined_internal_functions() {
        defined_functions("internal");
    }

    function defined_functions($type) {
        $funcs=get_defined_functions();
        print_result($funcs[$type]);        
    }

    function declared_classes(){
        print_result(get_declared_classes());
    }

    function print_result($arr) {
        echo '' . RESULT_MARK_BEGIN . "(";
        sort($arr);
        foreach($arr as $item) {
            echo "\"" . addslashes($item) . "\" "; 
        }
        echo ")" . RESULT_MARK_END . "\n";
    }
}    
