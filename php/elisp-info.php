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
;;  
*/
if(!defined("ELISP_INFO_LOADED")) {
    define("ELISP_INFO_LOADED", TRUE);
    define("RESULT_MARK_BEGIN", ";; -- php completion begin ;;");
    define("RESULT_MARK_END", ";; -- php completion end ;;");

    $__elisp_info_cache = array (
        'functions' => null,
        'short_docs' => array(),
        'docs' => array(),
        'classes' => null
    );

    /**
     * @return string elisp list string representation
     */
    function defined_functions() {
        global $__elisp_info_cache;
        if($__elisp_info_cache['functions']) {
            print_result($__elisp_info_cache['functions']);
        } else {
            $funcs=get_defined_functions();
            $funcs = array_merge($funcs['internal'],$funcs['user']);
            $__elisp_info_cache['functions'] = $funcs;
            print_result($funcs[$type]);
        }
    }

    /**
     * @return string elisp list string representation
     */
    function declared_classes(){
        global $__elisp_info_cache;
        if ($__elisp_info_cache['classes']) {
            print_result($__elisp_info_cache['classes']);
       } else {
            $classes = get_declared_classes();
            $__elisp_info_cache['classes'] = $classes;
            print_result($classes);
        }
    }

    /**
     * @param string $class class class
     * @param string type :: | ->
     * @return string elisp list string representation
     */
    function class_members($class, $type) {
        $result = array();
        $filter = ReflectionMethod::IS_PUBLIC | ReflectionMethod::IS_PROTECTED
        | ReflectionMethod::IS_PRIVATE | ReflectionMethod::IS_ABSTRACT
        | ReflectionMethod::IS_FINAL;
        
        if ($type == '::') {
            $filter = ReflectionMethod::IS_STATIC;
        }       
        if (class_exists($class)) {
            $rfl = new ReflectionClass($class);
            $methods = $rfl->getMethods($filter);
            foreach($methods as $method) {
                $result[] = $method->getName();
            };
            $props = $rfl->getProperties($filter);
            foreach($props as $prop) {
                $result[] = $prop->getName();
            }
            $consts = $rfl->getConstants();
            $consts = array_keys($consts);
            $result = array_merge($result, $consts);
        }
        print_result($result);
    }

    /**
     * @param string $name object name - function, class, member
     * @param boolean $short short description, or complete doc comment?
     * @param string $class if != "", $name is a member of $class
     * @return string elisp string
     */
    function doc_string($name, $short = false, $class = "") {
        global $__elisp_info_cache;
        if (!$name || $name == '') return "";
        global $__elisp_info_cache;
        $doc = "";
        if ($class != "" && $class != "nil") {
            $doc = doc_string_member($name, $class, $short);
        } else {
            if (function_exists($name)) {
                $doc = doc_string_function($name, $short);
            } else if (class_exists($name, false)) {
                $doc = doc_string_class($name, $short);
            }
        }

        $doc = preg_replace("~\n~s", "\\n", $doc);
        print_result($doc);

    }

    /**
     * @param string $name class name
     * @param boolean $short
     * @return string
     */
    function doc_string_class($name, $short) {
        global $__elisp_info_cache;
        $doc = "";
        if ($short) {
            if (array_key_exists("class_" . $name, 
                                 $__elisp_info_cache['short_docs'])) {
                $doc = $__elisp_info_cache['short_docs']["class_" . $name];
            } else {
                $rfl = new ReflectionClass($name);
                $doc = build_class_string($rfl);
                $__elisp_info_cache['short_docs']["class_" . $name] = $doc;
            }
        } else {
            if (array_key_exists("class_" . $name, 
                                 $__elisp_info_cache['docs'])) {
                $doc = $__elisp_info_cache['docs']["class_" . $name];
            } else {
                $rfl = new ReflectionClass($name);
                $doc = $rfl->getDocComment();
                $__elisp_info_cache['docs']["class_" . $name] = $doc;
            }
        }
        return $doc;
    }

    /**
     * @param string $name function name
     * @return string
     */
    function doc_string_function($name, $short = true) {
        $doc = "";
        if ($short) {
            if (array_key_exists($name, $__elisp_info_cache['short_docs'])) {
                $doc = $__elisp_info_cache['short_docs'][$name];
            } else {
                $rfl = new ReflectionFunction($name);
                $doc = build_func_string($rfl);
                $__elisp_info_cache['short_docs'][$name] = $doc;
            }
        } else {
            if (array_key_exists($name, $__elisp_info_cache['docs'])) {
                $doc = $__elisp_info_cache['docs'][$name];
            } else {
                $rfl = new ReflectionFunction($name);
                $doc = $rfl->getDocComment();
                $__elisp_info_cache['docs'][$name] = $doc;
            }
        }
        return $doc;
    }

    /**
     * @param string $name member to document
     * @param string $class
     * @param boolean $short
     * @return string
     */
    function doc_string_member($name, $class, $short) {
        $doc = "";
        $rfl = new ReflectionClass($class);
        $mem = $rfl->getMethod($name);
        if ($short) {
            $doc = build_func_string($mem);
        } else {
            $doc = $mem->getDocComment();
        }
        return $doc;
    }

    /**
     * prints $arg as elisp readable string between result markers
     * 
     * @param array|string $arg
     * @return void
     */
    function print_result($arg) {
        if (is_array($arg)) {
            echo '' . RESULT_MARK_BEGIN . "(";
            sort($arg);
            foreach($arg as $item) {
                echo "\"" . addslashes($item) . "\" ";
            }
            echo ")" . RESULT_MARK_END . "\n";
        } else {
            echo '' . RESULT_MARK_BEGIN . "\"";
            echo $arg;
            echo "\"" . RESULT_MARK_END . "\n";
        }
    }

    /**
     * @param ReflectionFunction $func function to build doc string for
     * @return string short doc string for $func
     */
    function build_func_string(ReflectionFunctionAbstract &$func) {
        $str = "";
        $matches = array();

        if (preg_match("~@return\s+([^ ]+)~", $func->getDocComment(),&$matches)) {
            $str .= $matches[1] . " ";
        }
        $name = $func->getName();
        $str .= $name . "";
        $str .= "(";
        if ($func->getNumberOfParameters() > 0) {
            foreach ($func->getParameters() as $param) {
                if ($param->isOptional()) {
                    $str .= "<opt> ";
                }
                if ($param->isArray()) {
                    $str .= "array ";
                } else {
                    if (preg_match("~@param\s+([^ ]+)\s+\${$name}~",
                                   $func->getDocComment(),&$matches)) {
                        $str .= $matches[1];
                    }
                }
                if ($param->isPassedByReference()) {
                    $str .= "&";
                }
                $str .= "$" . $param->getName();
                if ($param->isDefaultValueAvailable()) {
                    $str .= " = ";
                    if ($param->isDefaultValueConstant()) {
                        $str .= $param->getDefaultValueConstantName;
                    } else {
                        $str .= "" . $param->getDefaultValue();
                    }
                }
                $str .= ", ";
            }
            // remove last comma
            $str = substr($str, 0, strlen($str) - 2);
        }
        $str .= ")";
        return $str;
    }

    /**
     * @param ReflectionClass $class
     * @return string
     */
    function build_class_string(ReflectionClass $class) {
        $str = "";
        $class->isFinal() && $str .= "final ";
        $class->isAbstract() && $str .= "abstract ";
        $str .= $class->isInterface() ? 'interface ' : 
            PHP_VERSION_ID > 50400 && $class->isTrait() ? 'trait ' : 'class ';
        $str .= $class->getName();
        $method = $class->getConstructor();
        $method && ($str .= " " . build_func_string($method));
         return $str;
    }
}
