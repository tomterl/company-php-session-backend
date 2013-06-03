<?php

if(!defined("ELISP_INFO_LOADED")) {
    define("ELISP_INFO_LOADED", TRUE);
    define("RESULT_MARK_BEGIN", ";; -- php completion begin ;;");
    define("RESULT_MARK_END", ";; -- php completion end ;;");
    define("COMINT_END_MARKER", ";; -- php completion command finished ;;");

    function defined_internal_functions() {
        defined_functions("internal");
    }

    function defined_functions($type) {
        $funcs=get_defined_functions();
        print_result($funcs[$type]);        
    }

    function print_result($arr) {
        echo '' . RESULT_MARK_BEGIN . "\n(";
        sort($arr);
        foreach($arr as $item) {
            echo "\"{$item}\" "; 
        }
        echo ")\n" . RESULT_MARK_END . "\n";
        echo "" . COMINT_END_MARKER . "\n";
    }
}    
