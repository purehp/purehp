<?php
/**
 * generated by purehp version 0.1.0.0
 */

namespace Cases;

$foo = function ($x) {
    $__0 = $x;
    if ($__0 === 1) {
        return 0;
    }
    if ($__0 === 2) {
        return 1;
    }
    return 2;
    throw new $Error("Failed pattern match at Cases (line 4, column 9 - line 7, column 9): " + [$__0->constructor->name]);
};
$bar = function ($x) {
    $__1 = $x;
    if ($__1) {
        return "True!";
    }
    return "False!";
    throw new $Error("Failed pattern match at Cases (line 10, column 9 - line 10, column 40): " + [$__1->constructor->name]);
};

?>