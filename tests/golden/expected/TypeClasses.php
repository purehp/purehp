<?php
/**
 * generated by purehp version 0.1.0.0
 */
declare(strict_types=1);



class Foo {
    var $value0;
    public function __construct($value0) {
        $this->value0 = $value0;
    }
    public static function create($value0) {
        return new self($value0);
    }
}
class Functor {
    var $map;
    public function __construct($map) {
        $this->map = $map;
    }
}
$map = function ($dict) {
    return $dict->map;
};
$functorFoo = new Functor(function ($f) {
    return function ($v) use ($f) {
        $__0 = $f;
        $__1 = $v;
        $f1 = $__0;
        $__2 = $__1->value0;
        $i = $__2;
        return new Foo($f1($i));
        throw new $Error("Failed pattern match at TypeClasses (line 8, column 1 - line 9, column 28): " + [$__0->constructor->name, $__1->constructor->name]);
    };
});
$bar = $map($functorFoo)(function ($v) {
    return 2;
});
$baz = $bar(new Foo(1));

?>
