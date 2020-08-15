<?php
/**
 * generated by purehp version 0.1.0.0
 */
declare(strict_types=1);

class Functions {
    public function __construct() {
        $this->scope = ['__Functions_Inner' => new \Functions\Inner()];
        $this->foo = self::fun2(1)(2);
        $this->bar = ($this->scope["__Functions_Inner"])->fun3(5)(5);
    }
    public static function fun2($a) {
        return function ($b) use ($a) {
            return ($this->scope["__Functions_Inner"])->fun3($a)($b);
        };
    }
}

?>
