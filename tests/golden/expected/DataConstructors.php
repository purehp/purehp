<?php
/**
 * generated by purehp version 0.1.0.0
 */
declare(strict_types=1);

class DataConstructors {
    public function __construct() {
        $this->A = new class {

        };
        $this->bar3 = B::create(1)(2);
        $this->bar2 = new B(1, 2, 3);
        $this->bar1 = $A;
    }
    public static function NewFoo($x) {
        return $x;
    }
    class B {
        var $value0;
        var $value1;
        var $value2;
        public function __construct($value0, $value1, $value2) {
            $this->value0 = $value0;
            $this->value1 = $value1;
            $this->value2 = $value2;
        }
        public static function create($value0) {
            return fn($value1) => 
                fn($value2) => 
                    new self($value0, $value1, $value2);
        }
    }
}

?>
