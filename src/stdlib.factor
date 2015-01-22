!# towards a Factor Standard Library
: bi* ( x y p q -- ) [ dip ] dip call ;
: bi@ ( x y quot -- ) dup bi* ;
: other ( a b -- o ) + 6 - abs ;
: bi ( x p q -- ) [ keep ] dip call ;
: keep ( -- ) over [ call ] dip ;
: fib ( n -- f ) dup 1 <= [ drop 1 ] [ [ 1 - fib ] [ 2 - fib ] bi + ] if ;
: fact ( n -- n ) dup 0 <= [ drop 1 ] [ dup 1 - fact * ] if ;
: not ( n -- n ) [ f ] [ t ] if ;
: ? ( -- ) rot [ drop ] [ nip ] if ;
: dip2 ( -- ) swap [ dip ] dip ;
: keep2 ( -- ) swap [ dip ] dip ;