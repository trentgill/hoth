( Forth Standard Library )
( Words are implemented in Forth wherever possible )

1 2 3 . . .

( Stack Manipulations )
: NIP ( a b -- a ) SWAP DROP ;
: TUCK ( a b -- b a b ) DUP ROT SWAP ;
: OVER ( a b -- a b a ) SWAP TUCK ;
: 2DUP ( a b -- a b a b ) OVER OVER ;
: ?DUP ( a b -- a ?a ) IF DUP THEN ;

( Comparison Words )
: <0 ( a -- b ) 0 OVER < ;
: >0 ( a -- b ) 0 OVER > ;
: >= ( a b -- c b a ) 2DUP < NOT ;
: <= ( a b -- c b a ) 2DUP > NOT ;

( Some simple math words )
: SQ ( a -- a^2 ) DUP * ;
: CUBED ( a -- a^3 ) DUP DUP * * ;
: NEGATE ( a -- -a ) -1 * ;
: ABS  ( a -- |a| ) DUP <0 IF NEGATE THEN ;
: */ ( a b c -- a*b/c ) SWAP ROT * / ;
: *PI ( a -- 3a/4 ) 3142 1000 */ ;
: MIN ( ) 2DUP < IF DROP ELSE SWAP DROP THEN ;

( Partial Arithmetic Functions )
: 1+ ( a -- a+1 ) 1 + ;
: 1- ( a -- a-1 ) 1 SWAP - ;
: 2+ ( a -- a+2 ) 2 + ;
: 2- ( a -- a-2 ) 2 SWAP - ;
: 2* ( a -- a*2 ) 2 * ;
: 2/ ( a -- a/2 ) 2 SWAP / ;

