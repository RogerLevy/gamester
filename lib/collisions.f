( --== Tilemap collisions ==-- )

( what sides the object collided )
0 value lwall?
0 value rwall?
0 value floor?
0 value ceiling?

#1
    bit BIT_CEL
    bit BIT_FLR
    bit BIT_WLT
    bit BIT_WRT
drop

define collisioning
    0 value map
    0 value tileprops

    defer on-collide-tilemap  ( -- )
        ' noop is on-collide-tilemap

    : tileprops@   ( tilespec -- flags )
        1i $ffff and tileprops + c@ ;

    : map@  ( col row - tile )  map adr @ ;

    : cel? BIT_CEL and ; \ ' ceiling '
    : flr? BIT_FLR and ; \ ' floor '
    : wlt? BIT_WLT and ; \ ' wall left '
    : wrt? BIT_WRT and ; \ ' wall right '
    
    : vector   create 0 , here 0 , constant ;
    vector nx ny
    
    16 constant gap
    
    : px x @ ;
    : py y @ ;

    variable t
    : xy>cr  ( x y tilesize - ) dup  2/  2pfloor ;
    : pt  gap xy>cr  map@ dup t !  tileprops@ ;          \ point

    ( increment coordinates )
    : ve+  swap gap +  sbw @ #1 - px +  min  swap ;
    : he+  gap +  sbh @ #1 - ny @ +  min ;

    : +ny ny +! 0 vy ! ;
    : +nx nx +! 0 vx ! ;

    ( push up/down )
    : pu ( xy ) nip gap mod negate +ny  true to floor?  on-collide-tilemap  ;
    : pd ( xy ) nip gap mod negate gap + +ny  true to ceiling?  on-collide-tilemap ;

    ( check up/down )
    : cu sbw @ gap / 2 + for 2dup pt cel? if pd unloop exit then ve+ loop 2drop ;
    : cd sbw @ gap / 2 + for 2dup pt flr? if pu unloop exit then ve+ loop 2drop ;

    ( push left/right )
    : pl ( xy ) drop gap mod negate +nx  true to rwall?  on-collide-tilemap ;
    : pr ( xy ) drop gap mod negate gap + +nx  true to lwall?  on-collide-tilemap ;

    ( check left/right )
    : cl sbh @ gap /  2 + for 2dup pt wrt? if pr unloop exit then he+ loop 2drop ;
    : crt sbh @ gap / 2 + for 2dup pt wlt? if pl unloop exit then he+ loop 2drop ;

    : ud vy @ -exit vy @ 0 < if px ny @ cu exit then px ny @ sbh @ + cd ;
    : lr vx @ -exit vx @ 0 < if nx 2@ cl exit then   nx @ sbw @ + ny @ crt ;

    : init   px py  vx 2@  2+  nx 2!  0 to lwall? 0 to rwall? 0 to floor? 0 to ceiling? ;

common also collisioning

: collide-tilemap> ( tilemap tileprops -- <code> )  ( -- )
    to tileprops to map  r> code> is on-collide-tilemap  init ud lr
    nx 2@ x 2! ;

: collide-tilemap  ( tilemap tileprops -- )
    collide-tilemap> noop ;

0 value (code)
: each-tile>  ( tilemap -- <code> ) ( ... -- ... )  \ address is in A; you're in charge of incrementing
    r> (code) >r to (code)
    ibh @ 1 - gap / pfloor y @ gap / pfloor - 1 + for
        dup a!
        ibw @ 1 - gap / pfloor x @ gap / pfloor - 1 + for
            (code) call
        loop
        drop 512 cells +
    loop
    drop  r> to (code)
;

common