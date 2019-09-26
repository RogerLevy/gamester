depend ramen/lib/a.f

64 megs buffer: image
create blkpath  #256 allot
s" prg/gamester/test.blk" blkpath place
: revert  blkpath count image 64 megs @file ;
: save    image 64 megs blkpath count file! ;
revert

: block  #2 rshift image + ;
: blocks  #2 rshift ;
: used?  @ 0< ;
: mark   on ;
: unmark  off ;
: init   dup 1 blocks erase mark ;


: (field)  create dup , ;

: record    (field) #16 + does> @ + ;
: (discard)  bl parse 2drop ;
: skip       #16 + ;
: blockstruct  0 skip ;
: \record    skip (discard) ;
: offset+    ' >body @ + ;





( pool header - first block of every pool is reserved )

blockstruct
record capacity
record cursor      \ offset
record stride      \ bytes
value /bankheader


( asset header )

blockstruct
record path skip skip skip skip skip skip skip
record handle
value /assetheader


( group layout )

: reserved  0 block ;
: pics      256 block ;   : pic  blocks pics + ;
: actors    2048 block ;  : actor   blocks actors + ;
: templates 16384 block ;  : template  blocks templates + ;    
: tilemaps  32768 block ; : tilemap  blocks tilemaps + ;


: pool  ( start len stride - )
   rot block >r  dup r@ stride ! r@ cursor ! 1 - r@ capacity ! ;



( basic editing tools )

0 value this
: num  bl parse evaluate ;
: edit  '   num  swap execute to this   this #256 dump ;
: set  this offset+ a!>
    begin /source nip while
        bl parse over c@ [char] " = if
            #1 /string #1 - a@ cplace
        else
            evaluate !+
        then  
    repeat 
;
: what's  this offset+ #16 dump ;
: print  ccount type ;


: (size)  dup capacity @ swap stride @ * ;

: +cursor
    >r  r@ stride @  r@ cursor @ +  r@ (size) mod  r> cursor ! ;

: (new)
    locals| bank |
    bank capacity @ for
        bank dup cursor @ + used? not if
            bank dup cursor @ + dup init
            to this
            bank +cursor
            unloop exit
        then
        bank +cursor
    loop
    abort" No more left!"
;

: new  
   bl parse
   2dup s" pic" compare 0= if 2drop pics (new) ;then
   2dup s" actor" compare 0= if 2drop actors (new) ;then
   abort" Not a valid asset type! ( PIC, ACTOR )" ;
   

( pic )
/assetheader
record subsize
dup constant animations  \ each animation is 16 bytes.  first byte is count.  (can spill over)
drop

: animation  ( n pic - adr count )  animations swap #16 * + ccount ;
: set-animation
    num this animation dup #1 + a!>
    begin /source nip while
        bl parse evaluate 1i c!+
        #1 over c+!
    repeat
    drop
;

: load-pic  ( n - )
    pic >r
        r@ path ccount al_load_bitmap  r@ handle !
    r> drop
;

: add-pic  ( - n <path> )
    num ( n ) dup pic to this
    0 parse this path cplace
    16 this subsize !
    ( n ) load-pic
;

: draw-tile  ( n pic - )  \ only 16x16 supported for now, and no flipping
    handle @   swap 16 /mod 16 16 2*  16 16   0 bblit ;



( actor )
0 value me
blockstruct value /actor  \ space for mark etc
: var  /actor (field) cell+ to /actor  does>  @ me + ;

\ SET works with these.

var id 
var dead    \ if on, will be deleted at end of frame.
var x
var y
var vx
var vy
var state#
var hid     \ if hid is off and pic# is 0, a rectangle is drawn (using the solid hitbox)
var pic#
var sub#
var anim#
var frame#
var rate    \ animation speed
var hp
var maxhp
var attr    \ attribute flags
var ctype   \ collision flags
var cmask   \ collision mask
var ibx     \ interaction hitbox
var iby
var ibw
var ibh
var sbx     \ solid hitbox
var sby
var sbw
var sbh
/actor value /actorbase
#512 to /actor   \ reserve 512 bytes

\ user variables:
var p1 var p2 var p3 var p4 var p5 var p6 var p7 var p8
var p9 var p10 var p11 var p12 var p13 var p14 var p15 var p16

ld prg/gamester/runtime

cr .( Loaded blocks3 )