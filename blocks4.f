\ Conventions:
\  Stack diagram: ( @ - @ ) @ = a block #

\ TODO:
\ [ ] Bank's cursors should be stored in the image not in the dictionary.

[defined] save [if] save [then]

empty
depend ramen/lib/a.f

: |  postpone locals| ; immediate


( --== block image ==-- )

64 megs constant /image  
/image buffer: image
create blkpath  #256 allot
s" prg/blocks/test.blk" blkpath place
: revert  blkpath count image /image @file ;
: save    image /image blkpath count file! ;

revert 

( --== block stuff ==-- )

: block  #2 rshift image + ;
: block> image - #2 lshift ;
: blocks  #2 rshift ;
: block+  #1024 + ;
: free?  c@ 0= ;
: enabled?  c@ 0<> ; 
: unclaim  0 swap c! ;
: claim  $ff swap c! ;
: >nfa   ;
: copy  blocks move ;
: delete  1 blocks erase ;
: ref!  swap block> swap ! ;
: ref@  @ block ;

( --== field stuff ==-- )

: field     create over , + does> @ + ;
: record    #16 field ;
: skip      bl parse 2drop ;
: reserve       #16 * + ;
: blockstruct  0 1 reserve ;
: \record    1 reserve skip ;
: offset+    ' >body @ + ;


( --== bank stuff ==-- )

: bank   create   ( start: ) 1024 * ,  does> @ block ;
: header  ;
: >first  ( bank - adr ) block+ ;
: >current  ( bank - adr ) dup @ blocks + ;
: not0  dup ?exit 1 + ;
: +cursor  ( bank - bank )  dup @ 1 + 1023 and not0 over ! ;
: one  ( bank - adr )
    +cursor
    1023 for
        dup >current free? if
            >current dup claim
        unloop ;then
    +cursor
    loop true abort" No more left!"
;
: <name>  >in @ bl parse rot >in ! ;
: $  ( - <bank> <name> adr )  \ find a named block
    ' execute >first
    1023 for
        dup >nfa ccount
        <name> compare 0= if skip unloop ?literal ;then
        1 +
    loop  abort" Not found!"
; immediate
: clear-bank  ( bank - )
    header 0 over !  \ reset cursor
     >first 1023 blocks erase ;
: each>  ( bank - )  ( block - )
    r>  swap >first 1023 for
        dup enabled? if
            2dup 2>r swap call 2r>
        then
        block+        
    loop 2drop 
;


( --== basic editing tools ==-- )

0 value this
: num  bl parse evaluate ;
: edit  ( adr - )    to this   this #128 dump ;
: set  this offset+ a!>
    begin /source nip while
        bl parse over c@ [char] " = if
            #1 /string #1 - a@ cplace
        else
            evaluate !+
        then  
    repeat 
;
: 4. 2swap 2. 2. ;
: what's  this offset+ dup 4@ 4. #16 dump ;
: print  this offset+ ccount type ;


( --== asset stuff ==-- )

blockstruct
    record path 7 reserve
    record handle
value /assetheader




( --== scene stuff ==-- )

0
    record tilemap-config   ( tilebase, tileset-pic )
    record parallax         ( x, y )
    record scroll-offset    ( x, y )
    record priority
    record bounds           ( x, y, w, h )
drop #64 value /layer

blockstruct
    record world       \ block#
    record scenemask   \ bitmask that defines which actors will be included  ( TBD )
    record bgm         \ ( TBD ) probably a general sound #, which can optionally stream a file
                       \ could add extra params like volume and pitch
    record scroll
    drop #128      \ header
    
    /layer field layer0 
    /layer field layer1
    /layer field layer2
    /layer field layer3
value /scene





( ~~~~ data structure explanation ~~~ )

( A bank is 1024 blocks. )
( The first 16 banks are reserved for the engine. )
( You should name all of your banks via BANK . )

( A record is 16 bytes. They can hold 1-4 numbers or a string up to 15 chars. )
( Records can sometimes occupy multiple slots. )
( Not every kind of block uses records, for example actors. )

( Tilemaps are 512x512-tile arrays [1 tile = 1 cell], taking up a single bank slot. )
( A scene brings tilemaps and actors together.  A bitmask selects which of a bank of
( actors will be included in the scene.  )

( An animation is 16 bytes, a count and up to 14 indexes plus a loop offset. )
( Actually you can define an animation that's longer than 14, it just will spill }
( into the next animation slot, making it unusable for its own animation. )

( A world is a bank of actors. )

( There are a maximum of 1024 of the following: )
( - Pics )
( - Scenes )
( - Sounds, including BGM's )


( --== Engine memory layout ==-- )

: system  0 block ;
1 bank pics
2 bank sounds
3 bank scenes
4 bank templates  \ a place to store actors for instantiating in scenes
8 bank world0     \ the default world


0 value /system

: global  /system swap field to /system does> @ system + ;

cell global curScene     \ current scene (block)


: scene  scenes  + block ;


( --== Actor stuff ==-- )

0 value me
blockstruct value /actor  \ space for mark etc

: var  /actor cell field to /actor  does>  @ me + ;

\ SET works with these.

var id 
var scenebits  \ defines which scenes this actor will appear in
var dead    \ if on, will be deleted at end of frame.
var x
var y
var vx
var vy
var state#
var hid     \ if hid is off and pic# is 0, a rectangle is drawn (using the solid hitbox)
var picsrc
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


( --== Assumption ==-- )

create mestk  0 , 16 cells allot
: stage  curScene @ block world ref@ ;
: as  s" to me" evaluate ; immediate
: i{ ( actor - ) me mestk dup @ cells + cell+ !  mestk @ 1 + 15 and mestk ! as ;
: i} ( - ) mestk @ 1 - 15 and mestk !  mestk dup @ cells + cell+ @ as ; 
: {  ( actor - ) state @ if s" me >r as" evaluate else  i{  then ; immediate
: }  ( - ) state @ if s" r> as" evaluate else  i}  then ; immediate


( --== Scene stuff ==-- )

: init-scene ( world scene# - ) 
    world ref!
;
: filter-scene  ( scene actor-bank - )  \ removes excluded actors 
    | b s |
    b each>
    scenebits @ s scenemask @ and 0= if
        me delete
    then
;
: load-scene  ( src-scene dest-scene - )
    | s2 s1 |
    s2 world @
        s1 s2 1 copy  \ copy the headers
    s2 world !
    s1 world ref@ s2 world ref@ 1024 copy  \ copy the world
    s2 dup world ref@ filter-scene  \ filter out excluded actors
;



( --== Pic stuff ==-- )

/assetheader
    record subsize
    dup constant animations
drop

: animation  ( n pic@ - adr count )  block animations swap #16 * + ccount ;
: set-animation  ( - <anim#> <frame> <frame> <frame> ... )
    num this animation a!>
    0 c!+
    begin /source nip while
        bl parse evaluate 1i c!+
        #1 over c+!
    repeat
    drop
;

: load-pic  ( pic - )
    >r
        r@ path ccount zstring al_load_bitmap  r@ handle !
    r> drop
;

: add-pic  ( - <path> <name> )
    pics one to this
    bl parse this path cplace
    bl parse this >nfa cplace
    16 this subsize !
    this load-pic
;

: draw-tile  ( n pic - )  \ only 16x16 supported for now, and no flipping
    handle @   swap 16 /mod 16 16 2*  16 16   0 bblit ;

: draw  ( - )  \ draw current actor
    x 2@ at  sub# @ picsrc ref@ draw-tile ;


( --== Some startup stuff ==-- )


: load-pics
    pics each> load-pic 
;

: go
    show>
        black backdrop
        stage each> as draw
;

load-pics
go

\ stage one as # pics myconid picsrc ref!
320 240 resolution

cr .( Loaded Blocks4.)