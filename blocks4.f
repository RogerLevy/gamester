\ Conventions:
\  Stack diagram: ( @ - @ ) @ = a block #

\ TODO:
\ [x] Bank's cursors should be stored in the image not in the dictionary.
\ [ ] SET needs to work with block refs

[defined] save [if] save [then]

empty
depend ramen/lib/a.f

: |  postpone locals| ; immediate

\ TBD: PROJECT needs to be cleared in exported games.
: >projectPath  project count 2swap strjoin ;
: >dataPath     project count s" data/" strjoin 2swap strjoin ;
: <word>   bl word ccount ;

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
: skip      <word> 2drop ;
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
: named  ( block - <name> )
    <word> rot >nfa cplace ;
: <name>  >in @ <word> rot >in ! ;
: ($)  ( bank - <name> adr )
    >first
    1023 for
        dup >nfa ccount
        <name> compare 0= if skip unloop ?literal ;then
        1 +
    loop  abort" Not found!"
;
: $  ( - <bank> <name> adr )  \ find a named block
    ' execute ($) ; immediate
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
: num  <word> evaluate ;
: edit  ( adr - )    to this   this #128 dump ;
: set  this offset+ a!>
    begin /source nip while
        <word> over c@ [char] " = if
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

( There are a maximum of 1023 of the following: )
( - Pics )
( - Scenes )
( - Sounds, including BGM's )
( - Roles )
( - Templates )
( - Actors in a world )

( The number of available banks is dependent on the image size. )

( --== Engine memory layout ==-- )

: system  0 block ;
1 bank pic
2 bank sound
3 bank scene
4 bank template  \ a place to store actors for instantiating multiple times in different worlds
5 bank role      \ like classes, but just for actors
8 bank world0    \ the default world


0 value /system

: global  /system swap field to /system does> @ system + ;

cell global curScene     \ current scene (block)



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
var woke    \ if woke is off, state isn't executed.
var hid     \ if hid is off and pic# is 0, a rectangle is drawn (using the solid hitbox) TBD
var >role
var state#
var >pic
var sub#
var anim#   
var animctr
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
var var1 var var2 var var3 var var4 var var5 var var6 var var7 var var8
var var9 var var10 var var11 var var12 var var13 var var14 var var15 var var16


( --== Assumption ==-- )

create mestk  0 , 16 cells allot
: stage  curScene @ block world ref@ ;
: as  s" to me" evaluate ; immediate
: i{ ( actor - ) me mestk dup @ cells + cell+ !  mestk @ 1 + 15 and mestk ! as ;
: i} ( - ) mestk @ 1 - 15 and mestk !  mestk dup @ cells + cell+ @ as ; 
: {  ( actor - ) state @ if s" me >r as" evaluate else  i{  then ; immediate
: }  ( - ) state @ if s" r> as" evaluate else  i}  then ; immediate


( --== Role stuff ==-- )


/assetheader value /roleheader
#512 0 field vectors drop
1 value nextVector#  \ 0 state is NOOP


: runvec  ( ... n - ... )   cells >role ref@ vectors + @ execute ;
: act  woke @ -exit  state# @ runvec ;
: already  defined if >body cell+ @ $01234567 = ;then  drop false ;
: (?action)
    >in @ already if drop ;then
        >in ! create nextVector# , $01234567 , 1 +to nextVector#
        does>  @ runvec
;
: ?action  >in @ (?action) >in ! ;
: (?state)
    >in @ already if drop ;then
        >in ! create nextVector# , $01234567 , 1 +to nextVector#
        does>  woke on @ dup state# ! runvec
;
: ?state >in @ (?state) >in ! ;
: ?alias  ( xt - xt ) \ if alias return the XT it points to
    dup >body cell+ @ $C0FFEE = if @ then
;
: action: ( - <role> <name/alias> ...code... ; ) ( ... - ... ) \ doesn't set state#
    role ($) 
    ?action  \ create if doesn't exist
    vectors ' ?alias >body @ cells + :noname swap ! 
;
: state:  ( - <role> <name/alias> ...code... ; ) ( - ) \ sets state#
    role ($) 
    ?state  \ create if doesn't exist
    vectors ' ?alias >body @ cells + :noname swap ! 
;
: alias:  ( - <src> <name> )
    ' create , $C0FFEE ,
;
: load-role  ( role - )
    >r
        r@ path ccount >projectPath included
    r> drop
;
: add-role ( - <name> <path> )
    role one dup named   to this
    <word> this path cplace
    this load-role
;

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
    0 field animations
constant /pic

: animation  ( n pic - adr )  animations swap #16 * + ;
: set-animation  ( - <anim#> <frame> <frame> <frame> ... )
    num this animation dup a!>
    0 c!+  \ initialize length
    decimal
    begin /source nip while
        <word> evaluate c!+
        #1 over c+!
    repeat
    drop
    fixed
;

: load-pic  ( pic - )
    >r
        r@ path ccount >dataPath zstring al_load_bitmap  r@ handle !
    r> drop
;

: add-pic  ( - <name> <path> )
    pic one dup named   to this
    <word> this path cplace
    16 this subsize !
    this load-pic
;

: draw-tile  ( n pic - )  \ only 16x16 supported for now, and no flipping
    handle @   swap 16 /mod 16 16 2*  16 16   0 bblit ;


: sub@
    anim# @ >pic ref@ animation >r
    r@ c@ if
        r@ #1 + animctr @ 1i r@ c@ mod + c@ 1p sub# !
        rate @ animctr +!
    then
    r> drop
    sub# @
;

: draw  ( - )  \ draw current actor
    hid @ ?exit
    x 2@ at  sub@ >pic ref@ draw-tile ;

: animate  ( n speed - )
    rate ! 0 animctr ! anim# ! ;

( --== Some startup stuff ==-- )

(?action) start

: load-pics    pic each> load-pic ;
: load-roles   role each> load-role ;
: restart-all  stage each> as start ;

: go
    load-pics
    load-roles
    show>
        black backdrop
        stage each> as
        draw act
        \ 2 rnd 2 rnd 1 1 2- x 2+!
;

go

\ stage one named myboy as  $ pic myconid picsrc ref!
320 240 resolution

cr .( Loaded Blocks4.)