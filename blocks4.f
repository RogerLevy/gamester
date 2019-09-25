\ Conventions:
\  Stack diagram: ( @ - @ ) @ = a block #

\ TODO:
\ [x] Bank's cursors should be stored in the image not in the dictionary.
\ [ ] SET needs to work with block ref vars

[defined] save [if] save [then]

empty
only forth definitions
\ depend ws/ws.f
depend ramen/lib/rsort.f
depend ramen/lib/a.f
depend ramen/lib/std/kb.f
depend venery/venery.f

define Gamester

( --== Utilities ==-- )

: |  postpone locals| ; immediate
: <word>   bl word ccount ;
: <name>  >in @ <word> rot >in ! ;
: skip      <word> 2drop ;
: echo  cr 2dup type ;
: 4. 2swap 2. 2. ;

\ TBD: PROJECT needs to be cleared in exported games.
: >dataPath     project count s" data/" strjoin 2swap strjoin ;
: >rolePath     project count s" roles/" strjoin 2swap strjoin ;

( --== block image ==-- )

64 megs constant /image  
/image buffer: image
create blkpath  #256 allot
s" prg/blocks/test.blk" blkpath place
: revert  blkpath count image /image @file ;
: save    image /image blkpath count file! ;
:make bye   save  al_uninstall_system  0 ExitProcess ;

revert


( --== block stuff ==-- )

: block  #2 rshift image + ;
: block> image - #2 lshift ;
: blocks  #2 rshift ;
: block+  #1024 + ;
: free?  c@ 0 = ;
: enabled?  c@ 0 <> ; 
: claim  on ;
: >nfa   ;
: copy  blocks move ;
: delete  #1 swap c! ;
: >!  swap block> swap ! ;
: @>  @ block ;

( --== field stuff ==-- )

: field     create over , + does> @ + ;
: record    #16 field ;
: reserve       #16 * + ;
: \record    1 reserve skip ;
: offset+    ' >body @ + ;

0
    1 reserve      \ reserve the name field
    cell field id
drop #32 constant blockstruct

blockstruct
    record systemType   \ word
drop #64 constant /systemblock  


( --== Globals ==-- )

blockstruct value /system

: system  0 block ;  \ kludge, redefined in more logical way below
: global  /system swap field to /system does> @ system + ;
: \global  +to /system  0 parse 2drop ;

cell global gameSlew     \ current slew (block)
cell global tool         \ current tool  (block)
cell global nextid       \ next global ID (incremented by ONE)
cell global lasttool     \ last tool that was RUN (block#)
#512 to /system


( --== bank stuff ==-- )

#1024 1024 * constant /bank
: >bank  ( block - bank )   image /bank mod - dup   /bank mod -  image /bank mod + ;
: bank   create   ( start: ) 1024 * ,  does> @ block ;
: >first  ( bank - adr ) block+ ;
: >current  ( bank - adr ) dup @ blocks + ;
: not0  dup ?exit 1 + ;
: +cursor  ( bank - bank )  dup @ 1 + 1023 and not0 over ! ;
: one  ( bank - adr )
    +cursor
    1023 for
        dup >current free? if
            1 nextid +!
            >current dup 1 blocks erase  dup claim  nextid @ over id !
        unloop ;then
    +cursor
    loop true abort" No more left!"
;
: named  ( block - <name> )
    <word> rot >nfa cplace ;

: (?$)  ( bank - <name> adr|0 )
    >first
    1023 for
        dup >nfa ccount
        <name> compare 0 = if skip unloop
            state @ if  block> postpone literal postpone block  then
        ;then
        block+
    loop   drop  0
;
: ($)  (?$) dup 0 = abort" Not found!" ;

: $(  ( - <bank> <name> adr )  \ find a named block; ex: $( pic myconid )
    ' execute ($) skip ; immediate
: clear-bank  ( bank - )
    0 over !  \ reset cursor
     >first 1023 blocks erase ;
: each>  ( bank - )  ( block - )
    r>  swap >first 1023 for
        dup enabled? if
            2dup 2>r swap call 2r>
        then
        block+        
    loop 2drop 
;
: list  ( bank - )
    each> cr dup @ #-1 = if h. else >nfa ccount type then ;


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

( A slew is a scene combined with a bank of actors. )

( There are a maximum of 1023 of each of the following: )
( - Pics )
( - Scenes )
( - Sounds, including BGM's )
( - Roles )
( - Templates )
( - Actors in a slew )

( The number of available banks is dependent on the image size. )

( --== Engine memory layout ==-- )

0 bank system
1 bank pic
2 bank sound
3 bank scene
4 bank template  \ a place to store actors for instantiating multiple times in different stages
5 bank role      \ like classes, but just for actors
6 bank gui       \ slew; default for tool actors
8 bank playfield \ slew; default for game actors



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
: what's  this offset+ dup 4@ 4. #16 dump ;
: print  this offset+ ccount type ;


( --== asset stuff ==-- )

blockstruct
    record path 7 reserve
    record handle
value /assetheader    



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
        r@ path ccount >dataPath echo zstring al_load_bitmap  r@ handle !
    r> drop
;

: add-pic  ( - <name> <path> )
    pic one dup named   to this
    <word> this path cplace
    16 16 this subsize 2!
    this load-pic
;

decimal
: draw-tile  ( n pic - )  
    over >r dup >r  handle @   swap 16.0 /mod 4 lshift swap 4 lshift swap r> subsize 2@  r> 28 rshift bblit ;
fixed


( --== Tilemap stuff ==-- )

: adr  ( col row adr - adr )
    >r 2pfloor  512 * + cells r> + ;

: draw-tilemap  ( adr pic - )
    a@ | a p |  hold>
    viewh p subsize @ / 1 + for
        dup a! at@ 2>r
        vieww p subsize @ / 1 + for
            @+ p draw-tile p subsize @ 0 +at
        loop
        2r> p subsize @ + at
        512 cells +
    loop drop
    a a!
;


( --== Actor stuff ==-- )

0 value me
blockstruct value /actor  \ space for mark etc
create mestk  0 , 16 cells allot

: var  /actor cell field to /actor  does>  @ me + ;
: alias  ( - <old> <new> ) ' >body @ field drop  does> @ me + ;

\ SET works with these.

var zorder
var scenebits  \ defines which scenes this actor will appear in
var dead    \ if on, will be deleted at end of frame.
var x
var y
var vx
var vy
var tintr
var tintg
var tintb
var tinta
var sx
var sy
var rtn
var >role
var state#
var >pic
var sub#
var anim#   
var animctr
var rate    \ animation speed
/actor value /simple  \ for particles ... < 128 bytes

var woke    \ if woke is off, state isn't executed.
var hid     \ if hid is off and pic# is 0, a rectangle is drawn (using the solid hitbox)
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
var rolename #12 +to /actor
\ predefined common variables
var var1 var var2 var var3 var var4 var var5 var var6 var var7 var var8
var var9 var var10 var var11 var var12 var var13 var var14 var var15 var var16
/actor value /common
#768 to /actor   \ reserve 768 bytes
\ the remaining space is considered "volatile" and can be cleared at any time by the engine.
\ predefined reference variables
var ref1  var ref2  var ref3  var ref4  var ref5  var ref6  var ref7  var ref8

( --== Assumption ==-- )

: as  s" to me" evaluate ; immediate
: i{ ( actor - ) me mestk dup @ cells + cell+ !  mestk @ 1 + 15 and mestk ! as ;
: i} ( - ) mestk @ 1 - 15 and mestk !  mestk dup @ cells + cell+ @ as ; 
: {  ( actor - ) state @  if s" me >r as" evaluate else  i{  then ; immediate
: }  ( - ) state @ if  s" r> as" evaluate else  i}  then ; immediate


( --= Template stuff ==-- )

: instance  ( template slew -- actor )
    one {
        me 1 copy
        $ff me !   \ invalidates the name
        at@ x 2!
    me }
;

: role!  ( role -- )
    dup role ! >nfa ccount rolename cplace ;


( --== Slew stuff ==-- )

0 value xt
: shout  ( xt slew )
    swap to xt each> as xt execute ;

( --== Role stuff ==-- )

/assetheader value /roleheader
#512 0 field vectors drop
1 value nextVector#  \ 0 state is NOOP

: vexec  swap cells + @ execute ;
: runvec  ( ... n - ... )   >role @> vectors vexec ;
: act  woke @ -exit  state# @ runvec ;
: already  defined if >body cell+ @ $01234567 = ;then  drop false ;
: rolefield  create /roleheader , +to /roleheader does> @ + ;
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
    vectors ' ?alias >body @ cells + :noname swap !  \ TBD : should only set if compilation is without errors
;
: state:  ( - <role> <name/alias> ...code... ; ) ( - ) \ sets state#
    role ($) 
    ?state  \ create if doesn't exist
    vectors ' ?alias >body @ cells + :noname swap ! 
;
: load-role  ( role - )
    >r
        r@ path ccount >rolePath included
    r> drop
;
: add-role ( - <name> <path> )
    role one dup named   to this
    <word> this path cplace
    this load-role
;
: add-template  ( - <name> )
    template one dup as  named  
;


( --== Scene stuff ==-- )

0
    record tilemap-config   ( block#, tileset-pic )
    : tilemap-block   tilemap-config ;
    : tileset-pic     tilemap-config cell+ ;
    record parallax         ( x, y )
    record scroll-offset    ( x, y )
    record bounds           ( x, y, w, h ) 
    record viewport         ( x, y, w, h ) 
drop #128 value /layer  

blockstruct
    \record >slew      \ block# (unused)
    record scenemask   \ bitmask that defines which actors will be copied when loading to this scene
    record bgm         \ ( TBD ) probably a general sound #, which can optionally stream a file
                       \ could add extra params like volume and pitch
    record scroll
    record res
value /sceneheader
#512
    /layer field layer0 
    /layer field layer1
    /layer field layer2
    /layer field layer3
value /scene

create layer-template  /layer /allot
layer-template to this
    1 1 this parallax 2!
    0 0 8192 8192 this bounds 4!
    0 0 viewwh this viewport 4!

: init-scene ( scene - ) 
    >r
    layer-template r@ layer0 /layer move
    layer-template r@ layer1 /layer move
    layer-template r@ layer2 /layer move
    layer-template r@ layer3 /layer move
    viewwh r@ res 2!
    r> drop 
;
: filter-slew  ( src-scene slew - )  \ removes excluded actors 
    | b s |
    b each>
    scenebits @ s scenemask @ and 0 = if
        me delete
    then
;
: limit-scroll  ( scrollx scrolly layer - scrollx scrolly )
    >r
    r@ bounds xy@ 2max
    r@ bounds wh@ viewwh 2- 2min
    r> drop
;
: draw-layer ( scrollx scrolly layer - )
    dup tilemap-config 2@ swap block swap block dup subsize @
        | tsize pic baseadr layer scrolly scrollx |
    pic block> 0 = ?exit
    layer viewport xy@ at
    layer viewport 4@ clip>
    scrollx scrolly layer parallax 2@ 2*  layer scroll-offset 2@ 2+ 
        layer limit-scroll 2dup
        tsize dup 2mod 2negate +at
        tsize dup 2/ 2pfloor 512 * + cells baseadr + pic draw-tilemap
;
: stage  gameSlew @> ;
: init-layer  ( tilemap tileset-pic layer -- )
    >r
        r@ tileset-pic >! r@ tilemap-block >!
        1 1 r@ parallax 2!
        0 0 r@ scroll-offset 2!
        0 0 viewwh r@ viewport 4!
        0 0 8192 8192 r@ bounds 4!
    r> drop
;

( --== Actor rendering ==-- )

: sub@
    anim# @ >pic @> animation >r
    r@ c@ if
        r@ #1 + animctr @ 1i r@ c@ mod + c@ 1p sub# !
        rate @ animctr +!
    then
    r> drop
    sub# @
;

create colors  ' blue , ' green , ' red , ' orange , ' yellow , ' magenta , ' cyan , ' pink , 

: placeholder  ( - )
    x 2@ sbx 2@ 2+ at  me id @ 8 mod colors vexec  sbw 2@ rectf ;

defer draw

: draw-sprite  ( - )
    >pic @ 0 = if  placeholder  white  ;then
    x 2@  gameSlew @> scroll 2@ 2-  at  sub@ >pic @> draw-tile ;

' draw-sprite is draw

: animate  ( n speed - )
    rate ! 0 animctr ! anim# ! ;

( --== Z-sorted rendering ==-- )

create drawlist 1023 cells /allot
: gathered  drawlist dup a!> 0 rot each> !+ 1 + ;
: zorder@  { zorder @ } ;
: draws  ( slew - )
    gathered 2dup ['] zorder@ rsort swap a!> for @+ { draw act } loop ;

: (resolution)  ( scene -- )
    >r
    r@ res 2@ or 0 = if viewwh r@ res 2! then
    r> res 2@ resolution
;
: draw-scene ( scene - )
    me >r >r
    r@ (resolution)
    r@ scroll 2@ r@ layer0 draw-layer 
    r@ scroll 2@ r@ layer1 draw-layer 
    r@ draws
    r@ scroll 2@ r@ layer2 draw-layer 
    r@ scroll 2@ r@ layer3 draw-layer 
    r> drop r> as
;


( --== Additional commands ==-- )

: t( ( -- <name> <> template )   \ ex: t( myconid )
    template ($) skip ; immediate
: pic( ( -- <name> <> pic )     \ ex: pic( myconid )
    pic ($) skip ; immediate
: scene( ( -- <name> <> scene )   \ ex: scene( default )
    scene ($) skip ; immediate
: sound( ( -- <name> <> sound )   \ ex: sound( bang )
    sound ($) skip ; immediate
: role( ( -- <name> <> role )   \ ex: role( myconid )
    role ($) skip ; immediate
: import  ( -- <name> <scriptpath> )
    >in @  add-role  >in !
    >in @  add-template  >in !
    >in @  role ($) >role >!  >in ! 
    >in @  pic (?$) ?dup if  >pic >!  then  >in !
    skip skip
;
: add-actor  ( -- <template> actor )
    template ($)  stage instance dup as ;
: update  ( -- <role> )
    s" ld " role ($) path ccount >rolepath -ext strjoin evaluate ;
: u  update ;

( --== Tools stuff pt 1 ==-- )

0 value installing?

: toolfield  ( size -- <name> )
    field does> @ tool @> + ;

/systemblock
    #96 toolfield toolSource
    #32 toolfield starter  \ word
    #32 toolfield resumer  \ word
    #16 toolfield vocab    \ word
    cell toolfield >toolScene
constant /tool

: load-tool  ( tool -- )
    tool @> >r  tool >!
    warning on
    only forth also Gamester definitions
    toolSource ccount included
    warning off
    r> tool >!
;

( --== Some startup stuff ==-- )

defer resume

: load-pics    pic each> load-pic ;
: load-roles   role each> load-role ;
: load-systems system each>  false to installing? load-tool ;

: asdf  quit ;

: quit
    only forth also Gamester definitions
    0 to 'step  0 to 'pump
    tool @ lasttool !
    tool off
    ['] draw-sprite is draw
    show>
        <`> pressed if resume ;then
        <s> pressed ctrl? and if save then 
        black backdrop        
        stage draw-scene
;

: empty  only Forth definitions also empty ;

( --== Tool stuff pt 2 ==-- )

: tool-scene  >toolScene @> ;  
: install  ( -- <scriptpath> <name> )
    true to installing?
    quit
    system one tool >!
    s" tool" tool @> systemType cplace
    scene one
        dup init-scene
        >toolScene >!
    <word> toolSource cplace
    tool @> named
    tool @> load-tool
;
: contextualize  only forth also Gamester also  vocab ccount evaluate ;
:make resume ( -- )
    lasttool @ if lasttool @ tool ! then 
    tool @ 0 = if drop ;then
    contextualize
    resumer ccount evaluate
;
: run ( -- <name> )
    system ($) tool >!
    gui clear-bank
    contextualize
    starter ccount evaluate
;
: define-tool  ( - <name> flag )  \ defines a vocab and assigns it to the current tool
    only forth also Gamester definitions
    <name> vocab cplace
    >in @ postpone [undefined] swap >in !
    define
;
    
( ~~~~~~~~~~~~~~~~~~~~~~~~~ )

: warm
    load-pics
    load-roles
    load-systems
    tool @ if resume else quit then
;

warm

cr .( Loaded Blocks4.)
