\ Conventions:
\  Stack diagram: ( @ - @ ) @ = a block #

\ TODO:
\ [x] Bank's cursors should be stored in the image not in the dictionary.
\ [ ] SET needs to work with block ref vars

[defined] save [if] save [then]

only forth definitions
\ depend ws/ws.f
depend ramen/lib/rsort.f
depend ramen/lib/a.f
depend ramen/lib/std/kb.f
depend venery/venery.f

define Gamester

( --== Variables ==-- )

1 value nextVector#  \ 0 state is NOOP
0 value me
0 value this
0 value installing?

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
: ?datapath   2dup s" /" search nip nip if project count 2swap strjoin ;then  >dataPath ;
: ?rolePath   2dup s" /" search nip nip if project count 2swap strjoin ;then  >rolePath ;

( --== block image ==-- )

64 megs constant /image  
/image buffer: image
create blkpath  #256 allot
depth 0 = [if] s" default.blk" [then]
    blkpath place

: (blkpath)  project count blkpath count strjoin ;
: revert  (blkpath) image /image @file ;
: save    image /image (blkpath) file! ;
:make bye   save  al_uninstall_system  0 ExitProcess ;

(blkpath) file-exists not [if]
    (blkpath) r/w create-file drop close-file drop
    save
[else]
    revert
[then]


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

( --== Structures ==-- )

#1024 1024 * constant /bank

blockstruct
    record path 7 reserve
    record handle
constant /assetheader    

/assetheader constant /roleheader
#256 constant rolestruct
#512 0 field vectors drop

0
    record tilemap-config   ( block#, tileset-pic )
    : tilemap-block   tilemap-config ;
    : tileset-pic     tilemap-config cell+ ;
    record parallax         ( x, y )
    record scroll-offset    ( x, y )
    record bounds           ( x, y, w, h ) 
    record viewport         ( x, y, w, h ) 
drop #128 constant /layer  

blockstruct
    record scenemask   \ bitmask that defines which actors will be copied when loading to this scene
    record bgm         \ ( TBD ) probably a general sound #, which can optionally stream a file
                       \ could add extra params like volume and pitch
    record scroll
    record res
constant /sceneheader
#512
    /layer field layer0 
    /layer field layer1
    /layer field layer2
    /layer field layer3
constant /scene

create layer-template  /layer /allot
layer-template to this
    1 1 this parallax 2!
    0 0 8192 8192 this bounds 4!
    0 0 viewwh this viewport 4!

create mestk  0 , 16 cells allot
: actorvar  ( offset -- <name> offset+cell )  cell field  does>  @ me + ;
: alias  ( - <old> <new> ) ' >body @ field drop  does> @ me + ;

\ SET works with these.

blockstruct
    actorvar zorder
    actorvar scenebits  \ defines which scenes this actor will appear in
    actorvar dead    \ if on, will be deleted at end of frame.
    actorvar x
    actorvar y
    actorvar vx
    actorvar vy
    actorvar tintr
    actorvar tintg
    actorvar tintb
    actorvar tinta
    actorvar sx
    actorvar sy
    actorvar rtn
    actorvar >role
    actorvar state#
    actorvar >pic
    actorvar sub#
    actorvar anim#   
    actorvar animctr
    actorvar rate    \ animation speed
    actorvar woke    \ if woke is off, state isn't executed.
    actorvar hid     \ if hid is off and pic# is 0, a rectangle is drawn (using the solid hitbox)
constant simplestruct      \ for particles and environments

#256
    actorvar rolename #12 +
    actorvar attr    \ attribute flags
    actorvar ctype   \ collision flags
    actorvar cmask   \ collision mask
    actorvar ibx     \ interaction hitbox
    actorvar iby
    actorvar ibw
    actorvar ibh
    actorvar sbx     \ solid hitbox
    actorvar sby
    actorvar sbw
    actorvar sbh
    actorvar ref1  actorvar ref2  actorvar ref3  actorvar ref4  actorvar ref5  actorvar ref6  actorvar ref7  actorvar ref8
drop #512 constant commonvars

\ the remaining space is considered "volatile" and this is where roles should define their vars
#768 0 field volatile constant volatilevars

: system  0 block ;     \ kludge; redefined in more logical way below
: global  field does> @ system + ;
: \global  +  0 parse 2drop ;

blockstruct
    cell global gameSlew     \ current slew (block)
    cell global tool         \ current tool  (block)
    cell global nextid       \ next global ID (incremented by ONE)
    cell global lasttool     \ last tool that was RUN (block#)
drop
#256 constant globals

( --== bank stuff ==-- )

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
        r@ path ccount ?datapath echo zstring al_load_bitmap  r@ handle !
    r> drop
;

: add-pic  ( - <name> <path> )
    pic one dup named   to this
    <word> this path cplace
    16 16 this subsize 2!
    this load-pic
;

: draw-tile  ( n pic - )  
    over >r dup >r  handle @   swap 16 /mod #4 lshift swap #4 lshift swap r> subsize 2@  r> #28 rshift bblit ;


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

: vexec  swap cells + @ execute ;
: runvec  ( ... n - ... )   >role @> vectors vexec ;
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
    vectors ' ?alias >body @ cells + :noname swap !  \ TBD : should only set if compilation is without errors
;
: state:  ( - <role> <name/alias> ...code... ; ) ( - ) \ sets state#
    role ($) 
    ?state  \ create if doesn't exist
    vectors ' ?alias >body @ cells + :noname swap ! 
;
: load-role  ( role - )
    >r
        r@ path ccount ?rolePath included
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

: toolfield  ( size -- <name> )
    field does> @ tool @> + ;

/systemblock
    #128 toolfield toolSource
    #32 toolfield starter  \ word
    #32 toolfield resumer  \ word
    #16 toolfield vocab    \ word
    cell toolfield >toolScene
constant toolstruct

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
: contextualize  only forth also Gamester also  vocab ccount evaluate  definitions ;
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

cr .( Gamester: Loading block file... )
warm

cr .( Gamester: Done!  )
