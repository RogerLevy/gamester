\ Version 1.0

\ TODO:
\ [x] Bank's cursors should be stored in the image not in the dictionary.
\ [ ] SET needs to work with block ref vars
\ [ ] ACTION: and STATE: should only set XT if no errors during compilation 

create blkpath  #256 allot
depth 0 = [if] s" default.blk" [then]
blkpath place

include ramen/ramen.f

[defined] save [if] save [then]

only forth definitions
depend ramen/lib/rsort.f
depend ramen/lib/a.f
depend ramen/lib/std/kb.f
depend venery/venery.f
depend prg/gamester/lib/structs2.f

only forth definitions
define Gamester


only forth definitions
: common  only forth also Gamester definitions ;
common
: common-create  get-order get-current  common create  set-current set-order ;

( --== Variables ==-- )

0 value me                          \ current actor
0 value you                         \ other actor being collided with
0 value this                        \ current struct being edited
0 value installing?                 \ true when installing a module or tool
0 value newBlockFile?               \ true when initializing a new block file 
defer save-assets  ( -- )  :make save-assets ;
create mestk  0 , 16 cells allot


( --== Utilities ==-- )

: [dev]  s" [defined] dev" evaluate ; immediate
: |  postpone locals| ; immediate
: <word>   bl word ccount ;
: <name>  >in @ <word> rot >in ! ;
: skip      <word> 2drop ;
: echo  cr 2dup type ;
: 4. 2swap 2. 2. ;
: vexec  ( ... n vtable - ... ) swap cells + @ execute ;

: >prjPath    project count 2swap strjoin ;
: >dataPath   s" data/" >prjPath 2swap strjoin ;
: >rolePath   s" roles/" >prjPath 2swap strjoin ;
: ?datapath   2dup s" /" search nip nip ?exit  >dataPath ;
: ?rolePath   2dup s" /" search nip nip ?exit  >rolePath ;



( --== block image ==-- )

64 megs constant /image  
/image buffer: image

: (blkpath)
    blkpath count s" /" search nip nip if blkpath count ;then
    project count blkpath count strjoin
;
: revert  (blkpath) image /image @file ;
: save    image /image (blkpath) file! save-assets  cr ." Saved block image, and any modified assets." ;

(blkpath) file-exists not [if]
    (blkpath) r/w create-file drop close-file drop
    save
    true to newBlockFile?
[else]
    revert
[then]

( --== Struct stuff ==-- )

: ?overflow  dup #1024 >= abort" Block struct overflow." ;

: does-field  ; \ [dev] [if] does> dup   [then] ;
: field     ?overflow sfield does-field ;
: embed     dup sizeof ?overflow drop sembed does-field ;
: record    #16 field ;
: reserve   #16 * + ;
: offset+   ' >body @ + ;
: extend   dup sizeof ;

( --== Block stuff ==-- )

: inspect-typeid  ( adr size -- ) over @ if type else 2drop ." null" then space ;
s" BTYP" ' inspect-typeid fieldtype: <typeid

struct: %block
    1 reserve      \ reserve the name field
    cell field id      <fixed
    cell field >chain  <fixed
    cell field typeid  <typeid
    cell field lock    <flag
drop #32 ;struct

struct: %bank   
    %block embed blockheader
    cell field cursor  <fixed
drop #64 ;struct

struct: %module
    %block embed blockheader
    record moduleType   <cstring
    record vocab        <cstring
    #60 field source    <cstring
    cell field vtable   <addr
drop #128 ;struct

: block  #2 rshift image + ;
: block> image - #2 lshift ;
: blocks  #2 rshift ;
: block+  #1024 + ;
: free?  c@ 0 = ;
: locked?  lock @ ;
: claim  on ;
: >nfa   ;
: >!  swap block> swap ! ;
: @>  @ [dev] [if] dup 0 = abort" Invalid reference!" [then] block ;
: ?@>  @ dup if block then ;
: delete   dup locked? if drop ;then begin dup off >chain @ ?dup while block repeat ;
: chain  ( src dest - ) begin dup >chain @ dup while nip repeat drop >chain >! ;
: copy  #16 #16 2+ 1 blocks #16 - move ;
: ?null  ?dup 0 = if s" Not a block." r> drop then ;
: block>name  ?null dup block> 1i #4 (h.0) s" /" strjoin 2>r dup @ #-1 <> if >nfa ccount 2r> 2swap strjoin else drop 2r> then ;
: .block  block>name type space ;
: .name  >nfa ccount type space ;

( --== Structures ==-- )

#1024 1024 * constant /bank

struct: %asset
    %block embed blockheader
    #64 field path    <cstring
    record handle     <addr
    record modified   <flag
drop #256 ;struct

struct: %layer
    record tilemap-config   <fixed  ( ~tilemap, ~tileset )
    : >tilemap   tilemap-config ;
    : >tileset   tilemap-config cell+ ;
    record parallax         <fixed  ( x, y )
    record scroll-offset    <fixed  ( x, y )
    record bounds           <fixed  ( x, y, w, h ) 
    record viewport         <fixed  ( x, y, w, h ) 
drop #128 ;struct

struct: %sceneheader
    %block embed blockheader
    record scenemask   <hex   \ bitmask that defines which actors will be copied when loading to this scene
    record bgm         <fixed \ ( TBD ) probably a general sound #, which can optionally stream a file
                       \ could add extra params like volume and pitch
    record scroll      <fixed
    record res         <fixed
    record main-bounds <fixed     ( x, y, w, h )
    cell field >slew   <fixed     \ used to associate a slew in Scenester during save
;struct

struct: %scene
    %sceneheader embed sceneheader
    drop #512
    %layer embed layer1 
    %layer embed layer2
    %layer embed layer3
    %layer embed layer4
;struct

create layer-template  %layer struct,
layer-template to this
    1 1 this parallax 2!
    0 0 8192 8192 this bounds 4!
    0 0 viewwh this viewport 4!

: system  0 block ;     \ kludge; redefined in more logical way below

: simple-global  field does> datatype.offset @ system + ;

struct: %globals  
    %bank embed bankheader
    cell simple-global actionOffset <int
    cell simple-global stateOffset  <int
    cell simple-global globalOffset  <int
    cell simple-global globalOffset  <int
    cell simple-global nextid  <fixed
drop #128 ;struct

struct: %role  %module embed moduleheader
;struct


struct: %bank  
    %block embed blockheader
    cell field defaultType   <typeid
drop #128 ;struct

( --== Bank stuff ==-- )

: named  ( block - <name> )
    <word> rot >nfa cplace ;

: init-bank  ( n -- <name> )
    >in @ >r
    1024 * block dup named
    s" $$$$" drop @ swap defaultType !
    r> >in !
;

: >bank  ( block - bank )    block>  dup 1024 mod -  block ;
: bank   dup init-bank  create   ( start: ) 1024 * ,  does> @ block ;
: first@  ( bank - adr ) block+ ;
: current@  ( bank - adr ) dup cursor @ blocks + ;
: not0  dup ?exit 1 + ;
: +cursor  ( bank - bank )  dup cursor @ 1 + 1023 and not0 over cursor ! ;
: one  ( bank - adr )
    +cursor
    1023 for
        dup current@ free? if
            1 nextid +!
            dup >r current@ dup 1 blocks erase  dup claim  nextid @ over id !
            r> defaultType @ over typeid !
        unloop ;then
    +cursor
    loop true abort" No more left!"
;

: (?$)  ( bank - <name> adr|0 )  
    first@
    1023 for
        dup >nfa ccount
        <name> compare 0 = if
            skip unloop
            state @ if
                block> postpone literal postpone block
            then
        ;then
        block+
    loop   drop  0  skip
;
: ($)  (?$) dup 0 = abort" Block not found!" ;  

: ?$(  ( - <bank> <name> adr|0 )
    ' execute (?$) skip ; \ SKIP skips the ')'

: $(  ( - <bank> <name> adr )
    ' execute ($) skip ; immediate
    
: clear-bank  ( bank - )
    0 over cursor !  \ reset cursor
     first@ 1023 blocks erase
;
: each>  ( bank - )  ( block - )
    r>  swap first@ 1023 for
        dup free? not if
            2dup 2>r swap call 2r>
        then
        block+        
    loop 2drop 
;
: copy-bank  ( src dest -- )
    #16 #16 2+ /bank #16 - move ;

( --== Engine memory layout ==-- )

0 bank system
1 bank pic
2 bank sound
3 bank scene
4 bank template  \ a place to store actors for instantiating multiple times in different stages
5 bank role      \ like classes, but just for actors
6 bank gui       \ slew for the current tool to use (cleared by RUN)
7 bank env       \ offset for actions & states and misc. environment variables
8 bank playfield \ slew; default for game actors 
9 bank table     \ for general data table use

( --== Environment variables ==-- )

struct: %env
    %block embed blockheader
    record kind
    record data
;struct

: (env)  ( kind c - <name> adr )   \ address is of the value
    >in @ | (in) c k |
    env (?$) ?dup if
        k c third kind ccount compare 0= if  data  ;then
        true abort" Found constant is of the wrong kind."
    else
        (in) >in !
        env one dup named
        k c third kind cplace
        data
    then
;

: (n)  ( n kind c - env|0 )
    | c k n |
    0
    env each>
    >r
        k c r@ kind ccount compare 0= if
            r@ data @ n = if
                drop r@  ( leave on stack )
            then
        then
    r> drop
;


( --== Smart globals ==-- )

: global  ( size -- <name> )
    >in @ >r 
    s" global" (env) dup @ 0 = if
        cell globalOffset +!
        globalOffset @ ?overflow drop
        globalOffset @ swap ( env ) !
        globalOffset @
    else
        @ 
    then
        r> >in !
        ( size offset ) 
        %globals -rot swap create-field 2drop
    does>
        datatype.offset @ system +
;

cell global >stage       <fixed  \ current slew (block)
cell global tool         <fixed  \ current tool  (block)
cell global lasttool     <fixed  \ last tool that was RUN (block#)
cell global (me)         <fixed  
cell global (this)       <fixed  
cell global paused       <flag   \ disables actor logic
cell global (stage)      <fixed  \ for preserving in QUIT and RUN


( --== Actor vars ==-- )

: actorvar  ( offset -- <name> offset+cell )  cell field  <fixed  does> datatype.offset @ me + ;

struct: %simple  \ for particles and environments
    %block embed blockheader
    actorvar zorder  <fixed  
    cell+
    actorvar dead    <flag   \ if on, will be deleted at end of frame.
    actorvar x       <fixed  
    actorvar y       <fixed  
    actorvar vx      <fixed  
    actorvar vy      <fixed  
    actorvar tintr   <fixed  
    actorvar tintg   <fixed  
    actorvar tintb   <fixed  
    actorvar tinta   <fixed  
    actorvar sx      <fixed  
    actorvar sy      <fixed  
    actorvar rtn     <fixed  
    actorvar >role   <fixed  
    actorvar state#  <fixed  
    actorvar >pic    <fixed  
    actorvar sub#    <fixed  
    actorvar anim#   <fixed  
    actorvar animctr <fixed  
    actorvar rate    <fixed  \ animation speed
    actorvar woke    <flag   \ if woke is off, state isn't executed.
    actorvar hid     <flag   
drop #256 ;struct

struct: %common  
    %simple embed commonheader
    actorvar rolename #12 +  <cstring  \ effectively 16 bytes
    actorvar attr            <hex      \ attribute flags
    actorvar ctype           <hex      \ collision flags
    actorvar cmask           <hex      \ collision mask
    actorvar ibx             <fixed    \ interaction hitbox
    actorvar iby             <fixed    
    actorvar ibw             <fixed    
    actorvar ibh             <fixed    
    actorvar sbx             <fixed    \ solid hitbox
    actorvar sby             <fixed    
    actorvar sbw             <fixed    
    actorvar sbh             <fixed    
    actorvar important       <flag     \ actor won't be disabled if left outside of a jumpcut
    actorvar disabled        <flag     \ disabled; no display, logic, or collision detection.
drop #512 ;struct

\ the remaining space is considered "volatile" and this is where roles should define their vars
: volatilevars  %common #768 ;


( --== Pic stuff ==-- )

struct: %pic
    %asset embed assetheader
    record subsize
    record >coldata   \ user-defined byte-tables (4 of them)
    record >atrdata   \ user-defined byte-tables (4 of them)
drop #384
    0 field animations
;struct

#1024 0 animations - #16 / constant max-animations

: animation  ( n pic - adr )  animations swap max-animations mod  #16 * + ;

: load-pic  ( pic - )
    >r
        r@ path ccount ?datapath echo ['] loadbmp softcatch  ."  pic( " r@ .name ." ) "
        ?dup if r@ handle ! then
    r> drop
;

: save-pic  ( pic - )
    dup lock @ if drop ;then
    dup handle @ 0 = if drop ;then
    >r
        r@ handle @  r@ path ccount ?datapath ['] savebmp softcatch
    r> drop
;

: tile-region  ( n pic - x y w h )
    >r 16 /mod #4 lshift swap #4 lshift swap r> subsize 2@ ;

: draw-tile  ( n pic - )  
    over >r dup >r  handle @   swap r> tile-region  r> #28 rshift bblit ;


( --== Tilemap stuff ==-- )

: tile  ( col row adr - adr )
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

( --== Assumption ==-- )

: as  s" to me" evaluate ; immediate
: i{ ( actor - ) me mestk dup @ cells + cell+ !  mestk @ 1 + 15 and mestk ! as ;
: i} ( - ) mestk @ 1 - 15 and mestk !  mestk dup @ cells + cell+ @ as ; 
: {  ( actor - ) state @  if s" me >r as" evaluate else  i{  then ; immediate
: }  ( - ) state @ if  s" r> as" evaluate else  i}  then ; immediate


( --= Template stuff ==-- )

: instance  ( template slew -- actor )
    one {
        me copy 
        at@ x 2!
    me }
;

: role!  ( role -- )
    dup >role >! >nfa ccount rolename cplace ;


( --== Slew stuff ==-- )

0 value xt
: announce  ( xt slew )
    swap to xt each> {
        disabled @ not if
            >role @ if xt execute then
        then
    } ;

: in-bank?  ( val bank -- flag )  - dup /bank < swap 0 >= and ;

: copy-slew  ( src dest -- )
    | dest src |
    src dest copy-bank
    dest each> a! #1024 cell/ for
        @a src in-bank? if
            @a src - dest + !+
        else
            cell +a
        then
    loop
;

( --== Role stuff ==-- )

: runvec  ( ... ofs - ... )   ?dup -exit  >role @> vtable @ + @ execute ;

: already  >in @ >r defined r> >in ! if >body cell+ @ $01234567 = ;then  drop false ;


: ?action
    already ?exit
    >in @ >r 
    s" action" (env) dup @ 0 = if
        cell actionOffset +! 
        actionOffset @ swap ( env ) !  
        r> >in !  common-create actionOffset @ , $01234567 ,
    else
        r> >in !  common-create @ , $01234567 , 
    then 
    does>
        >role @ 0 = if drop ;then \ TODO: implement fallbacks
        @ runvec
;

: ?state
    already ?exit
    >in @ >r 
    s" state" (env) dup @ 0 = if
        cell stateOffset +! 
        stateOffset @ swap ( env ) !  
        r> >in !  common-create stateOffset @ 256 cells + , $01234567 ,
    else
        r> >in !  common-create @ 256 cells + , $01234567 , 
    then 
    does>
        >role @ 0 = if drop ;then \ TODO: implement fallbacks
        woke on
        @ dup state# ! runvec
;

: action: ( - <name> <role> ...code... ; ) ( ... - ... ) 
    >in @ ?action >in !
    ' >body @ role ($) vtable @ + :noname swap !  
;

: state:  ( - <role> <name> ...code... ; ) ( - )
    >in @ ?state >in !
    ' >body @ role ($) vtable @ + :noname swap .s !  
;

: load-role  ( role - )
    >r
        common
        r@ source ccount ?rolePath included
    r> drop
;
: define-role  ( - <role> <name> )  \ defines a vocab and assigns it to the current role
    role ($) >r
    512 cells allotment r@ vtable !
    <name> r@ vocab cplace
    common define
    r> drop
;


( --== Scene stuff ==-- )

: init-scene ( scene - ) 
    >r
    layer-template r@ layer1 %layer sizeof move
    layer-template r@ layer2 %layer sizeof move
    layer-template r@ layer3 %layer sizeof move
    layer-template r@ layer4 %layer sizeof move
    viewwh r@ res 2!
    0 0 512 16 * dup r@ main-bounds xywh!
    r> drop 
;

\ : filter-slew  ( src-scene slew - )  \ removes excluded actors 
\     | b s |
\     b each> {
\     scenebits @ s scenemask @ and 0 = if
\         me delete
\     then }
\ ;

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
: init-layer  ( tilemap tileset layer -- )
    >r
        layer-template r@ %layer sizeof move
        r@ >tileset >! r@ >tilemap >!
    r> drop
;

: load-slew  ( src-slew dest-slew -- )
    /bank move
;

( --== Actor stuff ==-- )

: stage  ( -- slew )
    >stage @ 0 = if  playfield >stage >!  then
    >stage @> ;
    
: switchto   ( slew -- )
    >stage >! ;

: actor  ( n slew -- actor )
    swap 1 + blocks + ;

: sub@+
    anim# @ >pic @> animation >r
    r@ c@ if
        r@ #1 + animctr @ 1i r@ c@ mod + c@ 1p sub# !
        rate @ animctr +!
    then
    r> drop
    sub# @
;

create colors  ' blue , ' green , ' red , ' orange , ' yellow , ' magenta , ' cyan , ' pink , 

: ?color 8 mod colors vexec  ;
: scrolled  >stage @> scroll 2@ 2- ;

: placeholder  ( - )
    x 2@ ibx 2@ 2+ scrolled at  me id @ ?color 0.75 alpha  ibw 2@ rectf
    x 2@ sbx 2@ 2+ scrolled at  red  sbw 2@ rect
    [dev] [if]
        unmount
        x 2@ scrolled globalscale dup 2* mountxy 2+ at
        >role ?@> ?dup if  default-font font>  >nfa ccount 2dup fnt @ stringwh black rectf white text  then
        mount
    [then]
;

defer draw

: draw-sprite  ( - )
    >pic @ 0 = if  placeholder  white  ;then
    x 2@  scrolled  at  sub@+ >pic @> draw-tile ;

' draw-sprite is draw

: animate  ( n speed - )
    rate ! 0 animctr ! anim# ! ;

: .state  state# @ s" state" (n) ?dup if >nfa ccount type space then ;



( --== Rendering/logic ==-- )

create drawlist 1023 cells /allot
: gathered  drawlist dup a!> 0 rot each> !+ 1 + ;
: zorder@  { zorder @ } ;

?action physics  ( -- ) 

: act   ( -- )
    >role @ -exit  state# @ runvec  dead @ ?exit  physics ;

: ?act   disabled @ ?exit  woke @ -exit  act ;
: ?draw  disabled @ ?exit  hid @ ?exit  draw ;

0 value 'code
: zsorted>  ( slew -- <code> )
    r> to 'code  gathered 2dup ['] zorder@ rsort swap a!> for @+ { 'code call } loop ;

[defined] dev [if]
    : (stop)  woke off cr ." STOPPED: " me .block ." -_-;;; while in state: " .state ;
    : acts  zsorted> ['] ?act catch ?dup if cr (throw) type (stop) then ;
[else]
    : acts  zsorted> ?act ;
[then]

: draws  ( slew - )
    zsorted> ?draw ;

: sweep  ( slew - )
    each> { dead @ if me delete then } ; 

create default-scene-options  0 , 0 , 0 , 0 ,
default-scene-options value scene-options
: :overlay  ( options n -- <code> ; ) cells + :noname swap ! ;
: overlay  ( n -- )  cells scene-options + @ execute ;

: limit-scene-scroll  ( scene -- ) >r
    r@ scroll 2@ r@ main-bounds xy@ 2max r@ main-bounds xy2@ viewwh 2- 2min r@ scroll 2!
r> drop ;

: draw-scene-layer  ( n scene -- ) >r
    dup
        r@ scroll 2@ 
        rot %layer sizeof * r@ layer1 + draw-layer
        overlay
r> drop ;

: draw-scene ( scene - )
    me { >r
    r@ res 2@ or 0 = if viewwh r@ res 2! then
    r@ res 2@ resolution
    0 r@ draw-scene-layer
    1 r@ draw-scene-layer
    r@ draws  \ temporary
    2 r@ draw-scene-layer
    3 r@ draw-scene-layer
    r> drop }
;

: aabb  ( x y w h - x1 y1 x2 y2 )  2over 2+ ;

: overlap? ( xyxy xyxy - flag )
  2swap 2rot rot >= -rot <= and >r rot >= -rot <= and r> and ;

: box  ( actor -- aabb )
    { x 2@ ibx 2@ sx 2@ 2* 2+ ibw 2@ sx 2@ 2* aabb } ;

: intersect?  ( obj1 obj2 -- flag )
    >r box r> box overlap? ;


?action hit ( actor -- )
?action struck ( other -- )

: detects ( slew -- )
    0 dup dup | hitter? hittee? n slew |
    slew each> {
        1 +to n
        cmask @ ctype @ or if
            1024 n do  \ only need to check the ones after this one
                i slew actor to you
                you free? not  you { disabled @ not }  and if
                    you { cmask @ } ctype @ and to hitter?
                    you { ctype @ } cmask @ and to hittee?
                    hitter? hittee? or if
                        me you intersect? if
                            sp@ >r
                            hitter? if  you hit  me you { struck } then
                            hittee? if  me you { hit }  you struck then
                            r> sp!
                        then
                    then
                then
            loop
        then
    }
;


( --== Tool stuff pt 1 ==-- )

: toolfield  ( size -- <name> )
    field does> @ tool @> + ;

struct: %tool  
    %module embed moduleheader
    #32 toolfield starter  <cstring  
    #32 toolfield resumer  <cstring  
    cell toolfield >toolScene  <fixed
drop #256 ;struct

: toolSource  tool @> source ;

: load-tool  ( tool -- )
    tool @ >r  tool >!
        warning on
        common
        toolSource ccount included
        warning off
    r> tool !
;




( --== Runtime/startup ==-- )

defer resume

: load-pics    pic each> load-pic ;
: load-roles   role each> load-role ;
: load-modules system each>  false to installing?  load-tool ;
: save-pics    pic each> dup modified @ if dup modified off save-pic else drop then ;
: free-pics    pic each> handle @ -bmp ;

: asdf  quit ;

: rtype  ( adr c -- )
    [dev] [if]
        unmount
        2dup default-font stringwh 8 8 2+ | h w c str |
            default-font font>
            displayw w - peny @ at   w h black rectf  4 4 +at  str c white text
            0 16 +at
        mount
    [else]
        2drop
    [then]    
;


: draw-stage-name
    0 0 at
    s" Stage: " stage block>name strjoin rtype
;

: quit
    common
    0 to 'step  0 to 'pump
    tool @ lasttool !
    tool off
    ['] draw-sprite is draw
    (stage) @ if (stage) @> else playfield then switchto
    default-scene-options to scene-options
    show>
        [dev] [if]
            <`> pressed if resume ;then
            <s> pressed ctrl? and if save then 
        [then]
        black backdrop
        stage limit-scene-scroll
        stage draw-scene
        paused @ not if
            stage acts
            stage detects
        then
        draw-stage-name
;

: presave  me (me) >!  this (this) >!  stage (stage) >! ;

\ : empty  presave  save free-pics only Forth definitions also empty ;

:make save-assets
    save-pics
;

: load-blocks
    cr ." Gamester: Loading... " show
    load-pics
    project count s" shared.f" strjoin file-exists if
        common
        s" depend " s[ project count +s s" shared.f" +s ]s ['] evaluate softcatch
    then
    load-roles 
    load-modules 
    cr ." Gamester: Done! "
;

: initialize
    (me) @ block as
    (this) @ block to this
    load-blocks
;


( --== Tool stuff pt 2 ==-- )

: tool-scene  >toolScene @> ;  

: toolVocab  tool @> vocab ;

: contextualize  only forth also Gamester   tool @ if also toolVocab ccount evaluate then   definitions ;

:make resume ( -- )
    lasttool @ if lasttool @ tool ! then 
    tool @ 0 = ?exit
    contextualize  
    resumer ccount evaluate
;

: define-tool  ( - <name> flag )  \ defines a vocab and assigns it to the current tool; returns true if it was already defined.
    common
    <name> toolVocab cplace
    >in @ postpone [undefined] swap >in !
    define
;


( --== Modes ==-- )

#16 global mode
 
: create-mode   create does> dup body> >name ccount mode cplace  @ execute ;

: mode:  ( -- <name> <code> ; )
    get-order get-current common 
    create-mode here 0 , :noname swap !
    set-current set-order
;


( --== Lookup shorthands ==-- )

: m( ( -- <name> <> system )   \ ex: m( mapster )
    system ($) skip ; immediate
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
: a( ( -- <name> <> actor )     \ ex: a( myconid )   (searches on the STAGE)
    stage ($) skip ; immediate


( --== Development ==-- )

: development
   tool @ if
        lasttool @ 0<> tool @ 0<> and if
            resume
        else
            quit tool @ lasttool !
        then
   else
        quit  mode find if execute then
   then
;


( ~~~~~~~~~~~~~~~~~~~~~~~~ )
(        Includes          )
( ~~~~~~~~~~~~~~~~~~~~~~~~ )

include prg/gamester/lib/cli.f


( ~~~~~~~~~~~~~~~~~~~~~~~~ )


( --== Finish Startup ==-- )

:make bye   presave  ['] save ?alert  al_uninstall_system  0 ExitProcess ;

newBlockFile? [if]
    playfield >stage >!
    add-pic default prg/gamester/data/default.png  this lock on
    install prg/gamester/tools/mapster.f mapster
    %globals sizeof globalOffset !
[then]

cr .( PROJECT :::::::::::: ) project count type

initialize
