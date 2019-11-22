\ : view   dup to this .fields ;
: list  ( bank - )  each> cr .block ;
: .paths ( bank - )  each> cr dup .block path ccount type ;

( --== Experimental editing tools ==-- )

: num  <word> evaluate ;
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

: anm  ( - <anim#> <frame> <frame> <frame> ... )  \ set animation
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


( --= Pic commands ==-- )

: ap  ( - <name> <path> )  \ add pic
    pic one dup named   to this
    <word> this path cplace
    16 16 this subsize 2!
    this ['] load-pic catch if
        this delete
    then
;

: np  ( w h - <name> <path> )  \ new pic
    pic one dup named  to this
    <word> this path cplace
    16 16 this subsize 2!
    *bmp this handle !
    this save-pic
;

( --== Actors, roles, templates, slews ==-- )

: asp ( - <pic> <name> )  \ add sprite
    pic ($) to this
    stage one dup as named
    this >pic >!
    16 16 sbw 2!
    16 16 ibw 2!
    1 1 sx 2!
    stage scroll 2@ viewwh 2 2 2/ 2+ x 2!
;

: ar ( - <name> <path> )  \ add role
    role one dup named   to this
    <word> this source cplace
    this ['] load-role catch ?dup if
        this delete
        throw
    then
    common
;

\ : initslew  ( tilemap tileset dest-bank -- )
\     >r
\     displaywh 3 3 2/ layer-template viewport wh!
\     r@ init-scene
\     displaywh 3 3 2/ r@ res 2!
\     ( tilemap tileset ) r@ layer2 init-layer
\     r> drop
\ ;


: st  ( -- <name> )    \ save template
    >in @ template (?$) ?dup 0 = if
        >in !
        template one dup named
    else
        nip
    then ( template )
    me over copy
    { woke off }
;

: na ( -- actor )  \ new actor
    stage one as 
    16 16 sbw 2!
    16 16 ibw 2!
    1 1 sx 2!
    stage scroll 2@ viewwh 2 2 2/ 2+ x 2!
    me
;

: (script)  s" .f" strjoin ;

: *script    w/o create-file throw >r  r> close-file throw ;

: ?role  ( -- <name> role )
    >in @ >r
    role (?$) ?dup 0 = if
        r@ >in !
        <name> (script) file-exists not if
            project count s[ s" roles/" +s <name> (script) +s ]s *script
        then
        s" ar " s[ <name> +s bl +c <name> (script) +s ]s evaluate
        this ( role )
        skip
    then
    r> drop
;

: nt ( -- <name> )  \ new template
    >in @ >r
    na named
    r@ >in ! ?role >role >!
    r@ >in ! st
    r> drop
;

: update  ( -- <role> )
    s" ld " role ($) path ccount >rolepath -ext strjoin evaluate ;

: u  update ;

: import  ( -- <name> <scriptpath> )
    >in @  ar  >in !
    >in @  template one dup named as  >in !
    >in @  role ($) >role >!  >in ! 
    >in @  pic (?$) ?dup if  >pic >!  then  >in !
    skip skip
;


( --== Tools ==-- )

: install  ( -- <scriptpath> <name> )
    quit
    true to installing?
    system one tool >!
    s" tool" tool @> moduleType cplace
    scene one
        dup init-scene
        >in @ over named >in !
        dup tool @> chain
        >toolScene >!
    <word> toolSource cplace
    tool @> named
    tool @> load-tool
;

: (run) ( -- <name> )
    quit
    system ($) tool >!
    tool @ lasttool !
    gui clear-bank
    contextualize
    starter ccount evaluate
    paused off
;

: run  ( -- <name> )
    stage (stage) >!
    ['] (run) catch if
        (stage) @> switchto
    then
;

: reinstall ( -- <scriptpath> <name> )
    >in @ >r
    <word> 2drop
    system (?$) ?dup if delete then
    r@ >in !
    install
    r> drop
;