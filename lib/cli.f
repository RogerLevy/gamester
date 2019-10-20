: view   dup to this .fields ;
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


( --= Pic commands ==-- )

: add-pic  ( - <name> <path> )
    pic one dup named   to this
    <word> this path cplace
    16 16 this subsize 2!
    this ['] load-pic catch if
        this delete
    then
;

: new-pic  ( w h - <name> <path> )
    pic one dup named  to this
    <word> this path cplace
    16 16 this subsize 2!
    *bmp this handle !
    this save-pic
;

( --== Actors, roles, templates, slews ==-- )


: add-sprite  ( - <pic> <name> )
    pic ($) to this
    stage one dup as named
    this >pic >!
    16 16 sbw 2!
    16 16 ibw 2!
    1 1 sx 2!
    stage scroll 2@ viewwh 2 2 2/ 2+ x 2!
;

: template-from  ( old - <new> )
    template one as 
    ( old ) me copy 
    me named
;

: add-role ( - <name> <path> )
    role one dup named   to this
    <word> this source cplace
    this ['] load-role catch ?dup if
        this delete
        throw
    then
    common
;

: init-slew  ( tilemap tileset dest-bank -- )
    >r
    displaywh 3 3 2/ layer-template viewport wh!
    r@ init-scene
    displaywh 3 3 2/ r@ res 2!
    ( tilemap tileset ) r@ layer2 init-layer
    r> drop
;

: add-instance  ( -- <template> actor )
    template ($)  stage instance dup as ;

: update  ( -- <role> )
    s" ld " role ($) path ccount >rolepath -ext strjoin evaluate ;

: u  update ;

: import  ( -- <name> <scriptpath> )
    >in @  add-role  >in !
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