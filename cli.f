
: list  ( bank - )
    each> cr dup @ #-1 = if h. else >nfa ccount type then ;


( --== Experimental editing tools ==-- )

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
    this load-pic
;

: new-pic  ( w h - <name> <path> )
    pic one dup named  to this
    <word> this path cplace
    16 16 this subsize 2!
    *bmp this handle !
;

( --== Actors, roles, templates, slews ==-- )

: add-template  ( - <name> )
    template one dup as  named  
;

: add-role ( - <name> <path> )
    role one dup named   to this
    <name> this path cplace
    this load-role
    common
;

: init-slew  ( tilemap tileset dest-bank -- )
    >r
    displaywh 3 3 2/ layer-template viewport wh!
    r@ init-scene
    displaywh 3 3 2/ r@ res 2!
    ( tilemap tileset ) r@ layer1 init-layer
    r> drop
;

: add-actor  ( -- <template> actor )
    template ($)  stage instance dup as ;

: update  ( -- <role> )
    s" ld " role ($) path ccount >rolepath -ext strjoin evaluate ;

: u  update ;

: import  ( -- <name> <scriptpath> )
    >in @  add-role  >in !
    >in @  add-template  >in !
    >in @  role ($) >role >!  >in ! 
    >in @  pic (?$) ?dup if  >pic >!  then  >in !
    skip skip
;

( --== Additional commands ==-- )

: s( ( -- <name> <> system )   \ ex: t( mapster )
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


( --== Tools ==-- )

: install  ( -- <scriptpath> <name> )
    true to installing?
    quit
    system one tool >!
    s" tool" tool @> moduleType cplace
    scene one
        dup init-scene
        >toolScene >!
    <word> toolSource cplace
    tool @> named
    tool @> load-tool
;

: run ( -- <name> )
    system ($) tool >!
    tool @ lasttool !
    gui clear-bank
    contextualize
    starter ccount evaluate
;

