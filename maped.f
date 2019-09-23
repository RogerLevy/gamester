
define-tool Maped [if]
    
    depend prg/blocks/apptools.f
    
    /tool
    cell toolfield >palette         \ block #
    /layer toolfield layer
    cell toolfield tile#
    4 cells toolfield fg            \ pen color
    cell toolfield tileseta
    cell toolfield mapa
    cell toolfield tilea
    cell toolfield colora
    cell toolfield pala
    cell toolfield hilitea
    drop
    
    /actor
    var kind#
    var w
    var h
    var sx
    var sy
    drop
    
    : @color  fg fore 4 cells move ;

    0
    cell field 'draw
    cell field 'logic
    constant /kind

    16 stack: kinds  16 cells /allot
    : kind:  ( - <name> adr )
        kinds length constant  here dup kinds push /kind /allot ;

    : kind  kind# @ kinds []@ ;
    : draw-kind  x 2@ at  kind 'draw @ execute ;
    : :draw  :noname over 'draw ! ;
    : :logic :noname over 'logic ! ; 

    : beside  { y @   x @ w @ sx @ * + } 16 + x !   y ! ;
    : below   { x @   y @ h @ sy @ * + } 16 + y !   x ! ;
    : outline  w 2@ sx 2@ 2*  2dup  white rect  -1 -1 +at  2 2 2+ black rect ;

    : scrollx  layer scroll-offset ;
    : tsize  layer tileset-pic @> subsize @ ;
    : tilebuf  layer tilemap-block @> ;

    : box  x 2@   w 2@ sx 2@ 2*   aabb 1 1 2- ;
    : (adr)  mapa @> { maus x 2@ 2- sx 2@ 2/ scrollx 2@ 2+ tsize dup 2/ tilebuf adr } ;
    : that   (adr) @ tile# ! ;
    : lay  tile# @ (adr) ! ;
    : mpos  maus x 2@ 2- sx 2@ 2/ ;
\    : pick   mpos tb subwh 2/ 2pfloor tb subcols * + 1 + tile# ! ;
\    : crayon  curColor  img @ >bmp  mpos 2i  al_get_pixel ;
\    : paint  selection 2drop rot onto> mpos 2+ 2i curColor 4@ al_put_pixel ;
\    : eyedrop  curColor selection 2drop mpos 2+ 2i al_get_pixel ;
    : hovering?  maus box within? ;
    : interact?  @ hovering? and ;
    : pan
        mapa @> {
            mdelta globalscale dup 2/ sx 2@ 2/ 2negate scrollx 2@ 2+ layer limit-scroll scrollx 2!
        } ;

 

    kind: mapk
        :draw
            layer tileset-pic @> -exit
            0 0 256 256 layer viewport 4!
            tileseta @> beside  ( outline )
            x 2@ layer viewport xy!
            0 0 layer draw-layer
        ;
        :logic
            lb interact? if  lay  ;then
            rb interact? if  that   ;then
        ;
    drop

    kind: tilesetk
        :draw  layer tileset-pic @> -exit
            layer tileset-pic @> handle @ bmpwh w 2!
            layer tileset-pic @> handle @ blit ;
    drop
    
    kind: tilek
        :draw  mapa @> below  tile# @ 1i (.) text
            \ tb img !  img @ imagewh w 2!
               \   0 0 tb imagewh 0 bsprite
               \   outline
        ;
    drop
    
    
    : *element  gui one dup { swap kind# !  1 1 sx 2!  16 16 x 2! } ; 
    
    : add-actors
        mapk *element dup mapa >! { 256 256 w 2! } 
        tilesetk *element dup tileseta >! { }
        tilek *element dup tilea >! { }
    ;
    
    : controls
        <`> pressed if quit ;then
        <s> pressed ctrl? and if save then
        <space> kstate lb @ and if  pan  ;then
        gui each> { kind 'logic @ execute }
    ;
    
    : resume-maped
        ['] draw-kind is draw
        show>
            black backdrop
            tool-scene gui 1 copy
            gui draw-scene
            controls
        pump>
            app-events
    ;
    
    : start-maped
        ."  HI MAPED!"
        add-actors
        resume-maped
    ;
    
    
[then]
installing? [if]
    s" start-maped" starter cplace
    s" resume-maped" resumer cplace
    displaywh 2 2 2/ tool-scene res 2!
    white fore 4@ fg 4!
    system one s" palette" third systemType cplace >palette !
[then]
