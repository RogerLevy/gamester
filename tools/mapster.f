
define-tool mapster [if]
    
    depend prg/gamester/apptools.f
    
    
    toolstruct
        cell toolfield >palette         \ pic; block #
        /layer toolfield layer
        cell toolfield curTile
        4 cells toolfield curColor      \ pen color
        cell toolfield tileseta
        cell toolfield mapa
        cell toolfield tilea
        cell toolfield colora
        cell toolfield palettea
        cell toolfield hilitea
    drop
    
    volatilevars
        actorvar kind#
        actorvar w
        actorvar h
    drop
    
    : @color  curColor fore 4 cells move ;

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
    : tilebuf  layer tilemap-block @> ;
    : tspic  layer tileset-pic @> ;
    : tsbmp  tspic handle @ ;
    : canvas  ( - x y w h )  curTile @ tspic tile-region ;
    : canvxy  canvas 2drop ;
    : canvwh  canvas 2nip ;
    : mark  tspic modified on ;
    : palbmp  >palette @> handle @ ;
    
    : box  x 2@   w 2@ sx 2@ 2*   aabb 1 1 2- ;
    : (adr)  mapa @> { maus x 2@ 2- sx 2@ 2/ scrollx 2@ 2+ 16 16 2/ tilebuf adr } ;
    : that   (adr) @ curTile ! ;
    : lay  curTile @ (adr) ! ;
    : mpos  maus x 2@ 2- sx 2@ 2/ ;
    : pick   mpos  16 16 2/ 2pfloor 16 * + curTile ! ;
    : crayon  curColor palbmp mpos   w 2@  palbmp bmpwh  2/  2/   2i  al_get_pixel ;
    : paint  tsbmp onto> canvxy mpos 2+ 2i curColor 4@ al_put_pixel   ;
    : eyedrop  curColor tsbmp canvxy mpos 2+ 2i al_get_pixel ;
    : hovering?  maus box within? ;
    : interact?  @ hovering? and ;
    : pan
        mapa @> {
            mdelta globalscale dup 2/ sx 2@ 2/ 2negate scrollx 2@ 2+ layer limit-scroll scrollx 2!
        } ;
    : ?eraser
        mapa @> { hovering? } if 0 curtile ! ;then
        tilea @> { hovering? } if 0 0 0 0 4af curColor 4! ;then
    ;
    : undo  tsbmp -bmp  tspic load-pic  wipe ;


    ( --== Commandline ==-- )
    
    : load  ( scene - )
        tool-scene 1 copy
        tool-scene layer0 layer /layer move
        tool-scene layer0 /layer erase ;


    ( --== Elements ==-- )

    kind: mapk
        :draw
            layer tileset-pic @> -exit
            tileseta @> beside  ( outline )
            x 2@ w 2@ layer viewport 4!
            at@ 
            0 0 layer draw-layer
            at
            outline
            0 h @ 2 + +at
            white layer @> scrollx 2@ swap 1i (.) s[ s"  " +s 1i (.) +s ]s text
        ;
        :logic
            lb interact? if  lay  ;then
            rb interact? if  that   ;then
        ;
    drop

    kind: tilesetk
        :draw  layer tileset-pic @> -exit
            layer tileset-pic @> handle @ bmpwh w 2!
            layer tileset-pic @> handle @ blit
        ;
        :logic
            lb interact? if  pick  ;then
        ;
    drop
    
    kind: tilek
        :draw  mapa @> beside
            canvwh w 2!  
            tsbmp canvas *subbmp
                dup w 2@ sx 2@ 2* sblit
            -bmp
            outline
            w @ sx @ * 0 +at  16 0 +at  white curTile @ 1i (.) text
        ;
        :logic
            mark                                    \ just constantly mark the tileset as modified.
            lb interact? if  paint    ;then
            rb interact? if  eyedrop  ;then
        ;
    drop
    
    kind: colork
        :draw  256 16 w 2!
            tilea @> below  @color  1 1 +at  w 2@ rectf
            outline  
        ;
    drop
    
    kind: palettek
        :draw
            256 64 w 2!
            colora @> below
            palbmp w 2@ sblit
            outline
        ;
        :logic
            lb interact? if  crayon  ;then
            rb interact? if  crayon  ;then
        ;
    drop
   
  
    : *element  gui one dup { swap kind# !  1 1 sx 2!  16 16 x 2! } ; 
    
    : add-actors
        mapk *element dup mapa >! { 256 256 w 2! } 
        tilesetk *element dup tileseta >! { }
        tilek *element dup tilea >! { 8 8 sx 2! }
        colork *element dup colora >! { }
        palettek *element dup palettea >! { }

    ;
        
    : clear-tile
        tsbmp onto> canvxy at
        write-src blend>  black 0 alpha 16 16 rectf
    ;

    : fill-tile
        tsbmp onto> canvxy at
        write-src blend>  @color 16 16 rectf
    ;

    : controls
        <space> kstate lb @ and if  pan  ;then
        <`> pressed if quit ;then
        <e> pressed if ?eraser ;then 
        <s> pressed ctrl? and if  save  ;then
        <z> pressed ctrl? and if  undo  ;then
        <del> pressed if  clear-tile  ;then
        <del> pressed shift? and if  fill-tile  ;then
        gui each> { kind 'logic @ execute }
    ;
    
    : (pump)
        pump>
            app-events
    ;
    
    : resume-mapster
        (pump)
        show>
            black backdrop
            tspic block> 0 = if s" No tileset loaded." text ;then
            tool-scene gui 1 copy
            ['] draw-kind is draw
            gui draw-scene
            controls
    ;
    
    : start-mapster
        ."  HI mapster!"
        add-actors
        resume-mapster
    ;
    
    
[then]
installing? [if]
    s" start-mapster" starter cplace
    s" resume-mapster" resumer cplace
    displaywh 2 2 2/ tool-scene res 2!
    white fore 4@ curColor 4!
    ?$( pic defaultpal ) 0 = [if]
        add-pic defaultpal defaultpal.png
        pic( defaultpal ) >palette >!
    [then]
[then]
