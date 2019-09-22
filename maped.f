define-tool Maped [if]
    depend venery/venery.f
    
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
    var kind
    var w
    var h
    var sx
    var sy
    drop
    
    : @color  fg fore 4 cells move ;

    0
    cell field 'draw
    constant /kind

    16 stack: kinds  16 cells /allot
    : kind:  ( - <name> adr )
        kinds length constant  here dup kinds push /kind /allot ;

    : draw-kind  x 2@ at  kind @ kinds []@ 'draw @ execute ;
    : :draw  :noname over 'draw ! ;

    : beside  { y @   x @ w @ sx @ * + } 16 + x !   y ! ;
    : below   { x @   y @ h @ sy @ * + } 16 + y !   x ! ;
    : outline  w 2@ sx 2@ 2*  2dup  white rect  -1 -1 +at  2 2 2+ black rect ;

    kind: mapk
        :draw
            >pic @ -exit
            0 0 256 256 layer viewport 4!
            tileseta @> beside  ( outline )
            layer draw-layer
        ; \ black 0.5 alpha w 2@ rectf  tilemap ;
    drop
    
    kind: tilesetk
        :draw  tile# @ 1i (.) text ; \ tb img !  img @ imagewh w 2!
               \   0 0 tb imagewh 0 bsprite
               \   outline ;
    drop
    
    : *element  gui one dup { swap kind !  1 1 sx 2! } ; 
    
    : add-actors
        mapk *element dup mapa >! { 256 256 w 2!  16 16 x 2! } 
        tilesetk *element dup tileseta >! { 16 16 x 2! }
    ;
    
    : start-maped
        ."  HI MAPED!"
        ['] draw-kind is draw
        add-actors  
        show>
            <s> pressed ctrl? and if save then 
            black backdrop
            tool-scene gui 1 copy
            gui draw-scene
    ;
    
    
[then]

installing? [if]
    s" start-maped" starter cplace
    displaywh 2 2 2/ tool-scene res 2!
    white fore 4@ fg 4!
    system one s" palette" third systemType cplace >palette !
[then]
