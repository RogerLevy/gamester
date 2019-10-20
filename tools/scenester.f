depend prg/gamester/lib/apptools.f
define-tool Scenester [if]

    %tool extend
        cell toolfield curScene
        cell toolfield curSlew    \ for saving/restoring
        cell toolfield curLayer
    2drop
    0 value draggee

    : layer  curLayer @ 4 mod %layer sizeof * stage layer1 + ;
    : scrollx  stage scroll ;
    : box  x 2@ ibx 2@ sx 2@ 2* 2+  ibw 2@ sx 2@ 2*   aabb 1 1 2- ;
    : maus  maus mountxy globalscale dup 2/ 2- ;
    : mdelt  mdelta globalscale dup 2/ ;
    : hovered   0 stage each> { maus scrollx 2@ 2+ box within? if drop me then } ;

    : new-actor ( -- )
        stage one as 
        16 16 sbw 2!
        16 16 ibw 2!
        1 1 sx 2!
        maus scrollx 2@ 2+ x 2!
    ;
    
    : dup-actor ( -- )
        me new-actor me copy
        maus scrollx 2@ 2+ x 2!
    ;
    
    : delete-actor  ( -- )
        me delete
    ;
    
    : scroll!  layer limit-scroll scrollx 2! ;
    
    : jump ( -- <name> )
        stage ($) as
        x 2@ layer viewport wh@ 2 2 2/ 2- scroll!
    ;
        
    : pan  mdelt 2negate scrollx 2@ 2+ scroll! ;
    : pick  hovered ?dup if dup to draggee as then ;
    : drag  draggee if  mdelt draggee { x 2+! } then ;
    : ?drop  draggee if  0 to draggee  x 2@ 2pfloor x 2! then ;
    
    

    : load-scene  ( scene -- )  \ not to be used to change the current slew.  use SWITCHTO for that.
        dup curScene >!
        dup >slew @ ?dup if block switchto then
        stage copy
    ;
    
    : load  ( -- <scene> )
        scene ($) load-scene ;

    : save-scene ( -- <name> )
        >in @ >r scene (?$) ?dup 0 = if
            r@ >in !
            scene one dup named
        then ( scene )
        stage over copy
        curScene >!
        stage curScene @> >slew >!
        r> drop
    ;
    
    : save  stage curScene @> copy  cr ." Saved current scene."  save ;
    
    : rename  curScene @> named ;

    : draw-scene-name
        curScene @ -exit
        s" Current Scene: "  curScene @> block>name strjoin rtype
    ;
    
    : draw-mouse-coords
        s" Mouse (game): " s[ maus scrollx 2@ 2+ swap 1i (.) +s bl +c 1i (.) +s ]s rtype
        s" Mouse (scene): " s[ maus scrollx 2@ 2+ stage main-bounds xy@ 2- swap 1i (.) +s bl +c 1i (.) +s ]s rtype
    ;
    
    : update-scenes
        stage curSlew >!
    ;
    
    : (pump)
        pump>
            app-events
    ;
    
    : controls
            <space> kstate lb @ and if  pan  ;then
            <`> pressed if quit ;then
            lb @ if
                draggee if  drag  else  pick  then
            else
                ?drop
            then
            <a> pressed ctrl? and if new-actor then
            <d> pressed ctrl? and if dup-actor then
            <s> pressed ctrl? and if save then 
            <del> pressed if delete-actor then
    ;
    
\     : (step)
\         step>
\     ;

    create tool-options 0 , 0 , 0 , 0 ,

    : resume-tool
        (pump)
\         (step)
        curSlew @ if curSlew @> switchto then
        stage curScene @> <> if  curScene @> load-scene  then 
        
        tool-options to scene-options
        show>
            unmount dgreen backdrop
            mount
            black backdrop
            update-scenes
            stage draw-scene
            
            \ draw highlight on current actor (me)
            x 2@ ibx 2@ 2+ scrolled at  ibw 2@ black rect
            -1 -1 +at ibw 2@ 2 2 2+ white rect
            
            \ draw main-bounds of the stage
            stage main-bounds xy@ scrolled at
            stage main-bounds wh@ red rect
            
            draw-stage-name
            draw-scene-name
            draw-mouse-coords
            
            controls
            
    ;
    
    : start-tool
        resume-tool
    ;
    
[then]

installing? [if]
    s" start-tool" starter cplace
    s" resume-tool" resumer cplace
    scene (?$) default 0 = [if]
        scene one named default
        scene( default ) init-scene
        scene( default ) curScene >!
    [then]
    1 curLayer !
[then]
