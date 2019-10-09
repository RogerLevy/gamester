define-tool Scenester [if]
    depend prg/gamester/apptools.f

    toolstruct
        cell toolfield curScene   \ scene we continually save to
        cell toolfield curSlew    \ for saving/restoring
        cell toolfield curLayer
    drop
    
    0 value draggee

    : layer  curLayer @ 4 mod /layer * stage layer1 + ;
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
    
    : load  ( scene -- )  \ not to be used to change the current slew.  use SWITCHTO for that.
        dup curScene >!
        dup >slew @ ?dup if block switchto then
        stage copy
    ;

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

    : draw-scene-name
        curScene @ -exit
        unmount
        s" Current Scene: "  curScene @> block>name strjoin
            2dup default-font stringwh 8 8 2+ | h w c str |
        default-font font>
        displayw w - 24 at   w h black rectf  4 4 +at  str c white text
        mount
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
        stage curScene @> <> if  curScene @> load  then 
        
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
                        
            ?draw-stage-name
            draw-scene-name
            
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