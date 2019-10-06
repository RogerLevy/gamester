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

    : pan  mdelt 2negate scrollx 2@ 2+ layer limit-scroll scrollx 2! ;
    : pick  hovered ?dup if dup to draggee as then ;
    : drag  draggee if  mdelt draggee { x 2+! } then ;
    : ?drop  draggee if  0 to draggee  then ;
    
    : load  ( scene -- )  \ not to be used to change the current slew.  use SWITCHTO for that.
        dup curScene >!
        stage copy
    ;
    
    : update-scenes
        stage curSlew >!
        stage curScene @> copy
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
    ;
    
\     : (step)
\         step>
\     ;

    create tool-options 0 , 0 , 0 , 0 ,

    : resume-tool
        (pump)
\         (step)
        curSlew @ if curSlew @> switchto then
        tool-options to scene-options
        show>
            unmount dgreen backdrop
            mount
            black backdrop
            update-scenes
            stage draw-scene
            
            \ draw highlight on current actor (me)
            x 2@ ibx 2@ 2+ scrolled at  ibw 2@ red rect
                        
            ?draw-stage-name
            
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