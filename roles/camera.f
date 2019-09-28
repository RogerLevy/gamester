depend ramen/lib/std/kb.f

define-role camera cameraing

: !scroll
    x 2@ stage layer0 limit-scroll x 2!
    x 2@ stage scroll 2!
;
state: camera state1
    <left> kstate if -1 x +! then
    <right> kstate if 1 x +! then
    <up> kstate if -1 y +! then
    <down> kstate if 1 y +! then
    !scroll
;
state: camera state2
    !scroll
;
action: camera stop   state2 ;
action: camera start  state1 ;
