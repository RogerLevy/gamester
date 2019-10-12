depend ramen/lib/std/kb.f

define-role camera cameraing

: !scroll
    x 2@ stage layer1 limit-scroll x 2!
    x 2@ stage scroll 2!
;
state: state1 camera
    <left> kstate if -1 x +! then
    <right> kstate if 1 x +! then
    <up> kstate if -1 y +! then
    <down> kstate if 1 y +! then
    !scroll
;
state: state2 camera 
    !scroll
;
action: stop camera  state2 ;
action: start camera  state1 ;
