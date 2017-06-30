; Purpose     : convert a map object into a map structure
; 
; Calling seqence map=omap2map(omap)
; 
; Input       : omap: map object
;
; Output      : map: map structure
;
; Written     : Gelu M. Nita
;
; Version     : 1, 24-Oct-2014
; 
; Modified    : Never
;-
function omap2map,omap
 on_error,2
 if ~obj_isa(omap,'map') then message,'input to 0map2map must be a map object'
 count=omap->get(/count)
 map=omap->get(0,/map)
 for k=1, count-1 do map=[map,omap->get(k,/map)]
 return,map
end