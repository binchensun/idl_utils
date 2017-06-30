function secs_to_str,number,decimal=decimal,wrap=wrap

; returns time string from seconds
; if DECIMAL set, returns DECIMAL places after decimal point, otherwise integer
; if /WRAP set, goes from 24 to 00, default is 24 to 25

; decide whether array or single number
sz = size(number)

if (sz[0] gt 0) then begin ; array
   
   secstring=strarr(sz[1])
   
   for j=0,sz[1]-1 do begin

      starts=strarr(3)
      startv=fix(sixty((number[j]+.01)/3600.0))
      if keyword_set(wrap) then startv[0] = startv[0] mod 24
      for i=0,2 do  $
          starts(i)=strtrim(string(startv(i),format='(i2.2)'),2)
      secstring[j]=starts(0)+':'+starts(1)+':'+starts(2)
   
      if keyword_set(decimal) then begin
         fmt="(f"+string(decimal+2,format='(i1)')+"."+string(decimal,format='(i1)')+")"
         dec=strtrim(string(number[j]-long(number[j])+.00001,format=fmt),2) 
         secstring[j]=secstring[j]+strmid(dec,1,decimal+1)
      endif
   end
 
endif else begin    ;    single number

   starts=strarr(3)
   secstring=' '
   startv=fix(sixty((number+.01)/3600.0))
   if keyword_set(wrap) then startv[0] = startv[0] mod 24
   for i=0,2 do  $
       starts(i)=strtrim(string(startv(i),format='(i2.2)'),2)
   secstring=starts(0)+':'+starts(1)+':'+starts(2)
   
   if keyword_set(decimal) then begin
      fmt="(f"+string(decimal+2,format='(i1)')+"."+string(decimal,format='(i1)')+")"
      dec=strtrim(string(number-long(number)+.00001,format=fmt),2) 
      secstring=secstring+strmid(dec,1,2)
   endif
    
endelse

return,secstring

end
