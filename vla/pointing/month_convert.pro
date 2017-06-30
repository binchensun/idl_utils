
function month_convert, month

; converts month number to 3-letter string or 3-letter string to number

mlist=['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC']

; use size to decide between integer and string

sz = size(month)
ty = sz[sz[0]+1]

if (ty eq 7) then begin
   mn = where(mlist eq strmid(strupcase(month),0,3))
   if (mn ne -1) then begin
      mn = mn+1
      mm = string(mn,format='(I2.2)')
   endif else mm=''
endif else begin
   if (month lt 13) then mm = mlist[month-1] else mm=''
endelse

return,mm

end
