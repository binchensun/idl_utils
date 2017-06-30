
function mjd2anytim, mjd, _EXTRA=e
; $Id: mjd2anytim.pro,v 1 2014/10/22 By Bin Chen @ cfa $
; $ modified from mjd2utc.pro v1.2 from the ssw NRL package $
; $ note the original mjd2utc did the INCORRECT conversion by subtracting $
; $ an extra 12 h to MJD to get UTC time -- mjd starts from midnight already $
;
; Purpose:
;  mjd2anytim function accepts an array of mjd times, converts them to
;  sswidl anytim format. It handles mjd by adding 50000 to it when necessary 
;  when converting to anytim. It inherits all anytim keywords  
;
; Calling Sequence:
;  result = mjd2utc(mjd, out_style='any anytim format')
;
; Input:
;  mjd: double array of mjd. Both modified mjd values, like 98.540861, and 
;       normal mjd values like, 50098.540861, are handled.
;
; Output:
;  sswidl anytim output 
;
; HISTORY:
;       Written 2014 Oct 22 by Bin Chen
;

 n= n_elements(mjd)
 ; convert to the mjd structure that anytim recognizes
 mjda= {mjd:0L,time:0L}
 mjda= replicate(mjda,n)
 for i=0D,n-1 do begin
   ; pick up mjd part of the mjd (mjd is double):
   mjda(i).mjd= long(strmid(mjd(i),0,strpos(mjd(i),'.')))
   if(mjda(i).mjd lt 50000L) then begin
     ;Modified JD. 50000 must be added to get mjd:
     mjda(i).mjd= mjd(i)+50000L
   end
   ; Make sure all 6 decimal digits are picked up:
   smjd= strtrim(string(mjd(i),format='(d18.12)'),2)

   ; Change the fraction of the day to miliseconds and save as time:
   mjda(i).time= double(strmid(smjd,strpos(smjd,'.'),12)) * 24. * 3600 * 1000
 end 
  return, anytim(mjda,_EXTRA=e)
end
