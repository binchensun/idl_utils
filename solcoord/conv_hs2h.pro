function conv_hs2h, str
;+
; NAME:
;	CONV_HS2H
; PURPOSE:
;	Convert from the string representation of heliocentric ('N12W23') to
;	integer values in degrees (N and W positive)
; CALLING SEQUENCE:
; 	helio = conv_hs2h(hel_str) 
; 	print, conv_hs2h('n12w23') 
; INPUTS: 
;	hel_str	- The string representation of heliocentric coordinates (both
;		  N/S and E/W, with N/S first).  There should be no spaces and
;		  all strings should be 6 characters long.
; OUTPUT:
;       helio   - The heliocentric angle in degrees
;                       (0,*) = longitude (degrees) W positive
;                       (1,*) = latitude (degrees) N positive
; HISTORY:
;	Written 16-Jun-93 by M.Morrison
;	22-Jun-93 (MDM) - Fixed error
;	21-Jul-93 (MDM) - Modified header
;-

siz = size(str)
typ = siz( siz(0) + 1 )
if (typ ne 7) then begin
    message, 'Input is expected to be string type.  Returning input as output'
    return, str
end
;
; 'N12W23'
ns = fix(strmid(str, 1, 2))
ss = where(strupcase(strmid(str, 0, 1)) eq 'S')
if (ss(0) ne -1) then ns(ss) = -ns(ss)
;
ew = fix(strmid(str, 4, 2))
ss = where(strupcase(strmid(str, 3, 1)) eq 'E')
if (ss(0) ne -1) then ew(ss) = -ew(ss)
;
n = n_elements(ew)
out = intarr(2,n)
out(0,*) = ew
out(1,*) = ns
return, out
end
