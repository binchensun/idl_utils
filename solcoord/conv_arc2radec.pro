FUNCTION conv_arc2radec, helarc, refdate,  arcmin=arcmin
;+
; NAME:
;       CONV_arc2radec, modified from the solarsoft routine CONV_H2A
; PURPOSE:
;	Computes RA and DEC offsets from the suncenter from 
;       heliocentric coordinate inputs
;       The original CONV_H2A provides arcsecs-from-suncenter coordinates from 
;	heliographic coordinate inputs (inverse of CONV_A2H). 
; CALLING SEQUENCE: 
;	dradec = conv_arc2radec(helarc, date [,/arcmin])
; INPUT:
;       helarc	- is a vector of heliocentric coordinates.  It should be 2xN.
;                       (0,*) = solar EW in arcseconds, W positive
;                       (1,*) = solar NS in arcseconds, N positive
;	date	- Unless the date is supplied, today's date and time is used.
;                 This will affect the Solar axial tilt, B0, and polar angle, P.
;		  If tracking is enabled		  
; OUTPUT:
;	dradec	- The RA and DEC offsets from suncenter in arcseconds
;                       (0,*) = RA increases to the geo-East, so positive is geo-East
;                       (1,*) = DEC increases to the geo-North, so positive is geo-North
;                 !!!!!!NOTE!!!!!!: no correction for the cos(dec) factor for RA!!, so
;                       dec_new=dec+ddec
;                       ra_new=ra+dra/cos(dec)
; OPTIONAL KEYWORD INPUT:
;	arcmin	- If set, output is in arcminutes, rather than 
;		  arcseconds.
; CALLS: ANYTIM2INTS, GET_RB0P                                               
; HISTORY: 
;       Written 19-Apr-2013 by Bin Chen based on the SSW routine CONV_H2A
;-

siz = size(helarc)
we = float(helarc(0,*))
ns = float(helarc(1,*))
nout = n_elements(ns)
;-------------------- Get the Date
;
if (n_elements(refdate) eq 0) then date = anytim2ints(!stime) $
                        else date = anytim2ints(refdate)
if ((n_elements(date) ne nout) and (n_elements(date) ne 1)) then begin
    message, 'Improper number of dates.  Using first date for all points.', /info
    date = date(0)
endif

ans = get_rb0p(date,/quiet)
b0   = ans(1,*) ;solar b-angle
p = ans(2,*) ;solar p-angle
sunr = ans(0,*)
;print,'solar radius (arcsec): ', sunr[0]
;print,'solar p angle (deg): ', p/!dtor
;print,'solar b0 (deg): ', b0/!dtor

dradec=fltarr(2,nout)
if n_elements(b0) eq 1 then begin
    ;apply the solar p angle rotation, which is to rotate the points by p-angle CCW
    dradec[0,*]=cos(p[0])*we-sin(p[0])*ns
    dradec[1,*]=sin(p[0])*we+cos(p[0])*ns
endif else begin
    for i=0, nout-1 do begin
        ;apply the solar p angle rotation, which is to rotate the points by p-angle CCW
        dradec[0,i]=cos(p[i])*we[i]-sin(p[i])*ns[i]
        dradec[1,i]=sin(p[i])*we[i]+cos(p[i])*ns[i]
    endfor
endelse
dradec[0,*]=-dradec[0,*] ;flip the sign of ra offset, because ra increase to East, opposite to heliocentric convention 
if keyword_set(arcmin) then dradec=dradec/60.

;jd=anytim2jd(date)
;jd=jd.int+jd.frac
;sunpos,jd,ra0,dec0,/radian
;dec=dec0+vect[2,*]/3600d/180d*!dpi
;if keyword_set(arcmin) then dec=dec0+vect[2,*]/60d/180d*!dpi
;vect(1,*)=vect(1,*)/cos(dec)
return, dradec
end
