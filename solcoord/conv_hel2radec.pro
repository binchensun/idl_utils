FUNCTION conv_hel2radec, helio0, refdate, behind=bhdlmb, arcmin=arcmin, dt=dt
;+
; NAME:
;       CONV_hel2radec, modified from the solarsoft routine CONV_H2A
; PURPOSE:
;	Computes RA and DEC offsets from the suncenter from 
;       Stonyhurst heliographic coordinate inputs
;       The original CONV_H2A provides arcsecs-from-suncenter coordinates from 
;	heliographic coordinate inputs (inverse of CONV_A2H). 
; CALLING SEQUENCE: 
;	dradec = conv_hel2radec(helio, date [,/arcmin], [dt])
; INPUT:
;       helio	- is a vector of heliocentric coordinates.  It should be 2xN.
;                       (0,*) = longitude (degrees) W positive
;                       (1,*) = latitude (degrees) N positive
;		  They can also be strings in the form 'N30W23'.
;	date	- Unless the date is supplied, today's date and time is used.
;                 This will affect the Solar axial tilt, B0, and polar angle, P.
;		  If tracking is enabled		  
; OUTPUT:
;	dradec	- The RA and DEC offsets from suncenter in arcseconds
;                       (0,*) = RA increases to the geo-East, so positive is geo-East
;                       (1,*) = DEC increases to the geo-North, so positive is geo-North
;                       !!!!!!NOTE!!!!!!: no correction for the cos(dec) factor for RA!!, so
;                       dec_new=dec+ddec
;                       ra_new=ra+dra/cos(dec)
; OPTIONAL KEYWORD INPUT:
;	arcmin	- If set, output is in arcminutes, rather than 
;		  arcseconds.
;	dt      - in hours. If set, solar differential rotation tracking is enabled. 
;                 refdate serves as the reference date and time for the input of 
;                 heliocentric coordinates helio0. The actual heliocentric coordinates 
;		  used for calculating output is differencial-rotation corrected  
; OPTIONAL KEYWORD OUTPUT:
;	behind  - Returns a binary vector set to 1 when points are behind
;		  the visible limb.
; CAUTIONS: 	There is a limit (although large) on how many pixels IDL's
;		memory will support at once. 
; CALLS: ANYTIM2INTS, GET_RB0P                                               
; HISTORY: 
;       Written 17-Apr-2013 by Bin Chen based on the SSW routine CONV_H2A
;-

siz = size(helio0)
typ = siz( siz(0)+1 )
if (typ eq 7) then helio = conv_hs2h(helio0) else helio = helio0
we = float(helio(0,*))
ns = float(helio(1,*))
nout = n_elements(ns)
;-------------differential rotation------------
if keyword_set(dt) then begin
    for i=0,nout-1 do begin
	we0=we[i] & ns0=ns[i]
        dwe = diff_rot(dt/24.,ns0,/synodic) ;relative to Earth
	;update the heliocentric longtitude
	we[i]=we[i]+dwe		
    endfor
endif
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
if keyword_set(arcmin) then sunr=sunr/60.

pix_num = n_elements(ns)

lon = we/!radeg 
colat = (90 - ns)/!radeg 

we=0				;reduce memory requirements
ns=0

; vect is the (x,y,z) location of the point for b0 = 0, where x is in the
; direction of Texas, y is west, and z is north. vect1 is rotated by b0. 

siz = size(helio)
typ = siz( siz(0)+1 )
vect = make_array(3, pix_num, type=typ>4)        ;create 3D array,
vect(0,*) = sin(colat)*cos(lon)			;back to xyz
vect(1,*) = sin(colat)*sin(lon)
vect(2,*) = cos(colat)

;correct for B0

if n_elements(b0) eq 1 then begin
    rotmx_b = [[cos(b0),0.0,-sin(b0)],[0.0,1.0,0.0],[sin(b0),0.0,cos(b0)]]
    rotmx_p = [[1.0,0.,0.],[0.,cos(p),sin(p)],[0.,-sin(p),cos(p)]]
    vect = rotmx_p # (rotmx_b # vect)
    vect = vect*sunr(0)
endif else begin
    for i = 0,pix_num-1 do begin
	rotmx_b = [[cos(b0(i)),0.0,-sin(b0(i))], [0.0,1.0,0.0], [sin(b0(i)),0.0,cos(b0(i))]]
        rotmx_p = [[1.0,0.,0.],[0.,cos(p[i]),sin(p[i])],[0.,-sin(p[i]),cos(p[i])]]
	vect(*,i) = rotmx_p # (rotmx_b # vect(*,i))
	vect(*,i) = vect(*,i)*sunr(i)
    endfor
endelse

bhdlmb=bytarr(n_elements(helio(0,*)))		;set flag for points behind the limb
bl_ss=where(vect(0,*) lt 0.0)
if bl_ss(0) ne -1 then bhdlmb(bl_ss)=1

;jd=anytim2jd(date)
;jd=jd.int+jd.frac
;sunpos,jd,ra0,dec0,/radian
;dec=dec0+vect[2,*]/3600d/180d*!dpi
;if keyword_set(arcmin) then dec=dec0+vect[2,*]/60d/180d*!dpi
;vect(1,*)=vect(1,*)/cos(dec)

;flip the sign of raoff because ra increases to the east, but y axis increase to the west
vect[1,*]=-vect[1,*]
return, vect(1:2,*)
end
