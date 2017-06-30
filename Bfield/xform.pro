PRO xform, xi,yi,zi, xo,yo,zo, TRANSFORM=t, HELP=help

; Apply transformation set up by function set_xform.
; Keyword TRANSFORM supplies the transformation matrix, e.g.
;   TRANSFORM=xf.helio2image.
; See set_xform for full documentation on the coordinate systems.
; Helio2image = heliographic to image-plane.
; Image2helio = image-plane to heliographic.
; These can be applied to coordinates, magnetic fields, velocities, ...
; Zimage2helio = image-plane xi,yi to heliographic xh,yh, with the point
; referred to by xi,yi understood to lie on the photosphere i.e. zh=0.
; The suppressed zi is nonzero. This transformation matrix is 2x2, whereas
; the others are 3x3.
;
; The following forms are accepted if TRANSFORM is helio2image or image2helio:
; xform,xyz		; replace the vector xyz=FLTARR(...,3)
;			; by the transformed vector.
; xform,xyzi,xyzo	; transform the vector xyzi=FLTARR(...,3) and return
;			; the result in xyzo=FLTARR(...,3).
; xform,x,y,z		; replace the components x,y,z, which can be of arb.
;			; dimensions, by the transformed components.
; xform,xi,yi,zi,xo,yo,zo ; transform the components xi,yi,zi, which can be
;			; of arb. dimensions, and return the resulting
;			; components in xo,yo,zo.
;
; The following forms are accepted if TRANSFORM is a zimage2helio matrix:
; xform,xy
; xform,xyi,xyo
; xform,x,y
; xform.xi,yi,xo,yo
; These are analogous to above, but the vectors are FLTARR(...,2).
;-

;;ON_ERROR,2
IF KEYWORD_SET(help) THEN BEGIN
  PRINT,'tranform between heliographic and image-plane'
  PRINT,'xform, xi,yi,zi, xo,yo,zo, TRANSFORM=t, HELP=help'
  PRINT,'  if TRANSFORM is helio2image or image2helio.'
  PRINT,'xform, xi,yi, xo,yo, TRANSFORM=t, HELP=HELP'
  PRINT,'  if TRANSFORM is zimage2helio.'
  PRINT,'Alternate forms:'
  PRINT,'xform, xyz         ; replace 3-vector by result'
  PRINT,'xform, xyzi, xyzo  ; 3-vector xyzi -> 3-vector xyzo'
  PRINT,'xform, x,y,z       ; replace components by result'
  PRINT,'A 3-vector is FLTARR(...,3).'
  PRINT,'Analogous forms are accepted for the zimage2helio transformation'
RETURN
ENDIF

IF NOT KEYWORD_SET(t) THEN MESSAGE,'no transformation specified'
st = SIZE(t) & IF st(0) NE 2 THEN MESSAGE,'invalid transformation'
IF st(1) NE st(2) THEN MESSAGE,'invalid transformation'
IF st(1) EQ 2 THEN zt=1 ELSE IF st(1) EQ 3 THEN zt=0 ELSE $
	MESSAGE,'invalid transformation'

np = N_PARAMS() &  n = N_ELEMENTS(xi)

; analyse the argument list
need2=0 & need3=0
CASE np OF
0:	MESSAGE,'transform what?'
1:	vector=1 ; replace [23]vector argument by transformed variable
2:	vector=1 ; in[23]vector -> out[23]vector
3:	BEGIN    ; replace x,y,z components by transformed components
	vector=0 & need3=1 & END
4:	BEGIN    ; xi,yi -> xo,yo
	vector=0 & need2=1 & END
5:	MESSAGE,'dont know what to do with 5 arguments'
6:	BEGIN    ; xi,yi,zi -> xo,yo,zo
	vector=0 & need3=1 & END
ELSE:	MESSAGE,'too many arguments'
ENDCASE
IF ( np EQ 1 OR np EQ 2 ) THEN BEGIN IF zt THEN need2=1 ELSE need3=1 & END

IF zt AND need3 THEN $
	MESSAGE,'2 components expected for zimage transform'
IF NOT zt AND need2 THEN $
	MESSAGE,'3 components expected for non-zimage transform'

sx = SIZE(xi)
IF vector THEN BEGIN
	IF sx(0) EQ 0 THEN MESSAGE,'expected vector arguments' $
	ELSE IF need2 AND sx(sx(0)) NE 2 THEN $
		MESSAGE,'2-vector expected for zimage transformation' $
	ELSE IF need3 AND sx(sx(0)) NE 3 THEN $
		MESSAGE,'3-vector expected for non-zimage transformation'
ENDIF
; finished checking

IF np EQ 1 OR np EQ 2 THEN BEGIN ; replace xi,yi[,zi] with 1-component 1D array
	xsave = xi ; save dimensions of xi
	IF need2 THEN BEGIN yi = xi(n/2:*) & xi = xi(0:n/2-1) & END $
	ELSE BEGIN yi = xi(n/3:2*n/3-1) & zi = xi(2*n/3:*) & xi = xi(0:n/3-1)
	     END
ENDIF
; ready to go

IF zt THEN BEGIN ; 1, 2, or 4 arguments
	zxx = t(0,0) & zxy = t(0,1)
	zyx = t(1,0) & zyy = t(1,1)
	xo = zxx*xi + zxy*yi
	yo = zyx*xi + zyy*yi
	IF np EQ 1 THEN BEGIN			; replace 2-vector in p1
		xi = xsave		; set up the correct dimensions
		xi(*) = [xo,yo]		; replace with output
	END ELSE IF np EQ 2 THEN BEGIN		; output 2-vector in p2
		xi = xsave		; restore the input 2-vector
		yi = xsave		; set up correct dimensions
		yi(*) = [xo,yo]		; output the 2-vector
	END ELSE BEGIN ; np==4 -- output 2 components in p3,p4
		zi = xo			; dimensions ok since xi,yi not
		xo = yo			; tampered with
	END
END ELSE BEGIN ; 1, 2, 3, or 6 arguments
	axx = t(0,0) & axy = t(0,1) & axz = t(0,2)
	ayx = t(1,0) & ayy = t(1,1) & ayz = t(1,2)
	azx = t(2,0) & azy = t(2,1) & azz = t(2,2)
	xo = axx*xi + axy*yi + axz*zi
	yo = ayx*xi + ayy*yi + ayz*zi
	zo = azx*xi + azy*yi + azz*zi
	IF np EQ 1 THEN BEGIN			; replace 3-vector in p1
		xi = xsave		; set up correct dimensions
		xi(*) = [xo,yo,zo]	; replace with output
	END ELSE IF np EQ 2 THEN BEGIN		; output 3-vector in p2
		xi = xsave		; restore the input 3-vector
		yi = xsave		; set up correct dimensions
		yi(*) = [xo,yo,zo]	; output the 3-vector
	END ELSE IF np EQ 3 THEN BEGIN		; replace components in p1,p2,p3
		xi = xo			; dimensions ok since xi,yi,zi
		yi = yo			; not tampered with
		zi = zo
	END ELSE BEGIN		; np=6 -- output is where it should be,
	END		 	; in xo,yo,zo, and xi,yi,zi werent disturbed.
ENDELSE
END
