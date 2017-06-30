FUNCTION set_xform, mag,p,bc,lc, HELP=help

; Calculate and store transformation between image-plane and heliographic
; coordinates and various other useful info. Tranformations are linear,
; assuming a tangent plane on the photosphere at the reference point.
; Thus the reference point should be the center of the FOV.
;
; INPUT:
;	b0	solar sub-Earth latitude (radians)
;	p	solar p-angle (radians)
;	bc	latitude of reference point on photosphere (radians)
;	lc	cmd of reference point on photosphere (radians)
; *OR*
;	mag	bfits structure containing mag.point information
; OUTPUT:
;	xf	a structure containing the information --
;
; transformations --
; xf.helio2image	; Heliographic xh,yh,zh -> image-plane xi,yi,zi.
;			; FLTARR(3,3).
; xf.image2helio	; Image-plane xi,yi,zi -> heliographic xh,yh,zh.
;			; FLTARR(3,3).
; xf.zimage2helio	; Image-plane xi,yi -> heliographic xh,yh.
;			; ASSUMING (the unobserved) zi is such that zh=0 i.e.
;			; the observed image lies in the photospheric plane.
;			; FLTARR(2,2).
; *NOTE*NOTE**NOTE*	"PRINT,xf.helio2image" will print it transposed.
;			Input using junk = [[1,2],[3,4]] is transposed too.
;			This is so 1st coord is identified with x in plotting
;			etc. (See IDL manual.) Internal matrix arithmetic 
;			works as expected.  Beware that everything is backwards
;			in I/O.
;	ANM 930610	IDL v3.0 has a lib. routine pm ("print matrix") which
;			prints matrices in the normal algebraic way, so
;			"pm,xf.helio2image" will print it "correctly".
;
; ; useful things to know --
; xf.b0			; Solar latitude of sub-Earth point (radians).
; xf.p			; Solar p-angle (radians).
; xf.bc			; Latitude of center of FOV (radians).
; xf.lc			; Cmd of center of FOV (radians).
; xf.DirCos		; Direction cosines of LOS relative to helio xh,yh,zh
; 			; coordinate system in radians (FLTARR(3)).
; xf.mu			; Cos(angle from disk centre) in radians.
; xf.SolRad		; Solar radius in Mm (megametres).
; xf.Mm2arcsec		; Mm on photosphere -> arcsec at Sun.
; xf.Mm2radian		; Mm on photosphere -> radians of CMD or latitude.
; xf.arcsec2Mm		; Arcsec at Sun -> Mm on photosphere.
; xf.radian2Mm		; Radians of CMD or latitude -> Mm on photosphere.
;
; DEFINITION OF COORDINATE SYSTEMS:
; xh,yh,zh	Heliographic coordinates. zh is normal to the photosphere
; 		(actually the tangent plane).  xh in Westerly (+ve CMD)
; 		direction, yh in Northerly (+ve latitude) direction.
; xi,yi,zi	Position in image plane.  xi is terrestrial West, yi is
;		terrestrial North.  zi is distance towards observer from
;		the image plane, where the origin (xi=0,yi=0,zi=0) of
;		the image plane touches the photosphere.
;		Could be generalized for images not oriented to terrestrial
;		North/West.
;
; It is not really necessary to store the 3 transformations in xf.
; helio2image and image2helio are inverses of each other; since the
; transformations are unitary? orthogonal? or something one is just
; the transpose of the other. Also, zimage2helio can be bypassed by
; computing the zi needed to make zh=0, given xi, yi, and image2helio,
; then using these xi,yi,zi to compute xh,yh,zh given image2helio.
; However, they are stored for generality.
; Clearly there is duplication among the constants too, specifically
; radian2Mm == SolRad == 1/Mm2radian. and arcsec2Mm = 1/Mm2arcsec.
;
; USAGE EXAMPLES:
;
; ; Get heliographic coordinates of observation points (pixels) on photosphere
; ; in Mm given pixel positions in arcsec.
; xyzh=FLTARR(Nx,Ny,3) & FOR i=0,Nx-1 DO FOR j=0,Ny-1 DO $
;	xyzh(i,j,0:1) =  xf.arcsec2Mm * xf.zimage2helio # [xi(i,j),yi(i,j)]
;
; ; Compute noise on Bx assuming 15G error in LOS field and 150G error
; ; in transverse field.
; dBx = SQRT( xf.dircos(0)^2*15.0^2 + (1-xf.dircos(0)^2)*150.0^2 )
;
; ; Get area of pixel projected on photosphere in Mm^2, given pixel
; ; dimension pxl in arcsec.
; area = (pxl*xf.arcsec2Mm)^2/xf.mu
;
; 91xxxx, 92xxxx ANM
; Last change 921109 ANM add info message if b0,p,bc,lc > !PI/2
;-

IF KEYWORD_SET(help) THEN BEGIN
  PRINT,'xf = set_xform(b0,p,bc,lc, HELP=help) ; *OR*'
  PRINT,'xf = set_xform(mag, HELP=help)'
  PRINT,'Set up transformation matrices between heliographic and image-plane'
  PRINT,'and other useful stuff in structure xf. Arguments in radians.'
  PRINT,'b0 = solar latitude of disk center. p = solar p-angle.'
  PRINT,'bc = latitude of center of FOV. lc = cmd of center of FOV.'
  RETURN,0
ENDIF
IF N_PARAMS() EQ 1 THEN BEGIN
  b0 = mag.point.b0 & p = mag.point.p & bc = mag.point.lat & lc = mag.point.cmd
END ELSE b0 = mag
IF MAX(ABS([b0,p,bc,lc])) GT !PI/2 THEN $
  MESSAGE,/INFO,'b0,p,bc and/or lc > !PI/2. Are you sure they are in radians?'

xf = { xf,	b0:0.0, p:0.0, bc:0.0, lc:0.0, $
		helio2image:FLTARR(3,3), $
		image2helio:FLTARR(3,3), zimage2helio:FLTARR(2,2), $
		DirCos:FLTARR(3), mu:1.0, SolRad:696.0, $
		Mm2arcsec:0.0, arcsec2Mm:0.722, Mm2radian:0.0, radian2Mm:0.0 }

xf.b0 = b0 & xf.p = p & xf.bc = bc & xf.lc = lc

au = 149.6e3 ; 1 AU in Mm
xf.arcsec2Mm = ( au - xf.SolRad ) / 206265.0
; *NOTE* arcsec2Mm is correct for the point at disk center when the Sun
; is at 1 AU.  It does not account for center-limb variation (0.5% effect)
; or variation in Earth-Sun distance (1.5%?) effect.
xf.Mm2arcsec = 1.0/xf.arcsec2Mm
xf.radian2Mm = xf.SolRad & xf.Mm2radian = 1.0/xf.radian2Mm

; These are the transformation matrix elements given by eq. (1)
; of Gary and Hagyard, Solar Phys. 126, 21 (1990).
; Derived independently ANM 10/91.
; Vector quantities (e.g. B, as well as position r)
; in heliographic coordinates (H) ; are given in terms of
; the image-plane components (I) by B_H = A B_I, where
; the B's are column vectors and A = [axx axy axz]
;				     |ayx ayy ayz|
;				     [azx azy azz].
; Note that A is orthogonal, so Inverse(A) = Transpose(A).
; Also note last column of A gives direction cosines of LOS in helio-coords.
axx = - SIN(b0)*SIN(p)*SIN(lc) + COS(p)*COS(lc)
axy = + SIN(b0)*COS(p)*SIN(lc) + SIN(p)*COS(lc)
axz = - COS(b0)*SIN(lc)
ayx = - SIN(bc)*( SIN(b0)*SIN(p)*COS(lc) + COS(p)*SIN(lc) ) $
      - COS(bc)*COS(b0)*SIN(p)
ayy = + SIN(bc)*( SIN(b0)*COS(p)*COS(lc) - SIN(p)*SIN(lc) ) $
      + COS(bc)*COS(b0)*COS(p)
ayz = - COS(b0)*SIN(bc)*COS(lc) + SIN(b0)*COS(bc)
azx = + COS(bc)*( SIN(b0)*SIN(p)*COS(lc) + COS(p)*SIN(lc) ) $
      - SIN(bc)*COS(b0)*SIN(p)
azy = - COS(bc)*( SIN(b0)*COS(p)*COS(lc) - SIN(p)*SIN(lc) ) $
      + SIN(bc)*COS(b0)*COS(p)
azz = + COS(bc)*COS(b0)*COS(lc) + SIN(bc)*SIN(b0)

xf.image2helio = [ [axx,axy,axz], [ayx,ayy,ayz], [azx,azy,azz] ]
xf.helio2image = TRANSPOSE(xf.image2helio) ; inverse

xf.DirCos = [axz,ayz,azz]
xf.mu = SIN(b0)*SIN(bc) + COS(b0)*COS(bc)*COS(lc) ; cos(dist. from sun centre)

;zi = - ( azx*xi + azy*yi )/azz ; "height" of photosphere (zh=0) along LOS
;xh = axx*xi + axy*yi + axz*zi & yh = ayx*xi + ayy*yi + ayz*zi
zxx = axx - axz*azx/azz & zxy = axy - axz*azy/azz
zyx = ayx - ayz*azx/azz & zyy = ayy - ayz*azy/azz
xf.zimage2helio = [ [zxx,zxy], [zyx,zyy] ]

; NOW TRANSPOSE MATRICES BECAUSE THE INPUT FORM [[a,b],[c,d]] IS TRANSPOSED
; INTERNALLY I.E. b IS STORED IN (1,0) NOT (0,1) BECAUSE IDL IDENTIFIES
; THE 1st INDEX WITH x ETC. (IF WE HAD SAID xf.image2helio(0,0)=axx ETC
; WE WOULDNT NEED TO DO THIS.)

xf.helio2image = TRANSPOSE(xf.helio2image)
xf.image2helio = TRANSPOSE(xf.image2helio)
xf.zimage2helio = TRANSPOSE(xf.zimage2helio)
RETURN,xf
END
