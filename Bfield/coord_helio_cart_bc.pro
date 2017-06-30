pro coord_helio_cart_bc,index,r,l,b,ix,iy,x,y,z,pangle=pangle
;+
; Project     : SOLAR 
;
; Name        : COORD_CART_HELIO()
;
; Purpose     : Coordinate transformation from heliographic coordinates
;		into cartesian image coordinates of an image
;		(inverse routine to COORD_CART_HELIO.PRO)
;
; Category    : Coordinate systems
;
; Explanation : The coordinate reference system of an image is defined
;		in the structure INDEX. The routine extracts the relevant
;		information from INDEX and calculates for a given
;		heliographic position (L,B) the corresponding
;		cartesian coordinates (X,Y) in pixel units of the image.
;		(See also definitions and descrition in reference
;		3D-Stereoscopic Analysis of Solar Active Region Loops:
;		I.SoHO/EIT Observations at Temperatures of 1.0-1.5 MK"
;               (Aschwanden et al. 1999, ApJ 515, 842)
;
; Syntax      : IDL> coord_helio_cart,index,r,l,b,ix,iy,x,y,z
;
; Examples    : IDL> ix=0.
;		IDL> iy=0. 
;		IDL> print,'image coordinate origin  ix,iy=',ix,iy
;		IDL> r=1.0
;      	        IDL> coord_cart_helio,index,r,ix,iy,x,y,l,b
;		IDL> print,'heliographic coordinates of disk center l,b = ',l,b 
;		IDL> l0=index.solar_l0
;		IDL> b0=index.solar_b0
;		IDL> print,'disk center coordinates in INDEX l0,b0 = ',l0,b0 
;      	        IDL> coord_helio_cart,index,r,l,b,r,ix_,iy_,x_,y_
;		IDL> print,'consistency check ix,iy-->l,b--ix_,iy_=',ix_,iy_
;
; Inputs      : index 	-  structure containing descriptors of data1 
;
;		r	-  altitude in units of solar radii 
;			   (r=1.000 for photoshpere )
;			   (r=1.004 for chromospheric height of 2500 km)
;
;               l	-  heliographic longitude of position (X,Y)
;
;               b	-  heliographic latitude of position (X,Y)
;
;		l,b,r 	-  can be vector arrays 
;
;
; Outputs     : ix 	-  x-pixel in image DATA (with structure INDEX)  
;
;		iy 	-  y-pixel in image DATA (with structure INDEX)  
;
;               x 	-  x-coordinate in arcseconds west from Sun center 
;
;		y 	-  y-coordinate in arcseconds north from Sun center
;
;	        ix,iy,x,y -  are vector arrays if (l,b,r) are
;
; Opt. Inputs : None 
;
; Opt. Outputs: None
;
; Keywords    : None
;
; Common      : None
;
; Restrictions: structure INDEX is required to have some minimal solar
;		keywords, such as 
;			.solar_r	solar radius in arcseconds
;			.solar_l0	heliograph.longitude of disk center
;			.solar_b0	heliograph.latitude  of disk center
;			.solar_p	solar position angle
;			.crota2 	roll angle correction 
;
; Side effects: None
;
; History     : 1998-Oct-06, Version 1 written, Markus J. Aschwanden
;               2000-Jan-17, replace ephemerides by GET_SUN
;
; Contact     : aschwanden@lmsal.com
;-


;extracting ephemerides from structure INDEX
;p	=index.solar_p
;r0	=index.solar_r
;l0	=index.solar_l0
;b0	=index.solar_b0
;p0	=index.crota2

timeobs =index.date_obs
eph     =get_sun(timeobs)
r0      =eph(1)
l0      =eph(10)
b0      =eph(11)
if keyword_set(pangle) then p=pangle else p=eph(12)
;p       =eph(12)
p0      =-p

crval1	=index.crval1
crval2	=index.crval2
crpix1	=index.crpix1
crpix2	=index.crpix2
cdelt1	=index.cdelt1
cdelt2	=index.cdelt2
;conversion deg-->rad and scaling 
q	=(!pi/180.)
p_	=p *q
p0_	=p0*q
l0_	=l0*q
b0_	=b0*q
l_	=l *q
b_	=b *q
;origin of coordinate system (x,y,z)
x1	=0.
y1	=0.
z1	=r
;rotation by latitude angle 
x2	=x1
y2	=y1*cos(-b_)-z1*sin(-b_)
z2	=y1*sin(-b_)+z1*cos(-b_)
;rotation by longitude angle difference 
x3	=x2*cos(l0_-l_)-z2*sin(l0_-l_)
y3	=y2
z3	=x2*sin(l0_-l_)+z2*cos(l0_-l_)
;rotation by latitude angle 
x4	=x3
y4	=y3*cos(b0_)-z3*sin(b0_)
z4	=y3*sin(b0_)+z3*cos(b0_)
;rotation by position angle 
x5 	=x4*cos(p_+p0_)-y4*sin(p_+p0_)
y5	=x4*sin(p_+p0_)+y4*cos(p_+p0_)
z5	=z4
;conversion into image coordinates
x	=x5*r0*r		;in arcseconds
y	=y5*r0*r		;in arcseconds 
z	=z5*r0*r		;in arcseconds
ix	=(crpix1-1.)+(x-crval1)/cdelt1		;in image pixel units 
iy	=(crpix2-1.)+(y-crval2)/cdelt2		;in image pixel units 
end
