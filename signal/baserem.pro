;************************************************************************
;+
;*NAME:
;
;   	BASEREM     30 SEPTEMBER 1980
;  
;*CLASS:
;  
;*CATEGORY:
;  
;*PURPOSE:
;
;   	To fit a polynomial of order NDEG to a user-specified
;   	spectral region for removing a baseline from a spectral feature.
;  
;*CALLING SEQUENCE:
;
;   	BASEREM,X,Y,NDEG,XL,XR,A,YBF,CHIS
;  
;*PARAMETERS:
;
;    	X	(REQ) (I) (1) (I L F D)
;		Array of independent variables.
;
;    	Y	(REQ) (I) (1) (I L F D)
;		Array of dependent variables.
;
;    	NDEG	(REQ) (I) (1) (I)
;		Degree of polynomial used to approximate baseline.
;
;	XL,XR   (REQ) (I/O) (0) (I L F D)
;		Scalar values which if non-zero are used as the left & right
;         	coordinates of the excluded region. If zero, the user is 
;		prompted for these values via terminal cursor positions.
;               The values are returned as the array indicies of the
;               baseline points just outside the excluded region.
;
;    	A	(REQ) (I/O) (1) (F)
;		Floating point array with at least NDEG+1 elements .
;        	(polynomial coefficients are written into the last NDEG+1 
;		elements).
;  		Input vector A with added coefficients as described above
;		(output).
; 
;    	YBF	(REQ) (O) (1) (I L F D)
;		Values of Y produced by polynomial.
;		
;    	CHIS	(REQ) (O) (0) (I L F D)
;		Variance of the fit (as calculated by WPOLYFIT without 
;		weighting).
;  
;*INTERACTIVE INPUT:
;
;    	User is prompted for cursor positions of spectral feature region not 
;    	to be included in baseline calculation (if XL & XR are initially 
;	zero).
;  
;*FILES USED:
;  
;*SYSTEM VARIABLES USED:
;
;	!d.name
;	!d.x_ch_size
;	!d.y_ch_size
;	!d.y_size
;  
;*SUBROUTINES CALLED:
;
;    	WPOLYFIT
;    	PARCHECK
;  
;*SIDE EFFECTS:
;  
;*RESTRICTIONS:
;  
;*NOTES:
;
;  -  	BASEREM will prompt user for cursor positions until the number of points
;    	between the limits is greater than the number of elements in vector A
;    	plus NDEG-2. 
;  -  	BASEREM is used by GAUSSFIT for removing a baseline of a Gaussian 
;       feature. In GAUSSFIT, the vector A has NDEG + 1 + 3*NCOMP elements
;    	where NCOMP is the number of Gaussian components in the input array.
;  -  	The points selected by the user to designate the edges of the feature
;    	can be specified in any order. These points are INCLUDED in the baseline
;    	fit.
;  -  	When BASEREM is used to fit the background of a region in which several
;    	Gaussians are to be fit, the user still specifies one set of endpoints.
;    	The endpoints should represent the leftmost and rightmost sides of the
;    	features. 
;  -  	BASEREM use to be restricted to fits of less than 3rd order when
;    	used on the PDP computer. Higher fits are now possible but depending on 
;    	the magnitude of the input X and Y arrays, problems may occur. 
;       Scaling the input Y array will have no effect as BASEREM now
;       does this before calling WPOLYFIT.
;
;	tested with IDL Version 2.1.2 (sunos sparc)	08 Aug 91
;	tested with IDL Version 2.1.2 (vms vax)    	08 Aug 91
;	tested with IDL Version 2.1.2 (ultrix mispel)	08 Aug 91
;  
;*PROCEDURE:
;
;    	BASEREM extracts the  region excluding that described by the cursor
;    	positions (or XL and RL), and uses WPOLYFIT to calculate a polynomial 
;    	fit, the YBF values, and the reduced chi square.If NDEG = 0, BASERERM
;    	returns 1 element in the vector A simply representing the average value
;    	of the baseline region. Since the weighting vector passed to WPOLYFIT
;    	is set to ones, the CHIS parameter is simply the variance of the fit.
;  
;*INF_1:
;  
;*EXAMPLES:
;
;    	To fit a baseline with a 5th order polynomial:
;      	A = FLTARR(9)     ; 6 for baseline, 3 for a possible Gaussian feature
;      	BASREM,W,F,5,0,0,A,YBF,CHISQ
;  
;*MODIFICATION HISTORY:
;
;     	PDP VERSION: I. DEAN AHMAD (modified VAX version: R. Thompson)
;     	7-16-84 RWT updated documentation & made user interaction optional
;     	8-8-84  RWT defined FXL & FXR for non-interactive mode 
;     	11-8-85 RWT RETALL used for 1st RETURN & NELEMENTS & # added for DIDL
;      	4-13-87 RWT VAX mods: add PARCHECK, replace TEKDATA with CURSOR, 
;		replace TKPLOT & XYOUT with PLOTS, XYOUTS, & SCTODC, use 
;		assignment statements.
;     	10-28-87 RWT remove restriction of NDEG being <3, allow sides of 
;		feature to be specified in any order, add procedure call 
;		listing, add PLOT,Y, add endpoints to baseline array, remove 
;		FXR, FXL, YL & YR calculations, and remove oplot and listing 
;		of CHIS.
;        8-22-89 RWT Unix mods: remove SCTODC & HARDCOPY, store coords.
;               in arrays, add get_kbrd
;	 3-04-91 PJL corrected upper limit test for XC
;	 6-19-91 PJL cleaned up; tested on SUN and VAX; updated prolog
;        8-07-91 GRA added /down keyword to cursor calls
;        8-08-91 GRA added section to identify region containing the 
;                    feature and reduce the x axis range to 40 angstroms
;                    if the input array spans more than 40 angstroms; 
;                    included the x vector in calls to plot, and used
;                    tabinv to return x array indicies; added else
;                    clause so baserem can be called with xl and xr
;                    (wavelength values) defined; scaled the y vector
;                    before calling WPOLYFIT; marked cursor selected
;                    points with an 'x'; tested on SUN, DEC, VAX;
;                    updated prolog. 
;	10-10-91 LLT removed 40A limit to allow CRSCOR to work, features
;		\PJL separated by more than 40A to be examined. and other
;		     units to be used
;
;-
;************************************************************************
 pro baserem,x,y,ndeg,xl,xr,a,ybf,chis
;
 npar = n_params()
 if npar eq 0 then begin
    print,' BASEREM,X,Y,NDEG,XL,XR,A,YBF,CHIS'
    retall
 endif  ; npar
 parcheck,npar,8,'BASEREM'
;
 xll = float(xl)
 xrr = float(xr)
 npts = n_elements(x)  
 sa   = n_elements(a)
 nterms = sa - ndeg - 1                     ; # of terms for gaussians
;
 if ((xll+xrr) eq 0.0) then begin  ; prompt user for coordinates
    xc = fltarr(2)
    plot,x,y
    if (strlowcase(!d.name) eq 'tek') then  $
       st = 'Place cursor at sides of feature; press any key ' else $
       st = 'Place cursor at sides of feature; press mouse button '
    xyouts,!d.x_ch_size,!d.y_size-!d.y_ch_size,font=0,/device,st 
    repeat begin
       cursor,xll,yl,1,/down,/data
       oplot,[xll],[yl],psym=7,symsize=1.5
       flush = get_kbrd(0)
       cursor,xrr,yr,1,/down,/data
       oplot,[xrr],[yr],psym=7,symsize=1.5
       flush = get_kbrd(0)
       tabinv,x,xll,xl
       tabinv,x,xrr,xr
       xc = fix([xl,xr] + 0.5)
       xc = xc(sort(xc))
       xc = xc > 1 < (npts-2)
       if (xc(1) - xc(0)) le 3 then print, $
          'ERROR in GAUSSFIT:  Set endpoints further apart'
    end until (xc(1)-xc(0))+1 gt nterms  ;  end repeat
    xl = xc(0)
    xr = xc(1)
 endif else begin
    tabinv,x,xll,xl
    tabinv,x,xrr,xr
    xl = fix(xl + 0.5)
    xr = fix(xr + 0.5)
 endelse  ; (xl+xr)
;
 newn = npts-xr+xl
 if (newn lt ndeg) then begin
    print,'Not enough baseline points specified for desired fit'
    retall
 endif  ; newn
;
; extract baseline segments from outside feature region
;
 ybase = fltarr(newn+1)
 xbase = ybase
 ybase(0) = y(0:xl)
 ybase(xl+1) = y(xr:*)
 xbase(0) = x(0:xl)
 xbase(xl+1) = x(xr:*)
;
; unless ndeg is negative, determine baseline
;
 if ndeg ge 0 then begin
   ;
   ; scale y parameters to avoid floating overflow in wpolyfit
   ;
    yav = total(ybase)/(newn - 1)
    ysclbase = ybase/yav
   ;
   ; fit polynomial
   ;
    wpolyfit,xbase,ysclbase,0*ysclbase+1.,ndeg,abscl,ysclbf,chiscl  
   ;
   ; unscale y parameters for output and store baseline params in a
   ;
    ybf = ysclbf*yav
    chis = chiscl*(yav^2)
    a(nterms) = yav*(abscl(0:ndeg))
 endif else chis = total(ybase*ybase)/(newn-1)
;
 print,' '
;
 return
 end  ; baserem
