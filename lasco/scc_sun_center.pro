;+
;$Id: scc_sun_center.pro,v 1.5 2012/07/26 19:36:28 nathan Exp $
;
; Project     : STEREO - SECCHI
;
; NAME: SCC_SUN_CENTER
;
; PURPOSE:
;       This function returns sun-center for a SECCHI image.
;
; CATEGORY:
;       SECCHI_SYNOPTIC
;
; CALLING SEQUENCE:
;       Result = SCC_SUN_CENTER(hdr,FULL=full)
;
; INPUTS:
;       hdr:    Input image hdr or array of header structures
;
; KEYWORDS:
;       FULL    Indicates mage has been placed in 2048x2048 field
;               and REBIN'd to full size.
;
; OUTPUTS:
;       result: structure or array of structures result.xcen, result.ycen in IDL coordinates
;
; COMMON: NA
;
;
; Prev. Hist. : None
;
; History     : Ed Esfandiari, NRL, Sep 2006.
;                              Note: for now, this is just a simple dummy routine to return 
;                                    a secchi sun center. It needs to be replaced with real 
;                                    code once details to do so are available. 
;
;-
;$Log: scc_sun_center.pro,v $
;Revision 1.5  2012/07/26 19:36:28  nathan
;scc_sun_center output = type FLOAT not DOUBLE
;
;Revision 1.4  2008/05/02 20:02:11  nathan
;Made output type double (not float); handle input of array of header structures
;
;Revision 1.3  2008/04/30 21:28:17  nathan
;Clarified that xcen,ycen are defined as IDL coordinates
;
;Revision 1.2  2007/11/21 22:32:09  nathan
;now really moved from dev/movie
;
;Revision 1.3  2007/11/21 22:28:23  nathan
;xcen and ycen need not be in header
;
;Revision 1.2  2007/08/17 15:57:51  herbst
;Mods for N.Sheeley Summer 2007
;
;Revision 1.1  2006/10/05 20:55:47  esfand
;first virsion - aee
;
;
;

FUNCTION SCC_SUN_CENTER, hdr, FULL=full, WCS=wcs
  
  ;sunc ={xcen:0d,ycen:0d} 
  sunc ={xcen:0.,ycen:0.}
  cc=sunc

;   IF (sunc.xcen EQ 0) THEN BEGIN  
;     ; not needed for real 0.5 hdrs (they will have correct xcen and ycen)
;     sunc.xcen = hdr.p1row+((hdr.p2row-hdr.p1row)+1)/2.0
;     sunc.ycen = hdr.p1col+((hdr.p2col-hdr.p1col)+1)/2.0
;   ENDIF
;   ;help,/st,sunc,full
;
;  IF KEYWORD_SET(FULL) THEN BEGIN  
;     ;image has been placed in 2048x2048 field and REBIN'd to full
;     factor = 2048.0 / full
;     sunc.xcen = sunc.xcen/factor
;     sunc.ycen = sunc.ycen/factor
;     RETURN, sunc
;   ENDIF

IF datatype(hdr) EQ 'STC' THEN BEGIN
    nh=n_elements(hdr) 
    hdr0=hdr[0]
ENDIF ELSE BEGIN
    nh=1
    hdr0=hdr
ENDELSE

IF keyword_SET(WCS) THEN wcsh = wcs ELSE wcsh=FITSHEAD2WCS(hdr0)

scale = (keyword_set(FULL) ? float(full) / hdr0.NAXIS1 : 1.0)

scen = WCS_GET_PIXEL(wcsh, [0.,0.])
sunc.xcen = scen[0]*scale
sunc.ycen = scen[1]*scale

IF nh GT 1 THEN $
FOR i=1,nh-1 DO BEGIN
    IF keyword_SET(WCS) THEN wcsh = wcs ELSE wcsh = FITSHEAD2WCS(hdr[i])

    scale = (keyword_set(FULL) ? float(full) / hdr[i].NAXIS1 : 1.0)

    scen = WCS_GET_PIXEL(wcsh, [0.,0.])
    cc.xcen = scen[0]*scale
    cc.ycen = scen[1]*scale
    sunc=[sunc,cc]
ENDFOR
    

   ;help,/st,sunc

RETURN, sunc

END
