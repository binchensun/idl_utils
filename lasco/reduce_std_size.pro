function reduce_std_size,img0,hdr, FULL=FULL , NO_REBIN=no_rebin, BIAS=bias, $
	SAVEHDR=savehdr, SOURCE=source, NOCAL=nocal
;+
; $Id: reduce_std_size.pro,v 1.15 2013/06/28 14:13:58 mcnutt Exp $
; NAME:
;
;	REDUCE_STD_SIZE
;
; PURPOSE:
;
;	Create a "full image" 512x512 of the input image.  Accounts for sub 
;	images and pixel summing.
;
; CATEGORY:
;
;	LASCO DATA REDUCTION
;
; CALLING SEQUENCE:
;
;	Result = REDUCE_STD_SIZE(Img,Hdr)
;
; INPUTS:
;
;	Img:	Input Image array
;	Hdr:	FITS header or LASCO header structure
;
; OPTIONAL INPUTS:
;	None
;	
; KEYWORD PARAMETERS:
;
;	FULL:	   Returns an image of size 1024x1024 pixels if set. The default is a 
;		   512x512 image.
;
;   	/NO_REBIN   Do not resize image
;
;	BIAS:	Subtracts bias and corrects values for binning before returning image. 
;		If the bias is set to 1, then the OFFSET_BIAS routine is called.
;               If the bias is greater than 1, then the value in the bias keyword is used.
;
;	SAVEHDR:	Do not put new values in header
;   	SOURCE= OPtionally return source of sun center/roll values
;   	/NOCAL	Do not use get_sun_center.pro center and roll values (but do correct for resizing)
;
; OUTPUTS:
;
;	The function result is a full image, with the input image inserted into
;	the correct place in the full image. Also, hdr is modified.
;
; OPTIONAL OUTPUTS:
;	bias
;
; COMMON BLOCKS:
;	None
;
; SIDE EFFECTS:
;	None
;
; PROCEDURE:
;
;	The input image is inserted into its proper place in a full image.  
;	On-chip and off-chip pixel summing are properly considered. The output 
;	image is resized to a 512 x 512 image.  Optionally, it can be resized 
;	to 1024 x 1024 or any size, or not resized at all.  
;
;	If the image is not resized, then each dimension would be determined
;	by the amount of summing along each axis according to 1024 / SUM,
;	where SUM is the number of pixels summed along the axis.  i.e., if 2x2 
;	summing is used, then the image size would be 512 x 512.  If 4x2 
;	summing were used, then the image size would be 256 x 512.
;
;	IF BIAS keyword is set, output image values are corrected for summing 
;	and offset bias. LEBSUM and SUM header values are changed to reflect output. 
;
; EXAMPLE:
;
;	To obtain the default 512 x 512 image:
;
;		Output = REDUCE_STD_SIZE(Img,Hdr)
;
;	To obtain a 1024 x 1024 image:
;
;		Output = REDUCE_STD_SIZE(Img,Hdr,/full)
;
;	To obtain an image sized by the summing:
;
;		Output = REDUCE_STD_SIZE(Img,Hdr,/no_rebin)
;
; $Log: reduce_std_size.pro,v $
; Revision 1.15  2013/06/28 14:13:58  mcnutt
; changed hdr cdelt1 values
;
; Revision 1.14  2013/03/29 19:03:29  nathan
; fix bias subtract for leb binning
;
; Revision 1.13  2013/03/29 17:50:29  nathan
; correctly implement /FULL with current get_roll_or_xy; update hdr.offset
;
; Revision 1.12  2012/11/16 22:23:47  nathan
; fix scale_to from previous fix
;
; Revision 1.11  2012/11/16 20:06:18  nathan
; fix /norebin to match requirement for c2/3_warp.pro
;
; Revision 1.10  2011/10/27 14:42:38  nathan
; remove stop
;
; Revision 1.9  2011/10/25 20:07:11  nathan
; fix error in previous mod
;
; Revision 1.8  2011/10/19 19:36:03  nathan
; updates for LASCO level-1
;
; Revision 1.7  2011/04/25 21:37:50  nathan
; add /silent
;
; Revision 1.6  2010/12/22 23:37:17  nathan
; check for crota in hdr
;
; Revision 1.5  2010/11/09 20:13:29  nathan
; add SOURCE= option
;
; Revision 1.4  2010/08/30 22:02:58  nathan
; make /no_rebin same as /full
;
; Revision 1.3  2010/06/04 16:21:55  nathan
; only reset lebxysum in hdr if /bias
;
; Revision 1.2  2010/02/01 22:40:29  nathan
; save new crpix; correct binning correction; fudge r1col/row error in some C1 images
;
; MODIFICATION HISTORY:		Written, RA Howard, NRL
;    VERSION 1   rah    29 Aug 1996
;    VERSION 1.1 sep    29 Sep 1996	added /FULL keyword, and ability to
;					handle header structures
;    VERSION 1.2 rah    22 Oct 1996	Added /no_rebin keyword
;    VERSION 1.3 rah    17 Jul 1997	Corrected handling of image sizes >1024
;    VERSION 1.4 nbr	11 Dec 1998	Corrected handling of summed images with sizes > 512
;    VERSION 1.5 nbr    11 Feb 1999	Modify R[1,2][row,col] in header (returned)
;   		 nbr	12 Feb 1999	Divide by lebxsum*lebysum if binned image
;		 nbr	17 Feb 1999	See notes below
;		 nbr	18 Feb 1999	Do not divide by lebxsum*lebysum if %P or PB image
;		 nbr	23 Mar 1999	Modify NAXIS[1,2] in header
;		 nbr 	23 Apr 1999	Added telescope check
;		nbr	 7 Dec 1999	Replace stc_flag with LASCO_FITSHDR2STRUCT; fix
;					problem with unsummed, rebinned images
;		nbr,  1 Aug 2000 - Modify for MK4 images
;	nbr, 30 Jan 01 - Add BIAS keyword and streamline program
;	nbr, 15 Jun 01 - Remove conditional on REBIN
;	nbr, 19 Nov 01 - Handle zero-size images
;	nbr, 14 Mar 03 - Correct binning correction again; add /SAVEHDR
;	nbr,  5 May 04 - Update crpix values in header if subfield
;       rah, 27 Feb 06 - Add the capability to use the bias value input to the program
;   	nbr, 24 Dec 09 - binning correction nxfac/nyfac not working for lebxsum=3; corrected
;   	nbr,  4 Jan 10 - make sure crpix is corrected with binning
;
; SCCS 1.16 06/02/27 15:08:49 howard
;-
;

IF (DATATYPE(hdr) NE 'STC') THEN hdr = LASCO_FITSHDR2STRUCT(hdr)

   sumrow = hdr.SUMROW > 1
   sumcol = hdr.SUMCOL > 1
   lebxsum = hdr.LEBXSUM > 1
   lebysum = hdr.LEBYSUM > 1
   naxis1 = hdr.NAXIS1
   naxis2 = hdr.NAXIS2
   polar = hdr.polar
   tel = hdr.TELESCOP

IF naxis1 LE 0 or naxis2 LE 0 THEN BEGIN
	help,naxis1,naxis2,img0
	message,hdr.filename+' -- Invalid Image -- Returning',/CONT
	return,img0
ENDIF
;IF STRPOS(polar,'P') NE -1 THEN sumfac = 1 ELSE sumfac = float(lebxsum*lebysum)

      r1col = hdr.R1COL
      r1row = hdr.R1ROW
      if (r1col lt 1) then r1col = hdr.P1COL
      if (r1row lt 1) then r1row = hdr.P1ROW

IF keyword_set(BIAS) and not keyword_set(NOCAL) THEN BEGIN
   IF (bias EQ 1)  then abias = offset_bias(hdr) ELSE abias=bias
   ; 
   ;help,abias
   ; value of lebsummed = 4(v + b), but offset_bias returns 4b!!! so
   ; correctedval = (val-b)/4
   ; value of chipsummed = 4v + b
   ; correctedval = (val - b)/4 = val/4 - b/4
   
   IF sumcol GT 1 THEN BEGIN
   	img = (img0 - abias)/(sumcol*sumrow)
   	message,'Correcting for Chip summing',/inform
   ENDIF ELSE BEGIN
   	IF lebxsum GT 1 THEN message,'Correcting for LEB summing',/INFORM
	img = (img0 - abias)/(lebxsum*lebysum)
   ENDELSE
    message,'Offset bias of '+trim(abias)+' subtracted.',/INFORM
    hdr.OFFSET=0
    ;hdr=scc_update_history(hdr,'Subtracted bias of '+TRIM(imgbias))
ENDIF ELSE img = img0

nxfac = 2^(sumcol+lebxsum-2)		; If image is binned, nxsum = 2
nyfac = 2^(sumrow+lebysum-2)		; If image is summed, assume size is halved.

IF tel EQ 'SOHO' THEN IF (hdr.r2col-r1col EQ 1023 $
	and hdr.r2row-r1row EQ 1023 $
	and naxis1 EQ 512) THEN BEGIN
	nxfac=2		; This indicates size, not summing.
	nyfac=2
ENDIF	;** Monthly model images are half-size but not summed
;stop
nx = nxfac*naxis1	; These values correspond to the r1,r2 values
ny = nyfac*naxis2

; Some C1 images have incorrect values for r row/col values
IF hdr.r2col-r1col+1 NE naxis1*lebxsum THEN r1col=r1col-32
IF hdr.r2row-r1row+1 NE naxis2*lebxsum THEN r1row=r1row-32

; ** First take care of subframes.
IF ((nx lt 1024) or (ny lt 1024)) and tel EQ 'SOHO' THEN BEGIN	; NBR, 12/11/98: was NE
   sz=size(img)
   FULL_img = MAKE_ARRAY(1024/nxfac,1024/nyfac,TYPE=sz(sz(0)+1))
   nx=sz(1)<1024	; These values redefined to correspond to image size
   ny=sz(2)<1024
   IF nxfac LT 2 THEN naxis1 = 1024 ELSE naxis1 = 512
   IF nyfac LT 2 THEN naxis2 = 1024 ELSE naxis2 = 512
   IF (r1row GT 1024) THEN offrow=1 ELSE offrow=r1row
   IF (r1col LT 20) THEN BEGIN
      startrow=(offrow-1)/nyfac
      startrow=startrow<(1024-ny)
	print,r1col-1,nxfac,offrow-1,nyfac,nx,ny
      full_img((r1col-1)/nxfac,startrow) = img(0:nx-1,0:ny-1)
   ENDIF ELSE BEGIN
      IF ((r1col-20)/nxfac+(nx-1) GT 1024/nxfac)  THEN startcol=0 ELSE startcol=(r1col-20)/nxfac
      IF ((offrow-1)/nyfac+(ny-1) GT 1024/nyfac)  THEN startrow=0 ELSE startrow=(offrow-1)/nyfac
      full_img(startcol,startrow)=img(0:nx-1,0:ny-1)
   ENDELSE
; ** If not subframe, then do nothing here.
ENDIF ELSE full_img = img	

scale_to = 512

IF keyword_set(NO_REBIN) THEN scale_to=naxis1	; should be 512 or 1024

IF KEYWORD_SET(FULL) THEN scale_to = 1024  

IF NOT(keyword_set(SAVEHDR)) THEN BEGIN

    IF keyword_set(NOCAL) THEN BEGIN
    ; use sun center in header, corrected for FFV
    	hdr.crpix1=((hdr.crpix1*nxfac)+r1col-20)*scale_to/1024.
	hdr.crpix2=((hdr.crpix2*nyfac)+r1row-1)*scale_to/1024.
    	source='previous header'
    ENDIF ELSE BEGIN
    
    	c=get_sun_center(hdr, source, ROLL=crota, /degrees, /silent) ; returns IDL coordinates for input hdr
    	hdr.crpix1=(c.xcen+1)*scale_to/naxis1
    	hdr.crpix2=(c.ycen+1)*scale_to/naxis2

	hdr.crota1=crota
	IF tag_exist(hdr,'crota') THEN hdr.crota=crota
	message,'Changing header CRPIX1/2',/info
    ENDELSE
;    wait,5
    
    IF tel EQ 'SOHO' THEN BEGIN
	hdr.R1COL = 20
	hdr.R1ROW = 1
	hdr.R2COL = 1043
	hdr.R2ROW = 1024
	IF keyword_set(BIAS) THEN BEGIN
	    hdr.lebxsum=1
	    hdr.lebysum=1
	    hdr.offset =0
	ENDIF
    ENDIF
    hdr.NAXIS1 = scale_to
    hdr.NAXIS2 = scale_to
    hdr.cdelt1=hdr.cdelt1*(1024/scale_to)
    hdr.cdelt2=hdr.cdelt2*(1024/scale_to)

ENDIF ELSE print, '!!!! REDUCE_STD_SIZE: Header RCOL and RROW values unchanged!!!!!'

;IF KEYWORD_SET(NO_REBIN)    THEN RETURN,full_img
;
; if image size is not equal to 512 x 512 then scale
;
sz=size(img)
;naxis1=sz(1)						; ** NBR, 2/17/99

;if (naxis1 ne scale_to) and tel EQ 'SOHO' then full_img=REBIN(full_img,scale_to,scale_to)
full_img=REBIN(full_img,scale_to,scale_to)
return,full_img

end
