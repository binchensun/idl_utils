function aia_rfilter_scalepar, wavelnth

;Purpose: Return scaling parameters for a given AIA band.
;
;Inputs: wavelnth = integer or string (e.g. 171 or '171')
;
;Outputs: Array containing scaling parameters.
;         
;Keywords: none.
;
;Written: 2012/10/17 by Patrick McCauley (pmccauley@cfa.harvard.edu) 

wavelnth = trim(wavelnth) ;convert to string

case wavelnth of 
  '171': scalepar={ $
       min:7.5, $ ;min for bytescaling
       max:25600.0, $ ;max for bytescaling
       exp:1.8, $ ;min exposure time for ejecting short exposures
       gamma: 1.025, $ ;exponential scaling factor
       a: 100.0, $ ;additional scaling factor
       scl:4.99803, $ ;scaling factor, originally based on expected exposures times prelaunch (I think)
       mincor: 0.0, $ ;min for bytescaling the off-limb component after radial filtering
       maxcor: 1.05, $ ;max for bytscaling the off-limb component after radial filtering 
       corscl: 5.5, $ ;scaling factor for radial filter (adjusts off-limb brightness)
       gammacor: 0.6} 
  '211': scalepar={ $
       min:0.0, $ 
       max:3000.0, $ 
       exp:2.7, $
       gamma: 1.025, $ 
       a: 100.0, $
       scl:4.99801, $
       mincor: 0.0, $
       maxcor: 1.2, $
       corscl: 3.0, $
       gammacor: 1.0} 
   '94': scalepar={ $
       min:0.25, $ 
       ;max:2080.0, $ 
       max:1500.0, $
       exp:2.7, $ 
       ;gamma: 1.0, $
       gamma: 1.15, $
       a: 1000.0, $
       scl:4.99803, $
       mincor: 0.0, $
       maxcor: 1.3, $
       corscl: 3.0, $
       gammacor: 1.0}    
  '335': scalepar={ $
       min:0.25, $ 
       max:1250.0, $ 
       exp:2.7, $ 
       gamma: 1.0, $
       a: 100.0, $
       scl:6.99734, $
       mincor: 0.0, $
       maxcor: 1.4, $
       corscl: 3.0, $
       gammacor: 1.0}            
  '193': scalepar={ $
       min:100.0, $ 
       max:5000.0, $ 
       exp:1.8, $ 
       gamma: 1.05, $
       a: 100.0, $
       scl:2.99950, $
       mincor: 0.00, $
       maxcor: 1.05, $
       corscl: 3.5, $
       gammacor: 0.6}    
  '304': scalepar={ $
       min:0.25, $ 
       max:2000.0, $
       exp:2.7, $ 
       gamma: 1.125, $
       a: 100.0, $
       scl:4.99441, $
       mincor: 0.0, $
       maxcor: 1.2, $
       corscl: 3.0, $
       gammacor: 1.0}    
  '131': scalepar={ $
       min:0.25, $ 
       max:2800.0, $ 
       exp:2.7, $ 
       gamma: 1.0, $
       a: 100.0, $
       scl:6.99685, $
       mincor: 0.0, $
       maxcor: 1.2, $
       corscl: 3.0, $
       gammacor: 1.0}    
  '1600': scalepar={ $
       min:0.25, $ 
       max:8800.0, $ 
       exp:1.1, $ 
       gamma: 1.0, $
       a: 100.0, $
       scl:2.99911, $
       mincor: 0.0, $
       maxcor: 0.8, $
       corscl: 3.0, $
       gammacor: 1.0}  
  '1700': scalepar={ $
       min:50.0, $ 
       max:32935.0, $ 
       exp:0.4, $ 
       gamma: 1.0, $
       a: 100.0, $
       scl:1.00026, $
       mincor: 0.0, $
       maxcor: 0.8, $
       corscl: 3.0, $
       gammacor: 1.0}  
  '4500': scalepar={ $
       min:0.25, $ 
       max:26000.0, $ 
       exp:0.4, $ 
       gamma: 1.0, $
       a: 100.0, $
       scl:1.00026, $
       mincor: 0.0, $
       maxcor: 8.0, $
       corscl: 3.0, $
       gammacor: 1.0}   
   else: begin
          box_message, ['Invalid wavelength. Check input.']
          return, -1
         end
endcase
       
return, scalepar

end

;--------------------------------------------------------------------------------------------------------

PRO aia_rfilter_bc, list, data_out, index_out, idl_out=idl_out, dir_out=dir_out, $
                 jp2=jp2, png=png, fits=fits, nsum=nsum, bin=bin, nring=nring, $
                 corscl=corscl, cutout=cutout, speed=speed, create_sav=create_sav, $
                 sav=sav, color=color, ver=ver, border=border, use_ref=use_ref, $ 
                 index_ref=index_ref, cross_correl=cross_correl, correl_img=correl_img, $
                 jhelio=jhelio, deconvolve=deconvolve, no_capt=no_capt, no_skip=no_skip, $
                 error_thresh=error_thresh, rotate=rotate, cadence=cadence, no_prep=no_prep, $
                 psf_save=psf_save, psf_restore=psf_restore, bkgnd_capt=bkgnd_capt, half_capt=half_capt, $
                 components=components, altct=altct, gamma_disk=gamma_disk, gamma_corona=gamma_corona, $
                 offset=offset, rundiff=rundiff, altrundiff=altrundiff, choke=choke, new_scale=new_scale, $
                 mincor=mincor, maxcor=maxcor, rmin=rmin, outsav=outsav, ring_struct=ring_struct
      
; =========================================================================
;+
; PROJECT:
;       SDO / AIA
;
; NAME:
;
;       AIA_RFILTER
;
; CATEGORY:
;
;       Image processing.
;
; PURPOSE:
;
;       Creates images in which the contrast of the corona is enchanced to show
;       fine structure. A number of images are summed and the coronal component 
;       is divided into rings. Each ring is then scaled based on its radius  
;       and associated pixel brightnesses relative to its neighbors. The 
;       disk is pulled from the central image in the sum to create the final 
;       output. A number of keywords are available for user control of output 
;       type, dimensions, scaling, and alignment; see below.
;
; CALLING SEQUENCE:
;
;      AIA_RFILTER_BC, list [,data_out] [,index_out] [,/idl_out] [,dir_out=dir_out] $
;                   [,/jp2] [,/png] [,/fits] [,nsum=nsum] [,bin=bin] [,nring=nring] $
;                   [,corscl=corscl] [,cutout=cutout] [,/speed] [,sav=sav] [,/create_sav] $
;                   [,/color] [,ver=ver] [,border=border] [,/use_ref] [,index_ref=index_ref] $
;                   [,/cross_correl] [,correl_img=correl_img] [,/jhelio] [,/deconvolve] $
;                   [,/no_capt] [,/no_skip] [,error_thresh=error_thresh] 
;
; INPUTS:
;
;       LIST          - [Mandatory] (string array)
;                       Array of image filepaths. Takes Level-1 or 1.5 data. The list
;                       MUST be populated with observations of the same wavelength, and 
;                       observatios from any AIA passband are accepted.
;                       
; OUTPUTS:
;     
;       DATA_OUT      - [Optional] (image array)
;                       Specify to output an IDL array of the resultant images. This variable 
;                       must be defined in conjunction with the /IDL_OUT keyword. This 
;                       is necessary to avoid carrying the output array in memory 
;                       if it's not actually desired upon completion. Use with caution, as 
;                       this output array may quickly eat up and exceed your machine's 
;                       memory. Note that if set, no file outputs will be created unless one 
;                       of the filetype keywords is also set (/JP2, /PNG, /FITS).
;       INDEX_OUT     - [Optional] (structure array)
;                       Specify to ouput a structure array containing the header info.                  
;
; KEYWORDS:
; 
; 
;       IDL_OUT       - [Optional] (boolean)
;                       Set to output an array of the resultant images. This keyword 
;                       must be set and the DATA_OUT variable must be defined. This 
;                       is necessary to avoid carrying the output array in memory 
;                       if it's not actually desired upon completion. Use with caution,
;                       as this output array may quickly eat up and exceed your machine's 
;                       memory. Note that if set, no file outputs will be created unless 
;                       one of the filetype keywords is also set (/JP2, /PNG, /FITS). 
;       DIR_OUT       - [Optional] (string)
;                       Directory for output images. If not set, current working 
;                       directory is used.
;       JP2           - [Optional] (boolean)
;                       Set to output jpeg2000s (.jp2). This is the default output because 
;                       the header info is preserved and they are considerably smaller than
;                       PNGs. Note that the file AIA_RFILTER_JP2GEN is also need to output 
;                       jp2s, but AIA_RFILTER can be run without the jp2gen function if one
;                       of the other file type keywords is set. Also Note that multiple file 
;                       types can be outputted by setting multiple keywords. 
;       PNG           - [Optional] (boolean)
;                       Set to output .png. Note that multiple filetypes can be outputted 
;                       by setting multiple keywords.
;       FITS          - [Optional] (boolean)
;                       Set to output .fits. Images will NOT include captions. Note that
;                       multiple filetypes can be outputted by setting multiple keywords. 
;       NSUM          - [Optional] (integer)
;                       Number of images to be summed to produce the image of the corona.
;                       The middle image of each sum will be used for the disk. If not set,
;                       defaults to 5. Accepts any value greater than or equal to 1. 
;       BIN           - [Optional] (integer)
;                       Set to specify binning (resolution) of output image. Uses FREBIN(), 
;                       which allows for any arbitrary binning. BIN=1: 4096x4096 (full res), 
;                       BIN=2: 2048x2048, etc. Default is 4 (1024x1024). Accepts any value 
;                       greater than or equal to 1. 
;       NRING         - [Optional] (integer)
;                       Set to specify how many rings the corona will be divided into. More
;                       rings will yield a smoother image but will slow the routine. The 
;                       routine tries to anticipate how many rings are needed given the 
;                       specified resolution by setting nring=500/sqrt(bin). Accepts any 
;                       value greater than or equal to 2.
;       CORSCL        - [Optional] (integer)
;                       Coronal scaling factor, useful for examining particularly bright or 
;                       faint structures. The default value is 3.75. Increasing this will 
;                       DECREASE the overall brightness of the corona and vice versa. 
;                       Accepts any value greater than 0.  
;       CUTOUT        - [Optional] (4-element array)
;                       Set for a cutout of orignal image. Array elements correspond 
;                       to the desired pixels to be read out: [x1,x2,y1,y1]. These pixel 
;                       values correspond to the full resolution (4096x4096) image. If you 
;                       wanted, for instance, a 2048x2048px cutout but in a 512x512 final 
;                       image, you might specify the following: CUTOUT=[0,2047,0,2047], BIN=8 
;       SPEED         - [Optional] (boolean)
;                       Normally, the coronal scaling factors are redefined for each filtered
;                       image. If /SPEED is set, these values will be computed only once and
;                       then then used throughout to save 20% or so of runtime. This could be
;                       be disadvantageous if the first few images in LIST are corrupted for
;                       some reason or contain a transient brightening, but works well in
;                       general. An alternative to this would be to use the CREATE_SAV and SAV 
;                       keywords to decide upon and specify a set of parameters that you like.
;       CREATE_SAV    - [Optional] (boolean)
;                       Set this keyword to output .sav files contaning the coronal scaling 
;                       parameters for each image, to be applied using the SAV keyword. A .sav 
;                       file will be created for each output image, which can then be used to 
;                       determine which set of parameters does the best job. This could be useful 
;                       if, for instance, you're looking to create a set of images with uniform 
;                       scaling over a time period where the overall brightness of the corona is 
;                       highly variable.                                                            
;       SAV           - [Optional] (string)
;                       .sav filepath containing parameters for scaling the corona, created
;                       using the /CREATE_SAV keyword.  
;       COLOR         - [Optional] (boolean)
;                       If set, the standard AIA color table for the given wavelength is 
;                       loaded via AIA_LCT.PRO. Grayscale is used if not set (LOADCT, 0).  
;       VER           - [Optional] (integer or string)
;                       Version indicator. Will be tacked onto the end of the filename 
;                       after an underscore.    
;       BORDER        - [Optional] (integer)
;                       Set to specify border that will be excluding from filtering. 
;                       Default is 15px. This is necessary to prevent edge effects due 
;                       to alignment from throwing off the filter. Accepts any value greater 
;                       than or equal to 0.        
;       USE_REF       - [Optional] (boolean)
;                       Set if you'd like the AIA_PREP call to align images to some reference
;                       image rather than the center pixel. If USE_REF is set and INDEX_REF
;                       is NOT set, then the first image in LIST is used as the reference.     
;       INDEX_REF     - [Optional] (structure)  
;                       Header structure corresponding to the image you'd like all others to 
;                       be aligned to. If set, USE_REF is also automatically set.
;       CROSS_CORREL  - [Optional] (boolean)
;                       Set if you'd like images to be aligned to eachother using a cross 
;                       correlation routine (CORREL_OPTIMIZE). This is not generally necessary
;                       and will slow the runtime considerably, but is useful if you suspect 
;                       that AIA_PREP is not fully aligning the images, as may be the case with 
;                       off-pointed observations in particular. If CROSS_CORREL is set and 
;                       CORREL_IMG is NOT set, then the first image in LIST is used as the base. 
;       CORREL_IMG    - [Optional] ([4096,4096] image array)
;                       Data array used as base for cross correlation alignment. If set, 
;                       /CROSS_CORREL is also automatically set.                                                                                                   
;       JHELIO        - [Optional] (boolean)
;                       Set to output jp2 files using the JHelioviewer naming convention.
;                       This is not the default because the files will not be named 
;                       in a sequential fashion that movie-making programs like QuickTime 
;                       Pro and FFmpeg will follow.                                                         
;       DECONVOLVE    - [Optional] (boolean)
;                       Set to apply image deconvolution; calls AIA_CALC_PSF() and 
;                       AIA_DECONVOLVE_RICHARDSONLUCY(). Resultant images will be noticeably 
;                       sharper, but the deconvolution is vvvery slow.                        
;       NO_CAPT       - [Optional] (boolean)
;                       Set to disable image captions.                    
;       NO_SKIP       - [Optional] (boolean)
;                       Set to disable skipping of short-exposure images. 
;       ERROR_THRESH  - [Optional] (integer) 
;                       Set to specify how many errors you'll tolerate within the loop that reads 
;                       in the images. This error handling is designed to circumvent rare errors 
;                       in READ_SDO.PRO when processing many files. Accepts any value greater 
;                       then or equal to 0. See note 1.  
;       OFFSET        - [Optional] (float array)
;                       [x_off, y_off], set to specify an offset in the crpix keyword positions. 
;                       Not necessary except for rare instances in which AIA is not pointed 
;                       normally and the keywords might not then be trusty. For the Comet Lovejoy 
;                       pointing in December, 2010, use [-23.,3.] for the image taken at 
;                       2011-12-15T23:25:59.12. The /USE_REF and /CROSS_CORREL keywords can then be 
;                       used to align all of the other images.                                      
;                                             
;                                   
; EXAMPLES:
;
;       Basic usage:
;       IDL> list = file_search('/data_directory/','*171.fits')
;       IDL> aia_rfilter, list, /speed
;
; COMMON BLOCKS:
;
;       none
;
; NOTES: 
;    
; 1) Regarding the use of CATCH error handling:
; 
;    Occasionally, READ_SDO.PRO will return an error and a .FITS file must be skipped
;    to prevent this routine from crashing. When this happens, the next index in 
;    the input list is used and the routine continues as normal with the same cadence, 
;    so there is some overlap between the output that required the skip and the subsequent
;    output. For instance, consider a list of 9 images to be used with 3-image sums. 
;    Normally, the output images would be composed of [0,1,2],[3,4,5],[6,7,8]. Now 
;    imagine that the 4th image is corrupted and must be skipped. In this scenario, the 
;    3 output images would be composed of [0,1,2],[3,5,6],[6,7,8]. 
;    
;    This type of error is fairly rare but is likely to crop up a few times if you're 
;    processing several days worth of AIA images (many thousands of images). Which files  
;    were skipped and the !ERROR_STATE information for each one are printed upon completion.
;    Any more than 5 errors will cause the routine to abort, as this may be indicative of 
;    a different problem (this threshhold can be user-defined using the ERROR_THRESH 
;    keyword). This error handling is accomplished using CATCH, which will respond the 
;    same to any error that occurs within the file-reading loop. To disable it when 
;    editting or debugging that section, set ERROR_THRESH=0. 
;
; CONTACT:
; 
;   Patrick McCauley - pmccauley@cfa.harvard.edu
;
; MODIFICATION HISTORY:
;
progver = 'V1.0' ;--- Written by Alec Engall, Steve Cranmer, & Patrick McCauley
progver = 'V1.1' ;--- Fixed small bugs with captions & /idl_out 
;                     - PM 2012/03/13
;
;-
; =========================================================================
progname = 'aia_rfilter'
 
t0 = systime(1) ;start time

;Allows AIA_RFILTER to compile and run without having the jp2gen function
;available to it. 
forward_function aia_rfilter_jp2gen

;return to caller of program unit if error
;on_error, 2

;Clear output arrays if they're already populated
if n_elements(data_out) gt 0 then undefine, data_out
if n_elements(index_out) gt 0 then undefine, index_out 

; =========================================================================
; Setting unspecified keywords to default values
; =========================================================================

if not keyword_set(nsum) then nsum = 5 ;# of imgs to sum
if not keyword_set(bin) then bin = 4 ;factor to scale down full res imgs by
;# of rings to divide coronal into, 500 or less depending on binning
if not keyword_set(nring) then nring = fix(500/sqrt(bin))
if n_elements(error_thresh) eq 0 then error_thresh = 5 ;# of errors allowed
if n_elements(border) eq 0 then border = 25 ;# px border excluded from filter
if not keyword_set(altct) then altct = 0
if not keyword_set(gamma_disk) then gamma_disk = 1.
if not keyword_set(offset) then offset = [0.,0.]
if not keyword_set(rmin) then rmin=1.005

;Compiles file type keyword information into an array
type_out = [keyword_set(jp2), keyword_set(png), $
            keyword_set(fits), keyword_set(idl_out)]
if (total(type_out) eq 0) then type_out[0] = 1 ;defaults to jp2's

;Version number/tag
if keyword_set(ver) then ver = '_'+strcompress(string(ver), /remove_all)
if not keyword_set(ver) then ver = ''
if not keyword_set(cadence) then cadence = 1

; =========================================================================
; Checking inputs/keywords for obvious errors
; =========================================================================

;Is there is enough data in the input array?
nl = n_elements(list)
message, 'Number of FITS files found: '+strtrim(nl,2), /informational 
if (nl lt nsum) then begin
  box_message, ['Whoops, insufficent data found. Returned.']
  return
endif

;Does the user-specified output directory exist?
if keyword_set(dir_out) then begin
  if (file_test(dir_out, /directory) eq 0) then begin
    print, dir_out
    box_message, ['Whoops, output directory above does not exist. Returned.']
    return
  endif
;Sets output directory to current working directory if not specified.
endif else begin
  cd, current = current
  dir_out = current+'/'
endelse

;Does the user-specified .sav file exist?
if keyword_set(sav) then begin
  if (file_test(sav) eq 0) then begin
    box_message, ['Whoops, specified .sav file not found. Returned.']
    return
  endif
endif

;Does the cutout region asked for make sense?
if keyword_set(cutout) then begin
  if total(where(cutout ge 4096 OR cutout lt 0)) ne -1 then begin
    box_message, ['Whoops, cutout region is out of bounds. Returned.']
    return
  endif
endif

; =========================================================================
; Initiates variables to be called or modified later
; =========================================================================

;Reads in first image to gather some initial information
read_sdo, list[0], hd, da, /silent, /uncomp_delete

;Use hd as reference index if USE_REF is set but no index specified
if keyword_set(use_ref) AND not keyword_set(index_ref) then index_ref=hd

;Prep data if not already level 1.5
if not keyword_set(no_prep) then begin
if (hd.lvl_num eq 1.) then aia_prep, temporary(hd), temporary(da), hd, da, $
                           use_ref=use_ref, index_ref=index_ref, /quiet
endif
                           
;Use prepped image as cross correlation base if CROSS_CORREL is set but no 
;image speficied                          
if keyword_set(cross_correl) AND not keyword_set(correl_img) then correl_img=da

;Some wavelength-specific values for byte scaling and ejecting short exposures
;mnmx = [min_corona, max_corona, min_disk, max_disk, min_exposure_time, normalization factor]
;case hd.wavelnth of 
;  171: mnmx = [0.0, 1.05, 5.0, 9.5, 1.8, 4.99803]
;  211: mnmx = [0.0, 1.2, 4.0, 8.0, 2.7, 4.99801]
;  094: mnmx = [0.0, 1.3, 0.5, 4.5, 2.7, 4.99803]
;  335: mnmx = [0.0, 1.4, 2.0, 6.5, 2.7, 6.99734]
;  193: mnmx = [0.0, 1.2, 4.75, 8.75, 1.8, 2.99950]
;  304: mnmx = [0.0, 1.2, 3.75, 6.75, 2.7, 4.99441]
;  131: mnmx = [0.0, 1.2, 2.2, 6.0, 2.7, 6.99685]
;  1600: mnmx = [0.0, 0.8, 3.5, 8.5, 1.1, 2.99911]
;  1700: mnmx = [0.0, 0.8, 6.0, 8.0, 0.4, 1.00026]
;  4500: mnmx = [0.0, 1.0, 8.0, 10.0, 0.4, 1.00026]
;endcase

scalepar = aia_rfilter_scalepar(hd.wavelnth)
if keyword_set(maxcor) then scalepar.maxcor = maxcor
if keyword_set(mincor) then scalepar.mincor = mincor
if keyword_set(corscl) then scalepar.corscl = corscl
if keyword_set(gamma_corona) then scalepar.gammacor = gamma_corona

;Wavelenth / nsum strings:
wv = strcompress(string(hd.wavelnth), /remove_all)
nsum_str = strcompress(string(nsum), /remove_all)

;if keyword_set(new_scale) and wv eq '193' then begin
;	 mnmx = [0.05, 1.2, 4.75, 8.75, 1.8, 2.99950]
;	 if not keyword_set(corscl) then corscl = 4.0
;endif
;if keyword_set(corscl) then scalepar.corscl = corscl

;Restores user-specified sav file with scaling parameters.
if keyword_set(sav) then restore, sav

cen = double([hd.crpix1+offset[0], hd.crpix2+offset[1]]) ;[x,y] center coords

yy = (indgen(4096)-cen[1]) ## (1. + fltarr(4096)) ;array of only x-values wrt SUN center
xx = (1. + fltarr(4096)) ## (indgen(4096)-cen[0]) ;array of only y-values wrt SUN center
rr = sqrt(xx^2 + yy^2) ;array of radial distaces from SUN center for each pixel
rsol = rr / hd.r_sun ;array of radial distances from center for each pixel in solar radii
rmax = max(abs(rsol)) ;max radius

;Distance of the radial rings from the center in solar radii.
rad  = dindgen(nring)/double(nring-1)*(rmax-1.)+1.
arad = alog10(rad) ;log scaled for polynomial fitting later

;Finds coefficients of a linear relationship for deciding which ring
;each pixel will be divided into. 
coefR = poly_fit(rad,dindgen(nring),1) 

;If aligning to something other than the image center, then a distinction needs to be 
;made between sun and image center to properly clip corners and exclude border. 
if keyword_set(use_ref) OR keyword_set(index_ref) then begin
  yy = (indgen(4096)-2048.5) ## (1. + fltarr(4096)) ;array of only x-values wrt IMAGE center
  xx = (1. + fltarr(4096)) ## (indgen(4096)-2048.5) ;array of only y-values wrt IMAGE center
  rr = sqrt(xx^2 + yy^2) ;array of radial distaces from IMAGE center for each pixel
endif

;Determines indices beyond AIA's FOV to be clipped off of final image,
;otherwise corners are populated with 'static' that's amplified by the
;radial filtering
corner_clip = where(rr gt 2400)

;Pixels corresponding to corona, excluding border to prevent incorporating 
;the bars that aia_prep introduces when it shifts the image for alignment.  
masking = where(abs(xx) le 2048-border AND abs(yy) le 2048-border AND rsol gt rmin)
border_mask = where(abs(xx) le 2048-border AND abs(yy) le 2048-border)

;Determines which ring a given pixel belongs in. Each array
;element corresponds to a coronal pixel and is populated with 
;the ring # it belongs in.
inow  = fix((coefR[0] + coefR[1]*rsol[masking]) +0.5)

;Some values related to the binning
if keyword_set(cutout) then res = [(cutout[1]-cutout[0]+1)/bin,(cutout[3]-cutout[2]+1)/bin] $
                       else res = [4096/bin,4096/bin]
charsize = min(res)/512 & charthick = min(res)/1024 ;for image captions                  

;define plot settings
starting_device = !d.name
set_plot,'z'
;loads default AIA color schemes if /color is set
if keyword_set(color) then aia_lct, rr, gg, bb, wavelnth=wv, /load
if not keyword_set(color) then begin
  loadct,altct ;grayscale if /color not set
  tvlct, rr, gg, bb, /get
endif

;set resolution and allow for true color output
device, set_r=[res[0],res[1]], set_pixel_depth=24, decomposed=0

;Point spread function for deconvolution
if keyword_set(deconvolve) then begin 
  if keyword_set(psf_restore) then restore, psf_restore $
    else psf = aia_calc_psf(hd.wavelnth, /use_preflightcore)
  if keyword_set(psf_save) then save, filename=dir_out+wv+'_psf.sav', psf
endif

nerror = 0

;avoids carrying uneeded variables in memory
;undefine, cen & undefine, coefr & undefine, xx & undefine, yy
;undefine, rr & undefine, rsol & undefine, rmax & undefine, rad

; =========================================================================
; Main Loop
; =========================================================================

;loop length = # elements in list minus list length modulo sum size -1 
;So the loop ends when there aren't enough images left for a full sum:
loop_len = nl - (nl mod nsum) - 1
  
last = fix(nsum-1) ;# of last image in sum
middle = fix(nsum/2) ;# of middle image in sum

for kk = 0D, loop_len, nsum*cadence do begin

  t1 = systime(1) ;Establishes an initial time for the current image.

  ;This is used to avoid modifying the loop variable when ejecting images 
  ;for read_sdo errors or short exposures
  aa = kk 
  
  message, 'Reading and summing '+nsum_str+' images...', /informational
  
  ;Reads in nsum fits files and sums them.
  for i=0, last do begin
  
    ;Error handling for read_sdo errors. See header note 1. 
    catch, error_status
    if (error_status ne 0) then begin
      nerror = nerror + 1
      if aa+i le n_elements(list)-1 then begin
      box_message, ['Error, image '+list[aa+i]+' skipped.']
      boost_array, skipped_files, list[aa+i] ;collect skipped files
      endif else begin
         box_message, ['Error, no files left in list to complete sum after skipping short exposures & errors.']
         boost_array, skipped_files, 'Not enough files in list after some skipped. '
      endelse
      ;Collect !error_state information:
      if n_elements(error_details) gt 0 then $
        error_details = concat_struct(temporary(error_details), !error_state)
      if n_elements(error_details) eq 0 then error_details = !error_state
      aa = aa+1 ;skip offending file
      ;Abort if too many errors collect because that's probably
      ;indicative of a different problem. 
      if nerror ge error_thresh then begin
        box_message, ['Too many errors. Returned.']
        print, 'Skipped image | error message:'
        for l=0, n_elements(error_details)-1 do print, skipped_files[0,l],' | ', error_details[l].msg
        return
      endif
      error_status = 0
    endif
  
    ;reads in data, testing for and skipping any short exposures:
    if keyword_set(no_skip) then read_sdo, list[aa+i], hd, da, /silent, /uncomp_delete else begin
      repeat begin
        read_sdo, list[aa+i], hd, da, /silent, /uncomp_delete
        if (hd.exptime lt scalepar.exp) then begin
          box_message, ['Skipped '+list[aa+i]+' for short exposure']
          aa = aa+1
        endif
      endrep until (hd.exptime ge scalepar.exp)
    endelse
    
    ;preps data to ensure proper alignment
    if not keyword_set(no_prep) then begin
    if (hd.lvl_num eq 1.) then aia_prep, temporary(hd), temporary(da), hd, da, $
                           use_ref=use_ref, index_ref=index_ref, /quiet
    endif
                           
    ;cross-correlate for even better alignment if desired                     
    if keyword_set(cross_correl) then begin
      correl_optimize, correl_img, da, xoff, yoff
      da = shift_img(temporary(da), [xoff,yoff])
    endif

    ;normalize for exposure time
    da = (temporary(da)*scalepar.scl/(1.0*hd.exptime))
    if (i eq 0) then begin
      d0 = hd.date_obs ;time of first observation in sum
      dataSUM = double(da) ;initiates sum, double to prevent overflow
    endif else begin
      dataSUM = temporary(dataSUM) + da ;creates sum
    endelse
    if (i eq middle) then begin
      if keyword_set(deconvolve) then $ 
         img = aia_deconvolve_richardsonlucy(da, psf, niter=25) $ 
         else img = da ;used to construct disk
    endif
 
  endfor
  
  ;Cancel CATCH error handling outside of the FITS reading loop because it 
  ;is only designed to bypass unexpected read_sdo errors and would otherwise
  ;loop ad infinitum if the error occurred outside the reading loop. 
  catch, /cancel
  
  ;Removing border before potential deconvolution because edge pixels are prone 
  ;to leaving noticeable artifacts. 
  temparr = fltarr(4096,4096)
  temparr[border_mask] = temporary(dataSUM[border_mask])
  dataSUM = temporary(temparr)
   
;  if keyword_set(deconvolve) then dataSUM = $
 ;    aia_deconvolve_richardsonlucy(temporary(dataSUM), psf, niter=25)
  
; =========================================================================
; Determining the observation time & filename
; =========================================================================

  ;The date and time for the first and last fits files are averaged.
  ;The header date and time are reconstructed from the average.
  t_avg = (anytim2tai(d0)+anytim2tai(hd.date_obs))/2
  t_a = anytim2cal(t_avg, form=7)
  t_a = '20'+strmid(t_a,0,8)+'T'+strmid(t_a,9,16) 
	
  ;Constructs filename based on instrument, observation time, wavelenth, & version tag.  
  filename = strmid(hd.instrume,0,3) + strmid(t_a,0,4) + strmid(t_a,5,2) + $
           strmid(t_a,8,2) + '_' + strmid(t_a,11,2) + strmid(t_a,14,2) + $
           strmid(t_a,17,2) + '_' + wv + ver

; =========================================================================
; The radial filter
; =========================================================================
  
  if keyword_set(rundiff) then begin
    current = dataSUM
    if aa ne 0 then dataSUM = current - previous
    previous = current
  endif
    
  ;Creates scaling paramters if .sav not specified. Otherwise params evaluated
  ;either on each loop increment or just the 1st if the SPEED keyword is set.
  if (kk eq 0) OR keyword_set(create_sav) AND not keyword_set(sav) then begin
   
    ring_avg = dblarr(nring) ;average intensity for given ring
    ring_min = dblarr(nring) ;min intensity for given ring
     
    ;Loop cycles through the rings. Note that max(inow)-1 is used instead
    ;of the # of rings because, depending on the border size, the outermost
    ;rings may not be populated.

    ;for k=0, max(inow)-1 do begin
    for k=min(inow), max(inow)-1 do begin
      idx = where(inow eq k) ;finds pixels in ring 'k'
      if idx[0] eq -1 then begin 
        if n_elements(choke) eq 0 then choke=k 
      endif else begin
        ;ring_avg[k] = total(dataSUM[masking[idx]]) / n_elements(idx)
        ring_avg[k] = median(dataSUM[masking[idx]]) 
        ring_min[k] = min(dataSUM[masking[idx]])
      endelse
    endfor 
    
    ring_struct = {rmax:rmax, nring:nring, inow:inow, ring_avg:ring_avg, ring_min:ring_min, masking:masking}
    
    if n_elements(choke) eq 0 then choke = max(inow)-1    
    
    if wv eq '171' OR wv eq '193' then begin
    
		fitdim = 7
    
		ring_min2 = alog10(ring_min >1e-6)

	    high = fix(nring*0.48)
	    low = fix(nring*0.03)
    
	    fity = ring_min2
	    increment = (fity[high]/3.7725703) / (n_elements(fity[high:*]) -1)
    	fity[high:*] = fity[high] - indgen(n_elements(fity[high:*]))*increment
    	fity[0:low] = fity[low] + reverse(indgen(n_elements(fity[0:low]))*increment*12)
    	fitx = arad
    
    	coef = svdfit(fitx,fity,fitdim,yfit=yfit)
    
    	diff = abs(fity - yfit)
    	keep = where(diff le .05)
    
    	coef = svdfit(fitx[keep],fity[keep],fitdim)
    
    	fit = fltarr(n_elements(arad))
    	for b=0, n_elements(coef)-1 do fit = fit + coef[b]*arad^b
    
    	min_fit = 10.^fit
    
		ring_avg2 = alog10(ring_avg > 1e-6)
		fity = ring_avg2
    	fity[high:*] = fity[high] - indgen(n_elements(fity[high:*]))*increment
    	fity[0:low] = fity[low] + reverse(indgen(n_elements(fity[0:low]))*increment*12)
    	fitx = arad
    	coef = svdfit(fitx,fity,fitdim,yfit=yfit)
    	diff = abs(fity - yfit)
	    keep = where(diff le .05)
    
	    coef = svdfit(fitx[keep],fity[keep],fitdim)
    
	    fit = fltarr(n_elements(arad))
	    for b=0, n_elements(coef)-1 do fit = fit + coef[b]*arad^b
    
	    max_fit = (10.^fit)*scalepar.corscl
    endif
    if wv eq '211' then begin
    	avg_min = avg(ring_min[0:choke-1] >1e-6) ;used for scaling up the lowest mins
    	ring_min = alog10(temporary(ring_min[0:choke-1]) >avg_min/10.)
    	coefdummy = svdfit(arad[0:choke-1],ring_min,5,yfit=yfit) ;polynomial fit accross mins
        ;idx_ = where(abs(ring_min - alog10(avg_min/10.)) le 0.01)
        ;yfit[idx_] = ring_min[idx_]
    	min_fit  = 10.^yfit
        ;maximum
        high = fix(nring*0.48)
        low = fix(nring*0.03)
        ring_min2 = alog10(ring_min >1e-6)
        fity = ring_min2
        increment = (fity[high]/3.7725703) / (n_elements(fity[high:*]) -1)
        ring_avg2 = alog10(ring_avg > 1e-6)
        fity = ring_avg2
    	fity[high:*] = fity[high] - indgen(n_elements(fity[high:*]))*increment
    	fity[0:low] = fity[low] + reverse(indgen(n_elements(fity[0:low]))*increment*5)
    	fitx = arad
    	coef = svdfit(fitx,fity,5,yfit=yfit)
    	diff = abs((fity - yfit)/yfit)
        keep = where(diff le 0.05 and fity gt -5.5)
        coef = svdfit(fitx[keep],fity[keep],7)
        fit = fltarr(n_elements(arad))
        for b=0, n_elements(coef)-1 do fit = fit + coef[b]*arad^b
        max_fit = (10.^fit)*scalepar.corscl
    endif
    if wv eq '335' or wv eq '131' or wv eq '94' then begin
        
    	avg_min = avg(ring_min[0:choke-1] >1e-6) ;used for scaling up the lowest mins
    	ring_min = alog10(temporary(ring_min[0:choke-1]) >avg_min/10.)
    	coefdummy = svdfit(arad[0:choke-1],ring_min,5,yfit=yfit) ;polynomial fit accross mins
        ;idx_ = where(abs(ring_min - alog10(avg_min/10.)) le 0.01)
        ;yfit[idx_] = ring_min[idx_]
    	min_fit  = 10.^yfit
    	max_fit  = ring_avg*scalepar.corscl ;increase corscl to decrease corona brightness
    
    endif
    
    ;Store pamaeters for later use if requested
    if keyword_set(create_sav) then begin 
      save, filename=dir_out+filename+'_params.sav', min_fit, max_fit
      message, 'Created ' + filename+'_params.sav', /informational
    endif
    
  endif
  
;  if not keyword_set(altrundiff) then begin
  
  ;bytescales disk component
  
  if keyword_set(rundiff) then img = fltarr(4096,4096) else img = bytscl(alog(scalepar.a*(temporary(img)/scalepar.max) + 1.0)/alog(scalepar.a), $
                                                                    min=alog(scalepar.a*(scalepar.min/scalepar.max) + 1.0)/alog(scalepar.a),  $
                                                                    max=alog(scalepar.a*(scalepar.max/scalepar.max) + 1.0)/alog(scalepar.a), /nan)
  ;img = bytscl(alog(temporary(img))^gamma_disk, min=mnmx[2], max=mnmx[3])
  if keyword_set(components) then disk = img
  ;renormalizes, bytescales, and overlays the fancified coronal component

  if keyword_set(new_scale) then img[masking] = bytscl((((dataSUM[masking] - min_fit[inow]) / $ 
                              	 (max_fit[inow] - min_fit[inow]))>1e-6)^0.6, $
                               	 min=scalepar.mincor, max=scalepar.maxcor) $
                               
 					   		else img[masking] = bytscl((((dataSUM[masking] - min_fit[inow]) / $ 
                              	 (max_fit[inow] - min_fit[inow]))>1e-6)^scalepar.gammacor, $
                               	 min=scalepar.mincor, max=scalepar.maxcor)
   if keyword_set(components) then begin
     corona = fltarr(4096,4096)
     corona[masking] = img[masking]
  endif
  
;  endif else begin
;    img = fltarr(4096,4096)
;    img[masking] = (dataSUM[masking] - min_fit[inow]) / (max_fit[inow] - min_fit[inow])
;    current = img
;    if aa ne 0 then img = current - previous
;    previous = current
;    img = bytscl(img)
;  endelse 
    
  
; =========================================================================
; Assembling and outputting the final image
; =========================================================================  
  
  ;clips off corners outside AIA's FOV
  if wv ne '171' AND wv ne '193' then img[corner_clip] = 0
  
  if keyword_set(components) then save, disk, corona, masking, corner_clip, img, datasum, filename=dir_out+filename+'_components.sav'
  
  ;creates cutout and resizes image
  if keyword_set(cutout) then begin 
    img = temporary(img[cutout[0]:cutout[1],cutout[2]:cutout[3]])
    if bin ne 1 then img = frebin(temporary(img),res[0],res[1])
  endif else begin
    if bin ne 1 then img = frebin(temporary(img),res[0],res[1])
  endelse

  if keyword_set(rotate) then img = rotate(temporary(img), rotate)
  if keyword_set(rotate) AND kk eq 0 then device, set_r=size(img, /dimensions)
  
  tv, img ;thar she blows
    
  ;Creates the image captions.  
  if not keyword_set(no_capt) then begin
    if keyword_set(bkgnd_capt) then begin
      polyfill, [.81,.81,.99,.99]*res[1], [.93,.995,.995,.93]*res[1], col=1, /device
      polyfill, [.02,.02,.1,.1]*res[1], [.96,.995,.995,.96]*res[1], col=1, /device
      polyfill, [.71,.71,.99,.99]*res[1], [.01,.045,.045,.01]*res[1], col=1, /device 
    endif
    xyouts,0.82,0.97,strmid(t_a,0,10),/norm,charsize=charsize, charthick=charthick ;date
    xyouts,0.82,0.94,strmid(t_a,11,8)+' UT',/norm,charsize=charsize, charthick=charthick ;time
    xyouts,0.03,0.97,wv+' '+STRING("305B),/norm,charsize=charsize, charthick=charthick ;wavelength
   if not keyword_set(half_capt) then xyouts,0.72,0.02,'AIA NASA SAO LMSAL',/norm,charsize=charsize, charthick=charthick ;credits
  endif
  
  ;Update header info for jp2s/idl output
  if (type_out[0] eq 1) OR (type_out[3] eq 1) then begin
    hd.date_obs = t_a ;update header time 
    hd.naxis1 = res[0]
    hd.naxis2 = res[1]
    hd.crpix1 = temporary(hd.crpix1)/bin
    hd.crpix2 = temporary(hd.crpix2)/bin
    hd.r_sun = temporary(hd.r_sun)/bin
    hd.cdelt1 = temporary(hd.cdelt1)*bin
    hd.cdelt2 = temporary(hd.cdelt2)*bin
    update_history, hd, version=progver
  endif
  
  ;Output jp2
  if (type_out[0] eq 1) then begin
    jp2gen = aia_rfilter_jp2gen(hd) ;returns encoding paramters and xml-formatted header.
    ;Use the jhelioviewer naming convention if requested
    if keyword_set(jhelio) then jp2_filename = strmid(filename,3,4)+'_'+strmid(filename,7,2)+ $
                                              '_'+strmid(filename,9,2)+'__'+strmid(filename,12,6)+ $
                                              '__'+observation+ver+'.jp2' $
                           else jp2_filename = filename+'.jp2'                          
    ;initiate jp2 object using jp2gen info
    oJP2 = OBJ_NEW('IDLffJPEG2000',dir_out+jp2_filename,/WRITE, $
                    bit_rate=jp2gen.bit_rate, n_layers=jp2gen.n_layers, $
                    n_levels=jp2gen.n_levels, bit_depth=jp2gen.bit_depth, $ 
                    signed=0, PROGRESSION = 'PCRL', xml=jp2gen.header)
    oJP2->SetData,tvrd(true=1)
    OBJ_DESTROY, oJP2 ;memory management
    message, 'Created ' + jp2_filename, /informational
  endif
  
  ;Output png
  if (type_out[1] eq 1) then begin
    write_png,dir_out+filename+'.png',tvrd(true=1),rr,gg,bb
    message, 'Created ' + filename+'.png', /informational
  endif
  
  ;Output fits
  if (type_out[2] eq 1) then begin
    fits_write, dir_out+filename+'.fits', img, hd
    message, 'Created ' + filename+'.fits', /informational
  endif
  
  ;Output IDL data/index arrays
  if (type_out[3] eq 1) then begin
    boost_array, data_out, img
    if kk eq 0 then index_out = hd
    if kk ne 0 then index_out = concat_struct(temporary(index_out), hd)
    message, 'Added '+filename+' to output array', /informational
  endif 
  
  ;output sav files
  if keyword_set(outsav) then save, img, hd, filename=dir_out+filename+'.sav'

; =========================================================================
; Elapsed time and estimating time left until completion
; ========================================================================= 

  ;Calculates the total elapsed time and converts to hours, minutes, and seconds.
  t_total = anytim(systime(1) - t0, /atime)
  message, 'Elasped time: '+strmid(t_total,11,12,/reverse_offset), /informational
            
  ;Time until completion based on previous loop increment time. 
  ;[(loop length - # of images processed) / loop increment]*tloop
  t_end = anytim(((nl-kk-nsum)/nsum)*(systime(1) - t1), /atime) 
  message, 'Estimated time to completion: '+strmid(t_end,11,12,/reverse_offset), /informational

endfor ;end main loop

;How many images were skipped due to read errors, which files, and what errors? 
if n_elements(error_details) eq 0 then begin
  message, 'Hurray! Finished without errors.', /informational
endif else begin
  box_message, ['Finished with errors.']
  print, 'Skipped image | error message:'
  for l=0, n_elements(error_details)-1 do print, skipped_files[0,l],' | ', error_details[l].msg
endelse
 
;Return plot device to whatever the user had before running <aia_rfilter>    
set_plot, starting_device    
    
return

     	
end
