;----------------------------------------------------------------------------------
pro gb_bkg_sub,spec,bkg,specsub

 ; determine background spectrum

 IF (n_params(0) LT 2) THEN BEGIN
   print,'Usage: gb_bkg_sub, spec, bkg [, specsub]'
   print,''
   print,'Determines background BKG for a spectrum SPEC using median of values '
   print,'from low times. '
   print,'Optionally returns background-subtracted spectrum SPECSUB.'
   return
 ENDIF

 nx=n_elements(spec[*,0])
 nfr=n_elements(spec[0,*])

 ; create background array
 ; identify low times in light curve: needs to be general, no
 ; assumptions about where frequency is, so use low data
 avespec=total(spec,1) ; sum in time
 ii=sort(avespec)      ; sort to order
 lc=total(spec[*,ii[0:nfr/3L]],2) ; use lowest third of frequency points

 ; Optional background subtraction: median of lowest 60 points
 jj=sort(lc) & kk=jj[0:((nx/10L)<59)] &  bkg=reform(spec[0,*])
 for ij=0,nfr-1 do bkg[ij]=median(spec[kk,ij])
 specsub=spec-rebin(reform(bkg,1,nfr),nx,nfr)

end

;------------------------------------------------------------------------------
function mpow, arr, power

return, (arr > 0.)^power -(-arr>0.)^power

end
;------------------------------------------------------------------------------
;+
; NAME:
;     GET_RSTN_SRS
; PURPOSE:
;     GETS RSTN SRS 25-180 MHz dynamic spectrum SRS files, gets file if needed.
; CATEGORY:
;     RSTN SRS
; CALLING SEQUENCE:
;     rstndata=get_rstn_srs(ssw_start, ssw_stop, stimes, sfreq, $
;        [, file=FILE, obs=OBS ] [,/no_backsub]   [,/uniq_freq]
; INPUTS:
;     ssw_start - event start time, any SSW time format (per anytim.pro)
;     ssw_stop - event stop time, any SSW time format 
;
; OPTIONAL INPUTS:
;     FILE      Name of file already present: if not supplied, wget is used to get a
;                  file from the NGDC ftp site
;     OBS (in/out) Observatory: 4-letter string from PALE (Palehua), LEAR (Learmonth),
;                  SVTO (San Vito), HOLL (Holloman)
;                  Typical time ranges SVTO 5-16, HOLL 13-24, PALE 17-04, 
;                  LEAR 23-10, SGMR 11-22
; OUTPUTS:
;    Function returns spectra (ntimes,nfrequencies)
;    stimes - vector of ssw times implied in spectra
;    sfrequency - vector of frequecies implied in spectra (25-180MHz)
;
; KEYWORD PARAMETERS: 
;    /NO_BACKSUB - if set, don't apply background subtraction alg.
;    /UNIQ_FREQ - if set, eliminate duplicate frequencies (75MHZ)
;    READ_INFO - original params from read_rstn_srs = looks like:
;                {fdate:'',freq:bytarr(nfreq),times:lonarr(ntimes)}
;    OBS - (in/out) if supplied, observatory to use; output is actual
;    status - True(1) is something returned, False(0)=problem
;    /NOSCALE - if set, don't apply default scaling
;
; RESTRICTIONS: 
;     assumed running under SSW environment;        
; 
; MODIFICATION HISTORY:
;     Written 14-Sep-2004 by Stephen White
;              7-Aug-2005 - S.L.Freeland - use sock_copy instead of unix wget
;                           change unix spawns to ssw-gen for WinXX compat 
;                           Allow two parameter call using any ssw format
;                           time range. 
;             15-Aug-2005 - S.L.Freeland - broke get data function
;                           from display data functions
;                           call ssw_time2rstn_files.pro
;                           (enables non-ngdc urls or local nfs/SSWDB)
;             30-aug-2005 - S.L.Freeland - use proper reference time->ssw times
;
;-
function get_rstn_srs_bc, ssw_start, ssw_stop, stimes,sfrequency,$
   file=file, obs=obs, read_info=read_info, uniq_freq=uniq_freq , $
   status=status, noscale=noscale

status=0
; file is optional if file already here, else wgets it
; obs specifies observatory HOLL, SVTO, LEAR, PALE if don't want default

if n_params() lt 2 then begin 
      box_message,[$
      'IDL> plot_rstn_srs,start_time,stop_time, file=file, obs=obs, /bw',$
      '     (where start_time & stop_time are any SSW standard fmts) ',$
      '---------------------------------------------------------------------',$
      'Reads data from SRS file local or @NGDC, via sockets observatory OBS if',$
      'FILE is not supplied or not found locally ',$ 
      'OBS can be SVTO, HOLL, PALE, LEAR, SGMR', $
      'Typical time ranges SVTO 5-16, HOLL 13-24, PALE 17-04, LEAR 23-10, SGMR 11-22']
          
       return,''
endif

 if not file_exist(file) then begin
    file=ssw_time2rstn_files(ssw_start, ssw_stop, _extra=_extra,obs=obs)
    if not file_exist(file(0)) then begin 
       break_url,file,ip,subdir,fname
       tmpdir=get_temp_dir()
       locfile=concat_dir(tmpdir,fname)
       if not file_exist(locfile) then begin
           box_message,['Transferring file via url...>',file]
           sock_copy,/progress,file,out_dir=tmpdir
           if not file_exist(locfile) then begin 
              box_message,'Problem with socket transfer, returning...'
              return,''
           endif
        endif
    endif 
    if file_exist(locfile) then file=locfile 
 endif 

 ; now read the file
 read_rstn_srs, file, ts, fr, sp, fdate, uniq_freq=uniq_freq
 sswt=anytim(file2time(fdate))+ts ; times -> ssw (anytim) 
time_window,sswt,t0,t1,out='ecs'
box_message,'File spans times: ' + t0 + ' -to- '+t1
 ss=sel_timrange(anytim(sswt,/int), /between, $
     anytim(ssw_start,/int), anytim(ssw_stop,/int))
 ; extract the desired period
 if n_elements(ss) lt 2 then begin 
   box_message,'no times within requested time range... returning'
   return,''
 endif else begin 
   i0=ss(0)
   i1=last_nelem(ss)
 endelse
 sswt=sswt[i0:i1]
 tsecs=ts[i0:i1]
 spec=sp[i0:i1,*]
 freq=fr
; dont want spectrum to be byte array or bkgsub doesn't work due to wrapping
 if not keyword_set(no_background) then begin 
   gb_bkg_sub,float(spec),bkg,specsub 
   spec=temporary(specsub)
 endif

if not keyword_set(noscale) then begin
    nx=n_elements(spec[*,0])
    nfr=n_elements(freq)
    ifr=where((freq gt 30.0) and (freq lt 81.0)) & smed=median(spec[*,ifr])
    smx=max(spec[*,ifr]>(smed))
    smn=stdev(spec[0:60,ifr])
    smx=min([10.*smn,smx])
    bb=bytscl(mpow(spec,0.5),min=sqrt(smn)/3.,max=sqrt(smx))
    if keyword_set(bw) then bb=255-bb
    bb=bytscl(spec)
    spec=temporary(bb)
endif
  
 utcint=anytim(sswt,/utc_int)
 retval=spec
 read_info={fdate:fdate,file:file,ssecs:tsecs,sfreq:fr}
 stimes=sswt
 sfrequency=fr
 status=1
 return,retval

end
