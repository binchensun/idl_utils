;
;+
; NAME:
;     SSW_PLOT_RSTN_SRS_BC
; PURPOSE:
;     Plots RSTN SRS 25-180 MHz dynamic spectrum SRS files, gets data if needed.
; CATEGORY:
;     RSTN SRS
; CALLING SEQUENCE:
;     plot_rstn_srs_bc, date, tstart, tend [, file=FILE, dmin=dmin, dmax=dmax, savfile=savfile, obs=OBS, /BW ,/NOPS]
;      -OR-
;     plot_rstn_srs_bc, ssw_start, ssw_stop [, file=FILE, dmin=dmin, dmax=dmax, savfile=savfile, obs=OBS, /BW,/NOPS]
; INPUTS:
;     DATE      String in the format YYYYMMDD ; original SWhite interface
;     TSTART    Starting time string in the format HHMMSS
;     TEND      Ending time string in the format HHMMSS
;     -or-
;     ssw_start - event start time, any SSW time format (per anytim.pro)
;     ssw_stop - event stop time, any SSW time format 
;
; OPTIONAL INPUTS:
;     FILE      Name of file already present: if not supplied, wget is used to get a
;                  file from the NGDC ftp site
;     OBS       Observatory: 4-letter string from PALE (Palehua), LEAR (Learmonth),
;                  SVTO (San Vito), HOLL (Holloman)
;                  Typical time ranges SVTO 5-16, HOLL 13-24, PALE 17-04, 
;                  LEAR 23-10, SGMR 11-22
;     /BW       If set, does black and white, else color table 39
;     /NO_GHOSTSCRIPT - inhibit attempt to display via ghostscript (aka gs)
;     /NOPS - if set, no PostScript file is generated (plot to current device)
;     /PS   - if set, follow PostScript logic (required post 10-feb-2006 mod)
;
; OUTPUTS:
;     Output an RSTN/SRS plot to current plot device or PostScript file 
; RESTRICTIONS: 
;     assumed running under SSW environment;        
;     display assumes unix/linux system w/ghostscript installed
; 
; MODIFICATION HISTORY:
;     Written 14-Sep-2004 by Stephen White
;              7-Aug-2005 - S.L.Freeland - use sock_copy instead of unix wget
;                           change unix spawns to ssw-gen for WinXX compat 
;                           Allow two parameter call using any ssw format
;                           time range. 
;             15-aug-2005 - S.L.Freeland
;                           split data get from data display functions
;                           permit SSWDB in addition to NGDC sources.
;                           calls get_rstn_srs.pro ; file logic -> 
;                           ssw_time2rstn_files.pro 
;                           add non-ps support (utplot_image->current device)
;             29-aug-2005 - S.L.Freeland - fixed a couple of SWhite flagged
;                           typos (affected the orig interface branch)
;              2-sep-2005 - S.L.Freeland - add _EXTRA->utplot_image
;             10-feb-2006 - S.L.Freeland - CHANGED DEFAULT output to 
;                           current plot device and added /PS to follow
;                           old default (PostScript)
;-

pro plot_rstn_srs_bc, date, tstart, tend, file=file, obs=obs, bw=bw, dmin=dmin, dmax=dmax,$
        savfile=savfile, no_ghostscript=no_ghostscript, ps=ps, _extra=_extra

nops=1-keyword_set(ps) ; default changed 10-feb-2006, SLFreeland

case n_params() of
   3:  begin ; original interface - map->ssw times 
           hh0=str_replace(tstart,':','')
           hh1=str_replace(tend,':','')
           nextday=time2file(reltime(file2time(date),days=1),/date_only)  
           sst0=file2time(date+'_'+hh0,out='ecs')
           sst1=file2time(([date,nextday])(hh0 gt hh1)+'_'+hh1,out='ecs')
      endcase
   2: begin ; S.L.Freeland 8-August - permit ssw standard time range inputs
      ; map ssw times -> original interface
        sst0=date
        sst1=tstart
   endcase
   else: begin 
      box_message,['Requires two or three inputs per:',$
      'IDL> plot_rstn_srs, date, tstart, tend, file=file, obs=obs, /bw',$
      '     -OR-',$   
      'IDL> plot_rstn_srs,start_time,stop_time, file=file, obs=obs, /bw',$
      '     (where start_time & stop_time are any SSW standard fmts) ',$
      '---------------------------------------------------------------------',$
      'Reads data from SRS file @NGDC, via sockets observatory OBS if',$
      'FILE is not supplied. ',$ 
      'OBS can be SVTO, HOLL, PALE, LEAR, SGMR', $
      'Typical time ranges SVTO 5-16, HOLL 13-24, PALE 17-04, LEAR 23-10, SGMR 11-22', $
      '/BW prints black-and-white reversed']
          
       return
    endcase
endcase

; 15-aug-2005 - S.L.Freeland - time->data now via get_srs_rstn.pro
rstndata=get_rstn_srs_bc(sst0,sst1,stimes,freq,status=status,$
       file=file,read_info=read_info, _extra=_extra, obs=obs, uniq_freq=nops)

if not status then begin 
   box_message,['Problem finding RSTN/SRS data for this period']
   return
endif

if keyword_set(savfile) then $
    save, file=savfile, rstndata, stimes, freq

if n_elements(obs) eq 0 then obs='OBSn'

if ~ keyword_set(dmin) then dmin=0
if ~ keyword_set(dmax) then dmax=255
specsub=bytscl((rstndata),min=dmin,max=dmax)
if nops then begin 
   if not keyword_set(bw) then loadct,39
   utplot_image,specsub,stimes,freq,$
      title='RSTN Dynamic Spectra - ' + OBS + ' (NGDC SRS Data)',$
      ytitle='Frequency (MHz)',_extra=_extra
endif else begin 
; set up device
   pltdev=!d.name
   savesys,/aplot
   psfile='rstn_'+date+'_'+obs+'.ps'
   set_plot,'PS' 
   device,filename=psfile
   !p.font=0
   !p.thick=3.0
   !x.thick=3.0
   !y.thick=3.0
   !p.charthick=3.0
   !p.charsize=1.0
   device,/portr,/inch,xs=6.5,ys=4.0,xoff=1.2,yoff=3.5
   device,/color,bits_per_pixel=8
   !x.margin=[0,0] & !y.margin=[0,0] & !p.multi=[0,1,1,0]
 ; get dimensions
   ;contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4
   ;px = !x.window * !d.x_vsize ;Get size of window in device units
   ;py = !y.window * !d.y_vsize
   ;swx = px(1)-px(0)       ;Size in x in device units
   ;swy = py(1)-py(0)       ;Size in Y
 
   if keyword_set(bw) then loadct,0 else loadct,39
   specsub=temporary(rstndata)
   tsecs=gt_tagval(read_info,/ssecs)

   if ~ keyword_set(dmin) then dmin=0
   if ~ keyword_set(dmax) then dmax=255
   bb=bytscl((specsub),min=dmin,max=dmax)
   tv, bb, 0.12, 0.11, xsize=0.85, ysize=0.87, /norm
   contour,specsub,tsecs,freq,/noerase, xstyle=5, ystyle=1,color=0,$
   pos = [0.12,0.11,0.97,0.98],/norm,$
   xthick=3.0,ythick=3.0,ytitle='Frequency (MHz)',levels=[1.e6]
   timeaxis,title='Time (UT) '+date,color=0

   loadct,0
   xyouts,0.50,1.02,'RSTN/'+obs,/norm,size=1.6,align=0.5
   device,/close 
   box_message,'PostScript plot written to> ' + psfile
   if os_family() eq 'unix' and 1-keyword_set(no_ghostscript) then begin
    spawn,['which','gs'],out,/noshell ; check for gs (ghostscript)
    if file_exist(out(0)) then $
       spawn,[out(0),psfile],/noshell else $
          box_message,'No Ghostscript available'
   endif
   restsys,/aplot,/init
endelse
set_plot,'x'
end

