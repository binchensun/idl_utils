pro plot_evla_fov, cenfile=cenfile, pntfile=pntfile, freq=freq, $
    pltfile=pltfile, reftime=reftime, tstep=tstep, aia=aia, hmi=hmi, $
    type=type,xrange=xrange,yrange=yrange
; -------INPUTS-----
; REQUIRED:
;   cenfile: RA and DEC of the Sun center, from JPL Horizons ephemeris
;   --- either of the following should be supplied for the pointing center ---
;   pntfile: RA and DEC of the reference (pointing center), from generate_fields_bc.pro
;   freq is in GHz for size of PB, PANG is P-angle for plotting
; OPTIONAL:
;   reftime in anytim format, used to calculate the p-angle, if not set, use the first record in centfile
;   type='helio' gives heliocentric representation;
;   type='radec' gives radec offset representation
;   tstep: time interval between each FOV circle, in minutes
;   aia: plot aia map according to reftime, default '304', 
;       accept inputs '94','131','171','193','211','335','1600'
;   hmi: plot hmi LOS magnetogram according to reftime, default '45s', accept inputs '45s','720s'
;   xrange, yrange: xrange and yrange to plot, default: [-1200,1200]
;IF (n_params(0) LT 3) THEN BEGIN
;   print,'Usage: plot_fields, cenfile, arfile, freq_GHz, pang=pang'
;   return
;end

pbrad=45.*60./freq/2.
nfreq=n_elements(freq)
if ~keyword_set(cenfile) then begin
    print, 'Please supply the solar center RA and DECs (from JPL Horizons)'
    return
endif else begin
    read_evla_ephem,cenfile,cpos
endelse
if ~keyword_set(pntfile) then begin
    print, 'Please supply the pointing info in RA and DECs (from generate_fields_bc)'
    return
endif else begin
    read_evla_ephem,pntfile,pos
endelse
npos=n_elements(pos)
datetime=anytim(pos.datetime)
if ~keyword_set(reftime) then reftime=datetime[0]
dt=min(datetime-anytim(reftime),t_ind,/absolute)
if dt gt 600. then begin
    print, 'reference time is not in the input time range' 
    return
endif
if ~keyword_set(tstep) then tstep=120. ;2 hr
if ~keyword_set(xrange) then xrange=[-1200,1200]
if ~keyword_set(yrange) then yrange=[-1200,1200]
print,reftime
;find out solar p angle, if not set, use the first record in the pntfile 
ang=pb0r(anytim(reftime))  
prad=-ang[0]*!dtor ;p angle increases to the east (CCW)
rsun=ang[2]*60. ; in arcsec
print,'p angle: ',ang[0]
if ~keyword_set(pltfile) and keyword_set(pntfile) then pltfile=pntfile+'.ps'

set_plot,'PS'
ysz=6.
if keyword_set(aia) and keyword_set(hmi) then begin
    !p.multi=[0,2,1] 
    xsz=12.
    figpos=bclayout([2,1],fov=[0.10,0.20,0.96,0.98],xgap=0.08,ygap=0.)
endif else begin
    !p.multi=[0,1,1]
    xsz=6.
    figpos=[0.1,0.2,0.95,0.95]
endelse
device,file=pltfile,/inches,/color,bits=8,xsize=xsz,ysize=ysz,/encaps
!p.font=0
!p.thick=3.0
!x.thick=3.0
!y.thick=3.0
!p.charthick=3.0
!p.charsize=1.2

if ~keyword_set(type) then type='helio'
if type eq 'radec' then begin
    xtitle='RA offset (arcsec)'
    ytitle='DED offset (arcsec)'
endif 
if type eq 'helio' then begin
    xtitle='X (arcsec)'
    ytitle='Y (arcsec)'
endif 
;loadct,0
;Polyfill, [1,1,0,0,1], [1,0,0,1,1], /normal, COLOR=cgColor('black')
if keyword_set(hmi) then begin
    if hmi eq 1 then hmi='720s'
    case hmi of 
      '45s': ds='hmi.M_45s' 
      '720s': ds='hmi.M_720s'
    endcase
    mnmx=[-500,500]
    bt=pos[0].datetime
    et=pos[npos-1].datetime
    ssw_jsoc_time2data,anytim(reftime,/vms),anytim(anytim(reftime)+12.,/vms),index,data,$
        ds=ds
    aia_prep,index,data,oindex,odata
    index2map,oindex,odata,hmimap0
    hmimap=grid_map(hmimap0,1024,1024)
    loadct,0
    if !p.multi[1] eq 1 then position=figpos else position=figpos[*,0]
    Polyfill, [position[0],position[2],position[2],position[0],position[0]], $
              [position[1],position[1],position[2],position[2],position[1]], $
              /normal, COLOR=cgColor('black')
    plot_map,hmimap,dmin=mnmx[0],dmax=mnmx[1],$
        xran=xrange,yran=yrange,tit=' ',position=position,$
            xsty=1,ysty=1,/noerase,color=cgcolor('black'),charsize=0.8,ticklen=-0.01
    xyouts,position[0]+0.01,position[1]+0.02,strmid(anytim(hmimap.time,/vms),0,20),$
        color=cgcolor('white'),charsize=0.8,/norm
    xx=rsun*cos(indgen(1001)*!pi/500.)
    yy=rsun*sin(indgen(1001)*!pi/500.)
    loadct,0
    oplot,xx,yy,color=cgcolor('white'),thick=1,linesty=2

    t1=0.
    n=0
    for j=0,npos-1 do begin
        if j gt 0 then dt=datetime[j]-datetime[j-1] else dt=0.
        if t1 ge tstep*60. then begin
            t1=0.
            n=n+1
            for f=0,nfreq-1 do begin
               xx=pbrad[f]*cos(indgen(201)*!pi/100.)
                   yy=pbrad[f]*sin(indgen(201)*!pi/100.)
               decoffdeg=pos[j].decdeg-cpos[j].decdeg
               raoffdeg=(pos[j].radeg-cpos[j].radeg)*cos(cpos[j].decdeg*!dtor)
               raoff=raoffdeg*3600. & decoff=decoffdeg*3600.
               xoff=(-raoff)*cos(prad)-decoff*sin(prad)
               yoff=(-raoff)*sin(prad)+decoff*cos(prad)
               loadct,0,/silent
               if type eq 'radec' then oplot,-raoff+xx,decoff+yy,linestyle=1,color=cgcolor('white')
               if type eq 'helio' then oplot,xoff+xx,yoff+yy,linestyle=1,color=cgcolor('white')
            endfor
        endif else begin
            t1=t1+dt
        endelse
    endfor
    ; plot solar axis
    if type eq 'radec' then plots,rsun*[sin(prad),-sin(prad)],rsun*[cos(prad),-cos(prad)],linestyle=1
    if type eq 'helio' then plots,[0,0],[-rsun,rsun],linestyle=1,color=cgcolor('white')
endif
if keyword_set(aia) then begin
    if aia eq 1 then aia='304'
    case aia of 
      '171': mnmx = [1.1, 5.0]
      '211': mnmx = [1.2, 5.0]
      '94': mnmx = [0.2, 2.5]
      '335': mnmx = [1., 4.0]
      '193': mnmx = [1., 5.0]
      '304': mnmx = [0.5, 3.0]
      '131': mnmx = [0.2, 2.0]
      '1600': mnmx = [0.8, 3.5] 
    endcase
    bt=pos[0].datetime
    et=pos[npos-1].datetime
    ssw_jsoc_time2data,anytim(reftime,/vms),anytim(anytim(reftime)+12.,/vms),index,data,$
        ds='aia.lev1',waves=aia
    aia_prep,index,data,oindex,odata
    index2map,oindex,odata,aiamap0
    aiamap=grid_map(aiamap0,1024,1024)
    if !p.multi[1] eq 1 then position=figpos else position=figpos[*,1]
    loadct,0,/silent
    Polyfill, [position[0],position[2],position[2],position[0],position[0]], $
              [position[1],position[1],position[2],position[2],position[1]], $
              /normal, COLOR=cgColor('black')
    aia_lct,wave=aia,/load
    plot_map,aiamap,/log,dmin=10.^mnmx[0],dmax=10.^mnmx[1],$
        xran=xrange,yran=yrange,tit=' ',position=position,$
            xsty=1,ysty=1,/noerase,color=cgcolor('black'),charsize=0.8,ticklen=-0.01
    xyouts,position[0]+0.01,position[1]+0.02,strmid(anytim(aiamap.time,/vms),0,20),$
        color=cgcolor('white'),charsize=0.8,/norm
endif
if ~keyword_set(aia) and ~keyword_set(hmi) then begin 
    loadct,0,/silent
    if !p.multi[1] eq 1 then position=figpos else position=figpos[*,1]
    loadct,0,/silent
    Polyfill, [position[0],position[2],position[2],position[0],position[0]], $
              [position[1],position[1],position[2],position[2],position[1]], $
              /normal, COLOR=cgColor('black')
    plot,xrange,yrange,xtitle=xtitle,$
       ytitle=ytitle,/nodata,xstyle=1,ystyle=1,$
       position=position, tit='VLA Pointing',color=cgcolor('white')
endif
    
xx=rsun*cos(indgen(1001)*!pi/500.)
yy=rsun*sin(indgen(1001)*!pi/500.)
loadct,0
oplot,xx,yy,color=cgcolor('white'),thick=1,linesty=2

nfreq=n_elements(freq)
pbrad=45.*60./freq/2.
t1=0.
n=0
raoffs=fltarr(npos) & decoffs=raoffs
xoffs=fltarr(npos) & yoffs=xoffs
for j=0,npos-1 do begin
    decoffdeg=pos[j].decdeg-cpos[j].decdeg
    raoffdeg=(pos[j].radeg-cpos[j].radeg)*cos(cpos[j].decdeg*!dtor)
    raoffs[j]=raoffdeg*3600. & decoffs[j]=decoffdeg*3600.
    xoffs[j]=(-raoffs[j])*cos(prad)-decoffs[j]*sin(prad)
    yoffs[j]=(-raoffs[j])*sin(prad)+decoffs[j]*cos(prad)
    if j gt 0 then dt=datetime[j]-datetime[j-1] else dt=0.
    if t1 ge tstep*60. then begin
        t1=0.
        n=n+1
        for f=0,nfreq-1 do begin
           xx=pbrad[f]*cos(indgen(201)*!pi/100.)
           yy=pbrad[f]*sin(indgen(201)*!pi/100.)
           loadct,0,/silent
           if type eq 'radec' then oplot,-raoffs[j]+xx,decoffs[j]+yy,linestyle=1,color=cgcolor('white')
           if type eq 'helio' then oplot,xoffs[j]+xx,yoffs[j]+yy,linestyle=1,color=cgcolor('white')
        endfor
    endif else begin
        t1=t1+dt
    endelse
endfor
; plot solar axis
if type eq 'radec' then plots,rsun*[sin(prad),-sin(prad)],rsun*[cos(prad),-cos(prad)],linestyle=1
if type eq 'helio' then plots,[0,0],[-rsun,rsun],linestyle=1,color=cgcolor('white')

for f=0,nfreq-1 do xyouts,0.12,0.94-0.03*f,'FOV at '+string(freq[f],format='(F3.1)')+' GHz = '+$
	string(2*pbrad[f]/60.,format='(F4.1)')+' arcmin',/norm,align=0,charsize=0.8,color=cgcolor('white')
y0=0.11
xyouts,0.12,y0,'Sol Center RA DEC File: '+cenfile,/norm,charsize=0.8,color=cgcolor('black')
xyouts,0.12,y0-0.025,'Pointing RA DEC File: '+pntfile,/norm,charsize=0.8,color=cgcolor('black')
xyouts,0.12,y0-0.05,'Time Range: '+strmid(anytim(pos[0].datetime,/vms),0,20)+' - '+$
    strmid(anytim(pos[npos-1].datetime,/vms,/time_only),0,8),/norm,charsize=0.8,color=cgcolor('black')
xyouts,0.12,y0-0.075,'Reference Time: '+strmid(anytim(reftime,/vms),0,20),/norm,charsize=0.8,color=cgcolor('black')
; Solar X and Y of the pointing center at the reference time
xyouts,0.12,y0-0.1,'Reference Pointing (Solar X, Y): '+string(xoffs[t_ind],'(f6.1)')+', '+$
    string(yoffs[t_ind],'(f6.1)'),/norm,charsize=0.8,color=cgcolor('black')
device,/close
!p.multi=0

end
