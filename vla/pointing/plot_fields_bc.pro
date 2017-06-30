pro plot_fields_bc, cenfile=cenfile, pntfile=pntfile, freq=freq, $
    pltfile=pltfile, refdate=refdate, type=type,$
    arxy=arxy
; -------INPUTS-----
; REQUIRED:
;   cenfile: RA and DEC of the Sun center, from JPL Horizons ephemeris
;   --- either of the following should be supplied for the pointing center ---
;   pntfile: RA and DEC of the reference (pointing center), from generate_fields_bc.pro
;   freq is in GHz for size of PB, PANG is P-angle for plotting
; OPTIONAL:
;   refdate in anytim format, used to calculate p-angle, if not set, use the first record in centfile
;   type='helio' gives heliocentric representation;
;   type='radec' gives radec offset representation
;   arxy=[solarx, solary] in arcsecs, showing a plus of the AR location

;IF (n_params(0) LT 3) THEN BEGIN
;   print,'Usage: plot_fields, cenfile, arfile, freq_GHz, pang=pang'
;   return
;end

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
; set up plot area

if ~keyword_set(pltfile) and keyword_set(pntfile) then pltfile=pntfile+'.ps'
set_plot,'PS'
device,file=pltfile,/portrait,/inches,xsize=6.5,ysize=6.5
!p.font=0
!p.thick=3.0
!x.thick=3.0
!y.thick=3.0
!p.charthick=3.0
!p.charsize=1.2
!p.multi=[0,1,1,0] ;& !x.margin=[0,0] & !y.margin=[0,0]

if ~keyword_set(type) then type='radec'
if type eq 'radec' then begin
    xtitle='RA offset (arcsec)'
    ytitle='DED offset (arcsec)'
endif 
if type eq 'helio' then begin
    xtitle='X (arcsec)'
    ytitle='Y (arcsec)'
endif 
;plot,[-1200,1200],[-1200,1200],xtitle=xtitle,$
;   ytitle=ytitle,/nodata,xstyle=1,ystyle=1
plot,[-1600,1600],[-1600,1600],xtitle=xtitle,$
   ytitle=ytitle,/nodata,xstyle=1,ystyle=1,$
   position=[0.17,0.22,0.9,0.95], tit='VLA Pointing'

;find out solar p angle, if not set, use the first record in the pntfile 
if ~keyword_set(refdate) then refdate=pos[0].datetime 
ang=pb0r(anytim(refdate))  
prad=-ang[0]*!dtor ;p angle increases to the east (CCW)
rsun=ang[2]*60. ; in arcsec

print,'p angle: ',ang[0]
xx=rsun*cos(indgen(201)*!pi/100.)
yy=rsun*sin(indgen(201)*!pi/100.)
oplot,xx,yy

nfreq=n_elements(freq)
pbrad=45.*60./freq/2.
for j=0,n_elements(pos)-1 do begin
	for f=0,nfreq-1 do begin
	   xx=pbrad[f]*cos(indgen(201)*!pi/100.)
           yy=pbrad[f]*sin(indgen(201)*!pi/100.)
	   decoffdeg=pos[j].decdeg-cpos[j].decdeg
	   raoffdeg=(pos[j].radeg-cpos[j].radeg)*cos(cpos[j].decdeg*!dtor)
	   raoff=raoffdeg*3600. & decoff=decoffdeg*3600.
	   xoff=(-raoff)*cos(prad)-decoff*sin(prad)
	   yoff=(-raoff)*sin(prad)+decoff*cos(prad)
	   if type eq 'radec' then oplot,-raoff+xx,decoff+yy,linestyle=2
	   if type eq 'helio' then oplot,xoff+xx,yoff+yy,linestyle=2
	endfor
endfor

; plot solar axis
if type eq 'radec' then plots,rsun*[sin(prad),-sin(prad)],rsun*[cos(prad),-cos(prad)],linestyle=1
if type eq 'helio' then plots,[0,0],[-rsun,rsun],linestyle=1

for f=0,nfreq-1 do xyouts,0.18,0.92-0.04*f,'FOV at '+string(freq[f],format='(F3.1)')+' GHz = '+$
	string(2*pbrad[f]/60.,format='(F4.1)')+' arcmin',/norm,align=0,charsize=1.2

xyouts,0.12,0.11,'Sol Center RA DEC File: '+cenfile,/norm,charsize=1.0
xyouts,0.12,0.11-0.03,'Pointing RA DEC File: '+pntfile,/norm,charsize=1.0
xyouts,0.12,0.11-0.06,'Time Begin: '+anytim(pos[0].datetime,/vms),/norm,charsize=1.0
xyouts,0.12,0.11-0.09,'Time End: '+anytim(pos[n_elements(pos)-1].datetime,/vms),/norm,charsize=1.0

if keyword_set(arxy) then begin
    sz=size(arxy)
    if sz[1] ne 2 then begin
        print, 'input must be [[x0,y0],[x1,y1]...]'
    endif else begin
        if sz[0] eq 1 then plots,arxy,psym=1,color=0,symsize=2
        if sz[0] eq 2 then begin
            nar=sz[2]
            for i=0,nar-1 do plots,arxy[*,i],psym=1,color=0,symsize=2
        endif
    endelse
endif
device,/close

end
