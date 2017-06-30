
pro plot_fields, cenfile, arfile, freq, pang=pang

; freq is in GHz for size of PB, PANG is P-angle for plotting

IF (n_params(0) LT 3) THEN BEGIN
   print,'Usage: plot_fields, cenfile, arfile, freq_GHz, pang=pang'
   return
end

read_evla_ephem,cenfile,cpos
read_evla_ephem,arfile,pos

; set up plot area

set_plot,'PS'
device,/portrait,/inches,xsize=6.5,xoff=1.2,ysize=6.5,yoff=3.5
!p.font=0
!p.thick=3.0
!x.thick=3.0
!y.thick=3.0
!p.charthick=3.0
!p.charsize=1.2
!p.multi=[0,1,1,0] & !x.margin=[0,0] & !y.margin=[0,0]

plot,[-20,20],[-20,20],xtitle='RA offset (arcmin)',$
   ytitle='Dec offset (arcmin)',/nodata,xstyle=1,ystyle=1

xx=16.*cos(indgen(201)*!pi/100.)
yy=16.*sin(indgen(201)*!pi/100.)
oplot,xx,yy

pbrad=45./freq/2.
xx=pbrad*cos(indgen(201)*!pi/100.)
yy=pbrad*sin(indgen(201)*!pi/100.)
; plot disk-center field
; oplot,xx,yy,linestyle=2

for j=0,n_elements(pos)-1 do begin
   radecoffset,cpos[j].rastr+' '+cpos[j].decstr,pos[j].rastr+' '+pos[j].decstr,$
       raoff,decoff,/quiet
   oplot,-raoff/60.+xx,decoff/60.+yy,linestyle=2
end

; plot solar axis
if keyword_set(pang) then begin
   prad=-pang/180.*!pi
   plots,16.*[sin(prad),-sin(prad)],16.*[cos(prad),-cos(prad)],linestyle=1
endif

xyouts,0.04,0.94,'PB at '+string(freq,format='(F3.1)')+' GHz = '+string(2*pbrad,format='(F4.1)')+' arcmin',/norm,align=0,charsize=1.2

xyouts,0.33,-0.13,'Files: ',align=1.0,/norm,charsize=1.2
xyouts,0.33,-0.13,cenfile,align=0,/norm,charsize=1.2
xyouts,0.33,-0.13-0.04,arfile,align=0,/norm,charsize=1.2

device,/close & spawn,'gs idl.ps'

end
