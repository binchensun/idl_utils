pro evla_poly2ephem, sunephem, polycoeff, outephem=outephem,outpos=outpos
;----------------------- INPUTS -------------------------
;sunephem: from JPL Horizons
;   read OBSERVER output from Horizons for topo with target=Sun, observer=VLA=-5
;   extra precision, quantities 1,20, REFRACTION
;polycoeff is an IDL structure containing the following items:
;   cra: 3-element double array, polynomials of RA (in rad) -- RA = cra[0]+cra[1]*(t-t0)+cra[2]*(t-t0)^2. 
;   cdec: 3-element double array, polynomials of DEC (in rad) -- DEC = cdec[0]+cdec[1]*(t-t0)+cdec[2]*(t-t0)^2. 
;   cdst: 3-element double array, polynomials of Sun-Earth distance (in AU) -- Distance = cdst[0]+cdst[1]*(t-t0)+cdst[2]*(t-t0)^2. 
;   t0: reference time in mjd

; ---- read the input solar ephem file, convert into the output position structure -------
pos=replicate({datetime: '', day: '', tstr:'', tt:0.0d0, rastr: '', decstr: '',$
       radeg: 0.0d0, decdeg: 0.0d0, delta:0.0d0, deldot:0.0d0},999)
first=0
line=''
openr,1,sunephem
while not eof(1) do begin
   readf,1,line
   if (strmid(line,2,3) eq 'SOE') then goto, readdat
endwhile

readdat:
i=0
while not eof(1) do begin
   readf,1,line
   if (strmid(line,2,3) eq 'EOE') then goto, enddat
   pos[i].datetime=strmid(line,1,17)
   pos[i].day=strmid(line,1,11)
   pos[i].tstr=strmid(line,13,5)
;   pos[i].tt=long(strmid(line,13,2))*3600.d0+long(strmid(line,16,2))*60.0d0
   pos[i].tt=anytim(strmid(line,1,17)) ;ephemerides date and time in seconds
   pos[i].rastr=strmid(line,23,13)
   pos[i].decstr=strmid(line,37,13)
   pos[i].delta=strmid(line,51,16)
   pos[i].deldot=strmid(line,69,10)
   pos[i].radeg=(long(strmid(line,23,2))+long(strmid(line,26,2))/60.0d0+double(strmid(line,29,7))/3600.0d0)*15.0d0
   pos[i].decdeg=(long(strmid(line,38,2))+long(strmid(line,41,2))/60.0d0+double(strmid(line,44,6))/3600.0d0)
   if (strmid(line,37,1) eq '-') then pos[i].decdeg=-pos[i].decdeg
   i=i+1
end

enddat:
nl=i
pos=pos[0:i-1]
close,1
outpos=pos

;------ generate output RA, DEC, Distance at the same times as the input ephem file ------
cra=polycoeff.cra ;in rad
cdec=polycoeff.cdec ;in rad
cdst=polycoeff.cdst ;in rad
t0=polycoeff.t0 ;in mjd
t0_=mjd2anytim(t0)

tt=(pos.tt-t0_)/86400d0
rarad=cra[0]+cra[1]*tt+cra[2]*tt^2d0
outpos.radeg=rarad*180d0/!dpi
decrad=cdec[0]+cdec[1]*tt+cdec[2]*tt^2d0
outpos.decdeg=decrad*180d0/!dpi
outpos.delta=cdst[0]+cdst[1]*tt+cdst[2]*tt^2d0

;------write out pointing ephem file--------
if keyword_set(outephem) then begin
    openw,lun,outephem,/get_lun
    printf,lun,'$$SOE'
    for i=0,nl-1 do begin
      newdec=outpos[i].decdeg
      newra=outpos[i].radeg/15d0
      ; format into string
      rah=long(newra)
      ram=long((newra-rah)*60.0d0)
      ras=long((newra-rah-ram/60.0d0)*3600.0d0)
      rasf=long((newra-rah-ram/60.0d0-ras/3600.d0)*3600.0d0*1.d4)
      rastr=string(rah,format='(I2.2)')+' '+string(ram,format='(I2.2)')+' '+$
            string(ras,format='(I2.2)')+'.'+string(rasf,format='(I4.4)')
      if (newdec lt 0.0d0) then sgn='-' else sgn=' '
      dech=long(abs(newdec))
      decm=long((abs(newdec)-dech)*60.0d0)
      decs=long((abs(newdec)-dech-decm/60.0d0)*3600.0d0)
      decsf=long((abs(newdec)-dech-decm/60.0d0-decs/3600.d0)*3600.0d0*1.d3)
      decstr=sgn+string(dech,format='(I2.2)')+' '+$
             string(decm,format='(I2.2)')+' '+$
             string(decs,format='(I2.2)')+'.'+string(decsf,format='(I3.3)')
      printf,lun,pos[i].day,pos[i].tstr,rastr,decstr,pos[i].delta,pos[i].deldot,$
        format='(1x,a11,1x,a5,5x,a13,1x,a13,1x,f16.14,2x,f10.7)'
      outpos[i].rastr=rastr
      outpos[i].radeg=newra*15.0d0
      outpos[i].decstr=decstr
      outpos[i].decdeg=newdec
    endfor
    printf,lun,'$$EOE'
    close,lun
    free_lun,lun
endif
end
