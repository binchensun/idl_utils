
pro generate_fields, infile, helio, reftime, outpos, notrack=notrack

; read OBSERVER output from Horizons for topo with target=Sun, observer=VLA=-5
; extra precision, quantities 1,20, NO REFRACTION
; routine goes through file to find SOE which is start of ephemeris
; offset to generate coords for HELIO position eg N..W.. at
; reftime='HH:MM' is reftime for heliographic coords
; NOTRACK does not track for solar rotation, uses fixed offsets, eg
;         for limb position
; returns structure OUTPOS 

IF (n_params(0) LT 3) THEN BEGIN
   print,'Usage: generate_fields, infile, heliopos, reftime, outpos [,/notrack]'
   return
end

; generate lat, lon from coord string
flat=float(strmid(helio,1,2))
if (strupcase(strmid(helio,0,1)) eq 'S') then flat=-flat
flon=float(strmid(helio,4,2))
if (strupcase(strmid(helio,3,1)) eq 'E') then flon=-flon

; generate field labels
nfl=1
flds=strarr(nfl)
for j=0,nfl-1 do begin
   if (flat[j] ge 0.0) then flds[j]='N'+string(flat[j],format='(I2.2)') $
                       else flds[j]='S'+string(abs(flat[j]),format='(I2.2)')
   if (flon[j] ge 0.0) then flds[j]=flds[j]+'W'+string(flon[j],format='(I2.2)') $
                       else flds[j]=flds[j]+'E'+string(abs(flon[j]),format='(I2.2)')
end

pos=replicate({day: '', tstr:'', tt:0.0d0, rastr: '', decstr: '',$
       radeg: 0.0d0, decdeg: 0.0d0, delta:0.0d0, deldot:0.0d0},999)
first=0
line=''
openr,1,infile
while not eof(1) do begin
   readf,1,line
   if (strmid(line,2,3) eq 'SOE') then goto, readdat
endwhile

readdat:
i=0
while not eof(1) do begin
   readf,1,line
   if (strmid(line,2,3) eq 'EOE') then goto, enddat
   pos[i].day=strmid(line,1,11)
   pos[i].tstr=strmid(line,13,5)
   pos[i].tt=long(strmid(line,13,2))*3600.d0+long(strmid(line,16,2))*60.0d0
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

; generate labels
date=strmid(pos[0].day,2,2)+month_convert(strmid(pos[0].day,5,3))+strmid(pos[0].day,9,2)
refsec=double(strmid(reftime,0,2))*3600.0d0+double(strmid(reftime,3,2))*60.0d0
print,refsec
refdate=strmid(pos[0].day,9,2)+' '+strupcase(strmid(pos[0].day,5,3))+' '+strmid(pos[0].day,0,4)+' '+strmid(reftime,0,2)+' '+strmid(reftime,3,2)+' 00'
print,date
print,refdate
; report PandB stuff for information purposes
lb_to_radec,refdate,flat[0],flon[0],raoff,decoff

for j=0,nfl-1 do begin      ; loop over fields
   openw,lun,flds[j]+'_'+date+'.radec',/get_lun
   printf,lun,'$$SOE'
   for i=0,nl-1 do begin
      if ((abs(pos[i].tt-refsec) lt 5.) or keyword_set(notrack)) then $
         lb_to_radec,refdate,flat[j],flon[j],raoff,decoff,/quiet $
      else $
         lb_to_radec,refdate,flat[j],flon[j],raoff,decoff,dt=(pos[i].tt-refsec)/3600.,/quiet
      newdec=pos[i].decdeg+decoff/3600.d0
      newra=(pos[i].radeg+raoff/3600.d0/cos(!dpi*newdec/180.0d0))/15.0d0
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
   end
   printf,lun,'$$EOE'
   close,lun
   free_lun,lun
end

; now generate polynomial coefficients for the EVLA
mjd=julday(month_convert(strmid(outpos[0].day,5,3)),fix(strmid(outpos[0].day,9,2)),$
           fix(strmid(outpos[0].day,0,4)),fix(strmid(reftime,0,2)),$
           fix(strmid(reftime,3,2)),0)-2400000.5d0
tt=(outpos.tt-refsec)/86400.d0
rarad=outpos.radeg/180.0d0*!dpi
decrad=outpos.decdeg/180.0d0*!dpi
dist=outpos.delta
cra=poly_fit(tt,rarad,2)
cdec=poly_fit(tt,decrad,2)
cdst=poly_fit(tt,dist,2)

print,''
print,'rightAscensionPolynomialCoefficients = [ ',string(cra[0],format='(E18.11)'),','
print,'                                         ',string(cra[1],format='(E18.11)'),','
print,'                                         ',string(cra[2],format='(E18.11)'),' ]'
print,'declinationPolynomialCoefficients = [ ',string(cdec[0],format='(E18.11)'),','
print,'                                      ',string(cdec[1],format='(E18.11)'),','
print,'                                      ',string(cdec[2],format='(E18.11)'),' ]'
print,'distancePolynomialCoefficients = [ ',string(cdst[0],format='(E18.11)'),','
print,'                                   ',string(cdst[1],format='(E18.11)'),','
print,'                                   ',string(cdst[2],format='(E18.11)'),' ]'
print,'polynomialModel01 = PolynomialInterferometerModel('+string(mjd,format='(F16.10)')+','
print,'                                      rightAscensionPolynomialCoefficients,'
print,'                                      declinationPolynomialCoefficients,'
print,'                                      distancePolynomialCoefficients)'

end
