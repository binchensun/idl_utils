pro read_evla_ephem, infile, outpos

; read OBSERVER output from Horizons for topo with target=Sun, observer=VLA=-5
; extra precision, quantities 1,20, REFRACTION
; routine goes through file to find SOE which is start of ephemeris
; returns structure OUTPOS 

IF (n_params(0) LT 2) THEN BEGIN
   print,'Usage: read_evla_ephem, infile, outpos'
   return
end

pos=replicate({datetime: '', day: '', tstr:'', tt:0.0d0, rastr: '', decstr: '',$
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
   pos[i].datetime=strmid(line,1,17)
   pos[i].day=strmid(line,1,11)
   pos[i].tstr=strmid(line,13,5)
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
end
