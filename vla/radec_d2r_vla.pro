pro radec_d2r_vla, ra, dec
;ra in degrees, should be double precision
;dec in degrees, should be double precision
; or ra in this string format '00h00m00.0000s'
;    dec in this string format '+00d00m00.0000s'
;                              '-00d00m00.0000s' 
;convert to rad in double precision to be used in the script
if size(ra,/tname) eq 'STRING' then begin
    ;convert the string to rad
    h=double(strmid(ra,0,strpos(ra,'h'))) 
    m=double(strmid(ra,strpos(ra,'h')+1,2)) 
    s=double(strmid(ra,strpos(ra,'m')+1,strpos(ra,'s')-strpos(ra,'m')-1))
    dd=double(strmid(dec,1,strpos(dec,'d')))
    if strmid(dec,0,1) eq '-' then dd=-dd
    mm=double(strmid(dec,strpos(dec,'d')+1,2))
    ss=double(strmid(dec,strpos(dec,'m')+1,strpos(dec,'s')-strpos(dec,'m')-1))
    ra0=ten(h,m,s)*15d
    dec0=ten(dd,mm,ss)
endif 
if size(ra,/tname) eq 'DOUBLE' then begin
    ra0=ra
    dec0=dec
endif
    
rastr=sixty(ra0/15d) & decstr=sixty(dec0)
h=rastr[0] & m=rastr[1] & s=rastr[2]
dd=decstr[0] & mm=decstr[1] & ss=decstr[2]
print, 'Input RA: ', string(h,format='(I2)')+'h '+string(m,format='(I2)')+'m '+string(s,format='(F9.6)')+'s  '
print, 'Input DEC: ', string(dd,format='(I3)')+'d '+string(mm,format='(I2)')+"' "+string(ss,format='(F9.6)')+'"'
print, '-----convert to radian to be used in observing script-----'
print, 'RA is :', string(ra0/180d*!dpi, '(F20.10)')
print, 'DEC is :', string(dec0/180d*!dpi, '(F20.10)')
end
