pro sunpos_UT, yr, mn, day, hr, ra, dec, NINC=ninc
;print RA and DEC of the Sun at a given Gregorian Date and Time (UT)
jdcnv, yr, mn, day, hr, jd
if keyword_set(NINC) then begin
  sunpos, jd+dindgen(ninc), ra, dec
  for i=0, ninc-1 do print, string(yr,'(I4)'),'/', string(mn,'(I2)'),'/',string(day+i,'(I2)'), $
  adstring(ra[i], dec[i], 1)
endif else begin
  sunpos, jd, ra, dec
  print, adstring(ra,dec,1)
endelse
return
end