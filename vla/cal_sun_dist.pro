pro cal_sun_dist, yr, mn, day, hr, ra, dec, $
NINC=ninc, SUNCOR=suncor, DIST=dist, LSTR=lstr
;yr, mn, day: year, month and date in Gregorian Date and Time (UT)
;ra, dec: RA and DEC of the calibrator
;NINC: increment number of dates
;SUNCOR: print the Sun's RA and DEC
;DIST: print the distance from the calibrator to the Sun
;LSTRG: print the LST range of the Sun with altitude > 20 deg
nlst=100
lsts=findgen(nlst) & az_sun=lsts & alt_sun=lsts & az_cal=lsts & alt_cal=lsts
hrs = findgen(nlst)/nlst*10.+14.
jdcnv, yr, mn, day, hr, jd ;Gregorian to Julian date format
if keyword_set(NINC) then begin
  sunpos, jd+dindgen(ninc), ra_sun, dec_sun ;Sun's ra and dec
  for i=0, ninc-1 do begin
    if keyword_set(suncor) then begin
      print, string(yr,'(I4)'),'/', string(mn,'(I2)'),'/',string(day+i,'(I2)'), $
      adstring(ra_sun[i], dec_sun[i], 1)
    endif
    if keyword_set(dist) then begin
      dist=sphdist(ra_sun[i], dec_sun[i], ra, dec,/degrees)
      ;print, string(yr,'(I4)'),'/', string(mn,'(I2)'),'/',string(day+i,'(I2)'), $
      ;        '   ', string(dist,'(F5.2)')
      print, string(dist,'(F5.2)')
    endif
    ;print, string(dist,'(F5.2)')
    if keyword_set(lstr) then begin
      ;print, string(yr,'(I4)'),'/', string(mn,'(I2)'),'/',string(day+i,'(I2)')
      for j=0,nlst-1 do begin
        eq2hor_vla, yr, mn, day+i, hrs[j], ra_sun[i], dec_sun[i], lst, az, alt
        lsts[j]=lst & az_sun[j]=az & alt_sun[j]=alt
        eq2hor_vla, yr, mn, day+i, hrs[j], ra, dec, lst, az, alt
        az_cal[j]=az & alt_cal[j]=alt
      endfor 
      ind_sun=where(alt_sun gt 15., n_sun)
      ind_cal=where(alt_cal gt 15., n_cal)
      if n_sun gt 1 then begin
        tmin_sun=sixty(min(lsts[ind_sun]))
        tmax_sun=sixty(max(lsts[ind_sun]))
        tmin_sun_s=string(tmin_sun[0],'(I2)')+':'+string(tmin_sun[1],'(I2)')+$
        ':'+string(tmin_sun[2],'(I2)')
        tmax_sun_s=string(tmax_sun[0],'(I2)')+':'+string(tmax_sun[1],'(I2)')+$
        ':'+string(tmax_sun[2],'(I2)')
        ;print,tmin_sun_s,' to ',tmax_sun_s
      endif else begin
        print, 'no LST range found for the Sun!'
      endelse 
      if n_cal gt 1 then begin
        tmin_cal=sixty(min(lsts[ind_cal]))
        tmax_cal=sixty(max(lsts[ind_cal]))
        tmin_cal_s=string(tmin_cal[0],'(I2)')+':'+string(tmin_cal[1],'(I2)')+$
          ':'+string(tmin_cal[2],'(I2)')
        tmax_cal_s=string(tmax_cal[0],'(I2)')+':'+string(tmax_cal[1],'(I2)')+$
           ':'+string(tmax_cal[2],'(I2)')
        print,tmin_cal_s,' to ',tmax_cal_s
      endif else begin
        print, 'no LST range found for the cal!'
      endelse
    endif
  endfor
endif else begin
   sunpos, jd, ra_sun, dec_sun
   if keyword_set(suncor) then begin
      print, string(yr,'(I4)'),'/', string(mn,'(I2)'),'/',string(day+i,'(I2)'), $
      adstring(ra_sun, dec_sun, 1)
   endif
    if keyword_set(dist) then begin
      dist=sphdist(ra_sun, dec_sun, ra, dec,/degrees)
      print, string(yr,'(I4)'),'/', string(mn,'(I2)'),'/',string(day+i,'(I2)'), $
      '   ', string(dist,'(F5.2)')
    endif
    ;print, string(dist,'(F5.2)')
    if keyword_set(lstr) then begin
      for j=0,nlst-1 do begin
        eq2hor_vla, yr, mn, day, hrs[j], ra_sun, dec_sun, lst, az, alt
        lsts[j]=lst & az_sun[j]=az & alt_sun[j]=alt
        ;print,j,lst,az,alt
        eq2hor_vla, yr, mn, day, hrs[j], ra, dec, lst, az, alt
        az_cal[j]=az & alt_cal[j]=alt
      endfor 
      ind_sun=where(alt_sun gt 20., n_sun)
      ind_cal=where(alt_cal gt 20., n_cal)
      print, string(yr,'(I4)'),'/', string(mn,'(I2)'),'/',string(day+i,'(I2)')
      if n_sun gt 1 then print, min(lsts[ind_sun]), max(lsts[ind_sun])
      if n_cal gt 1 then print, min(lsts[ind_cal]), max(lsts[ind_cal])
    endif
endelse
return
end