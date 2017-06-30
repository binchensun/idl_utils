pro eq2hor_vla, yr, mn, day, hr, ra0, dec0, lst, az, alt, $
	SHOW=show,OUTFILE=outfile
;providing the Gregorian time (UT), and ra dec of the object, to LST and altitude and azimuth 
;hr can be a vector
;ra, dec are in DEGREES.
rastr=sixty(ra0/15.) & decstr=sixty(dec0)
h=rastr[0] & m=rastr[1] & s=rastr[2]
dd=decstr[0] & mm=decstr[1] & ss=decstr[2]
if keyword_set(show) then begin
	print, 'For the day of '+string(mn,format='(I2)')+'/'+string(day,format='(I2)')+'/'+string(yr,format='(I4)')
	print, 'For RA and DEC: '+string(h,format='(I2)')+'h '+string(m,format='(I2)')+'m '+string(s,format='(F5.2)')+'s  '+$
		string(dd,format='(I3)')+'d '+string(mm,format='(I2)')+'" '+string(ss,format='(F5.2)')+"'"
	print, 'UTC hour  '+ '     LST    '+'     Azimuth (deg)  '+'  Altitude (deg)'
endif
if keyword_set(outfile) then begin
	openw, 1, outfile
	printf, 1, 'For the day of '+string(mn,format='(I2)')+'/'+string(day,format='(I2)')+'/'+string(yr,format='(I4)')
	printf, 1, 'For RA and DEC: '+string(h,format='(I2)')+'h '+string(m,format='(I2)')+'m '+string(s,format='(F5.2)')+'s  '+$
		string(dd,format='(I3)')+'d '+string(mm,format='(I2)')+'" '+string(ss,format='(F5.2)')+"'"
	printf, 1, 'UTC hour  '+ '     LST    '+'     Azimuth (deg)  '+'  Altitude (deg)'
	close,1
endif
hr_dim=n_elements(hr)
if hr_dim gt 1 then begin
	for i=0,hr_dim-1 do begin
		h=reform(hr[i])	
		jdcnv, yr, mn, day, h, jd
		;vla coordinates
		lat=ten(34,04,43.497)
		lon=-ten(107,37,03.819)
		elv=2124.
		ct2lst,lst,lon,0,jd
		eq2hor, ra0, dec0, jd, alt, az, lat=lat, lon=lon, altitude=elv
		if keyword_set(show) then print, strtrim(hr)+strtrim(lst)+strtrim(az)+strtrim(alt)
		if keyword_set(outfile) then begin 
			openw, 1, outfile,/append
			printf, 1, strtrim(hr)+strtrim(lst)+strtrim(az)+strtrim(alt)
			close,1
		endif
	endfor
endif else begin
	jdcnv, yr, mn, day, hr, jd
	;vla coordinates
	lat=ten(34,04,43.497)
	lon=-ten(107,37,03.819)
	elv=2124.
	ct2lst,lst,lon,0,jd
	eq2hor, ra0, dec0, jd, alt, az, lat=lat, lon=lon, altitude=elv
	if keyword_set(show) then print, strtrim(hr)+strtrim(lst)+strtrim(az)+strtrim(alt)
	if keyword_set(outfile) then begin
	        openw, 1, outfile,/append	
		printf, 1, strtrim(hr)+strtrim(lst)+strtrim(az)+strtrim(alt)
		close,1
	endif
endelse
return
end
