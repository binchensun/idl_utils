;+
;Name: goes_rhessi_plot
;
;PURPOSE : Creates a plot of the two GOES X-Ray channels for the selected time range, and  
;  optionally displays RHESSI observing times with indications of night,SAA, offpointing, or
;  annealing for non-observing times. If GOES data aren't available, use EVE GOES proxy data.
;  This is called by do_goes_rhessi_plots on hesperia.gsfc.nasa.gov to create the 24-hr, 
;  12-hr stacked, and rhessi orbit time plots used in the RHESSI browser at
;  http://sprg.ssl.berkeley.edu/~tohban/browser/.
;  
; Input Keywords:
;  time - Either a time range or a single time. For single time, will assume that day.
;  stacked - if set, divide time into 2 and plot two stacked plots (D=0)
;  orbit - if set, we're doing an orbit plot (Note that the times for the orbit are 
;    in time keyword (for batch runs called by do_goes_rhessi_plots) we just need to know here 
;    that it's an orbit plot, since plot params and browser output dir are different)
;  black_bk - if set, plot background will be black (D=white)
;  rhessi - if set, overlay RHESSI Sun observing times, and show anneal or offpoint times
;  png - if set, write plot in png file
;  outfile - png file name.  (D='goes_rhessi_yyyymmdd.png')
;  filename_time - if set, when constructing file name, use date and time
;  filename_append - add this string to end of outfile
;  browser - if set, then plot is for browser. Can only be used on hesperia.
;    png is set to 1, 
;    directory for output file is set to '/data/goes/xxx/ where 
;       xxx is the directory 24hr_plots, 12hr_plots, or rhessi_orbit_plots and
;    filename is set to goes_rhessi_yyyymmdd.png or goes_rhessi_yyyymmdd_hhmm.png for orbit plots
;    
;
; Output:
;   Either a single plot for the full duration requested, or 2 stacked plots each with half the duration, 
;   plotted on screen or in a .png file.
;   
; Examples:
;   goes_rhessi_plot, time='23-jul-2002', /rhessi, /stacked, /png
;   goes_rhessi_plot, time=['22-jul-2002', '24-jul-2002']
;   
;Written, Kim Tolbert 8-May-2012. Based on Steven Christe's my_goes_plot
;Modifications:
; 14-May-2012, Kim.  Moved call to hsi_linecolors to after set_plot,'z' because in batch mode can't
;  call it for X
;
;-

PRO hsi_good_times_plot, time=time, $
  black_bk=black_bk, $
  PNG=png, $
  OUTFILE=outfile, $
  filename_time=filename_time, $
  filename_append = filename_append,$
  accum=accum, $
  utc_ran=utc_ran

browser = keyword_set(browser)
png = keyword_set(png) or keyword_set(outfile)

default, tr, keyword_set(time) ? time : ['22-jul-2002', '22-jul-2002 23:59']
tr = anytim(tr)
if n_elements(tr) eq 1 then tr = anytim(tr,/date) + [0.,86400.]

default, utc_ran, keyword_set(utc_ran) ? utc_ran: ['15:00:00','23:00:00'] ;default is the VLA sun obs time in UTC
utc_ran = anytim(utc_ran)

year = strmid(anytim(tr[0], /ccsds), 0, 4)
month = strmid(anytim(tr[0], /ccsds), 5, 2)

default, charsize, 1.

ytitle = 'Watts m!U-2!N'
legend_obs = 'No RHESSI Solar Data'
rhessi_sun_legend = 0

IF png THEN BEGIN
  dir = chklog('HOME')
  if dir eq '' then dir = curdir()
  file = 'goes_rhessi_' + (keyword_set(filename_time) ? time2file(tr[0]) : time2file(tr[0],/date))		
  default, outfile, file

  if keyword_set(filename_append) THEN  outfile = outfile + filename_append
  outfile = outfile + '.png'

  save_dev = !d.name
  set_plot, 'z'
  ysize = 480
  res = [640,ysize]
  device, set_resolution = res
endif

hsi_linecolors, /pastel
green = 3
red = 6
blue = 7
purple = 10
gray = 19
ltblue = 15
ltpurple = 17
ltgreen = 14
ltgray = 18
bw = 0
bkcolor = 255
if keyword_set(black_bk) then begin
	  bw = 255
	  bkcolor = 0
endif
if NOT keyword_set(accum) then begin
	!p.multi=0
	leg_position = [.1,.92]
	leg_horiz = 1
	leg_right = 0
	leg_charsize = charsize
	tr_arr = tr
	yrange = [1.d-9, 1.d-2]
	autoscale = 0
	ystyle = 1

	tplot = tr

	; just plot axes and labels first
	title = 'RHESSI nights+SAA starting from ' + anytim(tplot[0],/vms) + ' UTC'
	utplot, anytim(/ext,tplot), yrange, /nodata, yrange=yrange, timerange=tplot, title=title, $
	ytitle=ytitle, /ylog, ystyle=ystyle, yminor=10, ymargin=[3.5,2.], $
	/xstyle, xmargin=[8,3], xtitle='', $
	position=position, $
	background=bkcolor, charsize = charsize, color=bw, /sav
	; for all good time intevals (not eclipse or SAA), set rhessi_sun flag to 1
	good_times = hsi_get_good_time(tplot, night_times=night_t, saa_times=saa_t)
	; Shade in the no good times, night, and saa intervals with pastel gray, purple, and green
	if good_times[0] ne -1 then for j=0,n_elements(good_times[0,*])-2 do color_box,x=[good_times[0,j+1],good_times[1,j]]-tplot[0],color=ltgray
	if night_t[0] ne -1 then for j=0,n_elements(night_t[0,*])-1 do color_box, x=night_t[*,j]-tplot[0], color=ltpurple
	if saa_t[0] ne -1 then for j=0,n_elements(saa_t[0,*])-1 do color_box, x=saa_t[*,j]-tplot[0], color=ltgreen
	; redraw axes in case we used polyfill above
	axis,yaxis=0,color=bw, yminor=10, ytickname=strarr(9)+' ', /ystyle
	axis,yaxis=1,color=bw, yminor=10, ytickname=strarr(9)+' ', /ystyle
	axis, xaxis=0, color=bw, xrange=!x.crange 
	axis, xaxis=1, color=bw, xtickname=strarr(20)+' ',xrange=!x.crange 
	ylims = crange('y')
	ytickv = 10.^[-13+indgen(12)]
	ytickname = [' ',' ',' ',' ',' ','A','B','C','M','X',' ',' ']
	ymm = ylims + ylims*[-1.e-7, 1.e-7]
	q = where(( ytickv ge ymm(0)) and ( ytickv le ymm(1)), kq)
	if kq gt 0 then begin
	axis, yaxis=1, ytickv = ytickv[q],/ylog, ytickname=ytickname[q], yrange=ylims, yticks=kq, color=bw, charsize=charsize*1.2
	for k=0,kq-1 do outplot, !x.crange, ytickv[q[k]]+[0.,0.], color=gray
	endif
	clear_utplot

	legend_chan=['1.0 to 8.0 A', '0.5 to 4.0 A']
	legend_obs='No RHESSI Solar Data'
	text = legend_chan
	color = [red,blue]
	thick = [3,3]
	if rhessi_sun_legend then begin
	  text = [text, legend_obs]
	  color = [color, gray]
	  thick = [thick, 1]
	endif

	; if stacked, legend will be written in between two plots
	;ssw_legend, text, color=color, lines=0, position=leg_position,/normal, right=leg_right, textcolor=bw, thick=thick, $
	;  horizontal=leg_horiz, linsize=.7, charsize=leg_charsize, box=0

	usersym,[-1.,13,13,-1],[-2.5,-2.5,2.5,2.5],/fill
	ssw_legend, ['Night','SAA','Nogood'],textcolor=0, position=[.1, .035], /normal,/horiz,box=0, $
	  /fill,psym=[8,8,8],colors=[ltpurple,ltgreen,ltgray],charsize=leg_charsize
	timestamp, charsize=charsize*.7, /bottom, color=bw
endif else begin
	;truncate the time range to be dates only (starting from 00:00 UT)
	tplot = anytim(tr,/date)
	if (tplot[1]-tplot[0]) lt 86400. then tplot[1] = tplot[0] + 2.*86400.
	ndates=fix((tplot[1]-tplot[0])/86400.)	
	gtimes = hsi_get_good_time(tplot)
        gtimes_date = anytim(gtimes,/date)
	gtimes_time = anytim(gtimes,/time)
        gtimes_dim = n_elements(gtimes[0,*])
	yrange = [0, utc_ran[1]-utc_ran[0]]/3600
	dates = findgen(ndates)*86400.+tplot[0]
	hours = fltarr(ndates)
	print,'ndates is: ',ndates
        for i=0,ndates-1 do begin
		date = dates[i]
		ind = where(abs(gtimes_date[0,*] - date) lt 86400,nind)
		print,'nind is: ',nind
		if nind ne -1 then begin
			if ind[0] ge 1 then begin
			;add one extra time slot at the beginning of the date slots if it does not start from the first one
				ind=[ind[0]-1, ind]
				nind=nind+1
			endif
		        if ind[nind-1] lt gtimes_dim-1 then begin
			;add one extra time slot at the end of the date slots if it does not end at the last one
				ind=[ind,ind[nind-1]+1]
				nind=nind+1
			endif	
			for j=0,nind-1 do begin
				if gtimes[0,ind[j]] ge date+utc_ran[0] and gtimes[1,ind[j]] le date+utc_ran[1] then $ 
				   hours[i]=hours[i]+(gtimes[1,ind[j]]-gtimes[0,ind[j]])/3600.
			endfor
		endif	
	endfor
	print,'hours is: ',hours
	utplot, anytim(dates,/ext), hours, yrange=yrange, timerange=dates,$
		psym=10,background=bkcolor, charsize = 1.3, color=bw,/ysty,$
		ytit='Available RHESSI Hours',tit='From '+anytim(utc_ran[0],/time,/vms)+' to '+anytim(utc_ran[1],/time,/vms)+' UT'
endelse

IF keyword_set(OUTFILE) THEN BEGIN
	tvlct, r, g, b, /get
	filename = concat_dir(dir, outfile)
	write_png, filename, tvrd(), r,g,b
	message, /info, 'Wrote plot file ' + filename
	set_plot, save_dev
ENDIF

!P.multi = 0

END
