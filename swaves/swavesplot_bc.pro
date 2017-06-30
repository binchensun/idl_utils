; swavesplot
; Plot STEREO/WAVES data.
; The plot consists of dynamic spectra of one day of data.
; The plot show ahead and behind data.
; The behind data is plotted from low to high frequencies and
; the ahead data is plotted from high to low frequencies.
; 
; Keyword Arguments:
;    data_dir     (input) This is a string containing the directory where the
;                 swaves one minute averages are located.  If this keyword is
;                 not specified, the program looks for the environmental variable
;                 SWAVESPLOT_DATA_DIR and uses that if available.
;                 If data_dir contains the string "%Y", those two characters are
;                 replaced with the four character string representing the
;                 year to be plotted.
;    date         (input) anytim format
;    btime        (input) format: hh:mm:ss. Starting time for the spectra plot.
;    etime        (input) format: hh:mm:ss. Ending time for the spectra plot.
;    savfile      (input) name of the savfile containing the output spectra data.
;    verbose      (input) If verbose is greater than 0, the name of each data file
;                 is printed out when it is opened.  The default value is 0.
;
; When swavesplot is run, it prompts the user to enter the year, month, and day to
; be plotted.  These should be entered as three separate values, such as
; "2010 2 15".  After the data are plotted, the program loops back and asks for
; the next year, month, and day to be plotted.
; If the string "." is entered at the prompt, the program returns to IDL command line.  
; If no string is entered (just return is pressed), the next day is plotted.
; If the string "+" is entered at the prompt, the next day is plotted.
; If the string "-" is entered at the prompt, the previous day is plotted.
; If a string like "+n" is entered, where "n" is an integer, the day "n" days
; following the current day is plotted.  Similiarly for the string "-n".
;
; Notes
;    This program uses color table 0, which should be the black-white linear table.

pro swavesplot_bc, data_dir=data_dir, date=date, btime=btime, etime=etime, savfile=savfile, verbose=verbose
    compile_opt idl2
    if n_elements(verbose) eq 0 then verbose = 0

    if n_elements(data_dir) eq 0 then begin
        data_dir = getenv('SWAVESPLOT_DATA_DIR')
        if data_dir eq '' then begin
            print, 'Location of 1 minute averages not set.'
            print, 'Set with keyword "data_dir" or '
            print, 'set environmental variable "SWAVESPLOT_DATA_DIR".'
            return
        endif
    endif

    loadct, 0, /silent ; black-white linear color table
    cblack = 0     ; black color
    cwhite = 255   ; white color

    ;data_dir = '/users/hess/documents/swaves/dynspec/old/'
    n_lfr_freq = 48
    n_hfr_freq = 319
    n_tot_freq = n_lfr_freq + n_hfr_freq ; 367
    n_time = 1440  ; Number of time points = minutes in a day

    ; spectra holds spectrum information.  
    ; Last dimension specifies spacecraft: 0 => ahead, 1 => behind.
    spectra = fltarr(n_time, n_tot_freq, 2)

    ; freqv holds frequency values.
    freqv = fltarr(n_tot_freq)

    julian_day = julday(10, 27, 2006) ; First day of data.

    ; Indefinite loop over day to be plotted.
    ;while 1 do begin
    ;
    ;    uin = '' ; User input string. Break on entering a ".".
    ;    read, uin, prompt = 'Enter year, month, and day to plot: '
    ;    c = strmid(uin, 0, 1)
    ;    if c eq '.' then break
    ;    if c eq '' || c eq '+' || c eq '-' then begin
    ;        delta = 0
    ;        if strlen(uin) le 1 then delta = 1 else reads, strmid(uin, 1), delta
    ;        if c eq '' || c eq '+' then julian_day = julian_day + delta
    ;        if c eq '-' then julian_day = julian_day - delta
    ;        caldat, julian_day, m, d, y
    ;        ymd = [y, m, d]
    ;    endif else begin
    ;        ymd = lonarr(3)
    ;        reads, uin, ymd
    ;    endelse

    if ~ keyword_set(date) then print, "Please enter the date to plot in anytim format."
        date0=anytim(date,/ccsds,/date_only)
        ymd=lonarr(3)
        ymd[0]=long(strmid(date0,0,4)) ;year
        ymd[1]=long(strmid(date0,5,2)) ;month
        ymd[2]=long(strmid(date0,8,2)) ;day
        julian_day = julday(ymd[1], ymd[2], ymd[0])
        symd = string(f='(i4,i2.2,i2.2)', ymd)
        sdymd = string(f='(i4,"-",i2.2,"-",i2.2)', ymd)

        ; If data_dir contains the string "%Y", substitue it with the year.
    ;    x_data_dir = data_dir
    ;    k = strpos(x_data_dir, '%Y')
    ;    if k ge 0 then begin
    ;        x_data_dir = strmid(x_data_dir, 0, k) + $
    ;            strmid(symd, 0, 4) + strmid(x_data_dir, k + 2)
    ;    endif

        ; Iterate over 'a' and 'b' spacecraft.
        for isc = 0, 1 do begin
            sc = (['a','b'])[isc]
            ; Iterate over 'lfr' and 'hfr' parts of the instrument.
            for ipart = 0,1 do begin
                part = (['lfr','hfr'])[ipart]
                ; Spectra data will be frequency range nb:nb+nf-1
                nf = ([n_lfr_freq, n_hfr_freq])[ipart]
                nb = ([0, n_lfr_freq])[ipart]
                filename = data_dir + path_sep() + $
                    'swaves_average_' + symd + '_' + sc + '_' + part + '.dat'
                if verbose ge 1 then print, 'Open file: ', filename
                openr, du, filename, /get_lun, error=err
                if err ne 0 then begin
                    printf, -2, !error_state.msg
                    ;goto, next_day
                endif
                f = fltarr(nf)  ; frequencies
                readf, du, f
                freqv[nb:nb + nf - 1] = f
                bg = fltarr(nf) ; Background values
                readf, du, f
                v = fltarr(nf)  ; Intensity values
                j = 0           ; Time point number
                for i_time = 0, n_time - 1 do begin
                    readf, du, j, v
                    spectra[i_time, nb:nb + nf - 1, isc] = v
                endfor
                free_lun, du
            endfor
        endfor
        ;print, freqv
        ;print, sort(freqv)
        ;print, freqv[sort(freqv)]
        isf = sort(freqv)
        freqv = freqv[isf]
        for isc = 0, 1 do spectra[*, *, isc] = spectra[*, isf, isc]

        n_yv = 301 ; Number of y values for each spectrum.
        ; Compute frequencies for logarithmic display.
        freq_min = 2.5
        freq_max = 16025.0
        ipl_freqv = freq_min * (freq_max/freq_min) ^ (findgen(n_yv)/(n_yv - 1))
        ipl_spectra = fltarr(n_time, n_yv, 2)
        for isc = 0, 1 do begin
            for itp = 0, n_time - 1 do begin
                u = interpol(spectra[itp, *, isc], freqv, ipl_freqv)
                ;help, u, ipl_spectra
                ipl_spectra[itp, *, isc] = u
            endfor
        endfor

        n_xv = 700 ; Number of x values for each spectrum.
        ipl2_spectra = fltarr(n_xv, n_yv, 2)
        ;select time range for plotting
        if strlen(btime) or strlen(etime) lt 8 then begin
            print, 'unrecognized time format. Should be hh:mm:ss, return...'
            return
        endif    
        btimemin=round(anytim(btime,/time_only)/60)
        etimemin=round(anytim(etime,/time_only)/60)
        n_timeplt=etimemin-btimemin
    	times=anytim((anytim(date+' 00:00:00')+findgen(n_timeplt)*60.+btimemin*60.),/vms)
        ipl_spectra=ipl_spectra[btimemin:etimemin-1,*,*]
	    spectra=spectra[btimemin:etimemin-1,*,*]
        print,'n_timeplt is: ', n_timeplt
        help,ipl_spectra
	    help,spectra
        time = findgen(n_timeplt) / (n_timeplt - 1)
        ipl_time = findgen(n_xv) / (n_xv - 1)
        for isc = 0, 1 do begin
            for ifreq = 0, n_yv - 1 do begin
                ; Reverse ahead in frequency so goes from high to low.
                if isc eq 0 then jfreq = n_yv - 1 - ifreq else jfreq = ifreq
                ipl2_spectra[*, jfreq, isc] = $
                    interpol(ipl_spectra[*, ifreq, isc], time, ipl_time)
            endfor
        endfor

	    if keyword_set(savfile) then save,file=savfile,spectra,freqv,ipl_spectra,ipl_freqv,times

        ; Minimum and maximum values when scaling data values.
        mini = 1
        maxi = 14

        ; Scale data values and subtract from 255.  This causes high 
        ; intensity to be black and low values to be white.
        ipl3_spectra = 255 - bytscl(ipl2_spectra, min=mini, max=maxi)

        ; compute locations of frequency tick marks
        ftm = [2.5, 1.e1, 1.e2, 1.e3, 1.e4]
        ;print, 'freq_index=', freq_index
        x0 = 80 ; X position of the plot.
        y0 = 50 ; Y position of the bottom plot.
        xextent = x0 + n_xv + 50      ; Space needed in x direction for plot.
        yextent = y0 + 2 * n_yv + 50  ; Space needed in y direction for plot.
        ; If no window, open one.  Otherwise, increase size if necessary.
        if !d.window eq -1 then begin
            window, xsize = xextent, ysize = yextent
        endif else if (!d.x_size lt xextent) || (!d.y_size lt yextent) then begin
            window, xsize = (!d.x_size > xextent), ysize = (!d.y_size > yextent)
        endif

        erase, cwhite

        for isc = 0, 1 do begin  ; Loop for ahead and behind.
            yp = ([y0 + n_yv, y0])[isc] ; Y position of this plot.
            tvscl, ipl3_spectra[*, *, isc], x0 + 1, yp
            freq_index = alog(ftm / freq_min) * (n_yv - 1.0) / (alog(freq_max / freq_min))
            ; Reverse freq_index for 'ahead' s/c, since its y scale goes from high to low.
            if isc eq 0 then freq_index = n_yv -1 - freq_index
            plot, [0,1], [0, n_yv - 1], /nodata, /noerase, color = cblack, $
                position = [x0, yp, x0 + n_xv, yp + n_yv], /device, $
                xtitle = 'time (hours)', $
                xticks = 1, xminor = 1, $  ; Suppress ticks along X axis.
                xtickname = [' ', ' '], $  ; Suppress labels at start and end of plot.
                ytitle = (['Ahead', 'Behind'])[isc], $
                yticks = 4, $
                ytickv = freq_index, $
                ytickname = ['2.5k', '10k', '100k', '1M', '10M'], $
                yticklen = -0.01
            ; Plot X axis. At top for ahead s/c, at bottom for behind s/c.
            ; decide tick interval of the time axis based the selected time range
            if n_timeplt gt 600 then xtickint = 120 ;two-hour interval
            if n_timeplt gt 180 and n_timeplt lt 600 then xtickint = 60 ;one-hour interval
            if n_timeplt gt 60 and n_timeplt lt 180 then xtickint = 30 ;half-hour interval
            if n_timeplt lt 60 then xtickint = 10 ;10 min interval
            if btimemin mod xtickint gt 0 then btick=fix(btimemin/xtickint)+1 $
                else btick=fix(btimemin)/xtickint
            etick=fix(etimemin/xtickint)
            xticks=etick-btick
            if btimemin mod xtickint gt 0 then bxtickv=(fix(btimemin/xtickint)+1)*xtickint $
                else bxtickv=btimemin
            xtickv=(findgen(xticks+1)*xtickint+bxtickv-btimemin)/n_timeplt
            xtickname=strmid(anytim(xtickv*n_timeplt*60+btimemin*60,/vms,/time_only,/truncate),0,5)
            axis, xaxis = ([1,0])[isc], color = cblack, $
                xticks = xticks, $
                xtickv = xtickv, $
                xtickname = xtickname, $
                xticklen = -0.02, $
                xminor = 3
        endfor
        ; Write Y axis units.
        xyouts, x0 - 50, y0 + n_yv, 'frequency (Hz)', color = cblack, /device, $
            alignment = 0.5, orientation = 90
        ; Plot black line between two plots.
        plots, [x0, x0 + n_xv], [0,0] + y0 + n_yv, color = cblack, /device
        ; Write title.
        xyouts, x0 + (n_xv / 2), y0 + 2 * n_yv + 30, /device, color = cblack, $
            'STEREO/WAVES ' + sdymd, align = 0.5, charsize = 1.5

     ;   next_day:
;    endwhile

end
