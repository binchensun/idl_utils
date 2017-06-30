;          PURPOSE : Read an IRIS level2 SJI data file, generate movie frames
;
; CALLING SEQUENCE : iris_sji2map, sjifile
; 
;           INPUTS : sjifile - an IRIS level2 SJI data file
; 
;         KEYWORDS : xrange, yrange: spatial range of the maps in solar coordinates
;                    ### not implemented yet ###
;                    fov: [fx, fy], field of view in arcsecs (similar to plot_map)
;                    center: [xc, yc], center coordinates in arcsecs (similar to plot_map)
;                    timerange: timerange of the output map cube, e.g., 
;                               ['2016-02-18T18:00:00','2016-02-18T18:30:00']
;                    savfile: if set, save the output as an IDL sav file
;                    movie: if set, produce a movie sequence
;                    moviedir: if set, direct the out png sequence into the specified location
;                              default to be the current directory
;
;          OUTPUTS : sswIDL map cube -- array of IDL structure; if keyword "movie" set, 
;                    generate a bunch of images in "moviedir". image frames in png format
;
;          EXAMPLES:
;
;          HISTORY : Written by Bin Chen (bin.chen@njit.edu), May 8, 2016
;
pro iris_sji2map_bc, sjifile, map, timerange=timerange, xrange=xrange, yrange=yrange,$
                  fov=fov, center=center, savfile=savfile, movie=movie

; check if the input file is a '.gz' file (as downloaded directly from the IRIS website)
if strmid(sjifile,strlen(sjifile)-3,3) eq '.gz' then begin
  spawn, 'gunzip '+sjifile 
  sjifile=strmid(sjifile,0,strlen(sjifile)-3)
endif

; read in the iris SJI level 2 fits file using the native object properties
d=iris_sji()  
d->read,sjifile  &  id=d->getsji_id()  &  sub=where(strlen(id) gt 0)  &  iwin=sub[0]
dat=d->getvar(iwin)  &  texp=d->getexp(iwin=iwin)  &  date_obs=d->ti2utc()  
solar_x=d->xscale()  &  solar_y=d->yscale() & xcen=d->getxcen() & ycen=d->getycen()
naxis1=d->getinfo('NAXIS1',iwin) & naxis2=d->getinfo('NAXIS2',iwin)
dx=d->getinfo('CDELT1',iwin) & dy=d->getinfo('CDELT2',iwin)
tele=d->getinfo('TELESCOP',iwin) & obs_id=d->getinfo('TDESC1',iwin) & lvl=d->getinfo('DATA_LEV',iwin) 
obj_destroy,d 

; selecting time range
times=anytim(date_obs)
ntime=n_elements(times)
if ~keyword_set(timerange) then begin
    bt=0
    et=ntime-1
endif else begin
    case 1 of:
        anytim(timerange[0]) lt times[0]: begin
                    print 'Specified starting time earlier than the obs start time' 
                    print 'Use the first record as the beginning time...'
                    bt=0
                end
        anytim(timerange[1]) gt times[ntime-1]: begin
                    print 'Specified ending time later than the obs end time' 
                    print 'Use the last record as the ending time...'
                    et=ntime-1
                end
        else: begin
                    dt1 = min(times-anytim(timerange[0]),/abs,bt)
                    dt2 = min(times-anytim(timerange[1]),/abs,et)
              end
    endcase
endelse

; make maps
for i=0,ntime-1 do begin
    dummy = make_map( dat( *, *, i), $
           xc     = xcen, $
           yc     = ycen, $
           dx     = dx, $
           dy     = dy, $
           time   = anytim(time,/vms), $
           id     = tele + ' ' + obs_id + ' '+ strtrim(string(fix(lvl)),1), $
           dur    = texp[i], $
           xunits = 'arcsec', $
           yunits = 'arcsec')
    if i eq 0 then begin
        map = replicate(dummy, ntime)
    endif else begin
        map[i]=dummy
endfor



end
