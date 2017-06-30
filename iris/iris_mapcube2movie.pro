pro iris_mapcube2movie,mapcube,timerange=timerange,figpath=figpath,$
        dt_frm=dt_frm,sclpar=sclpar,xysize=xysize,$
        mode=mode,rstep=rstep,_extra=ex
; INPUTS:
;   REQUIRED:
;        mapcube: a string value indicating the input mapcube (in an IDL sav file, output from aia_make_mapcube
;   KEYWORDS (OPTIONAL):
;        timerange: timerange of the movie, in anytim format. e.g., ['2014-11-01T19:00:00','2014-11-01T19:20:00']
;              default is the entire timerange of the mapcube
;        dt_frm: frame interval (cadence) of the movie in seconds, default is the median cadence of the mapcube
;        sclpar: scaling parameters. e.g., sclpar={min:10.,max:10000.}
;        xysize: size of the drawing window, in pixels; default is xysize=[800,800]
;        mode: 'inten' or 'rratio'; intensity (default) or running difference
; HISTORY:
;   2015-06-18 - Bin Chen (bin.chen@njit.edu)
;   2016-05-02 - Bin Chen. Added "mode" parameter with running difference option
if n_params() lt 1 then begin
    print,'Need to have an input mapcube IDL sav file!'
    return
endif
if ~keyword_set(mode) then mode='inten'
if ~keyword_set(rstep) then rstep=5
if ~keyword_set(xysize) then xysize=[800,800]
if ~keyword_set(figpath) then figpath='./'
;determine timerange and cadence
restore,mapcube,/verb
nmap=n_elements(maps)
timsecs=anytim(maps.time)
dts=dblarr(nmap-1)
for i=0,nmap-2 do dts[i]=timsecs[i+1]-timsecs[i]
dt_data=median(dts)
timeran=dblarr(2)
if ~keyword_set(timerange) then begin 
    timeran=[timsecs[0],timsecs[nmap-1]]
    print,'timerange not set, using timerange of the input mapcube'
endif else begin 
    timeran[0]=anytim(timerange[0]) > timsecs[0]
    timeran[1]=anytim(timerange[1]) < timsecs[nmap-1]
    print, 'input timerange: ', anytim(timerange,/vms)
    print, 'mapcube timerange: ', anytim(timsecs[0],/vms), anytim(timsecs[nmap-1],/vms)
    print, 'output timerange: ', anytim(timeran,/vms)
endelse

if ~keyword_set(dt_frm) then dt_frm=dt_data 
if ~keyword_set(sclpar) then begin
    ;default scalings
    sclpar={min:min(maps[0].data,/nan),max:max(maps[0].data,/nan),log:1}
endif
;set up reference times
nplt=fix((timeran[1]-timeran[0])/dt_frm)
if nplt lt 1 then begin
    print,'No plot produced (too short timerange or too long cadence), check inputs...'
    return
endif else begin
    plttims=dindgen(nplt)*dt_frm+timeran[0]
endelse
if mode eq 'inten' then begin
    ;make intensity movie
    aia_lct,r,g,b,wave='304',/load
    print,strtrim(string(nplt),2)+' images to process...' 
    for i=0,nplt-1 do begin
        plttim=plttims[i]
        difft=min(timsecs-plttim,t_ind,/absolute)
        set_plot,'z'
        clearplot
        erase
        device,set_resolution=xysize,z_buffer=0
        if ~keyword_set(log) then log=sclpar.log
        if ~keyword_set(dmin) then dmin=sclpar.min
        if ~keyword_set(dmax) then dmax=sclpar.max
        plot_map,maps[t_ind],log=log,dmin=dmin,dmax=dmax,_extra=ex
        snapshot=tvrd()
        device,z_buffer=1
        set_plot,'x'
        image24=bytarr(3,xysize[0],xysize[1])
        image24[0,*,*]=r[snapshot]
        image24[1,*,*]=g[snapshot]
        image24[2,*,*]=b[snapshot]
        otimstr=repstr(strmid(anytim(maps[t_ind].time,/vms,/time_only),0,8),':','')
        write_jpeg,figpath+'iris_sji_'+otimstr+'.jpg',image24,true=1,quality=90
        if (i gt 0) and ((i mod 10) eq 0) then print,strtrim(string(i),2)+' images drawn'
    endfor
    ;make javascript movie
    cd,cur=cur
    cd,figpath
    images=file_search('*.jpg')
    tbg=repstr(strmid(anytim(timeran[0],/vms,/time_only),0,8),':','')
    tend=repstr(strmid(anytim(timeran[1],/vms,/time_only),0,8),':','')
    jsmovie_name='iris_sji_'+tbg+'-'+tend+'.html'
    jsmovie,jsmovie_name,images
    cd,cur
endif
if mode eq 'rratio' then begin
    loadct, 0
    print,strtrim(string(nplt/rstep),2)+' images to process...'
    for i=0,nplt-1-rstep,rstep do begin
        plttim0=plttims[i]
        difft0=min(timsecs-plttim0,t_ind0,/absolute)
        map0=maps[t_ind0]
        plttim=plttims[i+rstep]
        difft=min(timsecs-plttim,t_ind,/absolute)
        map=maps[t_ind]
        rmap=map
        rmap.data=smooth(map.data,[5,5])/smooth(map0.data,[5,5])
        set_plot,'z'
        clearplot
        erase
        device,set_resolution=xysize,z_buffer=0
        plot_map,rmap,dmin=0.5,dmax=2.,_extra=ex
        snapshot=tvrd()
        device,z_buffer=1
        set_plot,'x'
        image24=bytarr(3,xysize[0],xysize[1])
        image24[0,*,*]=r[snapshot]
        image24[1,*,*]=g[snapshot]
        image24[2,*,*]=b[snapshot]
        otimstr=repstr(strmid(anytim(rmap.time,/vms,/time_only),0,8),':','')
        write_jpeg,figpath+'iris_sji_'+otimstr+'.jpg',image24,true=1,quality=90
        if (i gt 0) and ((i mod 10) eq 0) then print,strtrim(string(i),2)+' images drawn'
    endfor
endif
end
