pro aia_mapcube2movie,mapcube,moviedir=moviedir,timerange=timerange,$
        dt_frm=dt_frm,sclpar=sclpar,xysize=xysize,$
        mode=mode,rstep=rstep,fe12=fe12,fe16=fe16,fe18=fe18,savecube=savecube,_extra=ex
; INPUTS:
;   REQUIRED:
;        mapcube: a string value indicating the input mapcube (in an IDL sav file, output from aia_make_mapcube
;   KEYWORDS (OPTIONAL):
;        wave: aia passband name: '171', '211', '94', '335', '193', '304', '131', '1600', '1700'; 
;              default='171'
;        timerange: timerange of the movie, in anytim format. e.g., ['2014-11-01T19:00:00','2014-11-01T19:20:00']
;              default is the entire timerange of the mapcube
;        dt_frm: frame interval (cadence) of the movie in seconds, default is the median cadence of the mapcube
;        sclpar: scaling parameters. e.g., sclpar={min:10.,max:10000.}
;        xysize: size of the drawing window, in pixels; default is xysize=[800,800]
;        mode: 'inten', 'rratio', or 'rdiff'; intensity (default), running ratio, or running difference
;        fe12: Fe XII, 1.0 MK. Linear combination of 193 and 171 (Reference: Del Zanna 2013, A&A, 558, A73)
;        fe16: Fe XVI, 2.5 MK. Linear combination of 335 and 171 (Reference: Del Zanna 2013, A&A, 558, A73)
;        fe18: Fe XVIII, 6 MK. Linear combination of 94, 211, and 171
; HISTORY:
;   2015-06-18 - Bin Chen (bin.chen@njit.edu)
;   2016-05-02 - Bin Chen. Added "mode" parameter with running difference option
;   2017-10-21 - Bin Chen. Added option to do Fe XVIII movie
if n_params() lt 1 then begin
    print,'Need to have an input mapcube IDL sav file!'
    return
endif
if ~keyword_set(mode) then mode='inten'
if ~keyword_set(rstep) then rstep=1
if ~keyword_set(wave) then wave='171'
if ~keyword_set(xysize) then xysize=[800,800]
if ~keyword_set(moviedir) then moviedir='./'
;determine timerange and cadence
if isa(mapcube,/array) then begin
    ncube=n_elements(mapcube)
    if keyword_set(fe18) then begin
        if ncube ne 3 then begin
            print,'Fe XVIII requires 94, 211, and 171 cube. '+string(ncube)+' provided. Abort...'
            return
        endif
        ;94 is required to be the first mapcube
        waves=strarr(3)
        restore,mapcube[0]
        id0=maps[0].id
        wave0=strmid(id0,strpos(id0,'AIA_')+6)
        if wave0 ne '94' then begin
            print, 'The first mapcube must be AIA 94. This is '+wave0+'. Abort...'
            return
        endif else begin
            maps94=temporary(maps)
            nmap=n_elements(maps94)
            timsecs=anytim(maps94.time)
            for n=0,1 do begin
                restore,mapcube[n+1]
                id=maps[0].id
                wave=strmid(id,strpos(id,'AIA_')+6)
                if wave eq '211' then maps211=temporary(maps)
                if wave eq '171' then maps171=temporary(maps)
            endfor
            maps=maps94
            for i=0,nmap-1 do begin
                dt1=min(anytim(maps211.time)-timsecs[i],ti_211,/absolute)
                dt2=min(anytim(maps171.time)-timsecs[i],ti_171,/absolute)
                if dt1 lt 12. and dt2 lt 12 then $
                    maps[i].data=maps94[i].data-maps211[ti_211].data/120.-maps171[ti_171].data/450. $
                    else maps[i].data=0. 
            endfor
            maps.id='AIA_7 Fe XVIII'
        endelse
        if keyword_set(savecube) then $
            save,file=repstr(mapcube[0],'94','fe18'),maps,reftim,durs,dur_flgs    
    endif
    if keyword_set(fe16) then begin
        if ncube ne 2 then begin
            print,'Fe XVI requires 335 and 171 cube. '+string(ncube)+' provided. Abort...'
            return
        endif
        ;335 is required to be the first mapcube
        waves=strarr(2)
        restore,mapcube[0]
        id0=maps[0].id
        wave0=strmid(id0,strpos(id0,'AIA_')+6)
        if wave0 ne '335' then begin
            print, 'The first mapcube must be AIA 335. This is '+wave0+'. Abort...'
            return
        endif else begin
            maps335=temporary(maps)
            nmap=n_elements(maps335)
            timsecs=anytim(maps335.time)
            restore,mapcube[1]
            id=maps[0].id
            wave=strmid(id,strpos(id,'AIA_')+6)
            if wave eq '171' then begin
                maps171=temporary(maps)
                maps=maps335
                for i=0,nmap-1 do begin
                    dt1=min(anytim(maps171.time)-timsecs[i],ti_171,/absolute)
                    if dt1 lt 12. then $
                        maps[i].data=maps335[i].data-maps171[ti_171].data/70.$
                        else maps[i].data=0. 
                endfor
                maps.id='AIA_8 Fe XVI'
            endif else begin 
                print, 'The second mapcube must be AIA 171. This is '+wave+'. Abort...'
                return
            endelse
        endelse
        if keyword_set(savecube) then $
            save,file=repstr(mapcube[0],'335','fe16'),maps,reftim,durs,dur_flgs    
    endif
    if keyword_set(fe12) then begin
        if ncube ne 2 then begin
            print,'Fe XII requires 193 and 171 cube. '+string(ncube)+' provided. Abort...'
            return
        endif
        ;335 is required to be the first mapcube
        waves=strarr(2)
        restore,mapcube[0]
        id0=maps[0].id
        wave0=strmid(id0,strpos(id0,'AIA_')+6)
        if wave0 ne '193' then begin
            print, 'The first mapcube must be AIA 193. This is '+wave0+'. Abort...'
            return
        endif else begin
            maps193=temporary(maps)
            nmap=n_elements(maps193)
            timsecs=anytim(maps193.time)
            restore,mapcube[1]
            id=maps[0].id
            wave=strmid(id,strpos(id,'AIA_')+6)
            if wave eq '171' then begin
                maps171=temporary(maps)
                maps=maps193
                for i=0,nmap-1 do begin
                    dt1=min(anytim(maps171.time)-timsecs[i],ti_171,/absolute)
                    if dt1 lt 12. then $
                        maps[i].data=0.7*(maps193[i].data-maps171[ti_171].data/2.) $
                        else maps[i].data=0. 
                endfor
                maps.id='AIA_9 Fe XII'
            endif else begin 
                print, 'The second mapcube must be AIA 171. This is '+wave+'. Abort...'
                return
            endelse
        endelse
        if keyword_set(savecube) then $
            save,file=repstr(mapcube[0],'193','fe12'),maps,reftim,durs,dur_flgs    
    endif
endif
if isa(mapcube,/string) eq 1 and n_elements(mapcube) eq 1 then restore,mapcube,/verb
id=maps[0].id
wave=strmid(id,strpos(id,'AIA_')+6)
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
    case wave of
        '171': sclpar={min:100.,max:10000.,log:1}
        '211': sclpar={min:0.,max:10000.,log:1}
        '94': sclpar={min:1.,max:1500.,log:1}
        '335': sclpar={min:1.,max:1500.,log:1}
        '193': sclpar={min:100.,max:15000.,log:1}
        '304': sclpar={min:0.25,max:2000.,log:1}
        '131': sclpar={min:1.,max:1000.,log:1}
        '1600': sclpar={min:0.25,max:10000.,log:1}
        '1700': sclpar={min:50.,max:30000.,log:1}
        'Fe XVIII': sclpar={min:0.1,max:500.,log:1}
        'Fe XVI': sclpar={min:0.1,max:500.,log:1}
        'Fe XII': sclpar={min:10,max:5000.,log:1}
        else: begin
            box_message,['Invalid wavelength. Check input.']
        end
    endcase
endif
;set up reference times
nplt=fix((timeran[1]-timeran[0])/dt_frm)
if nplt lt 1 then begin
    print,'No plot produced (too short timerange or too long cadence), check inputs...'
    return
endif else begin
    plttims=dindgen(nplt)*dt_frm+timeran[0]
endelse
waveplt=wave
if wave eq 'Fe XVIII' then begin
    waveplt='94' 
    loadct,5
    tvlct,r,g,b,/get
endif
if wave eq 'Fe XVI' then waveplt='335'
if wave eq 'Fe XII' then waveplt='193'
if wave ne 'Fe XVIII' then aia_lct,r,g,b,wave=waveplt,/load
if mode eq 'inten' then begin
    ;make intensity movie
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
        map=maps[t_ind]
        plot_map,map,log=log,dmin=dmin,dmax=dmax,_extra=ex,title=map.id+' @ '+map.time
        snapshot=tvrd()
        device,z_buffer=1
        set_plot,'x'
        image24=bytarr(3,xysize[0],xysize[1])
        image24[0,*,*]=r[snapshot]
        image24[1,*,*]=g[snapshot]
        image24[2,*,*]=b[snapshot]
        otimstr=repstr(strmid(anytim(maps[t_ind].time,/vms,/time_only),0,8),':','')
        write_jpeg,moviedir+'/aia_'+wave+'_'+otimstr+'.jpg',image24,true=1,quality=90
        if (i gt 0) and ((i mod 10) eq 0) then print,strtrim(string(i),2)+' images drawn'
    endfor
endif
if mode eq 'rratio' then begin
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
        plot_map,rmap,dmin=sclpar.min,dmax=sclpar.max,_extra=ex,title=rmap.id+' @ '+rmap.time
        snapshot=tvrd()
        device,z_buffer=1
        set_plot,'x'
        image24=bytarr(3,xysize[0],xysize[1])
        image24[0,*,*]=r[snapshot]
        image24[1,*,*]=g[snapshot]
        image24[2,*,*]=b[snapshot]
        otimstr=repstr(strmid(anytim(rmap.time,/vms,/time_only),0,8),':','')
        write_jpeg,moviedir+'/aia_'+wave+'_'+otimstr+'.jpg',image24,true=1,quality=90
        if (i gt 0) and ((i mod 10) eq 0) then print,strtrim(string(i),2)+' images drawn'
    endfor
endif
if mode eq 'rdiff' then begin
    print,strtrim(string(nplt/rstep),2)+' images to process...'
    for i=0,nplt-1-rstep,rstep do begin
        plttim0=plttims[i]
        difft0=min(timsecs-plttim0,t_ind0,/absolute)
        map0=maps[t_ind0]
        plttim=plttims[i+rstep]
        difft=min(timsecs-plttim,t_ind,/absolute)
        map=maps[t_ind]
        rmap=map
        rmap.data=smooth(map.data,[5,5])-smooth(map0.data,[5,5])
        set_plot,'z'
        clearplot
        erase
        device,set_resolution=xysize,z_buffer=0
        plot_map,rmap,dmin=sclpar.min,dmax=sclpar.max,_extra=ex,title=rmap.id+' @ '+rmap.time
        snapshot=tvrd()
        device,z_buffer=1
        set_plot,'x'
        image24=bytarr(3,xysize[0],xysize[1])
        image24[0,*,*]=r[snapshot]
        image24[1,*,*]=g[snapshot]
        image24[2,*,*]=b[snapshot]
        otimstr=repstr(strmid(anytim(rmap.time,/vms,/time_only),0,8),':','')
        write_jpeg,moviedir+'/aia_'+wave+'_rdiff_'+otimstr+'.jpg',image24,true=1,quality=90
        if (i gt 0) and ((i mod 10) eq 0) then print,strtrim(string(i),2)+' images drawn'
    endfor
endif
cd,moviedir,cur=cur
;make javascript movie
images=file_search('*.jpg')
tbg=repstr(strmid(anytim(timeran[0],/vms,/time_only),0,8),':','')
tend=repstr(strmid(anytim(timeran[1],/vms,/time_only),0,8),':','')
jsmovie_name='aia_'+wave+'_'+tbg+'-'+tend+'.html'
jsmovie,jsmovie_name,images
cd,cur
end
