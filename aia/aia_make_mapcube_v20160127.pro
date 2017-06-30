pro aia_make_mapcube,date,timerange,wave=wave,data_area=data_area,dataset=dataset,$
    dt_frm=dt_frm, dt_data=dt_data, savfile=savfile,$
    average=average,normalize=normalize,drot=drot,$
    xran=xran,yran=yran,fov=fov,center=center,xshifts=xshift,yshifts=yshift,movie=movie
;+
; NAME
;   AIA_MAKE_MAPCUBE
; PURPOSE:
;   Perform image registration of AIA images, and make a map structure (array)
;   **************CAUSION!!!! DO NOT MAKE THE CUBE TOO LARGE!!!!! (LIMITED BY MEMORY SIZE)*********** 
; SAMPLE CALLS:
;   ; AIA 171 full-sun maps from 2014-11-01 19:00 to 19:20 UT, at a cadence of 120 sec. 
;   IDL> aia_make_mapcube,'2014-11-01', ['19:00:00','19:20:00'], wave='171', dt_frm=120.
;   ; AIA 171 partial-sun maps from 2014-11-01 19:00 to 19:20 UT, at a cadence of 120 sec. 
;     The field of view is from -300 to 0 in solar X, and from 0 to 300 in solar y. 
;     The "movie" keyword is set to make a javascript movie
;   IDL> aia_make_mapcube,'2014-11-01', ['19:00:00','19:20:00'], wave='171', dt_frm=120.,$
;            xran=[-300,0], yran=[0,300], /movie
; OUTPUTS:
;   an IDL save file containing the following:
;   maps: an array of map structure
;   durs: an array of image durations 
;   dur_flgs: an array containing the number of time sequence images; normal exposure, 1, short exposure, 0
; INPUTS:
;   REQUIRED:
;       date: date of the observation, e.g., '2012-03-03'
;       timerange: timerange of the cube, e.g., ['18:00:00','18:30:00']
;   KEYWORDS (OPTIONAL):
;       data_area: 'local': using data stored locally in $LOCAL_DATA_AREA
;                  'lan': using data stored on SSXG lan, defined in $AIA_DATA_AREA
;                       in ~/.cshrc.user add a line
;                       setenv AIA_DATA_AREA "/data/SDO/AIA/level1/"
;       dataset: ***only used for data stored locally. For 'lan', default to 'LEV1'***
;                  'LEV1': aia level 1
;                  'LEV15': aia level 1.5
;                  'CUT': aia cutout
;                  'DECON': deconvoluted AIA fits files
;       wave: aia passband names: '171', '211', '94', '335', '193', '304', '131', '1600', '1700'
;       dt_frm: frame interval in seconds, default, 60.
;       dt_data: data cadence in seconds, default, 12.
;       savfile: name of the save file. A default name is given based on timerange, wavelength,
;                and cadence, stored in the current directory
;       average: average over dt_frm?
;       normarlize: normalize exposure to 1 s?
;       drot: differentially rotate the map to compensate solar rotation?
;       xran: [x0, x1], xrange in solar X, arcsecs
;       yran: [y0, y1], yrange in solar Y, arcsecs
;       ### not implemented yet ###
;       fov: [fx, fy], field of view in arcsecs (similar to plot_map)
;       center: [xc, yc], center coordinates in arcsecs (similar to plot_map)
;       ###########################
;       xshift: shift of the solar X coordinate in arcsec, default = 0
;       yshift: shift of the solar Y coordinate in arcsec, default = 0
;       movie: want a javascript movie?
; HISTORY:
;   2015-06-12 - Bin Chen (bin.chen@cfa.harvard.edu) 
; 
if ~exist(data_area) then data_area='lan'
if ~exist(dataset) then dataset='LEV1'
if ~exist(wave) then wave='171'
if ~exist(date) then date='2012-03-03'
if ~exist(timerange) then timerange=['18:33:00','18:34:00']
if ~exist(dt_frm) then dt_frm=60.
if ~exist(dt_data) then dt_data=12.
if ~exist(xshift) then xshift=0.
if ~exist(yshift) then yshift=0.
case wave of
    '171': minexp=1.8 
    '211': minexp=2.7
    '94': minexp=2.7
    '335': minexp=2.7
    '193': minexp=1.8
    '304': minexp=2.7
    '131': minexp=2.7
    '1600': minexp=1.1
    '1700': minexp=0.4
    '4500': minexp=0.4
    else: begin
        box_message,['Invalid wavelength. Check input.']
        return
        end
endcase

timeran=[date+'T'+timerange[0],date+'T'+timerange[1]]
navg=fix(dt_frm/dt_data)
ntim=fix((anytim(timeran[1])-anytim(timeran[0]))/dt_frm)+1
reftim=strarr(ntim)
datedir=repstr(date,'-','/')+'/'
if data_area eq 'local' then begin
    if strlen(getenv('AIA_'+dataset)) eq 0 then $
        box_message,'local AIA data area not defined! Use setenv to define AIA_LEV1, AIA_LEV15 or AIA_CUT' 
    aiafits_path=getenv('AIA_'+dataset)+datedir
    ;get file list of AIA images
    if dataset eq 'LEV15' or dataset eq 'DECON' then begin 
        searchstring=aiafits_path+'*'+wave+'.fits'
        aiaflist=file_search(searchstring,count=cnt)
        aiaftim=dblarr(cnt)
        for m=0,cnt-1 do begin
            pos=strpos(aiaflist[m],repstr(date,'-',''))+9
            ftimstr=date+'T'+strmid(aiaflist[m],pos,2)+':'+$
                    strmid(aiaflist[m],pos+2,2)+':'+strmid(aiaflist[m],pos+4,2)
            aiaftim[m]=anytim(ftimstr)
        endfor
    endif
    if dataset eq 'LEV1' then begin
        searchstring=aiafits_path+'*'+wave+'.image_lev1.fits'
        aiaflist=file_search(searchstring,count=cnt)
        aiaftim=dblarr(cnt)
        for m=0,cnt-1 do begin
            pos=strpos(aiaflist[m],date)
            aiaftim[m]=anytim(strmid(aiaflist[m],pos,17))
        endfor
    endif
    if dataset eq 'CUT' then begin
        searchstring=aiafits_path+'*'+wave+'*.fts'
        aiaflist=file_search(searchstring,count=cnt)
        aiaftim=dblarr(cnt)
        for m=0,cnt-1 do begin
            pos=strpos(aiaflist[m],repstr(date,'-',''))+9
            ftimstr=date+'T'+strmid(aiaflist[m],pos,2)+':'+$
                    strmid(aiaflist[m],pos+2,2)+':'+strmid(aiaflist[m],pos+4,2)
            aiaftim[m]=anytim(ftimstr)
        endfor
    endif
endif

if data_area eq 'lan' then begin
    dataset='LEV1'
    if strlen(getenv('AIA_DATA_AREA')) eq 0 then $
        aiafits_path='/data/SDO/AIA/level1/'+datedir $
        else aiafits_path=getenv('AIA_DATA_AREA')+datedir
    ;find the number of hours
    spawn,'ls '+aiafits_path+' | grep H',hours
    nhours=n_elements(hours)
    aiaflist=strarr(10000L)
    cnt=0
    for h=0,nhours-1 do begin
        searchstring=aiafits_path+hours[h]+'/*'+wave+'.fits'
        aiaflist0=file_search(searchstring,count=c)
        aiaflist[cnt:cnt+c-1]=aiaflist0
        cnt=cnt+c
    endfor
    aiaflist=aiaflist[0:cnt-1]
    aiaftim=dblarr(cnt)
    for m=0,cnt-1 do begin
        pos=strpos(aiaflist[m],repstr(date,'-',''))+9
        ftimstr=date+'T'+strmid(aiaflist[m],pos,2)+':'+$
                strmid(aiaflist[m],pos+2,2)+':'+strmid(aiaflist[m],pos+4,2)
        aiaftim[m]=anytim(ftimstr)
    endfor
endif

dur_flgs=intarr(ntim)
durs=fltarr(ntim)

for i=0, ntim-1 do begin
    tim=anytim(timeran[0])+dt_frm*i
    timstr=anytim(tim,/vms)
    print,'Processing time: ',timstr
    if ~exist(average) then begin
        difft=min(aiaftim-anytim(tim),t_ind,/absolute)
        if difft gt 0.8*dt_data then flgs[i]=1
        aiafits=aiaflist[t_ind]
        if dataset eq 'CUT' or dataset eq 'LEV15' then read_sdo, aiafits, oindex, odata, /uncomp_delete
        if dataset eq 'LEV1' then aia_prep,aiafits,-1,oindex,odata
        durs[i]=oindex.exptime
        if (oindex.exptime lt minexp) then dur_flgs[i]=1
        if exist(normalize) then begin
            odata=odata/oindex.exptime
            oindex.exptime=1.0
        endif
        index2map,oindex,odata,map_in
        ;T_OBS gives the best estimate for AIA time
        ;DO NOT USE THE DEFAULT TIME FROM INDEX2MAP -- it uses DATE_OBS, which is the starting time of integration
        map_in.time=strtrim(anytim(oindex.t_obs,/vms),2)
        if exist(drot) then begin
            if i eq 0 then ref_dmap=map_in
            map_in=drot_map(map_in,ref_map=ref_dmap)
        endif
        if exist(xran) or keyword_set(yran) then begin
            if i eq 0 then begin
                sub_map,map_in,map,xran=xran,yran=yran 
                ref_smap=map ;use the first map as reference for submap 
            endif else begin
                sub_map,map_in,map,ref_map=ref_smap,/preserve ;make sure pixels are the same at the first map 
            endelse
        endif else begin
            map=temporary(map_in)
        endelse
        if (i eq 0) then maps=replicate(map,ntim) 
        maps[i]=map
        reftim[i]=timstr
    endif else begin
        flgsavg=intarr(navg)
        for j=0,navg-1 do begin
            timj=anytim(tim)+(j-fix((navg-1.)/2))*dt_data
            print,'Processing time for avg: ',anytim(timj,/vms)
            difft=min(aiaftim-timj,t_ind,/absolute)
            if difft gt 0.8*dt_data then flgsavg[j]=1
            aiafits=aiaflist[t_ind]
            if dataset eq 'CUT' or dataset eq 'LEV15' then read_sdo, aiafits, oindex, odata
            if dataset eq 'LEV1' then aia_prep,aiafits,-1,oindex,odata
            if (oindex.exptime lt minexp) then flgsavg[j]=1
            if exist(normalize) then begin
                odata=odata/oindex.exptime
                oindex.exptime=1.0
            endif
            index2map,oindex,odata,map_in
            ;T_OBS gives the best estimate for AIA time
            ;DO NOT USE THE DEFAULT TIME FROM INDEX2MAP -- it uses DATE_OBS, which is the starting time of integration
            map_in.time=strtrim(anytim(oindex.t_obs,/vms),2)
            if exist(drot) then begin
                if i eq 0 then ref_dmap=map_in 
                map_in=drot_map(map_in,ref_map=ref_dmap)
                map_in.time=anytim(oindex.t_obs,/vms)
            endif
            if exist(xran) and keyword_set(yran) then begin
                if i eq 0 then begin
                    sub_map,map_in,map,xran=xran,yran=yran 
                    ref_smap=map ;use the first map as reference for submap 
                endif else begin
                    sub_map,map_in,map,ref_map=ref_smap,/preserve ;make sure pixels are the same at the first map 
                endelse
            endif else begin
                map=temporary(map_in)
            endelse
            if j eq 0 then tmpmap=replicate(map,navg) 
            tmpmap[j]=map
        endfor
        avgmap=tmpmap[fix(navg-1.)/2]
        ind=where(flgsavg eq 0, nind)
        if nind gt 0 then avgmap.data=avg(tmpmap[ind].data,2) else dur_flgs[i]=1
        avgmap.time=anytim(avg(anytim(tmpmap.time),0),/vms)
        if (i eq 0) then maps=replicate(avgmap,ntim) 
        maps[i]=avgmap
        reftim[i]=timstr
    endelse
    if exist(movie) then begin
        case wave of
            '171': sclpar={min:10.,max:10000.}
            '211': sclpar={min:0.,max:10000.}
            '94': sclpar={min:0.2,max:1500.}
            '335': sclpar={min:0.2,max:1500.}
            '193': sclpar={min:30.,max:15000.}
            '304': sclpar={min:0.25,max:2000.}
            '131': sclpar={min:0.25,max:3000.}
            '1600': sclpar={min:0.25,max:10000.}
            '1700': sclpar={min:50.,max:30000.}
            else: begin
                box_message,['Invalid wavelength. Check input.']
            end
        endcase
        if i eq 0 then window,0,xs=800,ys=800,retain=2 else erase
        aia_lct,wave=wave,/load
        if ~exist(drot) then title='AIA '+wave+' '+anytim(maps[i].time,/vms) $
            else title='AIA '+wave+' '+anytim(maps[i].time,/vms)+ ' drot to '+$
                anytim(maps[i].rtime,/vms)
        pos=[0.12,0.12,0.95,0.95]
        plot_map,maps[i],/log,dmin=sclpar.min,dmax=sclpar.max,title=title,position=pos
        if dur_flgs[i] eq 1 then xyouts,pos[0]+0.015,pos[1]+0.02,'short exposure: '+string(durs[i]),color=cgcolor('white'),/norm
        write_png,'aia_'+wave+'_movie_'+repstr(strmid(anytim(maps[i].time,/vms,/time_only),0,8),':','')+'.png',tvrd(/true)
    endif
endfor

;shift the map centers according to xshift and yshift, if needed
for i=0,ntim-1 do begin
    maps[i]=shift_map(maps[i],xshift,yshift)
endfor
tbg=repstr(strmid(anytim(timeran[0],/vms,/time_only),0,8),':','')
tend=repstr(strmid(anytim(timeran[1],/vms,/time_only),0,8),':','')
if ~exist(savfile) then begin
    datestr=repstr(date,'-','')
    savfile='aiamaps_'+datestr+'_'+tbg+'-'+tend+'.'+strtrim(round(dt_frm),2)+'s.'+wave+'.sav'
endif
save,file=savfile, maps, reftim, durs, dur_flgs
print,'stored as: '+savfile
if exist(movie) then jsmovie,'aia_'+wave+'_movie.html',file_search('aia_'+wave+'_movie_*.png')
end
