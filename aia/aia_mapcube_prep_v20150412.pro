pro aia_mapcube_prep,dataset=dataset,waves=waves,date=date,timerange=timerange,$
    dt_avg=dt_avg, dt_data=dt_data, savfile=savfile,normalize=normalize,$
    xran=xran,yran=yran,xshifts=xshifts,yshifts=yshifts
;INPUTS:
; dataset: 'LEV1': aia level 1
;         'LEV15': aia level 1.5
;         'CUT': aia cutout
; waves: aia waves
; date: date of the observation, e.g., '2012-03-03'
; timerange: timerange of the cube, e.g., ['18:00:00','18:30:00']
; dt_avg: average time in seconds, default, 60.
; dt_data: data cadence in seconds, default, 12.
; savfile: name of the save file
; xran: xrange in solar X
; yran: yrange in solar Y
; xshifts: shift of the solar X coordinate
; yshifts: shift of the solar Y coordinate
if ~keyword_set(dataset) then dataset='LEV15'
if ~keyword_set(waves) then waves=['94','131','171','193','211','335']
nwave=n_elements(waves)
if ~keyword_set(date) then date='2012-03-03'
if ~keyword_set(timerange) then timerange=['18:33:00','18:34:00']
if ~keyword_set(dt_avg) then dt_avg=12.
if ~keyword_set(dt_data) then dt_data=12.
if ~keyword_set(xshifts) then xshifts=fltarr(nwave)
if ~keyword_set(yshifts) then yshifts=fltarr(nwave)

timeran=[date+'T'+timerange[0],date+'T'+timerange[1]]
datedir=repstr(date,'-','/')+'/'
aiafits_path=getenv('AIA_'+dataset)+datedir
;timran=['2012-03-03T17:10:00','2012-03-03T19:30:00']
navg=fix(dt_avg/dt_data)
ntim=fix((anytim(timeran[1])-anytim(timeran[0]))/dt_avg)
aiatim=strarr(ntim)
;xran=[-1070,-820] ;in solar coordinates
;yran=[240,440] ;in solar coordinates
;xran=[-1225,-800] ;in solar coordinates
;yran=[200,550] ;in solar coordinates
;xran=[-1300,1300] ;in solar coordinates
;yran=[-1300,1300] ;in solar coordinates
aiaflists=strarr(2000,6)
aiaftims=dblarr(2000,6)
for n=0, nwave-1 do begin
    wave=waves[n]
    ;get file list of AIA images
    if dataset eq 'LEV15' then begin 
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
    aiaflists[0:cnt-1,n]=aiaflist
    aiaftims[0:cnt-1,n]=aiaftim
endfor

for i=0, ntim-1 do begin
    tim=anytim(timeran[0])+dt_avg*i
    timstr=anytim(tim,/vms)
    print,'Processing time: ',timstr
    if navg lt 2 then begin
        aiareadlist=strarr(nwave)
        for n=0,nwave-1 do begin
            wave=waves[n]
            aiaflist=aiaflists[*,n]
            aiaftim=aiaftims[*,n]
            difft=min(aiaftim-anytim(tim),t_ind,/absolute)
            aiareadlist[n]=aiaflist[t_ind]
        endfor
        if dataset eq 'CUT' or dataset eq 'LEV15' then read_sdo, aiareadlist, oindex, odata
        if dataset eq 'LEV1' then aia_prep,aiareadlist,-1,oindex,odata
        if keyword_set(normalize) then begin
            for n=0,nwave-1 do odata[*,*,n]=odata[*,*,n]/oindex[n].exptime
            oindex.exptime=1.0
        endif
        reord=sort(oindex.wavelnth)
        index2map,oindex[reord],odata[*,*,reord],map_in
        ;T_OBS gives the best estimate for AIA time
        ;DO NOT USE THE DEFAULT TIME FROM INDEX2MAP, which uses DATE_OBS -- starting time of integration
        map_in.time=anytim(oindex[reord].t_obs,/vms)
        if keyword_set(xran) or keyword_set(yran) then $
            sub_map,map_in,map,xran=xran,yran=yran else $
            map=temporary(map_in)
        if (i eq 0) then aiamaps=replicate(map[0],nwave,ntim) 
        aiamaps[*,i]=map
        aiatim[i]=timstr
    endif else begin
        for j=0,navg-1 do begin
            timj=anytim(tim)+(j-fix((navg-1.)/2))*dt_data
            print,'Processing time for avg: ',anytim(timj,/vms)
            aiareadlist=strarr(nwave)
            for n=0,nwave-1 do begin
                wave=waves[n]
                aiaflist=aiaflists[*,n]
                aiaftim=aiaftims[*,n]
                difft=min(aiaftim-timj,t_ind,/absolute)
                aiareadlist[n]=aiaflist[t_ind]
            endfor
            if dataset eq 'CUT' or dataset eq 'LEV15' then read_sdo, aiareadlist, oindex, odata
            if dataset eq 'LEV1' then aia_prep,aiareadlist,-1,oindex,odata
            if keyword_set(normalize) then begin
                for n=0,nwave-1 do odata[*,*,n]=odata[*,*,n]/oindex[n].exptime
                oindex.exptime=1.0
            endif
            reord=sort(oindex.wavelnth)
            index2map,oindex[reord],odata[*,*,reord],map_in
            ;T_OBS gives the best estimate for AIA time
            ;DO NOT USE THE DEFAULT TIME FROM INDEX2MAP, which uses DATE_OBS -- starting time of integration
            map_in.time=anytim(oindex[reord].t_obs,/vms)
            if keyword_set(xran) or keyword_set(yran) then $
                sub_map,map_in,map,xran=xran,yran=yran else $
                map=temporary(map_in)
            if j eq 0 then tmpmap=replicate(map[0],nwave,navg) 
            tmpmap[*,j]=map
        endfor
        avgmap=tmpmap[*,fix(navg-1.)/2]
        avgmap.data=avg(tmpmap.data,3)
        avgmap.time=anytim(avg(anytim(tmpmap.time),1),/vms)
        if (i eq 0) then aiamaps=replicate(avgmap[0],nwave,ntim) 
        aiamaps[*,i]=avgmap
        aiatim[i]=timstr
    endelse
endfor
for n=0,nwave-1 do begin
    aiamaps[n,*].id='SDO/AIA '+waves[n]
    ;shift the map centers according to xshifts and yshifts
    for i=0,ntim-1 do begin
        aiamaps[n,i]=shift_map(aiamaps[n,i],xshifts[n],yshifts[n])
    endfor
endfor
tbg=anytim(timeran[0],/vms,/time_only)
tend=anytim(timeran[1],/vms,/time_only)
if ~keyword_set(savfile) then begin
    savpath='/home/bchen/work/IDLWorkspace/20120303/aia_dem/Hannah/'
    savname='aia_cutout_20120303_'+strmid(tbg,0,2)+strmid(tbg,3,2)+$
            strmid(tbg,6,2)+'-'+strmid(tend,0,2)+strmid(tend,3,2)+$
            strmid(tend,6,2)+'.'+string(dt_avg,format='(I2)')+'s.6wv.sav'
    savfile=savpath+savname
endif
save,file=savfile,aiamaps,waves
return
end
