pro aia_stackplt_trj,savfile=savfile,trjsav=trjsav,rescale=rescale,$
    baseplt=baseplt,runplt=runplt,base_timran=base_timran,$
    timran=timran,distran=distran,bminmax=bminmax,rminmax=rminmax,$
    degree=degree
if ~keyword_set(degree) then degree=1 ;linear fit
window,0,xs=2500,ys=700
;rescale the stackplot?
restore,savfile
if keyword_set(rescale) then begin
    t_dist_rescale_aia, savfile, base_timran, bminmax, rminmax,tims,dists,$
        bintensplt, rintensplt, bminmaxplt, rminmaxplt, $
        /b_log,ratio=[1,1],/smooth,smthsz=[2,4],dstep=2,/grid_t  
    if keyword_set(baseplt) then begin 
        intensplt=bintensplt
        minmaxplt=bminmaxplt
    endif
    if keyword_set(runplt) then begin 
        intensplt=rintensplt
        minmaxplt=rminmaxplt
    endif
endif else begin
    intensplt=alog10(intens)
    minmaxplt=[min(alog10(intens),/nan),max(alog10(intens),/nan)]
endelse
utbase=anytim(tims[0],/date_only)
if keyword_set(timran) then begin
    ind=where(tims ge anytim(timran[0]) and tims le anytim(timran[1]),nind)
    if nind gt 0 then begin
        tims=tims[ind]
        intensplt=intensplt[ind,*,*]
    endif else begin
        box_message,'No data in this time range. Check input.'
        return
    endelse
endif
if keyword_set(distran) then begin
    ind=where(dists ge distran[0] and dists le distran[1],nind)
    if nind gt 0 then begin
        dists=dists[ind]
        intensplt=intensplt[*,ind,*]
    endif else begin
        box_message,'No data in this distance range. Check input.'
        return
    endelse
endif
spectro_plot,bytscl(intensplt,min=minmaxplt[0],max=minmaxplt[1]),tims,dists,$
    ytit='Distance along cut (arcsec)',/xsty,/ysty
;draw the cut
ndstep=200
ts=dblarr(ndstep,100) & ds=ts
h0=dblarr(100) & h1=h0 & v0=h0 & v1=h0 & a=h0 
ans2=''
ntrj=0
next=1
while next eq 1 do begin 
    flag=0
    ts_=dblarr(30) & ds_=dblarr(30)
    ans1=''
    n=0
    wset,0
    !p.multi=0
    loadct,3
    spectro_plot,bytscl(intensplt,min=minmaxplt[0],max=minmaxplt[1]),tims,dists,$
        ytit='Distance along cut (arcsec)',/xsty,/ysty
    if ntrj gt 0 then begin 
        for i=0,ntrj-1 do cgplot,ts[*,i],ds[*,i],linesty=0,thick=2,color='white',/overplot 
    endif
    while flag eq 0 do begin
        !mouse.button=1
        while (!mouse.button eq 1) do begin
            spectro_plot,bytscl(intensplt,min=minmaxplt[0],max=minmaxplt[1]),tims,dists,$
                ytit='Distance along cut (arcsec)',/xsty,/ysty
            if ntrj gt 0 then begin 
                for i=0,ntrj-1 do cgplot,ts[*,i],ds[*,i],linesty=0,thick=2,color='white',/overplot 
            endif
            if n gt 0 then begin 
                if exist(tfits) then begin
                    cgplots,ts_[0:n-1],ds_[0:n-1],psym=1,symsize=2.0,thick=2,/data,color='white'
                    cgplot,tfits+utbase,dfits,linesty=0,thick=2,color='white',/overplot 
                endif else begin
                    cgplots,ts_[0:n-1],ds_[0:n-1],psym=-1,symsize=2.0,thick=2,/data,color='white'
                endelse
            endif

            print,'left click to pick up points of a trajectory, right click to close the trajectory: '
            cursor,t,d,/data,/down
            ts_[n]=t & ds_[n]=d
            ;do a 2nd-order polynomial fit
            if n ge 2 then begin
                effs=poly_fit(ts_[0:n]-utbase,ds_[0:n],degree,/double)
                tfits=dindgen(ndstep)*(ts_[n]-ts_[0])/(ndstep-1)+ts_[0]-utbase
                if degree eq 1 then dfits=effs[0]+effs[1]*tfits 
                if degree eq 2 then dfits=effs[0]+effs[1]*tfits+effs[2]*tfits^2.
                cgplot,tfits+utbase,dfits,linesty=0,thick=2,color='white',/overplot
            endif
            ;print,'time and distance: ',anytim(t,/vms,/time_only),d
            cgplots,ts_[0:n],ds_[0:n],psym=1,symsize=2.0,thick=2,/data,color='white'
            n=n+1
        endwhile
        ts_=ts_[0:n-1] & ds_=ds_[0:n-1]
        npnt=n_elements(ts_)
        wset,0
        !p.multi=0
        loadct,3
        spectro_plot,bytscl(intensplt,min=minmaxplt[0],max=minmaxplt[1]),tims,dists,$
            ytit='Distance along cut (arcsec)',/xsty,/ysty
        if ntrj gt 0 then begin 
            for i=0,ntrj-1 do cgplot,ts[*,i],ds[*,i],linesty=0,thick=2,color='white',/overplot 
        endif
        if npnt gt 2 then begin 
            cgplots,ts_,ds_,psym=1,symsize=2.0,thick=2,/data,color='white'
            cgplot,tfits+utbase,dfits,linesty=0,thick=2,color='white',/overplot 
        endif else begin
            cgplots,ts_,ds_,psym=-1,symsize=2.0,thick=2,/data,color='white'
        endelse
        ;print,'all time and distance: ',anytim(ts_,/vms,/time_only),ds_
        q1: read,'satisfied with this trajectory (y/n)? ',ans1
        case ans1 of
            'y' or 'Y': flag=1
            'n' or 'N': begin
                            flag=0
                            ts_=dblarr(30) & ds_=dblarr(30)
                            tfits=dblarr(ndstep) & dfits=dblarr(ndstep)
                            n=0
                        end
            else: begin
                    print,'please enter y/Y or n/N'
                    goto,q1
                  end
        endcase
    endwhile
    trjlen=sqrt((ts_[npnt-1]-ts_[0])^2.+(ds_[npnt-1]-ds_[0])^2.)
    if npnt gt 2 then begin
        effs=poly_fit(ts_-utbase,ds_,degree,/double)
        tfits=dindgen(ndstep)*(ts_[npnt-1]-ts_[0])/(ndstep-1)+ts_[0]-utbase
        if degree eq 1 then dfits=effs[0]+effs[1]*tfits 
        if degree eq 2 then dfits=effs[0]+effs[1]*tfits+effs[2]*tfits^2.
    endif else begin
        tfits=dindgen(ndstep)*(ts_[npnt-1]-ts_[0])/(ndstep-1)+ts_[0]-utbase
        dfits=dindgen(ndstep)*(ds_[npnt-1]-ds_[0])/(ndstep-1)+ds_[0]
    endelse
    wset,0
    !p.multi=0
    spectro_plot,bytscl(intensplt,min=minmaxplt[0],max=minmaxplt[1]),tims,dists,$
        ytit='Distance along cut (arcsec)',/xsty,/ysty
    if ntrj gt 0 then begin 
        for i=0,ntrj-1 do cgplot,ts[*,i],ds[*,i],linesty=0,thick=2,color='white',/overplot 
    endif
    cgplot,tfits+utbase,dfits,linesty=0,thick=2,color='white',/overplot
    ang=pb0r(tfits[0]+utbase,/arcsec)
    rsun=ang[2]
    asec2Mm=6.96d2/rsun
    asec2km=asec2Mm*1000.
    if degree eq 1 then begin
        h0_=effs[0]+effs[1]*tfits[0]
        h1_=effs[0]+effs[1]*tfits[ndstep-1]
        v0_=effs[1]*asec2km
        v1_=v0_
        a_=0.
    endif
    if degree eq 2 then begin
        h0_=effs[0]+effs[1]*tfits[0]+effs[2]*tfits[0]^2d0
        h1_=effs[0]+effs[1]*tfits[ndstep-1]+effs[2]*tfits[ndstep-1]^2d0
        v0_=(effs[1]+2.*effs[2]*tfits[0])*asec2km
        v1_=(effs[1]+2.*effs[2]*tfits[ndstep-1])*asec2km
        a_=2.*effs[2]*asec2km
    endif
    print,'starting height (arcsec)', h0_
    print,'ending height (arcsec)', h1_
    print,'starting velocity (km/s)', v0_
    print,'ending velocity (km/s)', v1_
    print,'acceleration (km s-2)',a_
    ts[*,ntrj]=tfits+utbase & ds[*,ntrj]=dfits
    h0[ntrj]=h0_ & h1[ntrj]=h1_
    v0[ntrj]=v0_ & v1[ntrj]=v1_
    a[ntrj]=a_
    q2: read,'Proceed to the next trajectory (y/n)? ',ans2
    case ans2 of
        'y' or 'Y': ntrj=ntrj+1
        'n' or 'N': next=0
        else: begin
                print,'please enter y/Y or n/N'
                goto,q2
              end
    endcase
endwhile
ts=ts[*,0:ntrj] & ds=ds[*,0:ntrj] 
h0=h0[0:ntrj] & h1=h1[0:ntrj]
v0=v0[0:ntrj] & v1=v1[0:ntrj]
a=a[0:ntrj]
if ~keyword_set(trjsav) then begin
    ind=strpos(savfile,'.spsav')
    trjsav=strmid(savfile,0,ind)+'.trjsav'
endif
save,file=trjsav,ts,ds,h0,h1,v0,v1,a
end
