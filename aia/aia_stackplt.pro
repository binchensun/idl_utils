pro aia_stackplt,mapcube,wave=wave,drawcut=drawcut,cutsav=cutsav,savfile=savfile,$
    cutwidth=cutwidth,cutang=cutang,dopoly=dopoly,dospline=dospline,$
    dminmax=dminmax,dropshort=dropshort,sat_lvl=sat_lvl,domovie=domovie,moviepath=moviepath,$
    xrange=xrange,yrange=yrange,timerange=timerange,xysize=xysize,refxy=refxy,$
    rdiff=rdiff,ratio=ratio,rminmax=rminmax,dstep=dstep,$
    bdiff=bdiff,tbase=tbase,bminmax=bminmax,log=log,dosmooth=dosmooth,doplot=doplot
;1. Set up two ends of each slit
;2. Divide the length of the slit into n distance steps
;3. At each distance point, draw a line perpendicular to the slit, and average along the line
;4. Assign the mean and standard deviation of the  intensity to the given distance point, 
;input parameters:
;        mapcube: map cube as a function of time 
;        wave: a string value indicating aia band, e.g., '193', '131', '94', '335', '211', '171', '304'
;        drawcut: draw linear cut
;        cutsav: name of the IDL sav file containing the cut information
;        savheader: name (header) of the output time-distance sav file
;        cutwidth: width of individual cut for average
;        cutang: if nonzero, width of the cut increases in the cut direction
;        dminmax: scaling of the AIA image, in logarithm scale
;        dropshort: drop short exposures? 0 no, 1 yes. Default no
;        sat_lvl: above this value consider pixel saturated and don't use (default 1.5e4)
;        refxy: reference location in solar x and y (arcsecs), can be a 2-element array (x, y), or a n x 2 array with n reference points
;optimum values for displaying movie
;wave='193', dmin=1.2, dmax=4.5
;wave='211', dmin=1.1, dmax=4.
;wave='171', dmin=1.1, dmax=4.
idl_work_dir=getenv('IDL_WORK_DIR')
if ~keyword_set(xysize) then xysize=[800,1000]
if ~keyword_set(sat_lvl) then sat_lvl=1.5e4
if ~keyword_set(dminmax) then begin
    if wave eq '193' then dminmax=10.^[1.5,4.]
    if wave eq '211' then dminmax=10.^[0.8,4.]
    if wave eq '171' then dminmax=10.^[1.1,4.]
    if wave eq '131' then dminmax=10.^[0.,3.]
    if wave eq '94' then dminmax=10.^[-0.5,2.5]
    if wave eq '335' then dminmax=10.^[-0.5,2.5]
    if wave eq '304'  then dminmax=10.^[0.5,4.]
    if wave eq 'fe18' or wave eq 'Fe XVIII' then dminmax=10.^[-0.5,2.5]
endif
if ~keyword_set(ratio) then ratio=0
if ~keyword_set(rminmax) then begin
    if ~keyword_set(ratio) then $
        rminmax=[-dminmax[1]/50.,dminmax[1]/50.] $
        else rminmax=[0.3,3.]
endif
if ~keyword_set(dstep) then dstep=1
if ~keyword_set(savfile) then begin
    if isa(mapcube,'String') then begin 
        ind=strpos(mapcube,'.sav')
        savfile=strmid(mapcube,0,ind)+'.spsav'
    endif else begin
        savfile='tmp.spsav'
    endelse
endif
if ~keyword_set(cutwidth) then cutwidth=5.
if ~keyword_set(cutang) then cutang=0.*!dtor ;angular width of individual cut, in rad
if ~keyword_set(dopoly) and ~keyword_set(dospline) then begin
    dospline=1
endif
;prep mapcube to the designated timerange and xyrange
if exist(mapcube) then begin
    if isa(mapcube,'Anonymous') then maps=mapcube
    if isa(mapcube,'String') then restore,mapcube 
endif else begin
    print,'Input map cube does not exist!'
    return
endelse
nmap=n_elements(maps)
if ~exist(durs) then durs=intarr(nmap)
;determine timerange to plot
timsecs=anytim(maps.time)
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
dt1=min(abs(timsecs-timeran[0]),tbg_ind)
dt2=min(abs(timsecs-timeran[1]),tend_ind)
maps=maps[tbg_ind:tend_ind]
if exist(reftim) then reftim=reftim[tbg_ind:tend_ind]
if exist(durs) then durs=durs[tbg_ind:tend_ind]
if exist(dur_flgs) then dur_flgs=dur_flgs[tbg_ind:tend_ind]
if keyword_set(xrange) and keyword_set(yrange) then begin 
    sub_map,maps,smaps,xran=xrange,yran=yrange
    maps=temporary(smaps)
endif

if keyword_set(cutsav) and ~keyword_set(drawcut) then begin
    restore,cutsav
    map0=maps[0] & dx=map0.dx & dy=map0.dy
    sz=size(map0.data)
    window,1,xs=1200,ys=800
    !p.multi=[0,3,2]
    !p.charsize=2.0
    fov=[0.06,0.08,0.97,0.97]
    asp=(float(sz[2])*dy)/(float(sz[1])*dx)
    pos=bclayout([3,2],fov=fov,asp=asp,xgap=0.08,ygap=0.05)
    if wave ne 'Fe XVIII' then aia_lct,wave=wave,/load else loadct,5,/silent
    nmap=n_elements(maps)
    mapinds=indgen(6)*(nmap-1)/5
    for i=0,5 do begin
        map=maps[mapinds[i]]
        plot_map,map,dmin=dminmax[0],dmax=dminmax[1],$
            log=log,position=pos[*,i],title=map.id+' @ '+map.time
        if keyword_set(refxy) then begin
            sz=size(refxy)
            if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
            if sz[0] eq 2 and sz[2] eq 2 then begin
                for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
            endif
        endif
        cgplot,xs,ys,linesty=0,thick=1,/data,color='white',/overplot
        cgplot,xs0,ys0,linesty=2,thick=1,color='white',/overplot
        cgplot,xs1,ys1,linesty=2,thick=1,color='white',/overplot
        if keyword_set(refxy) then begin
            sz=size(refxy)
            if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
            if sz[0] eq 2 and sz[2] eq 2 then begin
                for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
            endif
        endif
    endfor
endif
if keyword_set(drawcut) then begin
    nmap=n_elements(maps)
    mapinds=indgen(6)*(nmap-1)/5
    ;select six maps over the entire range
    map0=maps[0] & dx=map0.dx & dy=map0.dy
    sz=size(map0.data)
    window,1,xs=1200,ys=800
    !p.multi=[0,3,2]
    !p.charsize=2.0
    fov=[0.06,0.08,0.97,0.97]
    asp=(float(sz[2])*dy)/(float(sz[1])*dx)
    pos=bclayout([3,2],fov=fov,asp=asp,xgap=0.08,ygap=0.05)
    loadct,3
    for i=0,5 do begin
        map=maps[mapinds[i]]
        plot_map,map,dmin=dminmax[0],dmax=dminmax[1],log=log,position=pos[*,i],title=map.id+' @ '+map.time
        if keyword_set(refxy) then begin
            sz=size(refxy)
            if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
            if sz[0] eq 2 and sz[2] eq 2 then begin
                for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
            endif
        endif
    endfor
    timplt=''
    read,'Enter image index or time for setting the cut (in anytim format): ', timplt
    if strlen(timplt) le 1 then t_ind=mapinds[fix(timplt)] else $
        dt=min(anytim(maps.time)-anytim(timplt),t_ind,/abs)
    mapplt=maps[t_ind]
    window,0,xs=800,ys=800
    plot_map,mapplt,dmin=dminmax[0],dmax=dminmax[1],title=map.id+' @ '+map.time,log=log
    if keyword_set(refxy) then begin
        sz=size(refxy)
        if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
        if sz[0] eq 2 and sz[2] eq 2 then begin
            for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
        endif
    endif
    ;draw the cut
    flag=0
    ans=''
    xs_=fltarr(100) & ys_=fltarr(100)
    dstep=mean([dx,dy])*1. ;step size along the cut = pixel size
    n=0
    while flag eq 0 do begin
        !mouse.button=1
        while (!mouse.button eq 1) do begin
            wset,0
            !p.multi=0
            loadct,3
            plot_map,mapplt,dmin=dminmax[0],dmax=dminmax[1],log=log,title=mapplt.id+' @ '+mapplt.time
            if keyword_set(refxy) then begin
                sz=size(refxy)
                if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
                if sz[0] eq 2 and sz[2] eq 2 then begin
                    for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
                endif
            endif
            if n gt 0 then begin 
                if exist(xfits) then begin
                    cgplots,xs_[0:n-1],ys_[0:n-1],psym=1,symsize=2.0,thick=2,/data,color='white'
                    cgplot,xfits,yfits,linesty=0,thick=2,color='white',/overplot 
                endif else begin
                    cgplots,xs_[0:n-1],ys_[0:n-1],psym=-1,symsize=2.0,thick=2,/data,color='white'
                endelse
            endif

            cursor,x,y,/data,/down
            xs_[n]=x & ys_[n]=y
            if n ge 2 then begin
                ;rotate the coordinates defined by the 1st and last point
                theta=atan(y-ys_[0],x-xs_[0])
                m_rot=[[cos(-theta),-sin(-theta)],[sin(-theta),cos(-theta)]]
                m_rot1=[[cos(theta),-sin(theta)],[sin(theta),cos(theta)]]
                c0=[[xs_[0:n]],[ys_[0:n]]]
                c1=m_rot ## c0
                ;xfits=findgen(300)*(xs_[n]-xs_[0])/299.+xs_[0]
                xfits_=findgen(300)*(c1[n,0]-c1[0,0])/299.+c1[0,0]
                if keyword_set(dopoly) then begin
                    effs=poly_fit(c1[*,0],c1[*,1],2)
                    ;yfits=effs[0]+effs[1]*xfits+effs[2]*xfits^2.
                    yfits_=effs[0]+effs[1]*xfits_+effs[2]*xfits_^2.
                endif
                if keyword_set(dospline) then begin
                    yfits_=spline(c1[*,0],c1[*,1],xfits_,1)
                endif
                c2=[[xfits_],[yfits_]]
                c3=m_rot1 ## c2
                xfits=c3[*,0] & yfits=c3[*,1]
                cgplot,xfits,yfits,linesty=0,thick=2,color='white',/overplot
            endif
            print,'X Y Coordinates: ',x,y
            cgplots,xs_[0:n],ys_[0:n],psym=1,symsize=2.0,thick=2,/data,color='white'
            wset,1
            !p.multi=[0,3,2]
            for i=0,5 do begin
                map=maps[mapinds[i]]
                plot_map,map,dmin=dminmax[0],dmax=dminmax[1],$
                    log=log,position=pos[*,i],title=map.id+' @ '+map.time
                cgplots,xs_[0:n],ys_[0:n],psym=1,symsize=2.0,thick=2,/data,color='white'
                if n ge 2 then cgplot,xfits,yfits,linesty=0,thick=1,color='white',/overplot
                if keyword_set(refxy) then begin
                    sz=size(refxy)
                    if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
                    if sz[0] eq 2 and sz[2] eq 2 then begin
                        for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
                    endif
                endif
            endfor
            n=n+1
        endwhile
        xs_=xs_[0:n-1] & ys_=ys_[0:n-1]
        npnt=n_elements(xs_)
        wset,0
        !p.multi=0
        loadct,3
        plot_map,mapplt,dmin=dminmax[0],dmax=dminmax[1],log=log,title=mapplt.id+' @ '+mapplt.time
        if keyword_set(refxy) then begin
            sz=size(refxy)
            if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
            if sz[0] eq 2 and sz[2] eq 2 then begin
                for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
            endif
        endif
        if npnt gt 2 then begin 
            cgplots,xs_,ys_,psym=1,symsize=2.0,thick=2,/data,color='white'
            cgplot,xfits,yfits,linesty=0,thick=2,color='white',/overplot 
        endif else begin
            cgplots,xs_,ys_,psym=-1,symsize=2.0,thick=2,/data,color='white'
        endelse
        print,'all x and y coords: ',xs_,ys_
        read,'satisfied with this cut (y/n)? ',ans
        if (ans eq 'y') or (ans eq 'Y') then flag=1 
        if (ans eq 'n') or (ans eq 'N') then begin
            flag=0
            xs_=fltarr(100) & ys_=fltarr(100)
            n=0
        endif
    endwhile
    cutlen=sqrt((xs_[npnt-1]-xs_[0])^2.+(ys_[npnt-1]-ys_[0])^2.)
    ndstep=fix(cutlen/dstep)
    if npnt gt 2 then begin
        theta=atan(ys_[npnt-1]-ys_[0],xs_[npnt-1]-xs_[0])
        m_rot=[[cos(-theta),-sin(-theta)],[sin(-theta),cos(-theta)]]
        m_rot1=[[cos(theta),-sin(theta)],[sin(theta),cos(theta)]]
        c0=[[xs_],[ys_]]
        c1=m_rot ## c0
        ;xs=findgen(ndstep)*(xs_[npnt-1]-xs_[0])/(ndstep-1)+xs_[0]
        xs_tmp=findgen(ndstep)*(c1[npnt-1,0]-c1[0,0])/(ndstep-1)+c1[0,0]
        if keyword_set(dopoly) then begin
            effs=poly_fit(c1[*,0],c1[*,1],2)
            ;yfits=effs[0]+effs[1]*xfits+effs[2]*xfits^2.
            ys_tmp=effs[0]+effs[1]*xs_tmp+effs[2]*xs_tmp^2.
        endif
        if keyword_set(dospline) then begin
            ys_tmp=spline(c1[*,0],c1[*,1],xs_tmp,1)
        endif
        c2=[[xs_tmp],[ys_tmp]]
        c3=m_rot1 ## c2
        xs=c3[*,0] & ys=c3[*,1]
    endif else begin
        xs=findgen(ndstep)*(xs_[npnt-1]-xs_[0])/(ndstep-1)+xs_[0]
        ys=findgen(ndstep)*(ys_[npnt-1]-ys_[0])/(ndstep-1)+ys_[0]
    endelse
    wset,0
    !p.multi=0
    plot_map,mapplt,dmin=dminmax[0],dmax=dminmax[1],log=log,title=mapplt.id+' @ '+mapplt.time
    if keyword_set(refxy) then begin
        sz=size(refxy)
        if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
        if sz[0] eq 2 and sz[2] eq 2 then begin
            for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
        endif
    endif
    cgplot,xs,ys,linesty=0,thick=2,color='white',/overplot
    xs0=xs & ys0=ys 
    xs1=xs & ys1=ys
    dis=0.
    dists=fltarr(ndstep) & cutwidths=dists 
    posangs=dists & posang2s=dists
    for i=0,ndstep-1 do begin
        if i eq 0 then begin
            dis=0.
            cutwidth_=cutwidth
            xcen=xs[i] & ycen=ys[i]
            xup=xs[i+1] & yup=ys[i+1]
            posang=atan(yup-ycen,xup-xcen)
        endif
        if i ne 0 and i ne ndstep-1 then begin
            xdown=xs[i-1] & ydown=ys[i-1]
            xcen=xs[i] & ycen=ys[i]
            xup=xs[i+1] & yup=ys[i+1]
            dis=dis+sqrt((xcen-xdown)^2.+(ycen-ydown)^2.)
            cutwidth_=cutang*dis+cutwidth
            posang=atan(yup-ydown,xup-xdown)
        endif
        if i eq ndstep-1 then begin
            xdown=xs[i-1] & ydown=ys[i-1]
            xcen=xs[i] & ycen=ys[i]
            dis=dis+sqrt((xcen-xdown)^2.+(ycen-ydown)^2.)
            cutwidth_=cutang*dis+cutwidth
            posang=atan(ycen-ydown,xcen-xdown)
        endif
        dists[i]=dis
        cutwidths[i]=cutwidth_
        posangs[i]=posang
        posang2=!pi/2.+posang
        posang2s[i]=posang2
        xs0[i]=xcen-cutwidth_/2.*cos(posang2)
        ys0[i]=ycen-cutwidth_/2.*sin(posang2)
        xs1[i]=xcen+cutwidth_/2.*cos(posang2)
        ys1[i]=ycen+cutwidth_/2.*sin(posang2)
    endfor
    cgplot,xs0,ys0,linesty=2,thick=1,color='white',/overplot
    cgplot,xs1,ys1,linesty=2,thick=1,color='white',/overplot
    wset,1
    !p.multi=[0,3,2]
    for i=0,5 do begin
        map=maps[mapinds[i]]
        plot_map,map,dmin=dminmax[0],dmax=dminmax[1],$
            log=log,position=pos[*,i],title=map.id+' @ '+map.time
        cgplot,xs,ys,linesty=0,thick=1,/data,color='white',/overplot
        cgplot,xs0,ys0,linesty=2,thick=1,color='white',/overplot
        cgplot,xs1,ys1,linesty=2,thick=1,color='white',/overplot
        if keyword_set(refxy) then begin
            sz=size(refxy)
            if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
            if sz[0] eq 2 and sz[2] eq 2 then begin
                for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
            endif
        endif
    endfor
    if ~keyword_set(cutsav) then begin
        if isa(mapcube,'String') then begin
            ind=strpos(mapcube,'.sav')
            cutsav=strmid(mapcube,0,ind)+'.cutsav'
        endif else begin
            cutsav='tmp.cutsav'
        endelse
    endif
    save,file=cutsav,dists,xs,xs0,xs1,ys,ys0,ys1,cutwidths,posangs,posang2s,ncut
end

ndist=n_elements(dists)
tims=anytim(maps.time)
ntim=n_elements(tims)
nt=ntim

if ~exist(dur_flgs) then dur_flgs=intarr(ntim)
if keyword_set(dropshort) then begin
    ldur_ts=where(dur_flgs eq 0, nt) ;plot only normal exposure frames
    if ~exist(durs) then durs=intarr(ntim)
    if nt le 0 then begin
        box_message,'No time frames with normal exposures'
        return
    endif
    tims=tims[ldur_ts]
    maps=maps[ldur_ts]
endif
timstr=anytim(tims,/ccsds)
ncut=1
intens=fltarr(nt,ndist,ncut)
intens_sd=fltarr(nt,ndist,ncut)
npixs=intarr(nt,ndist,ncut)
widthres=0.3 ;resolution of cut width perpendicular to the cut, in arcsec
;nwidth=fix(cutwidth/widthres) ;number of steps perpendicular to the cut 
for n=0,ncut-1 do begin
    ;produce time-distance data
    ;xedges=fltarr(2,nwidth,ncut)
    ;widthsamp=(findgen(nwidth)-(nwidth-1)/2.)*widthres
    print,'=====cut ',n, ' to process====='
    for i=0, nt-1 do begin
        if (i mod 50) eq 0 then print, 'Processing',i,' out of ',nt, ' times...'
        map=maps[i]
        dur=durs[i]
        mapdata=map.data
        ;calculate xaxis and yaxis
        xc=map.xc & yc=map.yc
        dx=map.dx & dy=map.dy
        sz=size(map.data)
        nx=sz[1] & ny=sz[2]
        xaxis=findgen(nx+1)*dx-nx/2.*dx+xc
        yaxis=findgen(ny+1)*dy-ny/2.*dy+yc
        xedge=fltarr(2) & yedge=xedge
        for j=0, ndist-1 do begin
            dis=dists[j]
            xcen=xs[j] & ycen=ys[j]
            cutwidth=cutwidths[j]
            x0=xs0[j] & y0=ys0[j]
            x1=xs1[j] & y1=ys1[j]
        ;    print,'at dist: ',dis,xcen,ycen
        ;   print,'xedge and yedge', xedge, yedge
            image_size=[(x1-x0)/dx, (y1-y0)/dy]
            xy=find_pixel_intersects([x0,x1],[y0,y1],xaxis,yaxis,image_size,xvals=xvals,yvals=yvals,dist=dist0)
            ;find x and y inside the image FOV
            i1=where((xy[*,0] ge 0) and (xy[*,0] le nx-1),ni1)
            if ni1 ge 1 then i2=where((xy[i1,1] ge 0) and (xy[i1,1] le ny-1),ni2) $
                else ni2=0
            if ni1 lt 1 or ni2 lt 1 then begin
                ;print,'pixels are not in the image fov, skip dist: ',dis
                intens[i,j,n]=0.
            endif else begin
                newxy=xy[i1[i2],*]
                newdist0=dist0[i1[i2]]
                npix0=n_elements(i2)
                imval0=fltarr(npix0)
                for k=0, npix0-1 do imval0[k]=mapdata[newxy[k,0],newxy[k,1]]
                unsatpx=where(imval0*dur lt sat_lvl, npix)
                if npix gt 1 then begin
                    intens[i,j,n]=mean(imval0[unsatpx])
                    intens_sd[i,j,n]=stddev(imval0[unsatpx])
                endif else begin
                    ;saturated. assign a really big number
                    intens[i,j,n]=1e10
                    intens_sd[i,j,n]=1e10
                    npix=0
                endelse
                npixs[i,j,n]=npix
            endelse    
        endfor
    endfor
endfor
if ncut eq 1 then begin
    intens=reform(intens,nt,ndist,1)
    intens_sd=reform(intens_sd,nt,ndist,1)
    npixs=reform(npixs,nt,ndist,1)
endif
save,file=savfile,intens,intens_sd,npixs,tims,timstr,dists,xs,xs0,xs1,ys,ys0,ys1,cutwidths,posangs,posang2s,ncut,dur_flgs   
if keyword_set(domovie) then begin
    if ~keyword_set(moviepath) then moviepath='./'
    ;make intensity movie
    nmap=n_elements(maps)
    if wave ne 'Fe XVIII' then begin
        aia_lct,r,g,b,wave=wave,/load
    endif else begin
        loadct,5
        tvlct,r,g,b,/get
    endelse
    print,strtrim(string(nmap),2)+' images to process...' 
    for i=0,nmap-1 do begin
        set_plot,'z'
        clearplot
        erase
        device,set_resolution=xysize,z_buffer=0
        !p.multi=[0,1,2]
        if keyword_set(rdiff) then begin
            if i ge dstep then begin
                if keyword_set(dosmooth) then begin
                    maps[i].data=smooth(maps[i].data,[2,2])
                    maps[i-dstep].data=smooth(maps[i-dstep].data,[2,2])
                endif
                map=diff_map(maps[i],maps[i-dstep],ratio=ratio) 
            endif else begin
                map=maps[i]
                map.data=0.
            endelse
            plot_map,map,dmin=rminmax[0],dmax=rminmax[1],_extra=ex,title=map.id+' @ '+map.time
        endif
        if keyword_set(bdiff) then begin
            if keyword_set(tbase) then begin
                dt=min(anytim(maps.time)-anytim(tbase),b0i,/abs)
            endif else begin
                b0i=0 ;use the first map of the cube
            endelse
            if keyword_set(dosmooth) then begin
                maps[i].data=smooth(maps[i].data,[2,2])
                maps[b0i].data=smooth(maps[b0i].data,[2,2])
            endif
            map=diff_map(maps[i],maps[b0i],ratio=ratio) 
            plot_map,map,log=log,dmin=bminmax[0],dmax=bminmax[1],_extra=ex,title=map.id+' @ '+map.time
        endif
        if ~keyword_set(rdiff) and ~keyword_set(bdiff) then begin
            map=maps[i]
            plot_map,map,log=log,dmin=dminmax[0],dmax=dminmax[1],_extra=ex,title=map.id+' @ '+map.time
        endif
        if keyword_set(refxy) then begin
            sz=size(refxy)
            if sz[0] eq 1 and sz[1] eq 2 then plots,refxy[0],refxy[1],symsize=3,psym=4,color=255
            if sz[0] eq 2 and sz[2] eq 2 then begin
                for k=0,sz[1]-1 do plots,refxy[k,0],refxy[k,1],symsize=3,psym=4,color=255
            endif
        endif
        cgplot,xs,ys,linesty=0,thick=1,/data,color=255,/overplot
        cgplot,xs0,ys0,linesty=1,thick=1,color=255,/overplot
        cgplot,xs1,ys1,linesty=1,thick=1,color=255,/overplot
        loadct,39,/silent
        if keyword_set(rdiff) then begin
            t_dist_rescale_aia, intens, tims, dists, mode='run', dstep=dstep,$
                    ratio=ratio,rintensplt=rintensplt, rminmax=rminmax,r_lim=r_lim,/smooth,smthsz=[1,1,1]
            spectro_plot,bytscl(rintensplt,min=r_lim[0],max=r_lim[1]),tims,dists,$
                ytit='Distance along cut (arcsec)',/xsty,/ysty
        endif
        if keyword_set(bdiff) then begin
            t_dist_rescale_aia, intens, tims, dists, mode='base', tbase=tbase,$
                    b_log=log, bintensplt=bintensplt, b_lim=b_lim, bminmax=bminmax,/smooth,smthsz=[1,1,1]
            spectro_plot,bytscl(bintensplt,min=b_lim[0],max=b_lim[1]),tims,dists,$
                ytit='Distance along cut (arcsec)',/xsty,/ysty
        endif
        if ~keyword_set(rdiff) and ~keyword_set(bdiff) then begin
            spectro_plot,bytscl(alog10(intens),min=alog10(dminmax[0]),max=alog10(dminmax[1])),tims,dists,$
                ytit='Distance along cut (arcsec)',/xsty,/ysty
        endif
        oplot,replicate(anytim(map.time),2),[dists[0]-10,dists[-1]+10],linesty=0,thick=2,color=255
        if keyword_set(refxy) then begin
            sz=size(refxy)
            if sz[0] eq 1 and sz[1] eq 2 then begin
                dd=min((xs-refxy[0])^2.+(ys-refxy[1])^2.,ind)
                oplot,[tims[0],tims[-1]],replicate(dists[ind],2),lines=1,color=255
            endif
            if sz[0] eq 2 and sz[2] eq 2 then begin
                for k=0,sz[1]-1 do begin
                    dd=min((xs-refxy[k,0])^2.+(ys-refxy[k,1])^2.,ind)
                    oplot,[tims[0],tims[-1]],replicate(dists[ind],2),lines=1,color=255
                endfor
            endif
        endif
        snapshot=tvrd()
        device,z_buffer=1
        set_plot,'x'
        image24=bytarr(3,xysize[0],xysize[1])
        image24[0,*,*]=r[snapshot]
        image24[1,*,*]=g[snapshot]
        image24[2,*,*]=b[snapshot]
        otimstr=repstr(strmid(anytim(map.time,/vms,/time_only),0,8),':','')
        write_jpeg,moviepath+'aia_'+wave+'_'+otimstr+'.jpg',image24,true=1,quality=90
        if (i gt 0) and ((i mod 10) eq 0) then print,strtrim(string(i),2)+' images drawn'
    endfor
    ;make javascript movie
    cd,moviepath,cur=cur
    images=file_search('*.jpg')
    jsmovie_name='aia_stackplot_'+wave+'.html'
    jsmovie,jsmovie_name,images
    cd,cur
endif else begin
    if keyword_set(doplot) then begin
        window,2,xs=800,ys=1000
        ;if wave ne 'Fe XVIII' then aia_lct,wave=wave,/load else loadct,5,/silent
        loadct,39
        cgerase
        !p.multi=[0,1,3]
        tbase=[maps[0].time,maps[5].time]
        bminmax=[0.1,10.]
        spectro_plot,bytscl(alog10(intens),min=alog10(dminmax[0]),max=alog10(dminmax[1])),tims,dists,$
            ytit='Distance along cut (arcsec)',/xsty,/ysty
        t_dist_rescale_aia, intens, tims, dists, mode='base',$
            tbase=tbase, bminmax=bminmax, bintensplt=bintensplt,b_lim=b_lim,/b_log,$
            /smooth,smthsz=[1,2,1]
        spectro_plot,bytscl(bintensplt,min=b_lim[0],max=b_lim[1]),tims,dists,$
            ytit='Distance along cut (arcsec)',/xsty,/ysty
        t_dist_rescale_aia, intens, tims, dists, mode='run', dstep=3,$
                /ratio,rintensplt=rintensplt, rminmax=[0.5,2],r_lim=r_lim,/smooth,smthsz=[1,2,1]
        spectro_plot,bytscl(rintensplt,min=r_lim[0],max=r_lim[1]),tims,dists,$
            ytit='Distance along cut (arcsec)',/xsty,/ysty
        !p.multi=0
    endif
endelse
end
