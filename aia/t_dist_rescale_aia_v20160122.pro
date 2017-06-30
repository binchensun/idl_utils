pro t_dist_rescale_aia, sav_file, base_timran, bminmax, rminmax, tims,dists,$
    bintensplt, rintensplt, bminmaxplt, rminmaxplt, b_log=b_log, $
    ratio=ratio,dstep=dstep,$
    smooth=smooth, gsmooth=gsmooth, smthsz=smthsz,grid_t=grid_t 
if ~keyword_set(smthsz) then begin
    if keyword_set(smooth) then smthsz=[8,8]
    if keyword_set(gsmooth) then smthsz=[3,3]
endif
sz=size(smthsz,/dim)
if n_elements(smthsz) ne 0 then begin
    if n_elements(sz) lt 2 then begin
        if sz lt 3 then smthsz=[smthsz,1]
        smthsz=rebin(smthsz,3,2)
    endif else begin 
        if sz[1] eq 2 and sz[0] lt 3 then begin
            smthsztmp=smthsz
            smthsz=intarr(3,2)
            for i=0,1 do begin
                smthsz[0:1,i]=smthsztmp[*,i]
                smthsz[2,i]=1
            endfor
        endif
    endelse
endif
if ~keyword_set(dstep) then dstep=1
if ~keyword_set(ratio) then ratio=[0,0]
if ~keyword_set(smooth) then smooth=[0,0]
if ~keyword_set(gsmooth) then gsmooth=[0,0]
if n_elements(ratio) lt 2 then ratio=replicate(ratio, 2) 
if n_elements(smooth) lt 2 then smooth=replicate(smooth, 2) 
if n_elements(gsmooth) lt 2 then gsmooth=replicate(gsmooth, 2) 
restore,sav_file
aia_tims=tims
dt1=min(anytim(tims)-anytim(base_timran[0]),tbg_ind_base,/absolute)
dt2=min(anytim(tims)-anytim(base_timran[1]),tend_ind_base,/absolute)
ndist=n_elements(dists[*,0])
ntim=n_elements(tims)
timsecs=anytim(tims)
ind=where(finite(intens) eq 0, nind)
if nind gt 0 then intens[ind]=0.
if keyword_set(grid_t) then begin
    ;make the grid regular for the time axis
    dts=dblarr(ntim-1)
    for i=0,ntim-2 do dts[i]=timsecs[i+1]-timsecs[i]
    dt=median(dts)
    nt=fix((timsecs[ntim-1]-timsecs[0])/dt)+1
    timsecs_=dindgen(nt)*dt+timsecs[0]
    intens_=dindgen(nt,ndist,ncut)
    if ncut eq 1 then intens_=reform(intens_,nt,ndist,ncut)
    ts_intp=dindgen(nt)*ntim/(nt-1d0)
    for k=0,ncut-1 do intens_[*,*,k]=interpolate(intens[*,*,k],ts_intp,dindgen(ndist),cubic=-0.5,/grid)
    ntim=nt & tims=timsecs_
    intens=reform(intens_,ntim,ndist,ncut)
endif

bintens=intens & rintens=intens
if smooth[0] then bintens=smooth(intens,smthsz[*,0],/edge_truncate)
if smooth[1] then rintens=smooth(intens,smthsz[*,1],/edge_truncate)
if gsmooth[0] eq 1 then begin
    for k=0,ncut-1 do begin
        bintens[*,*,k]=gauss_smooth(intens[*,*,k],smthsz[0:1,0],/edge_truncate)
    endfor
endif
if gsmooth[1] eq 1 then begin
    for k=0,ncut-1 do begin
        rintens[*,*,k]=gauss_smooth(intens[*,*,k],smthsz[0:1,1],/edge_truncate)
    endfor
endif
base=avg(intens[tbg_ind_base:tend_ind_base,*,*],0)
if ratio[1] eq 1 then rintens[0:dstep,*,*]=1. else rintens[0:dstep,*,*]=0.
for k=0,ncut-1 do begin
    if ratio[0] eq 1 then begin 
        for i=0,ntim-1 do bintens[i,*,k]=bintens[i,*,k]/base[*,k]
    endif else begin
        for i=0,ntim-1 do bintens[i,*,k]=bintens[i,*,k]-base[*,k]
    endelse
    rintenstmp=rintens
    if ratio[1] eq 1 then begin 
        for i=dstep,ntim-1 do begin
            rintens[i,*,k]=rintenstmp[i,*,k]/rintenstmp[i-dstep,*,k]
        endfor
    endif else begin
        for i=dstep,ntim-1 do begin
            rintens[i,*,k]=rintenstmp[i,*,k]-rintenstmp[i-dstep,*,k]
        endfor
    endelse
endfor
if b_log eq 1 then begin 
   bintensplt=alog10(bintens)
   bminmaxplt=alog10(bminmax)
endif else begin
   bintensplt=bintens
   bminmaxplt=bminmax
endelse
rintensplt=rintens
rminmaxplt=rminmax
end
