pro t_dist_rescale_aia, intens, tims, dists, savfile=savfile, grid_t=grid_t, mode=mode, $
    ominmax=ominmax, ointensplt=ointensplt,o_lim=o_lim,log=log,$
    tbase=tbase, bminmax=bminmax, bintensplt=bintensplt, b_lim=b_lim, b_log=b_log,$ 
    rminmax=rminmax, rintensplt=rintensplt, r_lim=r_lim,dstep=dstep,$
    ratio=ratio,smooth=smooth, gsmooth=gsmooth, smthsz=smthsz 
; INPUTS:
;       mode: 'inten', 'base','run'
sz=size(intens,/dim)
ntim=sz[0] & ndist=sz[1] & ncut=sz[2]
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
if ~keyword_set(smthsz) then begin
    if keyword_set(smooth) then smthsz=[8,8]
    if keyword_set(gsmooth) then smthsz=[3,3]
endif
;sz=size(smthsz,/dim)
;if n_elements(smthsz) ne 0 then begin
;    if n_elements(sz) lt 2 then begin
;        if sz lt 3 then smthsz=[smthsz,1]
;        smthsz=rebin(smthsz,3,2)
;    endif else begin 
;        if sz[1] eq 2 and sz[0] lt 3 then begin
;            smthsztmp=smthsz
;            smthsz=intarr(3,2)
;            for i=0,1 do begin
;                smthsz[0:1,i]=smthsztmp[*,i]
;                smthsz[2,i]=1
;            endfor
;        endif
;    endelse
;endif
case mode of 
    'inten': begin
                if keyword_set(log) then begin 
                   ointensplt=alog10(intens)
                   o_lim=alog10(ominmax)
                endif else begin
                   ointensplt=intens
                   o_lim=ominmax
                endelse
            end
    'base': begin
                bintens=intens
                if keyword_set(smooth) then bintens=smooth(intens,smthsz,/edge_truncate)
                if keyword_set(gsmooth) then begin
                    for k=0,ncut-1 do begin
                        bintens[*,*,k]=gauss_smooth(intens[*,*,k],smthsz,/edge_truncate)
                    endfor
                endif
                if keyword_set(tbase) then begin
                    sz=size(tbase)
                    if sz[0] eq 0 then begin ;only one value
                        dt=min(anytim(tims)-anytim(tbase),tbg_ind_base,/absolute)
                        base=reform(intens[tbg_ind_base,*,*],ndist,ncut)
                    endif else begin
                        if sz[0] eq 1 and sz[1] eq 2 then begin ;one dimension array with 2 elements
                            dt1=min(anytim(tims)-anytim(base_timran[0]),tbg_ind_base,/absolute)
                            dt2=min(anytim(tims)-anytim(base_timran[1]),tend_ind_base,/absolute)
                            base=avg(intens[tbg_ind_base:tend_ind_base,*,*],0)
                        endif else begin
                            print,'input tbase not understood...'
                            break
                        endelse
                    endelse
                endif
                for k=0,ncut-1 do begin
                    if keyword_set(ratio) then $ 
                        for i=0,ntim-1 do bintens[i,*,k]=bintens[i,*,k]/base[*,k] $
                        else for i=0,ntim-1 do bintens[i,*,k]=bintens[i,*,k]-base[*,k]
                endfor
                if keyword_set(b_log) then begin 
                   bintensplt=alog10(bintens)
                   b_lim=alog10(bminmax)
                endif else begin
                   bintensplt=bintens
                   b_lim=bminmax
                endelse
            end
    'run': begin
                if ~keyword_set(dstep) then dstep=1
                rintens=intens
                if keyword_set(smooth) then rintens=smooth(intens,smthsz,/edge_truncate)
                if keyword_set(gsmooth) then begin
                    for k=0,ncut-1 do $
                        rintens[*,*,k]=gauss_smooth(intens[*,*,k],smthsz,/edge_truncate)
                    
                endif
                if keyword_set(ratio) then rintens[0:dstep,*,*]=1. else rintens[0:dstep,*,*]=0.
                for k=0,ncut-1 do begin
                    rintenstmp=rintens
                    if keyword_set(ratio) then $ 
                        for i=dstep,ntim-1 do rintens[i,*,k]=rintenstmp[i,*,k]/rintenstmp[i-dstep,*,k] $
                    else $
                        for i=dstep,ntim-1 do $
                            rintens[i,*,k]=rintenstmp[i,*,k]-rintenstmp[i-dstep,*,k]
                endfor
                rintensplt=rintens
                r_lim=rminmax
            end
    else: begin
        print, 'Please use "inten", "base", or "run"'
        end
endcase
end
