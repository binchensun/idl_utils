pro read_casa_dspec, specfile, dim, $
    specsav=specsav, bkgsub=bkgsub, bkgind=bkgind
;PURPOSE: read dat files from the output of my python script vspec_mtim.wrt_vspec or dspec.wrt_dspec 
;         to retrieve the vector dynamic spectra
;REQUIRED INPUTS:  specfile: dat file from the output of my python script vspec_mtim.wrt_vspec or dspec.wrt_spec
;                  dim: four element array, format is in 
;                         [# of spectral channels, # of time pixels, # of polarizations, # of spatial regions]
;OPTIONAL INPUTS:  specsav: output IDL sav file containing the dynamic spectral data
;                  bkgsub: do you wish to do a background subtraction? 
;                          the background is obtained from an empty, emission-free spatial region
;                  bkgind: index of the background spatial region (default is 0)
if ~keyword_set(specsav) then begin
    len=strlen(specfile)
    i=strpos(specfile,'.dat',/reverse_search)
    if keyword_set(bkgsub) then specsav=strmid(specfile,0,i)+'.bkgsub.sav' $
            else specsav=strmid(specfile,0,i)+'.sav'
endif
if ~keyword_set(bkgsub) then bkgsub=0
if keyword_set(bkgsub) and ~keyword_set(bkgind) then bkgind=0
openr,lun,specfile,/get_lun
nfreq = dim[0] & ntim = dim[1] & npol = dim[2] & nreg = dim[3]
freqs = fltarr(nfreq) ;freqs in MHz
tims = dblarr(ntim)
flux = fltarr(nreg, npol, ntim, nfreq) ;flux is usually in Jy/beam, order of indices is reversed from Python
readu, lun, freqs, tims, flux
close, lun
;check if it is in seconds since the beginning of the day or mjd seconds
if max(tims,/nan) lt 86400d then begin
    print,'Input time is in seconds from the beginning of the day'
    date=''
    read, date, prompt='Enter Date (yyyy-mm-dd):'
    tims=anytim(date+'T00:00')+tims
endif
if max(tims,/nan) gt anytim('2049-12-31T00:00') then begin
    print,'Input time is probably in mjd seconds'
    tims=mjd2anytim(tims/86400d)
endif
if keyword_set(bkgsub) then begin
    bkg = flux[bkgind, *, *, *]
    flux2=fltarr(nreg-1, npol, ntim, nfreq)
    n2=0
    for n=0,nreg-1 do begin
        if n ne bkgind then begin
            flux2[n2,*,*,*] = flux[n,*,*,*] - bkg
            n2=n2+1
        endif
    endfor
    flux=flux2
endif
save, file=specsav,flux,freqs,tims
end
