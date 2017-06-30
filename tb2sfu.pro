pro tb2sfu,freq,tb,sz,flux
;frequency in Hz
;flux in sfu
;size: angular width of the radio source, [major,minor],in arcsec
if n_elements(sz) eq 1 then sz=replicate(sz,2)
sfu2cgs=1d-19
vc=2.998d10
kb=1.38065d-16
sr=!dpi*(sz[0]/206265d0/2d0)*(sz[1]/206265d0/2d0)
;sr=!dpi*(size/206265d0/2d0)^2d0
flux=2d0*kb*tb*freq^2d0/vc^2d0*sr/sfu2cgs
print,'Brightness temperature in K', tb
print,'Frequency in Hz',freq
print,'Source size in arcsec (diameter)', sz
print,'flux in sfu: ',flux
return
end
