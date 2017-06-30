pro sfu2tb,freq,flux,size,Tb
;frequency in Hz
;flux in sfu
;size: width of the radio source, [major,minor],in arcsec
sfu2cgs=1d-19
vc=2.998d10
kb=1.38065d-16
sr=!dpi*(size[0]/206265d0/2d0)*(size[1]/206265d0/2d0)
;sr=!dpi*(size/206265d0/2d0)^2d0
Tb=flux*sfu2cgs*vc^2d0/(2d0*kb*freq^2d0*sr)
return
end
