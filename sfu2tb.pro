function sfu2tb,flux,freq,bmaj,bmin
;frequency in Hz
;flux in sfu
;bmaj: half-power beam width along the major axis in arcsec
;bmin: half-power beam width along the minor axis in arcsec
sfu2cgs=1d-19
vc=2.998d10
kb=1.38065d-16
rad2asec=180d0/!dpi*3600d0
sr=!dpi*(bmaj/rad2asec)*(bmin/rad2asec)/(4.*alog(2d0))
Tb=flux*sfu2cgs*vc^2d0/(2d0*kb*freq^2d0*sr)
return,Tb
end
