pro aia_map_coalign_test,mapset,plot_dir,io,ct,$
    nsig,nsm,h,dx,dy,km=km
;+
; Project     : AIA/SDO
;
; Name        : AIA_IMAGES
;
; Category    : Data analysis   
;
; Explanation : displays a set AIA images (6 wavelengths)
;		with overlay of optical limb given in FITS header
;		and produces multi-panel display in ps-file
;
; Syntax      : IDL>aia_coalign_test,mapset,wave_,io,ct,nsig,nsm
;
; Inputs      : fileset  = common filename part of 6 wavelength FITS images
;		wave_	 = strarr(6) with wavelengths in Angstroem
;		io	 = 0=screen, 1=ps, 2=eps, 3=color ps
;		ct	 = color table
;		nsig     = contrast of image (standard deviations from mean)
;               nsm	 = smoothing of limb profile
;
; Outputs     : h	 = FLTARR(4,6) heights of chromosphere [arcsec or km]
;			   in 6 wavelengths and 4 limb positions (E,W,N,S)
;		dx	 = coalignment mean and standard deviation in EW
;		dy	 = coalignment mean and standard deviation in SN
;	        postscript file <plotname>_coalign_test_col.ps (if io=3)
;
; History     : 22-Dec-2014, Version 1 written by Bin Chen
;             : based on aia_coalign_test.pro by Markus J. Aschwanden version 1 (3-Mar-2011)
;
; Contact     : bin.chen@cfa.harvard.edu
;-

;____________________FILENAMES_________________________________________
nmap	=n_elements(mapset)
nwave = nmap
wave_=strarr(6)
h	=fltarr(4,6)
nfov	=4
nx	=128
ny	=128


;____________________DISPLAY____________________________________________
form   =1       ;0=landscape, 1=portrait
char   =0.7     ;character size
fignr  =''
if ~keyword_set(plot_dir) then plot_dir='./'
plotname=plot_dir+'aia_map_coalign_test'
ref     =''     ;label at bottom of Fig.
unit    =0      ;window number
fig_open,io,form,char,fignr,plotname,unit
loadct,ct

for iw=0,nwave-1 do begin
    map=mapset[iw]
    data=map.data
    wave=strmid(map.id,2,3,/reverse)
    wave_[iw]=wave
    print,'wave: ', wave
    sz=size(data)
    naxis1=sz[1]
    naxis2=sz[2]
    cdelt1=map.dx
    cdelt2=map.dy
    crpix1=(naxis1+1.)/2.-map.xc/cdelt1
    crpix2=(naxis2+1.)/2.-map.yc/cdelt2
    rsun  =map.rsun
    ;dsun  =rsun*2.
    rpix  =rsun/cdelt1
    ;arcsec=2.*!pi*dsun/(1.e3*360.*60.*60.)	;1 arcsec in km 
    au=149597870d0 ; in km
    arcsec=au*1d0/3600d0*!dtor
    for ip=0,nfov-1 do begin
        if (ip eq 0) then begin &x0=crpix1-rpix &y0=crpix1      &endif
        if (ip eq 1) then begin &x0=crpix1+rpix &y0=crpix1      &endif
        if (ip eq 2) then begin &x0=crpix1      &y0=crpix2-rpix &endif
        if (ip eq 3) then begin &x0=crpix1      &y0=crpix2+rpix &endif
        if (ip eq 0) then !p.title='East limb, '+wave+' A'
        if (ip eq 1) then !p.title='West limb, '+wave+' A'
        if (ip eq 2) then !p.title='South pole, '+wave+' A'
        if (ip eq 3) then !p.title='North pole, '+wave+' A'
        i1	=long(x0-nx/2)
        i2	=long(x0+nx/2)
        j1	=long(y0-ny/2)
        j2	=long(y0+ny/2)
        nx	=i2-i1+1
        ny	=j2-j1+1
        image =data(i1:i2,j1:j2)

        dx   =0.9/float(4)
        x1_  =0.05+dx*ip
        x2_  =x1_+dx*0.8
        y2_  =0.95-0.15*iw
        y1_  =y2_-0.12

        !p.position=[x1_,y1_,x2_,y2_]
        !x.range=[i1,i2]
        !y.range=[j1,j2]
        !x.style=1
        !y.style=1
        !x.ticks=4
        !y.ticks=4
        statistic,image,zm,zd
        if (io eq 0) then begin
            nxw   =long(!d.x_vsize*(x2_-x1_)+0.5)
            nyw   =long(!d.y_vsize*(y2_-y1_)+0.5)
            z     =congrid(image,nxw,nyw)
        endif
        if (io ne 0) then z=image
        plot,[0,0],[0,0]
        tv,bytscl(z,min=zm-zd*nsig,max=zm+zd*nsig),x1_,y1_,xsize=(x2_-x1_),ysize=(y2_-y1_),/normal
        xyouts,nfov/20,nfov/20,wave+' A',size=1.5,color=255
        plot_circle,crpix1,crpix2,rpix,3,-1,255,2
        !noeras=1

        ;limbfit
        prof	=fltarr(nx)
        i3	=nx/2-20
        i4	=nx/2+20
        if (ip le 1) then begin
            for j=0,nx-1 do prof=prof+image(*,j)
            prof=smooth(prof,nsm)
            x	=i1+findgen(nx)
            y	=j1+(j2-j1)*(prof-min(prof))/(max(prof)-min(prof))
            oplot,x,y,color=255,thick=3
            oplot,[i1,i2],y0*[1,1],color=255
            dp_max=max(abs(prof(i3:i4)-prof(i3-1:i4-1)),im)
            xm	=(x(i3+im)+x(i3+im+1))/2.
            oplot,xm*[1,1],[j1,j2],color=255
            h_pix=abs(xm-crpix1)-rpix
            h_arcsec =h_pix*cdelt1
            h_km =long(((h_pix*cdelt1)*arcsec)/100.)*100.
            if ~keyword_set(km) then begin
                xyouts_norm,0.0,0.9,string(h_arcsec,'(f4.1)')+' "',1,0,255
                h(ip,iw)=h_arcsec
            endif else begin
                xyouts_norm,0.0,0.9,string(h_km,'(I5)')+' km',1,0,255
                h(ip,iw)=h_km
            endelse
        endif
        if (ip ge 2) then begin
            for i=0,ny-1 do prof=prof+reform(image(i,*))
            prof=smooth(prof,nsm)
            y	=j1+findgen(ny)
            x	=i1+(i2-i1)*(prof-min(prof))/(max(prof)-min(prof))
            oplot,x,y,color=255,thick=3
            oplot,x0*[1,1],[j1,j2],color=255
            dp_max=max(abs(prof(i3:i4)-prof(i3+1:i4+1)),im)
            ym	=(y(i3+im)+y(i3+im+1))/2.
            oplot,[i1,i2],ym*[1,1],color=255
            h_pix=abs(ym-crpix2)-rpix
            h_arcsec =h_pix*cdelt1
            h_km =long(((h_pix*cdelt1)*arcsec)/100.)*100.
            if ~keyword_set(km) then begin
                xyouts_norm,0.0,0.9,string(h_arcsec,'(f4.1)')+' "',1,0,255
                h(ip,iw)=h_arcsec
            endif else begin
                xyouts_norm,0.0,0.9,string(h_km,'(I5)')+' km',1,0,255
                h(ip,iw)=h_km
            endelse
        endif

    endfor
endfor
fig_close,io,fignr,ref

;Statistics of chromospheric heights and coalignment accuracy:
if ~keyword_set(km) then q = cdelt1 else q=arcsec*cdelt1
for iw=0,nwave-1 do begin 
    statistic,h(*,iw),h_avg,h_sig
    if ~keyword_set(km) then $
        print,'Chromospheric height '+wave_(iw)+' : ',h_avg,'+',h_sig>q,' "',$
            format='(a,f4.1,a,f4.1,a)' $
    else $
        print,'Chromospheric height '+wave_(iw)+' : ',h_avg,'+',h_sig>q,' km',$
            format='(a,i6,a,i4,a)'
endfor
statistic,abs(h(*,0)-h(*,1))/(2.*q),dx_avg,dx_sig
statistic,abs(h(*,2)-h(*,3))/(2.*q),dy_avg,dy_sig
dx	=[dx_avg,dx_sig]
dy	=[dy_avg,dy_sig]
print,'Coalignment in x-direction = ',dx(0),'+',dx(1),' pixels',$
format='(a,2(f5.2,a))'
print,'Coalignment in y-direction = ',dy(0),'+',dy(1),' pixels',$
format='(a,2(f5.2,a))'

end
