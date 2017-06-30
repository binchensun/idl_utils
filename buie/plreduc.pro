;+
; NAME:
;  plreduc
; PURPOSE:
;  Computes the standard magnitude of Pluto for each image per observing night.
; DESCRIPTION:
;  
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  plreduc
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURES:
;  
; MODIFICATION HISTORY:
;  2012/01/17 Written by Erin R. George, Southwest Research Institute
;  2012/03/29, MWB, added NOSAVE and ONEFILE keywords
;-
pro plreduc,NOSAVE=nosave,ONEFILE=onefile

   self='plreduc: '
   if badpar(nosave,[0,1,2,3],0,caller=self+'(NOSAVE) ',default=0) then return
   if badpar(onefile,[0,7],0,caller=self+'(ONEFILE) ',default='') then return

   maxsig=50000.0
   border=20

   ; Read in the tfile
   rdtfile,object,tempobj,marker,fnstack,outsize,degree,error,obscode=obscode
   if error ne 0 then return

   if obscode eq '688' then begin
      keyfile='/net/frakir/raid/buie/Reduced/roboccd.key' 
   endif else if obscode eq '705' then begin
      keyfile='/net/frakir/raid/buie/Reduced/nmsu.key'
   endif else if obscode eq 'E10' then begin
      keyfile='/net/frakir/raid/buie/Reduced/lco_fs01.key'
   endif else begin
      print,self,'Unrecognized observatory code'
      return
   endelse

   loadkeys,keyfile,hdrlist,FOUNDIT=foundkey
   if not foundkey then begin
      print,self,'Keylist ',keylist,' could not be loaded. Aborting.'
      return
   endif

   ang=findgen(101)/50.0*!pi
   xcirc=sin(ang)
   ycirc=cos(ang)

   if obscode eq '688' then begin
      ; Load the reduction information
      infofile='reduc.inf'
      if not exists(infofile) then begin
         print,self,'Error: reduc.inf does not exist for this night.'
         return
      endif
      rdreduc,infofile,instrument,ddir,rundate,rad,sky1,sky2,gain,rdnoise
      ddir=addslash(ddir)
      dir=addslash(ddir+rundate)
      imdir=dir+'diff/'
   endif else if obscode eq '705' then begin
      instrument='HoltzCAM'
      ddir='/net/amber/raid1/buie/rawfits/nmsu/'
      cd,'.',current=cwd
      words=strsplit(cwd,'/',/extract)
      l=n_elements(words)-1
      if words[l] eq 'nmsu' then rundate=words[l-1] else rundate=words[l]
      imagedate = rundate
      ddir=ddir+imagedate+'/'
      imdir=ddir+'cal/diff/'
      sky1=25.0
      sky2=130.0
      gain=1.9
      rdnoise=6.0
   endif else if obscode eq 'E10' then begin
      instrument='Spectral'
      ddir='/net/amber/raid1/buie/rawfits/lco/'
      cd,'.',current=cwd
      words=strsplit(cwd,'/',/extract)
      l=n_elements(words)-1
      if words[l] eq 'lco' then rundate=words[l-1] else rundate=words[l]
      imagedate = '20'+rundate
      ddir=ddir+imagedate+'/'
      imdir=ddir+'diff/'
      sky1=25.0
      sky2=130.0
      gain=5.2
      rdnoise=8.2
   endif
   refid=rundate+'-'+instrument

   ; Getting utdate for database
   yr=strmid(rundate,0,2)
   if yr gt 89 then begin
      year='19'+yr
   endif else begin
      year='20'+yr
   endelse
   month=strmid(rundate,2,2)
   day=strmid(rundate,4,2)
   utdate=year+'-'+month+'-'+day

   ; Filters
   filters=['B','V']
   colfil=filters[0]+'-'+filters[1]

   if obscode eq '688' then begin
      openmysql,dblun,'roboccd'
      cmd=['select image.filename,jdmid', $
           'from image,instrument where', $
           'image.filename=instrument.filename', $
           'and object='+quote(object), $
           'and image.filename like '+quote(rundate+'%'), $
           'and (filtname2='+quote(filters[0]), $
           'or filtname2='+quote(filters[1])+')', $
           'order by jdmid;']
      mysqlquery,dblun,cmd,filenames,jd,format='a,d',ngood=nfiles
      free_lun,dblun
   endif else if obscode eq '705' then begin
      filenames=file_search(imdir+'interp.*',count=nfiles)
      if nfiles eq 0 then begin
         print,'No interp/difference files found for ',imdir
         return
      endif
      filenames=strmid(filenames,strlen(imdir))
      ; get the times
      jd=dblarr(nfiles)
      for i=0,nfiles-1 do begin
         hdr=headfits(imdir+filenames[i])
         parsekey,hdr,hdrlist,hdrinfo
         jd[i]=hdrinfo.jd
      endfor
   endif else if obscode eq 'E10' then begin
      filenames=file_search(imdir+'interp.*',count=nfiles)
      if nfiles eq 0 then begin
         print,'No interp/difference files found for ',imdir
         return
      endif
      filenames=strmid(filenames,strlen(imdir))
      ; get the times
      jd=dblarr(nfiles)
      for i=0,nfiles-1 do begin
         hdr=headfits(imdir+filenames[i])
         parsekey,hdr,hdrlist,hdrinfo
         jd[i]=hdrinfo.jd
      endfor
   endif

   if exists('badframes') then $
      readcol,'badframes',fnbad,format='a' $
   else fnbad=''

   if not exists('Res') then file_mkdir,'Res'

   ; scan through and tag these images as good or bad
   badfr=bytarr(nfiles)
   for i=0,nfiles-1 do begin
      z=where(filenames[i] eq fnbad,count)
      if count ne 0 then begin
         print,'Image ',filenames[i],' has been marked bad, skipping.'
         badfr[i] = 1B
      endif
   endfor

   ; Remove the bad images from further consideration
   z=where(badfr eq 0B,count)
   if count eq 0 then begin
      print,'Apparently all the images have been marked bad.  Unable to proceed.'
      return
   endif
   filenames=filenames[z]
   jd=jd[z]
   nfiles=n_elements(z)

   ; compute the ephemeris position of Pluto
   ephem,jd,688,2,'P9',eph
   ephra=trimrank(eph[0,*])
   ephdec=trimrank(eph[1,*])

   if obscode eq '688' then begin
      num=intarr(nfiles)
      for ii=0,nfiles-1 do begin
         parts=strsplit(filenames[ii],'.',/extract)
         num[ii]=fix(parts[1])
      endfor
      imname='interp.'+string(num,format='(i4.4)')
      diffname='diff.'+marker+'.'+string(num,format='(i4.4)')
   endif else if obscode eq '705' then begin
      suffix=strmid(filenames,6)
      imname='interp'+suffix
      diffname='diff.'+marker+suffix
   endif else if obscode eq 'E10' then begin
      suffix=strmid(filenames,6)
      imname='interp'+suffix
      diffname='diff.'+marker+suffix
   endif

   ; First image of series
   hdr1=headfits(imdir+imname[0])
   x1=sxpar(hdr1,'naxis1')
   y1=sxpar(hdr1,'naxis2')
   astinfo,hdr1,info1
   xrange1=[0,x1]
   yrange1=[0,y1]
   astcvt,'xy',xrange1,yrange1,info1,'rd',r1,d1
   if obscode eq 'E10' or obscode eq '705' then $
      rad=sxpar(hdr1,'OBJRAD')

   ; Last image of series
   hdr2=headfits(imdir+imname[-1])
   x2=sxpar(hdr2,'naxis1')
   y2=sxpar(hdr2,'naxis2')
   astinfo,hdr2,info2
   xrange2=[0,x2]
   yrange2=[0,y2]
   astcvt,'xy',xrange2,yrange2,info2,'rd',r2,d2

   ; Build the ra and dec range
   r1=minmax(r1)
   r2=minmax(r2)
   ra=minmax([r1,r2])
   dec=minmax([d1,d2])

   ; Stage 1 and Stage 2 stars
   stageselect,ra[0],ra[1],dec[0],dec[1], $
               sra,sdec,srasig,sdecsig, $
               sb,sbsig,sv,svsig,scol,scolsig
   nstds=n_elements(sra)
   print,strn(nstds),' standard stars loaded.'

   goodfil=['B','V','B','V','B','V']
   otherfil=['B','V','Bessell-B','Bessell-V','6','8']

   ; Loop through the images
   plimag0=fltarr(nfiles)
   plierr0=fltarr(nfiles)
   plimag=fltarr(nfiles)
   plierr=fltarr(nfiles)
   pra=dblarr(nfiles)
   pdec=dblarr(nfiles)
   filter=strarr(nfiles)
   zp=fltarr(nfiles)
   zperr=fltarr(nfiles)
   ct=fltarr(nfiles)
   cterr=fltarr(nfiles)
   cf=fltarr(nfiles)
   cferr=fltarr(nfiles)
   apc=fltarr(nfiles) ; a posteriori error correction
   medfwhm=fltarr(nfiles)
   for ii=0,nfiles-1 do begin

      if onefile ne '' and onefile ne imname[ii] then continue

      print,'Read ',imname[ii],' and ',diffname[ii]

      ; Read images
      image=readfits(imdir+imname[ii],hdr)
      imdiff=readfits(imdir+diffname[ii],hdrdiff)

      ; supporting data, same for both images
      parsekey,hdr,hdrlist,hdrinfo
      astinfo,hdr,info
      nx=sxpar(hdr,'NAXIS1')
      ny=sxpar(hdr,'NAXIS2')
      objrad=sxpar(hdr,'IMGFWHM')*1.2
      z=where(hdrinfo.filter eq otherfil,count)
      if count eq 0 then begin
         print,'Filter [',hdrinfo.filter,'] from header is not recognized.'
         return
      endif
      filter[ii]=goodfil[z[0]]

      print,'objrad: ',objrad,'  Filter ',filter[ii]

      ; Compute the position of Pluto on this image from its ephemeris
      astcvt,'rd',ephra[ii],ephdec[ii],info,'xy',ephx,ephy

      ; Measure Pluto on the difference image, this gives us the instrumental
      ;   magnitude we want as well as the position
      basphote,gain,imdiff,hdrinfo.exptime,ephx,ephy,objrad,sky1,sky2, $
               RDNOISE=rdnoise,MAG=plmag1,ERR=plerr1,xcen=px,ycen=py, $
               /nolog,/silent,boxmrad=3,max=plmax,skymean=plsky, $
               skyerr=plskyerr,fwhm=dplfwhm
      plimag[ii]=plmag1
      plierr[ii]=plerr1

      ; Sanity check on data quality here.
      if plmax lt plsky+5.0*plskyerr or plerr1 gt 0.1 or $
                                        plmag1 gt 80.0 then begin
         showsrc,image,window=0
         oplot,objrad*xcirc+px,objrad*ycirc+py,color='0000ff'xl
         print,'The Pluto signal is very low on this image.  Most likely this image needs to'
         print,'be marked as bad.'
         print,'Peak signal is ',plmax,' counts'
         print,'Sky signal is ',plsky,' +/-',plskyerr
         print,'Instrumental magnitude is ',plmag1,' +/-',plerr1
         return
      endif

      ; Generate astrometric position and save
      astcvt,'xy',px,py,info,'rd',ra,dec
      pra[ii]=ra
      pdec[ii]=dec

      ; Instrumental magnitude of Pluto from interp image, this is just for
      ;    some data sanity checks.
      basphote,gain,image,hdrinfo.exptime,px,py,objrad,sky1,sky2, $
               /exact,RDNOISE=rdnoise,MAG=plmag0,ERR=plerr0, $
               /nolog,/silent,boxmrad=2,fwhm=iplfwhm
      plimag0[ii]=plmag0
      plierr0[ii]=plerr0

      print,'diff',plmag1,plerr1,px,py, $
            'interp',plmag0,plerr0,plmag0-plmag1, $
         format='(a,1x,f6.3,1x,f5.3,2(1x,f5.1),1x,a,1x,f6.3,1x,f5.3,2x,f6.3)'

      ; Now let's take care of the stars.  First compute their positions on
      ;   the current image.
      astcvt,'rd',sra,sdec,info,'xy',sx,sy

      ; Interp image magnitude extraction for standards
      basphote,gain,image,hdrinfo.exptime,sx,sy,objrad,sky1,sky2, $
               RDNOISE=rdnoise,MAG=inst,ERR=instsig,XCEN=sxcen,YCEN=sycen, $
               /nolog,/silent,max=smax,boxmrad=2,skymean=skymean, $
               skyerr=skyerr,fwhm=sfwhm

      ; Now we need to filter out stars that may not be a good choice
      bad=bytarr(nstds)

      ; saturated stars
      z=where(smax gt maxsig,count)
      if count gt 0 then bad[z]=1
      ; skip those that are too faint
      z=where(inst gt 80.0,count)
      if count gt 0 then bad[z]=1
      z=where(smax lt skymean+5.0*skyerr,count)
      if count gt 0 then bad[z]=1
      ; skip those with large errors
      z=where(instsig gt 0.05,count)
      if count gt 0 then bad[z]=1
      ; skip standards with extreme color
;      z=where(scol gt 1.2 or scol lt 0.3,count)
;      if count gt 0 then bad[z]=1
      ; skip those too close to the edge or off the array
      z=where(sx lt objrad+border or sy lt objrad+border or $
              sx gt nx-objrad-border or sy gt ny-objrad-border,count)
      if count gt 0 then bad[z]=1
      ; skip those too close to Pluto
      plsep=sqrt((sx-px)^2+(sy-py)^2)
      z=where(plsep lt 3.0*objrad,count)
      if count gt 0 then bad[z]=1

      zg=where(bad eq 0,countg)
      if countg eq 0 then begin
         print,'There are no good standards on this image.'
         return
      endif

      ; Solve for Zero Point
      if filter[ii] eq filters[0] then begin
         dmag = sb-inst
         derr = sqrt(instsig^2+sbsig^2)
      endif else begin
         dmag = sv-inst
         derr = sqrt(instsig^2+svsig^2)
      endelse

      pass=0
      csq=1.0
      final=0
      repeat begin
         zg=where(bad eq 0,countg)
         medsfwhm=median(sfwhm[zg])
         totalbadold=long(total(bad))
         ; Fit a simplified photometric transformation to the data
         nterms=3
         ind = fltarr(nterms,countg) ; Independent variables.
         ind[0,*] = 1.0
         ind[1,*] = sfwhm[zg]-medsfwhm
         ind[2,*] = scol[zg]
;         ind[3,*] = sfwhm[zg]^2
         ; predefine variables for mysvdfit
         yfit=1
         var=1
         chisq=1
         sing=1
         ; do the fit
         if final gt 0 then weight=1/(derr[zg]*csq) else weight=1/derr[zg]
         coeff = mysvdfit(ind,dmag[zg],1,weight=weight, $
                          yfit=yfit,var=var,chisq=chisq,sing=sing)
         sigma = sqrt(var)
;         sigma = sqrt(var/float(countg-nterms))
         redchi = chisq/(float(countg-nterms))
         if final eq 0 then csq = sqrt(redchi)
         fmt='(2x,a,1x,f7.4,1x,a,1x,f6.4)'
         print,'Pass ',strn(pass),', ',strn(countg),' stars'
         print,'Zp',coeff[0],'+/-',sigma[0],format=fmt
         print,'Fw',coeff[1],'+/-',sigma[1],format=fmt
;         print,'F2',coeff[3],'+/-',sigma[3],format=fmt
         print,'Ct',coeff[2],'+/-',sigma[2],format=fmt
         print,'Chisq',chisq,'RedChi',redchi,'sigcorr',csq, $
               'Scatter',mean(abs(dmag[zg]-yfit)), $
               format='(2x,a,1x,f7.1,2x,a,1x,f6.2,2x,a,1x,f6.3,2x,a,1x,f5.3)'

;         dcalc = coeff[0] + coeff[1]*sfwhm + coeff[2]*scol + coeff[2]*sfwhm^2
         dcalc = coeff[0] + coeff[1]*(sfwhm-medsfwhm) + coeff[2]*scol
         sigerr = (dmag-dcalc)/derr
         zz=where(abs(sigerr) gt 3.0 and bad eq 0,countzz)
         if countzz ne 0 then bad[zz]=1
         totalbadnew=long(total(bad))
         badchange=totalbadnew-totalbadold
         if badchange eq 0 then final++ else final=0
         pass++
      endrep until final gt 1 or pass eq 5

      zp[ii]=coeff[0]
      zperr[ii]=sigma[0]
      cf[ii]=coeff[1]
      cferr[ii]=sigma[1]
      ct[ii]=coeff[2]
      cterr[ii]=sigma[2]
      apc[ii]=csq
      medfwhm[ii]=medsfwhm

      zg=where(bad eq 0,countg)
      print,strn(countg),' good standards on this image.'

;      robomean,dmag,3.0,0.5,bad=bad

      if onefile ne '' then begin
         setwin,2
         ploterror,sxcen[zg],dmag[zg],derr[zg],psym=8, $
            xtitle='X position',ytitle='Mag-mag',title=onefile
         setwin,3
         ploterror,sycen[zg],dmag[zg],derr[zg],psym=8, $
            xtitle='Y position',ytitle='Mag-mag',title=onefile
         setwin,4
;         ploterror,scol[zg],dmag[zg]-coeff[1]*sfwhm[zg]-coeff[3]*sfwhm[zg]^2,derr[zg],psym=8, $
         ploterror,scol[zg],dmag[zg]-coeff[1]*sfwhm[zg],derr[zg],psym=8, $
            xtitle='(B-V)',ytitle='Mag-mag',title=onefile
         setwin,5
         ploterror,sfwhm[zg],dmag[zg]-coeff[2]*scol[zg],derr[zg],psym=8, $
            xtitle='FWHM',ytitle='Mag-mag',title=onefile
         setwin,6
         plot,sxcen[zg],sfwhm[zg],psym=4, $
            xtitle='X position',ytitle='FWHM',title=onefile
         setwin,7
         plot,sycen[zg],sfwhm[zg],psym=4, $
            xtitle='Y position',ytitle='FWHM',title=onefile
         setwin,8
         ploterror,sfwhm[zg],dmag[zg]-yfit,derr[zg],psym=4, $
            xtitle='FWHM',ytitle='Residuals',title=onefile
      endif

;      meanerr,dmag[zg],derr[zg],zp1,zpm1,zpd1
;      zp[ii]=zp1
;      zperr[ii]=zpm1
;      print,'ZP ',zp1,zpm1,zpd1

      showsrc,image,window=0
      setusym,-1
      oplot,sxcen[zg],sycen[zg],psym=8,symsize=5,color='00ff00'xl
      oplot,objrad*xcirc+px,objrad*ycirc+py,color='0000ff'xl
      oplot,rad*xcirc+px,rad*ycirc+py,color='00afaf'xl
      setusym,1
      xyouts,5,5,imname[ii],/device,color='ffff00'xl,charsize=3
      tvgrab,'Res/'+imname[ii]+'.png',0,/png

      showsrc,imdiff,window=1
      setusym,-1
      oplot,sxcen[zg],sycen[zg],psym=8,symsize=5,color='00ff00'xl
      oplot,objrad*xcirc+px,objrad*ycirc+py,color='0000ff'xl
      setusym,1
      xyouts,5,5,diffname[ii],/device,color='ffff00'xl,charsize=3
      tvgrab,'Res/'+diffname[ii]+'.png',1,/png

      jdstr,jd[ii],0,jds
      rastr,ra,3,ras
      decstr,dec,2,decs
      print,jds,' ',ras,' ',decs,'  X=',hdrinfo.airmass

   endfor

   if onefile ne '' then return

   zb=where(filter eq 'B',countb)
   zv=where(filter eq 'V',countv)

   time=(jd-(double(long(jd-0.5d0))+0.5d0))*24.0

   ; Need to calibrate Pluto now, but, this cannot assume knowlege of the
   ;   color (but we've got a good starting point).
   plbmv=0.9 ; starting guess

   pass=0
   repeat begin
      plmag = plimag + zp + cf*(dplfwhm-medfwhm) + ct*plbmv
      plerr = sqrt( (plierr*apc)^2 + zperr^2 + $
                    (cferr*(dplfwhm-medfwhm))^2 + (cterr*plbmv)^2 )
;      plerr = sqrt( (plierr*apc)^2 + $
;                    (cferr*(dplfwhm-medfwhm))^2 + (cterr*plbmv)^2 )
      interp,time[zv],plmag[zv],e1=plerr[zv],time[zb],v_plmag,e_plmag

      bmvall = plmag[zb]-v_plmag
      bmvsigall = sqrt(plerr[zb]^2-e_plmag^2)
      newplbmv = mean(bmvall)
      done = abs(plbmv-newplbmv) < 0.0005
      plbmv=newplbmv
      pass++
      print,pass,plbmv
   endrep until done or pass eq 10

   magcorr=plimag0-plimag

   xtitle='UT time'

   setwin,2,xsize=512,ysize=512
   plot,[0],psym=8,/nodata, $
      xrange=minmax(time),yrange=minmax([zp+zperr,zp-zperr]), $
      xtitle=xtitle,ytitle='Zero-point',title=utdate
   oploterror,time[zb],zp[zb],zperr[zb],psym=8, $
      color='ff2020'xl,errcolor='ff4040'xl
   oploterror,time[zv],zp[zv],zperr[zv],psym=8, $
      color='20ff20'xl,errcolor='40ff40'xl
   tvgrab,'Res/zp.png',2,/png

   setwin,3,xsize=512,ysize=512
   ploterror,time[zb],plmag[zb],plerr[zb],psym=8, $
      yr=maxmin([plmag[zb]+plerr[zb],plmag[zb]-plerr[zb]]), $
      xtitle=xtitle,ytitle='B mag',title=utdate
   tvgrab,'Res/Bmag.png',3,/png

   setwin,4,xsize=512,ysize=512
   ploterror,time[zv],plmag[zv],plerr[zv],psym=8, $
      yr=maxmin([plmag[zv]+plerr[zv],plmag[zv]-plerr[zv]]), $
      xtitle=xtitle,ytitle='V mag',title=utdate
   tvgrab,'Res/Vmag.png',4,/png

   setwin,5,xsize=512,ysize=512
   ploterror,time,magcorr,plierr,psym=8, $
      xtitle=xtitle,ytitle='mag - diff mag',title=utdate
   tvgrab,'Res/magdiff.png',5,/png

   interp,time[zv],plmag[zv],e1=plerr[zv],time[zb],v_plmag,e_plmag

   bmvall = plmag[zb]-v_plmag
   bmvsigall = sqrt(plerr[zb]^2+e_plmag^2)

print,time[zb]
print,plmag[zv]
print,plerr[zv]
print,plmag[zb]
print,plerr[zb]
print,v_plmag
print,e_plmag
print,bmvall
print,bmvsigall
print,'---'
print,zperr
print,'---'
print,cferr*(dplfwhm-medfwhm)
print,'---'
print,cterr*plbmv
print,'---'
   setwin,6,xsize=512,ysize=512
   ploterror,time[zb],bmvall,bmvsigall,psym=8, $
      xtitle=xtitle,ytitle='(B-V)',title=utdate

   robomean,bmvall,3.0,0.5,bmv,stdmean=bmvsig
   fmt='(f10.3)'
   str='(B-V) '+strn(bmv,format=fmt)+'+/-'+strn(bmvsig,format=fmt)
   print,str
   xyouts,5,5,str,/device,color='2060ff'xl

   tvgrab,'Res/bmv.png',6,/png

   setwin,7,xsize=1000,ysize=500
   raoff = (pra-ephra[0])*cos(ephdec)
   decoff = (pdec-ephdec[0])
   raoff = raoff*180.0d0/!dpi*3600.0d0
   decoff = decoff*180.0d0/!dpi*3600.0d0
   raeoff = (ephra-ephra[0])*cos(ephdec)
   deceoff = (ephdec-ephdec[0])
   raeoff = raeoff*180.0d0/!dpi*3600.0d0
   deceoff = deceoff*180.0d0/!dpi*3600.0d0
   plot,raoff,decoff,psym=4,/iso, $
      xrange=maxmin([raoff,raeoff]), $
      yrange=maxmin([decoff,deceoff]), $
      xtitle='(E)    delta-RA     (W)', $
      ytitle='(S)    delta-Dec    (N)', $
      title=utdate
   oplot,raeoff,deceoff,psym=5,color='0000ff'xl

   raoff = (pra-ephra)*cos(ephdec)
   decoff = (pdec-ephdec)
   raoff = raoff*180.0d0/!dpi*3600.0d0
   decoff = decoff*180.0d0/!dpi*3600.0d0
   robomean,raoff,3.0,0.5,raerr,stdmean=raerr_sig
   robomean,decoff,3.0,0.5,decerr,stdmean=decerr_sig
   fmt='(f10.1)'
   str1='Mean ra  offset '+strn(raerr*1000.0,format=fmt)+ $
                   ' +/- '+strn(raerr_sig*1000.0,format=fmt)+' mas'
   str2='Mean dec offset '+strn(decerr*1000.0,format=fmt)+ $
                   ' +/- '+strn(decerr_sig*1000.0,format=fmt)+' mas'
   print,str1
   print,str2

   xyouts,30,480,str1,charsize=2,color='2060ff'xl,/device
   xyouts,30,450,str2,charsize=2,color='2060ff'xl,/device

   tvgrab,'Res/ast.png',7,/png

   openmysql,dblun,'phot'
   ; Delete statement for old data
   cmd=['delete from data', $
        'where refid='+quote(refid), $
        'and objname='+quote('p9')+';']
   if not nosave then mysqlcmd,dblun,cmd

   ; Writing into the database
   c=','
   for i=0,nfiles-1 do begin
      cmd=['insert into data values', $
           '('+quote(refid)+c, $               ; refid: rundate - instrument
           quote('p9')+c, $                    ; objname
           string(jd[i],format='(f13.5)')+c, $    ; jd
           string(pra[i],format='(f11.9)')+c, $ ; ra
           string(pdec[i],format='(f12.9)')+c, $ ; decl
           quote(filter[i]) +c, $               ; filter
           quote(colfil)+c, $                  ; color: B-V
           string(plmag[i], format='(f8.5)')+c, $ ; standard mag
           string(plerr[i], format='(f7.5)')+',0);']; standard mag error
      if not nosave then mysqlcmd,dblun,cmd else print,cmd
   endfor

   free_lun,dblun

   if nosave then print,'WARNING!  Save to database was disabled.'

end


