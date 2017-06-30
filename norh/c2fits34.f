      program c2fits34
c----------------------------------------------------------------------
c   This task will calibrate selected data from the radioheliograph
c   and write the result to disk in FITS format. This version handles
c   34 GHz data. One can turn the amplitude calibration on or off,
c   apply smoothing to the calibration, or apply a single calibration
c   to the data.
c
c   Written by T. Bastian, 16 Oct 1996
c   Subroutines UVCMPRS, PUTEO1, and PUTEO2 written by K. Shibasaki
c   Calls subroutines RDPAR, CALEPH, JST2UT, PUTHDR, RDDAT34, PHSHFT,
c         FI2D, ADDAPA, PHCAL, AMCAL from the helioglib library.
c
c   Updated 99 Aug by S. White due to changes in helioglib routines:
c     - added arg za to CALEPH
c     - JST2UT has changed to require input date as yyyymmdd, not yymmdd
c       and this should be the format now returned by rddat34/suv34 -
c       In fact all dates are now yyyymmdd: variables NDUT and NDJST.
c       Removed all calls to ndut and msut - only needed in PUTHDR
c       where internal conversion JST2UT takes place.
c     - removed arguments ndut, msut and added nstat in call to PUTHDR
c     - removed arguments ndut, msut and added nstat in call to RDDAT34
c     - changed declarations of nantst in suv34 from (1) to (84)
c       for consistency with all other references
c   Modified 00 Apr by S. White: replaced SUV34 with version
c       of SNAPUV34 from helioglib62.f, since original
c       was just renamed versions of those routines from helioglib50.f
c       Changes are replacing call to rddcal34 with call to rdcl34,
c       taking account of interpretation of sign of ical used here.
c       Neither RDCL34(=RDDCAL34) nor UVCMPR needs to be replaced.
c       Other changes determined by comparison of suv17 with snapuv17.
c   Modified 03 Feb by S. White: modified input handling so that it can
c       read 0.1s event data. Steady data use times as inputs that
c       are converted to frame numbers assuming 1 s intervals which
c       doesn't wotrk for event data, so you can now supply frame
c       numbers, triggered if the first time is negative (of start frame
c       number). In this case, all times are assumed to be frame numbers,
c       otherwise they are all assumed to be hhmmss and converted to times.
c       NOTE: still can only do 600 frames at a time.
c----------------------------------------------------------------------
      integer naxes(4),nantst(84),iimg(600)
      integer*2 icald34(168)
      integer*2 ifrac,jfrac
      integer*4 ist,ien,jst,jen
      real flx(84),mapa(512,512),mapb(512,512),pmat(4)
      real cca(57,55),ccb(57,55)
      real crlab(57,55,600,2),drlab(57,55,600)
      character*80 filehd,infpar,infph,outf,outfft,infof,dum1
      character*18 dattyp
      character*3 tst, fixc, amc, dmpc, skpo
c
c   Read inputs from c2fits34.inp
c
      open (unit=12,file='c2fits34.inp',status='old',form='formatted')
      read (12,'(a)') dum1
      read (12,'(a)') dum1
      read (12,'(32x,a80)') filehd
      read (12,'(32x,a80)') infpar
      read (12,'(32x,a80)') infof      
      read (12,'(32x,a80)') infph
      read (12,'(32x,a80)') outf
c      read (12,'(32x,3i2)') ihs, ims, iss
c      read (12,'(32x,3i2)') ihe, ime, ise
      read (12,'(32x,i6)') ist
      read (12,'(32x,i6)') ien
      read (12,'(32x,a3)') tst
      read (12,'(32x,i3)') imavg
      read (12,'(32x,i3)') iminc
      read (12,'(32x,i3)') ibox
      read (12,'(32x,a3)') amc
      read (12,'(32x,a3)') fixc
c      read (12,'(32x,3i2)') jhs, jms, jss
c      read (12,'(32x,3i2)') jhe, jme, jse
      read (12,'(32x,i6)') jst
      read (12,'(32x,i6)') jen
      read (12,'(32x,a3)') dmpc
      read (12,'(32x,a3)') skpo
      close (unit=12)
      iunit=11
c
c   Read parameters of physical ephemeris
c
      call rdpar(iunit,infpar,
     -   dec1,dec2,dec3,ha1,ha2,ha3,
     -   solr1,solr2,solr3,solp1,solp2,solp3,solb1,solb2,solb3,
     -   nantst)
c
c parse the time/frame input to decide what to do
c if first time is negative, assume frame numbers for event files
c

      if (ist.lt.0) then

c
c      Assume "times" are frame numbers
c
         ndfr1 = abs(ist)
         ndfr2 = abs(ien)
         ncfr1 = abs(jst)
         ncfr2 = abs(jen)

      else

c
c      Parse the long integers as hh mm ss
c
         ihs = int(ist/10000)
         ims = int((ist-10000*ihs)/100)
         iss = ist-10000*ihs-100*ims
c         write(6,*) ist,ihs,ims,iss

         ihe = int(ien/10000)
         ime = int((ien-10000*ihe)/100)
         ise = ien-10000*ihe-100*ime
c         write(6,*) ien,ihe,ime,ise

         jhs = int(jst/10000)
         jms = int((jst-10000*jhs)/100)
         jss = jst-10000*jhs-100*jms
c         write(6,*) jst,jhs,jms,jss

         jhe = int(jen/10000)
         jme = int((jen-10000*jhe)/100)
         jse = jen-10000*jhe-100*jme
c         write(6,*) jen,jhe,jme,jse

c
c      Convert times to JST,if necessary
c
         if (tst.eq.'JST') go to 10
         ihs=mod(ihs+9,24)
         ihe=mod(ihs+9,24)
         jhs=mod(ihs+9,24)
         jhe=mod(ihs+9,24)
c
c   Convert JST times to frame numbers
c
 10      call jst2fr(ihs,ims,iss,infof,ndfr1,istat)
         if (istat.ne.0) go to 800
         call jst2fr(ihe,ime,ise,infof,ndfr2,istat)
         if (istat.ne.0) go to 800
         call jst2fr(jhs,jms,jss,infof,ncfr1,istat)
         if (istat.ne.0) go to 800
         call jst2fr(jhe,jme,jse,infof,ncfr2,istat)
         if (istat.ne.0) go to 800

      end if
c
c      Report what we've found so far
c
      write (6,'(//,''The following processing will be performed:'')')
      write (6,'(''Frequency: 34 GHz'')') 
      write (6,'(''Data file: '',a)') filehd(1:48)
      write (6,'(''Ephemeris file: '',a)') infpar(1:48)
      write (6,'(''Tracking info: '',a)') infof(1:48)
      write (6,'(''Phase diff file: '',a)') infph(1:48)
      write (6,'(''Start time '',i2,'':'',i2,'':'',i2,
     &   '' JST, Frame number '',i5)') ihs,ims,iss,ndfr1
      write (6,'(''  End time '',i2,'':'',i2,'':'',i2,
     &   '' JST, Frame number '',i5)') ihe,ime,ise,ndfr2
      write (6,'(''     Image increment: '',i3,'' frames'')') iminc
      write (6,'(''Image averaging time: '',i3,'' frames'')') imavg
      if (fixc.eq.'YES') then 
         write (6,'(''Applying single calibration to the interval'')')
         write (6,'(''Start calibration time '',i2,'':'',i2,'':'',i2,
     &      '' JST, Frame number '',i5)') jhs,jms,jss,ncfr1
         write (6,'(''  End calibration time '',i2,'':'',i2,'':'',i2,
     &      '' JST, Frame number '',i5)') jhe,jme,jse,ncfr2
      else
         write (6,'(''  Smoothing interval: '',i3,
     &      '' frames'')') ibox
         end if
      if (amc.eq.'YES') write (6,
     &   '(''Amplitude calibration has been disabled'',//)')
      if (dmpc.eq.'YES') then 
         write (6,
     &   '(''Dumping calibration solutions to calib.dat'')')
         open (unit=12,file='calib.dat',status='unknown',
     &   form='formatted')
         end if
      if (skpo.eq.'YES') write (6,
     &   '(''No FITS files will be produced'',//)')
c
c   Some bookkeeping
c
      nimg=0
      do 15 i=ndfr1,ndfr2,iminc
         nimg=nimg+1
         iimg(nimg)=i
         if (nimg.eq.600) then
            write (6,'(''Limiting number output frames to 600!'')')
            go to 16
            end if
 15      continue
c
c   Fixed calibration interval?
c
 16      if (fixc.eq.'YES') then
            ical=ncfr2-ncfr1+1
            if (amc.eq.'YES') ical=-iabs(ical)
            call rdcl34(iunit,filehd,ncfr1,ical,nantst,icald34,nframe)
            end if
c
c   Begin major loop
c
      jfrac=10
      do 100 ii=1,nimg
          ifr=iimg(ii)
          ifrac=int(100.*ii/nimg+0.01)
          if (ifrac.ge.jfrac) then 
             write (6,*) '==> ', ifrac,'% of the requested ',
     &          'frames have been processed '
             jfrac=jfrac+10
             end if
          ical=ibox
          if (amc.eq.'YES') ical=-iabs(ical)
          if (fixc.eq.'YES') ical=0
          ifr1=ifr-imavg/2
          ifr2=ifr+imavg/2
          call suv34(iunit,filehd,ifr1,ifr2,
     -       nantst,ical,icald34,ndjst,msjst,
     -       msjsts,msjste,natt,nfalt,nfreq,nstat,flx,
     -       mapa,mapb,nfrmst)
          if (nfrmst.ne.0) go to 100
          if (dmpc.eq.'YES') write (12,*) icald34
          call uvcmpr(mapa,mapb,cca,ccb)
          do 60 l=1,55
             do 60 i=1,57
                crlab(i,l,ii,1)=cca(i,l)
  60            continue    
          do 70 l=1,55
             do 70 i=1,57
                crlab(i,l,ii,2)=ccb(i,l)
  70            continue    
c
c   Start parameters
c 
       if (ii.eq.1) then
          call caleph(dec1,dec2,dec3,ha1,ha2,ha3,
     -       solr1,solr2,solr3,solp1,solp2,solp3,solb1,solb2,solb3,
     -       msjst,dec,ha,solr,solp,solb,az,alt,za,pmat)
          dec11=dec
          ha11=ha
          solr11=solr
          solp11=solp
          solb11=solb
          az11  =az
          alt11 =alt
c          ndut11=ndut
c          msut11=msut
          ndjst11=ndjst
          msjst11=msjst
          ifrm1=iimg(1)
c
c   End parameters
c
       else if (ii.eq.nimg) then
          call caleph(dec1,dec2,dec3,ha1,ha2,ha3,
     -       solr1,solr2,solr3,solp1,solp2,solp3,solb1,solb2,solb3,
     -       msjst,dec,ha,solr,solp,solb,az,alt,za,pmat)
          dec12=dec
          ha12=ha
          solr12=solr
          solp12=solp
          solb12=solb
          az12  =az
          alt12 =alt
c          ndut12=ndut
c          msut12=msut
          ndjst12=ndjst
          msjst12=msjst
          ifrm2=iimg(nimg)
          endif
c
  100 continue
c
c   Skip output if desired
c
      if (skpo.eq.'YES') go to 1000
c
c   Dump data to FITS files
c
      iounit=2
      nbit=-32
      naxis=3
      naxes(1)=57
      naxes(2)=55
      naxes(3)=nimg
c
c   Time in UT
c
      mh1=msjst11/10000000
      mm1=mod(msjst11,10000000)/100000
      ms1=mod(msjst11,100000)
      m1=(mh1*60+mm1)*60000+ms1
      mh2=msjst12/10000000
      mm2=mod(msjst12,10000000)/100000
      ms2=mod(msjst12,100000)
      m2=(mh2*60+mm2)*60000+ms2
      m=(m1+m2)/2
      mh3=m/3600000
      mm3=mod(m,3600000)/60000
      ms3=mod(m,60000)
      msjst=(mh3*100+mm3)*100000+ms3
      ndjst=ndjst11
c      msut=msjst
c      ndut=ndjst
c      call jst2ut(ndut,msut)
c
c   Write files
c
      loutf=index(outf,' ')-1
      do 600 i=1,2
         if(i.eq.1) outfft=outf(1:loutf)//'c.fits'
         if(i.eq.2) outfft=outf(1:loutf)//'s.fits'
         if(i.eq.1) dattyp='uv data(lin,cos)'
         if(i.eq.2) dattyp='uv data(lin,sin)'
         ipol=1-((i-1)/2)*2
         do 500 l=1,nimg
            do 500 k=1,55
               do 500 j=1,57
                  drlab(j,k,l)=crlab(j,k,l,i)
 500              continue
         call ftinit(iounit,outfft,2880,istat)
         if (istat.ne.0) go to 900
         call puthdr(iounit,nbit,naxis,naxes,
     &      ndjst,msjst,msjst11,msjst12,ifrm1,ifrm2,
     &      ipol,natt,nfalt,34,nstat,dattyp)
         call putoe1(iounit,solr11,solp11,solb11,
     &      dec11,ha11,az11,alt11)
         call putoe2(iounit,solr12,solp12,solb12,
     &      dec12,ha12,az12,alt12)
         call ftpdef(iounit,nbit,naxis,naxes,0,0,istat)
         call ftppre(iounit,0,1,
     &      naxes(1)*naxes(2)*naxes(3),drlab,istat)
         call ftclos(iounit,istat)
         if (istat.ne.0) go to 900
         write(6,'('' File '',a40,'' written'')') outfft
 600     continue
      go to 1000
 800  write(6,'('' Bad time range specified - no frame found '')')
 900  write(6,'('' istat ='',i8)') istat
 1000 if (dmpc.eq.'YES') close (unit=12)
      stop
      end
***********************************************************************
*        suv34 - create 2-dimensional uv-map from raw data            *
*                and calibrate                                        *
*                for 34 GHz data                                      *
*       iunit  : input,integer fortran file unit number               *
*                  for raw data file                                  *
*       filehd : input,character*80 raw data filename header          *
*       nfrst  : input,integer  first frame number for integration    *
*       nfrend : input,integer  last frame number for integration     *
*       nantst : input,integer(84) antenna status                     *
*       ical   : input,integer  calibration menu                      *
*         ical=999 : no calibration                                   *
*         ical=0   : use icald as calibration data                    *
*         ical<>0  : calculate calibration data integrating           *
*                    abs(ical) frames                                 *
*                    if ical<0, calculated antenna phases are         *
*                    shifted, using icald as reference data           *
*       icald  : input,integer*2(168) initial calibration data        *
*              : output               obtained calibration data       *
*                if integrate frames, cal data for the last frame     *
*       ndjst  : output,integer observing date in jst (yymmdd)        *
*       msjst  : output,integer center of observing time              *
*                  in jst (hhmmsssss)                                 *
*       msjsts : output,integer start of observing time               *
*                  in jst (hhmmsssss)                                 *
*       msjste : output,integer end of observing time                 *
*                  in jst (hhmmsssss)                                 *
*       natt   : output,integer attenuator in db                      *
*       nfalt  : output,integer frequency alternation                 *
*                  0 : fix, 1 : alternate                             *
*       nfreq  : output,integer observing frequency in GHz            *
*       nstat  : output,integer frame status                          *
*                  0 : not recorded                                   *
*                  1 : calibration data                               *
*                  2 : solar observation data                         *
*                  3 : event data                                     *
*                 15 : re-transferred data                            *
*       flx    : output,real(84)      total flux                      *
*       suma   : output,real(512,512) cos uv-map                      *
*       sumb   : output,real(512,512) sin uv-map                      *
*       nfrmst  : output,integer status                                *
*         0 : all frames have been read                               *
*         1 : there is an invalid frame; when nfrmst=1, other output   *
*             is not valid                                            *
*                                    November 27, 1998  Y. Hanaoka    *
***********************************************************************
c
      subroutine suv34(iunit,filehd,nfrst,nfrend,
     -     nantst,ical,icald,
     -     ndjst,msjst,msjsts,msjste,natt,nfalt,nfreq,nstat,
     -     flx,suma,sumb,nfrmst)
c
      character*80 filehd
      integer nantst(1)
      integer*2 icald(168),icald0(168),icaldc(168)
      real corc(84,85),cors(84,85),cora(512,512),corb(512,512),
     -     flx(84),suma(512,512),sumb(512,512)
      real calib(168)
c
c
      do 300 j=1,512
      do 300 i=1,512
       suma(i,j)=0.
       sumb(i,j)=0.
  300 continue
      do 310 i=1,84
       flx(i)=0.
  310 continue
      do 320 i=1,168
  320 icald0(i)=icald(i)
c
      do 350 iframe=nfrst,nfrend
       call rddat34(iunit,filehd,iframe,iframe,nantst,
     -     ndjst,msjst,jsts,jste,natt,nfalt,nfreq,nstat,
     -     corc,cors,nframe)
c
***** if there is a no data frame, exit *****
       if (nframe.eq.0) then
        nfrmst=1
        go to 900
       end if
c
***** start and end time of integration *****
       if (iframe.eq.nfrst) then
        msjsts=jsts
        mdjsts=jsts/10000000*3600000+mod(jsts,10000000)/100000*60000
     -       +mod(jsts,100000)
       end if
c
       if (iframe.eq.nfrend) then
        msjste=jste
        mdjste=jste/10000000*3600000+mod(jste,10000000)/100000*60000
     -       +mod(jste,100000)
       end if
c
***** get calibration data and calculate r-l phase shift *****
       dcns=0.
       dcew=0.
       rlns=0.
       rlew=0.
       if (ical.ne.999) then
        if (ical.ne.0) then
         call rdcl34(iunit,filehd,iframe,
     -            ical,nantst,icaldc,nframe)

c Call to dcshft in rddcal17 not appropriate here where ical<0
c has a different meaning. Therefore following 2 lines uncommented.

         do 580 i=1,168
  580    icald(i)=icaldc(i)
        end if
       end if
c
c
***** create and calibrate 2d uv-map *****
       call fi2d(corc,cors,cora,corb)
       if (ical.ne.999) then
        do 600 i=1,84
  600   calib(i)=real(icald(i))/100.
        do 610 i=85,168
  610   calib(i)=real(icald(i))/10000.
        call addapa(cora,corb,calib,dcns,dcew)
       end if
c
***** integration *****
       do 360 i=1,84
  360  flx(i)=flx(i)+corc(i,85)
       do 370 j=1,512
       do 370 i=1,512
        suma(i,j)=suma(i,j)+cora(i,j)
        sumb(i,j)=sumb(i,j)+corb(i,j)
  370  continue
c
  350 continue
c
c
***** center of the integration time *****
      msjst=(mdjsts+mdjste)/2
      msjst=msjst/3600000*10000000+mod(msjst,3600000)/60000*100000
     -      +mod(msjst,60000)
c
***** normalize *****
      framen=real(nfrend-nfrst+1)
      do 700 i=1,84
       flx(i)=flx(i)/framen
  700 continue
      do 710 j=1,512
      do 710 i=1,512
       suma(i,j)=suma(i,j)/framen
       sumb(i,j)=sumb(i,j)/framen
  710 continue
c
c --- REMOVE TAPERING FOR AIPS VISIBILITIES
c      call filter1(taper,sumra)
c      call filter1(taper,sumrb)     
c      call filter1(taper,sumla)     
c      call filter1(taper,sumlb)          
c
      nfrmst=0
  900 return
      end
c
c
***********************************************************************
*     rdcl34 - read raw heliograph data and calibrate               *
*                for 34 GHz data                                      *
*       iunit  : input,integer fortran file unit number               *
*                  for raw data file                                  *
*       filehd : input,character*80 raw data filename header          *
*       iframe : input,integer  frame number for calibration          *
*       itgr   : input,integer  number of frames for integration      *
*                if > 0, normal calibration                           *
*                if < 0, bypass amplitude calibration
*       nantst : input,integer(84) antenna status                     *
*       icald  : output,integer*2(168) antenna phases and gains       *
*       nframe : output,integer number of frames which is actually    *
*                  used                                               *
*                                         June 5, 1998  Y. Hanaoka    *
***********************************************************************
c
      subroutine rdcl34(iunit,filehd,iframe,itgr,nantst,
     -     icald,nframe)
c
      character*80 filehd
      integer nantst(84)
      real corc(84,85),cors(84,85)
c
      integer iposns(28),iposew(57),icv(9)
      real phans(200),phaew(200),amans(200),amaew(200)
      integer*2 icald(168)
c
      data iposns/0,1,2,3,4,5,6,7,8,9,10,11,
     -          12,14,16,20,24,28,
     -          32,40,48,56,
     -          64,80,96,112,128,144/
      data iposew/-160,-144,-128,-112,-96,-80,-64,
     -          -56,-48,-40,-32,
     -          -28,-24,-20,-16,-14,-12,
     -          -11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,
     -          0,1,2,3,4,5,6,7,8,9,10,11,
     -          12,14,16,20,24,28,
     -          32,40,48,56,
     -          64,80,96,112,128,144,160/
      data icv/1,2,4,6,8,12,16,24,32/
c
      nantns=28
      nantew=57
      ncv=9
      ampcal=itgr
      itgr=abs(itgr)
c
       call rddat34(iunit,filehd,iframe-(itgr-1)/2,
     -     iframe+itgr/2,nantst,
     -     ndjst,msjst,jsts,jste,natt,nfalt,nfreq,nstat,
     -     corc,cors,nframe)

       inic=0
       call phcal(nantns,iposns,ncv,icv,1,corc,cors,inic,phans,
     -      sums,nmax)
       call phcal(nantew,iposew,ncv,icv,2,corc,cors,inic,phaew,
     -      sums,nmax)
       if (ampcal.lt.0.) go to 100
       call amcal(nantns,iposns,ncv,icv,1,corc,cors,inic,amans,
     -      sums,nmax)
       call amcal(nantew,iposew,ncv,icv,2,corc,cors,inic,amaew,
     -      sums,nmax)
c
  100  icald(1)=0
       do 600 i=2,28
  600  icald(i)=int(phans(i-1)*100.)
       do 610 i=1,56
  610  icald(i+28)=int(phaew(i)*100.)
       icald(85)=10000
c
       do 620 i=2,28
        icald(i+84)=int(amans(i-1)*10000.)
        if (ampcal.lt.0.) icald(i+84)=10000
        if (nantst(i).eq.1) icald(i+84)=0
  620  continue
       do 630 i=1,28
        icald(i+112)=int(amaew(i)*10000.)
        if (ampcal.lt.0.) icald(i+112)=10000
        if (nantst(i+56).eq.1) icald(i+112)=0
  630  continue
       do 640 i=1,28
        icald(i+140)=int(amaew(i+28)*10000.)
        if (ampcal.lt.0.) icald(i+140)=10000
        if (nantst(57-i).eq.1) icald(i+140)=0
  640  continue
c
      return
      end
c
***********************************************************************
*     uvcmprs - extract observable uv-points in compressed form       *
*           coordinate of the dc component : (29,27)                  *
*       nantst : input,integer(84) antenna status                     *
*       a : input,real(512,512)  cos of uv map                        *
*       b : input,real(512,512)  sin of uv map                        *
*       ca: output,real(57,55)   cos of uv map in compressed form     *
*       cb: output,real(57,55)   sin of uv map in compressed form     *
*                                         april 19,1994  k. shibasaki *
***********************************************************************
c
      subroutine uvcmpr(a,b,ca,cb)
c
      integer ipos(-28:28)
      real a(512,512),b(512,512)
      real ca(57,55),cb(57,55)
      data ipos/-160,-144,-128,-112,-96,-80,-64,
     -          -56,-48,-40,-32,
     -          -28,-24,-20,-16,-14,-12,
     -          -11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,
     -          0,1,2,3,4,5,6,7,8,9,10,11,
     -          12,14,16,20,24,28,
     -          32,40,48,56,
     -          64,80,96,112,128,144,160/
c
      do 200 l=-27,27
      do 200 i=-28,28
  200 ca(i+29,l+28)=a(ipos(i)+257,ipos(l)+257)
c
      do 210 l=-27,27
      do 210 i=-28,28
  210 cb(i+29,l+28)=b(ipos(i)+257,ipos(l)+257)
c
      return
      end
c
***********************************************************************
*     putoe1 - write initial ephemeris parameters to fits file                *
*       iounit : input,integer fortran unit number for fits file      *
*       solr   : input,real solar radius 1.0125*optical(arcsecond)    *
*       solp   : input,real solar polar angle (degree)                *
*       solb   : input,real solar b0 (degree)                         *
*       dec    : input,real declination (degree)                      *
*       ha     : input,real hour angle (second)                       *
*       az     : input,real azimuth (degree)                          *
*       alt    : input,real altitude (second)                         *
*                                       april 19, 1994  k. shibasaki  *
***********************************************************************
c
      subroutine putoe1(iounit,solr,solp,solb,dec,ha,az,alt)
c
      call ftpkyf(iounit,'solr1',solr,2,
     -     'optical solar radius (arcsec) at naxis3=1',istat)
      call ftpkyf(iounit,'solp1',solp,4,
     -     'solar polar angle (degree) at naxis3=1',istat)
      call ftpkyf(iounit,'solb1',solb,4,
     -     'solar b0 (degree) at naxis3=1',istat)
      call ftpkyf(iounit,'dec1',dec,4,
     -     'declination (degree) at naxis3=1',istat)
      call ftpkyf(iounit,'houra1',ha,2,
     -     'hour angle (second) at naxis3=1',istat)
      call ftpkyf(iounit,'azimuth1',az,4,
     -     'azimuth (degree) at naxis3=1',istat)
      call ftpkyf(iounit,'altitud1',alt,4,
     -     'altitude (degree) at naxis3=1',istat)
c
      if (istat.ne.0) write(6,'('' putoe1 : istat ='',i6)') istat
      return
      end
c
***********************************************************************
*     putoe2 - write final ephemeris parameters to fits file                *
*       iounit : input,integer fortran unit number for fits file      *
*       solr   : input,real solar radius 1.0125*optical(arcsecond)    *
*       solp   : input,real solar polar angle (degree)                *
*       solb   : input,real solar b0 (degree)                         *
*       dec    : input,real declination (degree)                      *
*       ha     : input,real hour angle (second)                       *
*       az     : input,real azimuth (degree)                          *
*       alt    : input,real altitude (second)                         *
*                                       april 19, 1994  k. shibasaki  *
***********************************************************************
c
      subroutine putoe2(iounit,solr,solp,solb,dec,ha,az,alt)
c
      call ftpkyf(iounit,'solr2',solr,2,
     -     'optical solar radius (arcsec) at naxis3=max',istat)
      call ftpkyf(iounit,'solp2',solp,4,
     -     'solar polar angle (degree) at naxis3=max',istat)
      call ftpkyf(iounit,'solb2',solb,4,
     -     'solar b0 (degree) at naxis3=max',istat)
      call ftpkyf(iounit,'dec2',dec,4,
     -     'declination (degree) at naxis3=max',istat)
      call ftpkyf(iounit,'houra2',ha,2,
     -     'hour angle (second) at naxis3=max',istat)
      call ftpkyf(iounit,'azimuth2',az,4,
     -     'azimuth (degree) at naxis3=max',istat)
      call ftpkyf(iounit,'altitud2',alt,4,
     -     'altitude (degree) at naxis3=max',istat)
      if (istat.ne.0) write(6,'('' putoe2 : istat ='',i6)') istat
      return
      end
c
      subroutine jst2fr(ih,im,is,infof,ifr,istat)
      character*80 infof, line
      itim=ih*3600+im*60+is
      open (11,file=infof,status='old',form='formatted')
 10   read (11,'(a)',err=99) line
      if (line(63:67).eq.'TRACK') then
         read (line,'(34x,i2,i2,i2,1x,i2,i2,i2)') 
     &      jhs,jms,jss,jhe,jme,jse
         jtims=jhs*3600+jms*60+jss
         jtime=jhe*3600+jme*60+jse
         if (itim.ge.jtims.and.itim.le.jtime) then
            read (line,'(48x,i6)') ifs
            ifr=ifs+itim-jtims
            go to 100
            end if
         end if
      go to 10
 99   istat=1
 100  close (11)
      return
      end
