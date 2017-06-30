      program c2fits17
c----------------------------------------------------------------------
c   This task will calibrate selected data from the radioheliograph
c   and write the result to disk in FITS format. This version handles
c   17 GHz data. One can turn the amplitude calibration on or off,
c   apply smoothing to the calibration, or apply a single calibration
c   to the data.
c
c   Written by T. Bastian, 16 Oct 1996
c   Subroutines UVCMPRS, PUTEO1, and PUTEO2 written by K. Shibasaki
c   Calls subroutines RDPAR, CALEPH, JST2UT, JST2FR, PUTHDR, RDDAT17, 
c         PHSHFT, FI2D, ADDAPA, PHCAL, AMCAL from the helioglib library.
c
c   Updated 99 Aug by S. White due to changes in helioglib routines:
c     - added arg za to CALEPH
c     - JST2UT has changed to require input date as yyyymmdd, not yymmdd
c       and this should be the format now returned by rddat17/suv17 -
c       In fact all dates are now yyyymmdd: variables NDUT and NDJST.
c       Removed all calls to ndut and msut - only needed in PUTHDR
c       where internal conversion JST2UT takes place.
c     - removed arguments ndut, msut and added nstat in call to PUTHDR
c     - removed arguments ndut, msut and added nstat in call to RDDAT17
c     - changed declarations of nantst and rlph in suv17 from (1) to (84)
c       for consistency with all other references. NOTE however that
c       according to PJT this is unnecessary: as long as nantst is
c       declared as integer(84) in the main program then it can be
c       declared as integer(1) in subroutines (where it has to be 
c       declared to function as an array) but will still act as if it
c       were integer(84) because FORTRAN uses addresses.
c   Modified 99 Nov by S. White: replaced SUV17 with version
c       of SNAPUV17 from helioglib62.f, since original
c       was just renamed versions of those routines from helioglib50.f
c       Neither RDCL17(=RDDCAL17) nor UVCMPR needs to be replaced.
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
      integer*2 icald17(336)
      integer*4 ist,ien,jst,jen
      real flxr(84),flxl(84),
     -     mapra(512,512),maprb(512,512),mapla(512,512),maplb(512,512),
     -     pmat(4)
      real rlph(84)
      real ccra(57,55),ccrb(57,55),ccla(57,55),cclb(57,55)
      real crlab(57,55,600,4),drlab(57,55,600)
      character*80 filehd,infpar,infph,outf,outfft,infof,dum1
      character*18 dattyp
      character*3 tst, fixc, amc, dmpc, skpo
c
c   Read inputs from c2fits17.inp
c
      open (unit=12,file='c2fits17.inp',status='old',form='formatted')
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
c   Read RCP/LCP instrumental phase difference    
c   Now incorporated in library subroutine GTRLPH ? If so
c       these lines are not needed - set 1st element to 1.0
c
c      open(iunit, file=infph, access='sequential', status='old')
c      read(iunit,*) rlph
c      close(iunit)
      rlph(1) = 1.0

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
c      Convert JST times to frame numbers
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
c   Report what we've found so far
c
      write (6,'(//,''The following processing will be performed:'')')
      write (6,'(''Frequency: 17 GHz'')') 
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
            call rdcl17(iunit,filehd,ncfr1,ical,nantst,icald17,nframe)
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

          call suv17(iunit,filehd,ifr1,ifr2,
     -       nantst,ical,icald17,rlph,ndjst,msjst,
     -       msjsts,msjste,natt,nfalt,nfreq,nstat,
     -       flxr,flxl,mapra,maprb,mapla,maplb,nfrmst)
          if (nfrmst.ne.0) go to 100
          if (dmpc.eq.'YES') write (12,*) icald17
          call uvcmpr(mapra,maprb,ccra,ccrb)
          call uvcmpr(mapla,maplb,ccla,cclb)
          do 20 l=1,55
             do 20 i=1,57
                crlab(i,l,ii,1)=ccra(i,l)
  20            continue    
          do 30 l=1,55
             do 30 i=1,57
                crlab(i,l,ii,2)=ccrb(i,l)
  30            continue    
          do 40 l=1,55
             do 40 i=1,57
                crlab(i,l,ii,3)=ccla(i,l)
  40            continue    
          do 50 l=1,55
             do 50 i=1,57
                crlab(i,l,ii,4)=cclb(i,l)
  50            continue
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
      ifrm1=iimg(1)
      ifrm2=iimg(nimg)
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
      do 400 i=1,4
         if(i.eq.1) outfft=outf(1:loutf)//'rc.fits'
         if(i.eq.2) outfft=outf(1:loutf)//'rs.fits'
         if(i.eq.3) outfft=outf(1:loutf)//'lc.fits'
         if(i.eq.4) outfft=outf(1:loutf)//'ls.fits'
         if(i.eq.1) dattyp='uv data(rcp,cos)'
         if(i.eq.2) dattyp='uv data(rcp,sin)'
         if(i.eq.3) dattyp='uv data(lcp,cos)'
         if(i.eq.4) dattyp='uv data(lcp,sin)'
         ipol=1-((i-1)/2)*2
         do 300 l=1,nimg
            do 300 k=1,55
               do 300 j=1,57
                  drlab(j,k,l)=crlab(j,k,l,i)
  300             continue
         call ftinit(iounit,outfft,2880,istat)
         if (istat.ne.0) go to 900
         write(6,*) nfreq
         call puthdr(iounit,nbit,naxis,naxes,
     &      ndjst,msjst,msjst11,msjst12,ifrm1,ifrm2,
     &      ipol,natt,nfalt,nfreq,nstat,dattyp)
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
 400     continue
      go to 1000
 800  write(6,'('' Bad time range specified - no frame found '')')
 900  write(6,'('' istat ='',i8)') istat
 1000 if (dmpc.eq.'YES') close (unit=12)
      stop
      end
***********************************************************************
*       suv17 - create 2-dimensional uv-map from raw data             *
*                and calibrate                                        *
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
*       icald  : input,integer*2(336) initial calibration data        *
*              : output               obtained calibration data       *
*                if integrate frames, cal data for the last frame     *
*       rlph   : input,real(84)  instrumental phase differences of    *
*                                antennas                             *
*                if rlph(1)=-1, no phase correction is done           *
*                if rlph(1)=1., phase differences are initialized     *
*       ndjst  : output,integer observing date in jst (yyyymmdd)      *
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
*       flxr   : output,real(84)      rcp total flux                  *
*       flxl   : output,real(84)      lcp total flux                  *
*       sumra  : output,real(512,512) rcp/cos uv-map                  *
*       sumrb  : output,real(512,512) rcp/sin uv-map                  *
*       sumla  : output,real(512,512) lcp/cos uv-map                  *
*       sumlb  : output,real(512,512) lcp/sin uv-map                  *
*       nfrmst : output,integer status                                *
*         0 : all frames have been read                               *
*         1 : there is an invalid frame; when nfrmst=1, other output  *
*             is not valid                                            *
*                                    November 27, 1998  Y. Hanaoka    *
***********************************************************************
c
      subroutine suv17(iunit,filehd,
     -     nfrst,nfrend,nantst,ical,icald,rlph,
     -     ndjst,msjst,msjsts,msjste,natt,nfalt,nfreq,nstat,
     -     flxr,flxl,sumra,sumrb,sumla,sumlb,nfrmst)
c
      character*80 filehd
      integer nantst(84)
      integer*2 icald(336),icald0(336),icaldc(336)
      real corrc(84,85),corrs(84,85),corlc(84,85),corls(84,85),
     -     cora(512,512),corb(512,512),
     -     flxr(84),flxl(84),
     -     sumra(512,512),sumrb(512,512),sumla(512,512),sumlb(512,512)
      real calib(168),rlph(84)
c
c
      do 300 j=1,512
      do 300 i=1,512
       sumra(i,j)=0.
       sumrb(i,j)=0.
       sumla(i,j)=0.
       sumlb(i,j)=0.
  300 continue
      do 310 i=1,84
       flxr(i)=0.
       flxl(i)=0.
  310 continue
      do 320 i=1,336
  320 icald0(i)=icald(i)
c
      do 350 iframe=nfrst,nfrend
       call rddat17(iunit,filehd,iframe,iframe,nantst,
     -     ndjst,msjst,jsts,jste,natt,nfalt,nfreq,nstat,
     -     corrc,corrs,corlc,corls,nframe)
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
         call rdcl17(iunit,filehd,iframe,
     -               ical,nantst,icaldc,nframe)

c Call to dcshft in rddcal17 not appropriate here where ical<0
c has a different meaning. Therefore following 2 lines uncommented.

         do 580 i=1,336
  580    icald(i)=icaldc(i)
        end if
c R-L phase differences now incorporated in library subroutine GTRLPH
        if (rlph(1).ne.-1.) then
         if (rlph(1).eq.1.) call gtrlph(ndjst,rlph)
c        write(6,'('' '',7f10.2)') rlph
         call phshft(rlph,icald,rlns,rlew)
c         write(6,'('' phase shift ='',2f10.2)') rlns,rlew
        end if
       end if
c
c
***** create and calibrate 2d uv-map (rcp) *****
       call fi2d(corrc,corrs,cora,corb)
       if (ical.ne.999) then
        do 600 i=1,84
  600   calib(i)=real(icald(i))/100.
        do 610 i=85,168
  610   calib(i)=real(icald(i))/10000.
        call addapa(cora,corb,calib,dcns,dcew)
       end if
c
       do 360 i=1,84
  360  flxr(i)=flxr(i)+corrc(i,85)
       do 370 j=1,512
       do 370 i=1,512
        sumra(i,j)=sumra(i,j)+cora(i,j)
        sumrb(i,j)=sumrb(i,j)+corb(i,j)
  370  continue
c
***** create and calibrate 2d uv-map (lcp) *****
       call fi2d(corlc,corls,cora,corb)
       if (ical.ne.999) then
        do 620 i=1,84
  620   calib(i)=real(icald(168+i))/100.
        do 630 i=85,168
  630   calib(i)=real(icald(168+i))/10000.
        call addapa(cora,corb,calib,dcns+rlns,dcew+rlew)
       end if
c
***** integration *****
       do 380 i=1,84
  380  flxl(i)=flxl(i)+corlc(i,85)
       do 390 j=1,512
       do 390 i=1,512
        sumla(i,j)=sumla(i,j)+cora(i,j)
        sumlb(i,j)=sumlb(i,j)+corb(i,j)
  390  continue
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
       flxr(i)=flxr(i)/framen
       flxl(i)=flxl(i)/framen
  700 continue
      do 710 j=1,512
      do 710 i=1,512
       sumra(i,j)=sumra(i,j)/framen
       sumrb(i,j)=sumrb(i,j)/framen
       sumla(i,j)=sumla(i,j)/framen
       sumlb(i,j)=sumlb(i,j)/framen
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
*     rdcl17 - read raw heliograph data and calibrate                 *
*       iunit  : input,integer fortran file unit number               *
*                  for raw data file                                  *
*       filehd : input,character*80 raw data filename header          *
*       iframe : input,integer  frame number for calibration          *
*       itgr   : input,integer  number of frames for integration      *
*                if > 0, normal calibration                           *
*                if < 0, bypass amplitude calibration                 *
*       nantst : input,integer(84) antenna status                     *
*       icald  : output,integer*2(336) antenna phases and gains       *
*       nframe : output,integer number of frames which is actually    *
*                  used                                               *
*                                      October 13,1995  Y. Hanaoka    *
*                              Revised October 16,1996 T. Bastian     *
***********************************************************************
c
      subroutine rdcl17(iunit,filehd,iframe,itgr,nantst,
     -     icald,nframe)
c
      character*80 filehd
      integer nantst(84)
      real corrc(84,85),corrs(84,85),corlc(84,85),corls(84,85)
c
      integer iposns(28),iposew(57),icv(9)
      real phansr(200),phansl(200),phaewr(200),phaewl(200),
     -     amansr(200),amansl(200),amaewr(200),amaewl(200)
      integer*2 icald(336)
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
       call rddat17(iunit,filehd,iframe-(itgr-1)/2,
     -     iframe+itgr/2,nantst,
     -     ndjst,msjst,jsts,jste,natt,nfalt,nfreq,nstat,
     -     corrc,corrs,corlc,corls,nframe)
       inic=0
       call phcal(nantns,iposns,ncv,icv,1,corrc,corrs,inic,phansr,
     -      sums,nmax)
       call phcal(nantew,iposew,ncv,icv,2,corrc,corrs,inic,phaewr,
     -      sums,nmax)
       if (ampcal.lt.0.) go to 100
       call amcal(nantns,iposns,ncv,icv,1,corrc,corrs,inic,amansr,
     -      sums,nmax)
       call amcal(nantew,iposew,ncv,icv,2,corrc,corrs,inic,amaewr,
     -      sums,nmax)
c
 100   call phcal(nantns,iposns,ncv,icv,1,corlc,corls,inic,phansl,
     -      sums,nmax)
       call phcal(nantew,iposew,ncv,icv,2,corlc,corls,inic,phaewl,
     -      sums,nmax)
       if (ampcal.lt.0.) go to 200
       call amcal(nantns,iposns,ncv,icv,1,corlc,corls,inic,amansl,
     -      sums,nmax)
       call amcal(nantew,iposew,ncv,icv,2,corlc,corls,inic,amaewl,
     -      sums,nmax)
c
 200   icald(1)=0
       do 600 i=2,28
 600      icald(i)=int(phansr(i-1)*100.)
       do 610 i=1,56
 610      icald(i+28)=int(phaewr(i)*100.)
       icald(85)=10000
c
       do 620 i=2,28
          icald(i+84)=int(amansr(i-1)*10000.)
          if (ampcal.lt.0.) icald(i+84)=10000
          if (nantst(i).eq.1) icald(i+84)=0
  620     continue
       do 630 i=1,28
          icald(i+112)=int(amaewr(i)*10000.)
          if (ampcal.lt.0.) icald(i+112)=10000
          if (nantst(i+56).eq.1) icald(i+112)=0
  630     continue
       do 640 i=1,28
          icald(i+140)=int(amaewr(i+28)*10000.)
          if (ampcal.lt.0.) icald(i+140)=10000
          if (nantst(57-i).eq.1) icald(i+140)=0
  640     continue
c
       icald(169)=0
       do 700 i=2,28
  700     icald(i+168)=int(phansl(i-1)*100.)
       do 710 i=1,56
  710     icald(i+196)=int(phaewl(i)*100.)
c
       icald(253)=10000
       do 720 i=2,28
          icald(i+252)=int(amansl(i-1)*10000.)
          if (ampcal.lt.0.) icald(i+252)=10000
          if (nantst(i).eq.1) icald(i+252)=0
  720     continue
       do 730 i=1,28
          icald(i+280)=int(amaewl(i)*10000.)
          if (ampcal.lt.0.) icald(i+280)=10000
          if (nantst(i+56).eq.1) icald(i+280)=0
  730     continue
       do 740 i=1,28
          icald(i+308)=int(amaewl(i+28)*10000.)
          if (ampcal.lt.0.) icald(i+308)=10000
          if (nantst(57-i).eq.1) icald(i+308)=0
  740     continue
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
*     putoe1 - write initial ephemeris parameters to fits file        *
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
*     putoe2 - write final ephemeris parameters to fits file          *
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
***********************************************************************
*     jst2fr - convert JST HHMMSS to frame number                     *
***********************************************************************
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
