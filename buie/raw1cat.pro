;+
; NAME:
;  raw1cat
; PURPOSE:
;  Determine standard magnitudes of isolated sources for the raw1 catalog.
; DESCRIPTION:
;  This program is meant to be run in the reduced directory of a single
;  observing night. It reads in the rundate, image, and data from the
;  roboccd database and the transformation coefficients from the transf
;  table in the phot database. It puts all the sources through colorsol
;  to generate the standard magnitudes. Then it builds the Raw1 catalog
;  for the specified catid and writes it into the plphot table in phot.
;
;  The sources chosen from the data are those that are well separated from
;    any other sources in the images.  These are processed with the already
;    known all-sky large-aperture photometric transformation.  The individual
;    magnitudes are saved to the database in the raw1 catalog, so-called
;    because these stars have yet to be averaged across nights.  That step
;    is handled by stage1cat.pro
;
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  raw1cat,objpatt,catid
; INPUTS:
;  objpatt - String of the pattern present in object names. Include a % 
;            on the end if it's for more than one object, such as PL2010A 
;            and PL2010B. (ex: PL2010%)  (this is the mysql wildcard character)
;  catid - String of the id for plphot table in the phot database.
;             (ex: PL2010)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;   Uses a few hard-coded paths and fixed database
;      (let us know if you want this generalized)
; PROCEDURES:
;  In a single night's reduced directory, reads the reduc.inf file to get
;    rundate and instrument.
;  Reads in data from roboccd for that rundate using the objpatt input -
;    for 2010 data, objpatt='PL2010%'.
;  Gets coefficients from transf that have already been generated.
;  Puts all sources through colorsol, which returns final magnitudes.
;  Generates Raw1 catalog using the input id - for 2010 data, id='PL2010' -
;    and writes into the plphot table of phot database.
; MODIFICATION HISTORY:
;  2011/01/26 Written by Erin R. George, Southwest Research Institute
;  2011/03/17 ERG, Changed the database for the data to be inserted into
;                  in light of the creation of the plphot table in the 
;                  phot database. Also changed the input from catname to catid.
;  2011/03/28 ERG, Subid is now internal - this will be Raw1.
;  2011/04/01 ERG, final version.
;  2012/03/07 MWB, incorporated into library
;-
pro raw1cat,objpatt,catid

   self='raw1cat: '
   if badpar(objpatt,7,0,CALLER=self+'(objpatt) ') then return
   if badpar(catid,7,0,CALLER=self+'(catid) ') then return

   subid='Raw1'

   infofile='reduc.inf'
   if not exists(infofile) then begin
      print,self,'Error: reduc.inf does not exist for this night.'
      return
   endif
   rdreduc,infofile,instrument,ddir,rundate,rad,sky1,sky2,gain,rdnoise,oplines
   refid=rundate+'-'+instrument

   rundatedir=addslash(rundate)
   rdir='/net/frakir/raid/buie/Reduced/'+rundatedir
   sdir=rdir+'Src/'
   keylist='/net/frakir/raid/buie/Reduced/roboccd.key'
   cdir=ddir+rundatedir+'cal/'

   year=strmid(rundate,0,2)
   if year gt 89 then begin
      year='19'+year
   endif else begin
      year='20'+year
   endelse
   month=strmid(rundate,2,2)
   day=strmid(rundate,4,2)
   utdate=year+'-'+month+'-'+day

   loadkeys,keylist,hdrlist,FOUNDIT=foundkey
   if not foundkey then begin
      print,self,'Keylist ',keylist,' could not be loaded. Aborting.'
      return
   endif

   filters=['B','V']
   color1=filters[0]
   color2=filters[1]
   color=color1+'-'+color2
   dcr=1.5/3600.0d0*!pi/180.0d0  ; 1.5 arcsec converted to radians

   ;coordinates for observatory
   ; This is the GPS position for the 42", derived 1993 Sep 08
   lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
   lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
   name= 'Lowell Observatory - Anderson Mesa Station'

   openmysql,dblun,'roboccd'
   cmd=['select object from image', $
        'where filename like '+quote(rundate+'%'), $
        'and object like '+quote(objpatt), $
        'group by object;']
   mysqlquery,dblun,cmd,objectlist,ngood=nobjects
   if nobjects eq 0 then begin
      print,'Error: The number of objects for this rundate and objpatt is 0.'
      goto,bailout
   endif

   for ii=0,nobjects-1 do begin
      cmd=['select filename from image', $
           'where object='+quote(objectlist[ii]), $
           'and filename like '+quote(rundate+'%'), $
           'group by filename;']
      mysqlquery,dblun,cmd,imagelist,ngood=nimages
      if nimages eq 0 then begin
         print,'Error: There are no images for this object.'
         goto,bailout
      endif

      for jj=0,nimages-1 do begin
         fn=string(imagelist[jj])
         fnsr1=fn+'.sr1'
         if not exists(sdir+fn+'.srd') then begin
            print,self,'Error: ',sdir+fn+'.srd not found'
            goto,bailout
         endif
         if not exists(sdir+fnsr1) then begin
            siftstar,fn,KEYLIST=keylist
         endif
         if not exists(sdir+fnsr1) then begin
            print,self,'Fatal Error: ',fnsr1, $
                       ' not found, this should not happen.'
            goto,bailout
         endif

         ; Load all of the sources already measured on this image.  These
         ;   are all just instrumental measurements at this point but we
         ;   have good positions to use.
         data=readfits(sdir+fnsr1,hdr1)
         fwhm0=trimrank(data[*,2])
         inst0=trimrank(data[*,3]) ; instrumental magnitude
         instsig0=trimrank(data[*,4]) ; uncertainty of instrum. mag.
         ra=trimrank(data[*,5])
         dec=trimrank(data[*,6])

         if not exists(cdir+fn) then begin
            print,self,'Error: ',cdir+fn,' does not exist.'
            goto,bailout
         endif
         image=readfits(cdir+fn,hdr)
         parsekey,hdr,hdrlist,hdrinfo
         fil0=replicate(hdrinfo.filter,n_elements(ra)) ;string array
         jd0=replicate(hdrinfo.jd,n_elements(ra)) ;double array
         am0=airmass(jd0,ra,dec,lat,lon)

         msrcor,set,[ra],[dec],dcr
         if jj eq 0 and ii eq 0 then begin
            fwhm=[fwhm0]
            inst=[inst0]
            instsig=[instsig0]
            fil=[fil0]
            jd=[jd0]
            am=[am0]
         endif else begin
            fwhm=[fwhm,fwhm0]
            inst=[inst,inst0]
            instsig=[instsig,instsig0]
            fil=[fil,fil0]
            jd=[jd,jd0]
            am=[am,am0]
         endelse
      endfor  ; jj loop
   endfor  ; ii loop
   free_lun,dblun   ; for roboccd database

   openmysql,dblun,'phot'
   ; first filter
   cmd=['select k,k2,kcolor,zeropt,ktime, ', $
        'e_k,e_k2,e_kcolor,e_zeropt,e_ktime,jdref', $
        'from transf', $
        'where filter='+quote(filters[0]), $
        'and color1='+quote(filters[0]), $
        'and color2='+quote(filters[1]), $
        'and utdate='+quote(utdate)+';']
   mysqlquery,dblun,cmd,k,k2,kcolor,zeropt,ktime, $
                        e_k,e_k2,e_kcolor,e_zeropt,e_ktime,jdref1, $
                        format='f,f,f,f,f,f,f,f,f,f,d'
   trans1=[k,k2,kcolor,zeropt,ktime]
   trsig1=[e_k,e_k2,e_kcolor,e_zeropt,e_ktime]

   ; second filter
   cmd=['select k,k2,kcolor,zeropt,ktime, ', $
        'e_k,e_k2,e_kcolor,e_zeropt,e_ktime,jdref', $
        'from transf', $
        'where filter='+quote(filters[1]), $
        'and color1='+quote(filters[0]), $
        'and color2='+quote(filters[1]), $
        'and utdate='+quote(utdate)+';']
   mysqlquery,dblun,cmd,k,k2,kcolor,zeropt,ktime, $
                        e_k,e_k2,e_kcolor,e_zeropt,e_ktime,jdref2, $
                        format='f,f,f,f,f,f,f,f,f,f,d'
   trans2=[k,k2,kcolor,zeropt,ktime]
   trsig2=[e_k,e_k2,e_kcolor,e_zeropt,e_ktime]

   stand=string(set.objid)
   serial=replicate(0,n_elements(set.objid))

   bad=bytarr(n_elements(stand))
   for ll=0,n_elements(fwhm)-1 do begin
      if fwhm[ll] le 1.6 then bad[ll]=1B
      if inst[ll] ge 50 then bad[ll]=1B
      if instsig[ll] gt .1 then bad[ll]=1B
   endfor

   colorsol,stand,fil,jd,am,serial,inst,instsig, $
            color1,color2,trans1,trsig1,jdref1,trans2,trsig2,jdref2, $
            object,std1,stdsig1,std2,stdsig2,stdcol,stdcolsig, $
            BADFLAGS=bad,/noprint,/noedit

   ; The delete statement for the data in the new
   ;  database phot:plphot.
   cmddel=['delete from plphot', $
           'where id='+quote(catid), $
           'and subid='+quote(subid), $
           'and refid='+quote(refid)+';']
   mysqlcmd,dblun,cmddel,answer,nlines

   c=','
   ; building raw1 catalog
   for kk=0,n_elements(object)-1 do begin
      zz=where(stand eq object[kk],count)
      if count lt 4 then continue
      avgjd=mean(jd[zz])
      rasig=stdev(set.x[zz],avgra)
      decsig=stdev(set.y[zz],avgdec)
      rasig=rasig*180.0d0/!pi*3600.0d0*cos(avgdec) ; radians-->arcsec
      decsig=decsig*180.0d0/!pi*3600.0d0           ; radians-->arcsec

      zb=where(stand eq object[kk] and $
               fil eq filters[0] and $
               bad eq 0,nbobs)
      b1=where(stand eq object[kk] and $
               fil eq filters[0] and $
               bad eq 1,bbad1)
      b2=where(stand eq object[kk] and $
               fil ne filters[0],bbad2)
      nbbad=bbad1+bbad2
      zv=where(stand eq object[kk] and $
               fil eq filters[1] and $
               bad eq 0,nvobs)
      v1=where(stand eq object[kk] and $
               fil eq filters[1] and $
               bad eq 1,vbad1)
      v2=where(stand eq object[kk] and $
               fil ne filters[1],vbad2)
      nvbad=vbad1+vbad2

      cmd=['insert into plphot values', $
           '('+quote(catid)+c, $                     ; PL2010 catalog
           quote(subid)+c, $                         ; subid: raw1
           quote(refid)+c, $                         ; yymmdd-instrument
           strn(avgjd,format='(f13.5)')+c, $       ; jdate
           strn(avgra,format='(f11.9)')+c, $       ; ra (radians)
           strn(avgdec,format='(f12.9)')+c, $      ; decl (radians)
           strn(rasig,format='(f11.9)')+c, $       ; ra error (arcsec)
           strn(decsig,format='(f11.9)')+c, $      ; dec error (arcsec)
           strn(std1[kk],format='(f8.5)')+c, $     ; B mag
           strn(stdsig1[kk],format='(f7.5)')+c, $  ; B mag error
           strn(std2[kk],format='(f8.5)')+c, $     ; V mag
           strn(stdsig2[kk],format='(f7.5)')+c, $  ; V mag error
           strn(stdcol[kk],format='(f8.5)')+c, $   ; B-V color
           strn(stdcolsig[kk],format='(f7.5)')+c, $; B-V color error
           strn(nbobs)+c, $                        ; # B observations
           strn(nvobs)+c, $                        ; # V observations
           strn(nbbad)+c, $                        ; # bad b measurements
           strn(nvbad)+c, $                        ; # bad v measurements
           strn(1)+c, $                            ; # of nights for b
           strn(1)+');']                           ; # of nights for v
      mysqlcmd,dblun,cmd,answer,nlines

   endfor  ; kk loop

bailout:
   free_lun,dblun ; phot database

end
