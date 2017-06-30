;+
; NAME:
;  raw2cat
; PURPOSE:
;  Determine standard magnitudes of crowded sources for the raw2 catalog.
; DESCRIPTION:
;  This program is meant to be run in the reduced directory of a single
;   observing night. It processes the stars from .sr2 files with the smaller
;   aperture. The Stage1 catalog is subtracted from the sources, which are
;   then put through colorsol to generate their standard magnitudes. Then it
;   builds the Raw2 catalog for the specified catid and writes the data into
;   the plphot table in the phot database.
;
;  The sources chosen from the data are those that are not well separated from
;    other sources in the images.  These crowded sources are measured with
;    a small aperture to avoid contamination and the isolated stars (stage1)
;    are used to calibrate the sources using the small aperture.  The individual
;    magnitudes are saved to the database in the raw2 catalog, so-called
;    because these stars have yet to be averaged across nights.  That step
;    is handled by stage2cat.pro
;
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  raw2cat,objpatt,catid
; INPUTS:
;  objpatt - String, pattern present in object names in the roboccd
;            database. Include a % on the end if it's for more than
;            one object, such as 'PL2010%' for PL2010A and PL2010B.
;  catid- String, id for the plphot table in the phot database.
;             (ex: PL2010)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  At the moment, this routine is pretty specific to our local data reductions
;   here.  Paths, filter handling, and database setup are all very specific.
; PROCEDURES:
;  In a single night's reduced directory, reads the reduc.inf file to get
;   rundate and instrument.
;  Gets coefficients from transf that have already been generated.
;  Reads in data from roboccd for that rundate using objpatt input - for 
;   2010 data, objpatt='PL2010%' - and from .sr2 files.
;  Subtracts the Stage1 catalog from the data.
;  Puts remaining sources through colorsol, which returns final magnitudes.
;  Generates Raw2 catalog using input catid - for 2010 data, id='PL2010' -
;   and writes into the plphot table of phot database.
; MODIFICATION HISTORY:
;  2011/04/08 Written by Erin R. George, Southwest Research Institute
;  2011/12/14 ERG, Final program, header updated. 
;  2012/03/07 MWB, incorporated into library
;-
pro raw2cat,objpatt,catid,NOSAVE=nosave,VERBOSE=verbose,AUDITFIELD=auditfield

   self='raw2cat: '
   if badpar(objpatt,7,0,CALLER=self+'(objpatt) ') then return
   if badpar(catid,7,0,CALLER=self+'(catid) ') then return
   if badpar(nosave,[0,1,2,3],0,CALLER=self+'(NOSAVE) ',default=0) then return
   if badpar(verbose,[0,1,2,3],0,CALLER=self+'(VERBOSE) ',default=0) then return
   if badpar(auditfield,[0,7],0,CALLER=self+'(AUDITFIELD) ', $
                                default='') then return

   s1subid='Stage1'
   r2subid='Raw2'

   dcr=1.5/3600.0d0*!pi/180.0d0  ; 1.5 arcsec converted to radians

   ; Coordinates for the observatory
   ; Hardcoded position for 42"
   ; This is the GPS position for the 42", derived 1993 Sep 08
   lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
   lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
   name= 'Lowell Observatory - Anderson Mesa Station'

   infofile='reduc.inf'
   if not exists(infofile) then begin
      print,self,'Error: reduc.inf does not exists for this night.'
      return
   endif
   rdreduc,infofile,instrument,ddir,rundate,rad,sky1,sky2,gain,rdnoise,oplines
   refid=rundate+'-'+instrument

   if exists('badframes') then $
      readcol,'badframes',fnbad,format='a' $
   else fnbad=''

   ; Building the utdate from the date in the reduc.inf file.
   year=strmid(rundate,0,2)
   if year gt 89 then begin
      year='19'+year
   endif else begin
      year='20'+year
   endelse
   month=strmid(rundate,2,2)
   day=strmid(rundate,4,2)
   utdate=year+'-'+month+'-'+day

   rundatedir=addslash(rundate)
   sdir='/net/frakir/raid/buie/Reduced/'+rundatedir+'Src/'
   cdir=ddir+rundatedir+'cal/'
   keylist='/net/frakir/raid/buie/Reduced/roboccd.key'

   loadkeys,keylist,hdrlist,FOUNDIT=foundkey
   if not foundkey then begin
      print,self,'Keylist ',keylist,' could not be loaded. Aborting.'
      return
   endif

   ; Stage1 catalog stars
   openmysql,dblun,'phot'
   cmd=['select ra,decl,rasig,decsig,b,bsig,v,vsig,bmv,bmvsig,'+ $
        'nbobs,nvobs,nbbad,nvbad,nbni,nvni from plphot where', $
        'id='+quote(catid), $
        'and subid='+quote(s1subid)+';']
   mysqlquery,dblun,cmd,ra,dec,rasig,decsig,bmag,bsig,vmag,vsig, $
        bmv,bmvsig,nbobs,nvobs,nbbad,nvbad,nbni,nvni, $
        format='d,d,f,f,f,f,f,f,f,f,i,i,i,i,i,i',ngood=nstage1
   if nstage1 eq 0 then begin
      print,'Error: The number of stage1 stars is 0.'
      goto,bailout
   endif

   filters=['B','V']
   color1=filters[0]
   color2=filters[1]
   color=color1+'-'+color2

   ; Coefficients from transf table in phot database
   ; B filter
   cmd=['select k,k2,kcolor,ktime,'+ $
        'e_k,e_k2,e_kcolor,e_ktime,jdref', $
        'from transf where filter='+quote(filters[0]), $
        'and color1='+quote(color1), $
        'and utdate='+quote(utdate)+';']
   mysqlquery,dblun,cmd,kb,k2b,kcolorb,ktimeb, $
              e_kb,e_k2b,e_kcolorb,e_ktimeb,jdref1, $
              format='f,f,f,f,f,f,f,f,d'

   ; V filter
   cmd=['select k,k2,kcolor,ktime,'+ $
        'e_k,e_k2,e_kcolor,e_ktime,jdref', $
        'from transf where filter='+quote(filters[1]), $
        'and color1='+quote(color1), $
        'and utdate='+quote(utdate)+';']
   mysqlquery,dblun,cmd,kv,k2v,kcolorv,ktimev, $
              e_kv,e_k2v,e_kcolorv,e_ktimev,jdref2, $
              format='f,f,f,f,f,f,f,f,d'
   free_lun,dblun  ; phot database

   ; Objects for each observation night.
   openmysql,dblun,'roboccd'
   cmd=['select object from image', $
        'where filename like '+quote(rundate+'%'), $
        'and object like '+quote(objpatt), $
        'group by object;']
   mysqlquery,dblun,cmd,objectlist,ngood=nobjects
   if nobjects eq 0 then begin
      print,self,'Error: The number of objects in the roboccd database', $
                 'for this rundate and objpatt is 0.'
      goto,bailout
   endif
   if verbose then begin
      print,strn(nobjects),' found with input template:'
      print,objectlist
   endif

   firstframe=1
   if auditfield ne '' then begin
      fnb=''
      fnv=''
   endif
   ; Loop over objects and find images for each object.
   for ii=0,nobjects-1 do begin
      cmd=['select filename from image', $
           'where object='+quote(objectlist[ii]), $
           'and filename like '+quote(rundate+'%'), $
           'group by filename;']
      mysqlquery,dblun,cmd,imagelist,ngood=nimages,format='a'
      if nimages eq 0 then begin
         print,self,'Error: There are no images for object: ',objectlist[ii]
         goto,bailout
      endif
      if verbose then begin
         print,objectlist[ii],nimages,' images found.'
      endif

      ; Loop over images in each object.
      for jj=0,nimages-1 do begin
         fn=imagelist[jj]
         fnsr2=fn+'.sr2'

         z=where(fnbad eq fn,count)
         if count ne 0 then continue

         if not exists(sdir+fnsr2) then begin
            print,'Error: There is no .sr2 file for ',imagelist[jj],' Aborting.'
            goto,bailout
         endif
         data=readfits(sdir+fnsr2,hdr2)
         x2=trimrank(data[*,0])
         y2=trimrank(data[*,1])
         fwhm2=trimrank(data[*,2])
         inst2=trimrank(data[*,3])
         instsig2=trimrank(data[*,4])
         ra2=trimrank(data[*,5])
         dec2=trimrank(data[*,6])
         nobj=n_elements(ra2)

         ; Get the small photometric aperture from the sr2 header
         radius=float(sxpar(hdr2,'OBJRAD'))
         if radius lt 1.0 then begin
            ; this error really shouldn't ever happen
            print,self,'Error: OBJRAD too small',radius
            print,'reading file ',sdir+fnsr2
            goto,bailout
         endif

         if not exists(cdir+fn) then begin
            print,self,'Error: ',cdir+fn,' does not exist.'
            goto,bailout
         endif
         hdr=headfits(cdir+fn)
         parsekey,hdr,hdrlist,hdrinfo
         jd2=replicate(hdrinfo.jd,nobj)
         exptime=hdrinfo.exptime
         filter=hdrinfo.filter
         if filter ne filters[0] and filter ne filters[1] then continue
         if auditfield ne '' then begin
            if filter eq 'B' and fnb eq '' and $
               hdrinfo.object eq auditfield then begin
               fnb=fn
               print,strn(nobj),' sr2 sources found in ',fnb
            endif
            if filter eq 'V' and fnv eq '' and $
               hdrinfo.object eq auditfield then begin
               fnv=fn
               print,strn(nobj),' sr2 sources found in ',fnv
            endif
         endif
         fil2=replicate(filter,nobj)
         am2=airmass(jd2,ra2,dec2,lat,lon)

         ; Link the Stage1 catalog with the sr2 stars
         srcor,ra,dec,ra2,dec2,dcr,ind1,ind2
         nstds=n_elements(ind1)
         std=replicate(0B,nobj)
         std[ind2]=1B

         ; Solve for the zero point of the Stage 1 stars in sr2 list.
         if filter eq filters[0] then begin
            inst2std,jd2[ind2],am2[ind2],inst2[ind2],instsig2[ind2], $
                     bmv[ind1],bmvsig[ind1], $
                     [kb,k2b,kcolorb,0.0,0.0], $
                     [e_kb,e_k2b,e_kcolorb,0.0,0.0], $
                     jdref2,stdmag,stdsig
            zpall=bmag[ind1]-stdmag
            zpallsig=sqrt(bsig[ind1]^2+stdsig^2)
         endif else if filter eq filters[1] then begin
            inst2std,jd2[ind2],am2[ind2],inst2[ind2],instsig2[ind2], $
                     bmv[ind1],bmvsig[ind1], $
                     [kv,k2v,kcolorv,0.0,0.0], $
                     [e_kv,e_k2v,e_kcolorv,0.0,0.0], $
                     jdref2,stdmag,stdsig
            zpall=vmag[ind1]-stdmag
            zpallsig=sqrt(vsig[ind1]^2+stdsig^2)
         endif

         bad=bytarr(nstds)
         robomean,zpall,3.0,0.5,zp0,bad=bad
         zg=where(bad eq 0,count)
         if count eq 0 then begin
            print,'WARNING!  All points died in robomean, keeping all.'
            zg=lindgen(nstds)
            count=nstds
         endif
         meanerr,zpall[zg],zpallsig[zg],zp0,zpsigm0,zpsigd

         ; ra2[sr2] are sr2 stars not in the Stage 1 catalog.
         sr2=where(std eq 0B,count)

         fwhm0=fwhm2[sr2]
         inst0=inst2[sr2]
         instsig0=instsig2[sr2]
         fil0=fil2[sr2]
         jd0=jd2[sr2]
         am0=am2[sr2]

         msrcor,set,[ra2[sr2]],[dec2[sr2]],dcr
         if firstframe then begin
            fwhm=[fwhm0]
            inst=[inst0]
            instsig=[instsig0]
            fil=[fil0]
            jd=[jd0]
            am=[am0]
            zp=[replicate(zp0,count)]
            zpsig=[replicate(zpsigm0,count)]
            firstframe++
         endif else begin
            fwhm=[fwhm,fwhm0]
            inst=[inst,inst0]
            instsig=[instsig,instsig0]
            fil=[fil,fil0]
            jd=[jd,jd0]
            am=[am,am0]
            zp=[zp,replicate(zp0,count)]
            zpsig=[zpsig,replicate(zpsigm0,count)]
         endelse
         if verbose then begin
            print,ii,jj,filter,set.nlists,set.nobj,n_elements(set.x),nstds, $
               zp0,zpsigm0, $
               format='(i3,1x,i3,1x,a,1x,i3,1x,i6,1x,i8,1x,i5,1x,f6.3,1x,f5.3)'
            setwin,0
            plot,[0],/nodata,/iso, $
               xrange=maxmin([set.x,ra]),yrange=minmax([set.y,dec]), $
               xtitle='(E)   RA   (W)', $
               ytitle='(S)   Dec   (N)', $
               title=utdate
            oplot,ra,dec,psym=4
            oplot,set.x,set.y,psym=3,color='00ffff'xl
         endif
      endfor  ; jj loop
   endfor  ; ii loop

   if verbose then begin
      setwin,1
      time=(jd-(double(long(jd[0]+0.5d0))-0.5d0))*24.0
      ploterror,time,zp,zpsig,psym=8, $
         xtitle='UT time (hours)',title=utdate, $
         ytitle='Photometric zero-point',/nodata
      z=where(fil eq 'B',count)
      if count ne 0 then $
         oploterror,time[z],zp[z],zpsig[z],psym=8, $
            color='ff8080'xl,errcolor='ff8080'xl
      z=where(fil eq 'V',count)
      if count ne 0 then $
         oploterror,time[z],zp[z],zpsig[z],psym=8, $
            color='80ff80'xl,errcolor='80ff80'xl
   endif
   free_lun,dblun  ; roboccd database

   trans1=[kb,k2b,kcolorb,0.0,0.0]
   trsig1=[e_kb,e_k2b,e_kcolorb,0.0,0.0]

   trans2=[kv,k2v,kcolorv,0.0,0.0]
   trsig2=[e_kv,e_k2v,e_kcolorv,0.0,0.0]

   stand=string(set.objid)
   serial=replicate(0,n_elements(set.objid))

   bad=bytarr(n_elements(stand))
   z=where(fwhm le 1.6,count)
   if count ne 0 then bad[z]=1
   z=where(inst ge 50.0,count)
   if count ne 0 then bad[z]=1
   z=where(instsig gt 0.05,count)
   if count ne 0 then bad[z]=1
   if verbose then print,total(bad),' marked bad'

   ; add the zero-point to the instrumental magnitude since it varies with
   ;   the data.
   instp = inst+zp
   instpsig = sqrt( instsig^2 + zpsig^2 )
   colorsol,stand,fil,jd,am,serial,instp,instpsig,color1,color2, $
            trans1,trsig1,jdref1,trans2,trsig2,jdref2, $
            object,std1,stdsig1,std2,stdsig2,stdcol,stdcolsig, $
            BADFLAGS=bad,/noprint,/noedit
   nobj=n_elements(object)
   badobject=bytarr(nobj)

   openmysql,dblun,'phot'
   ; The delete statement for the data in the new
   ;  database phot:plphot.
   cmddel=['delete from plphot where', $
           'id='+quote(catid), $
           'and subid='+quote(r2subid), $
           'and refid='+quote(refid)+';']
   if nosave then begin
;      print,cmddel
   endif else begin
      mysqlcmd,dblun,cmddel,answer,nlines
   endelse

   sigthresh=0.10
   zb=where(stdsig1 gt sigthresh or stdsig2 gt sigthresh,countb)
   if countb gt 0 then begin
      badobject[zb]=1
      if verbose then print,'Eliminating ',strn(countb), $
                            ' sources with errors > ',strn(sigthresh),' mag'
   endif

   c=','
   ; Building raw2 catalog
   raw2ra=dblarr(nobj)
   raw2dec=dblarr(nobj)
   for kk=0,nobj-1 do begin
      if badobject[kk] then continue
      zz=where(stand eq object[kk],count)
      avgjd=mean(jd[zz])
      rasig=stdev(set.x[zz],avgra)
      decsig=stdev(set.y[zz],avgdec)
      raw2ra[kk]=avgra
      raw2dec[kk]=avgdec
      rasig=rasig*180.0d0/!pi*3600.0d0*cos(avgdec) ; radians-->arcsec
      decsig=decsig*180.0d0/!pi*3600.0d0           ; radians-->arcsec
;      if count lt 4 then begin
;         badobject[kk]=1
;         continue
;      endif
;print,kk,object[kk],count

      ab=where(stand eq object[kk] and $
               fil eq filters[0] and $
               bad eq 0,nbobs)
      b1=where(stand eq object[kk] and $
               fil eq filters[0] and $
               bad eq 1,nbbad)
      av=where(stand eq object[kk] and $
               fil eq filters[1] and $
               bad eq 0,nvobs)
      v1=where(stand eq object[kk] and $
               fil eq filters[1] and $
               bad eq 1,nvbad)

      cmd=['insert into plphot values', $
           '('+quote(catid)+c, $                     ; catalog
           quote(r2subid)+c, $                       ; subid: raw2
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
      if nosave then begin
;         print,cmd
      endif else begin
         mysqlcmd,dblun,cmd,answer,nlines
      endelse
      if verbose then begin
;         print,object[kk],nbobs,std1[kk],stdsig1[kk], $
;                          nvobs,std2[kk],stdsig2[kk], $
;               stdcol[kk],stdcolsig[kk],nbbad,nvbad, $
;            format='(a,2(1x,i1,1x,f7.4,1x,f6.4),1x,f7.4,1x,f6.4,2(1x,i1))'
      endif

   endfor  ; kk loop

   if verbose then begin
      z=where(badobject eq 0,count)
      if count ne 0 then begin
         setwin,2
         ploterror,std1[z],std2[z],stdsig1[z],stdsig2[z],psym=8, $
            xtitle='B mag',ytitle='V mag',title=utdate
         oploterror,bmag,vmag,bsig,vsig,psym=4, $
            color='00008f'xl,errcolor='00008f'xl
         setwin,3
         stats,stdcol[z],/silent,nbins=100,xtitle='(B-V)',title=utdate
         setwin,4
         ploterror,std2[z],stdcol[z],stdsig2[z],stdcolsig[z], $
            xtitle='V mag',ytitle='(B-V)',psym=8,title=utdate
         oploterror,vmag,bmv,vsig,bmvsig,psym=4, $
            color='00008f'xl,errcolor='00008f'xl
      endif
      print,'There are ',strn(nstage1),' stage 1 sources and ', $
            strn(count),' new raw 2 sources.'
   endif

   if auditfield ne '' then begin
      print,'Special audit graphics for field ',auditfield
      print,'B image in window 5: ',fnb
      print,'V image in window 6: ',fnv
      bhdr=headfits(cdir+fnb)
      astinfo,bhdr,binfo
      vhdr=headfits(cdir+fnv)
      astinfo,vhdr,vinfo
      astcvt,'rd',ra,dec,binfo,'xy',s1bx,s1by
      astcvt,'rd',ra,dec,vinfo,'xy',s1vx,s1vy
      zb=where(fil eq 'B')
      zv=where(fil eq 'V')
      astcvt,'rd',raw2ra,raw2dec,binfo,'xy',s2bx,s2by
      astcvt,'rd',raw2ra,raw2dec,vinfo,'xy',s2vx,s2vy
      zg=where(badobject eq 0,countg)

      showsrc,cdir+fnb,'Src/'+fnb+'.src',windownum=5
      ; mark the stage 1 sources
      oplot,s1bx,s1by,psym=4,color='00ff00'xl,symsize=4.0
      oplot,s1bx,s1by,psym=4,color='00ff00'xl,symsize=5.0
      ; mark the raw 2 sources
      astcvt,'rd',set.x,set.y,binfo,'xy',s2x,s2y
      oplot,s2x[zb],s2y[zb],psym=4,color='0000ff'xl,symsize=3.0
      oplot,s2x[zv],s2y[zv],psym=4,color='0000ff'xl,symsize=4.0
      oplot,s2bx[zg],s2by[zg],psym=4,color='00ff00'xl,symsize=5.0

      showsrc,cdir+fnv,'Src/'+fnv+'.src',windownum=6
      z=where(fil eq 'V')
      ; mark the stage 1 sources
      oplot,s1vx,s1vy,psym=4,color='00ff00'xl,symsize=4.0
      oplot,s1vx,s1vy,psym=4,color='00ff00'xl,symsize=5.0
      ; mark the raw 2 sources
      astcvt,'rd',set.x,set.y,vinfo,'xy',s2x,s2y
      oplot,s2x[zv],s2y[zv],psym=4,color='0000ff'xl,symsize=3.0
      oplot,s2x[zb],s2y[zb],psym=4,color='0000ff'xl,symsize=4.0
      oplot,s2vx[zg],s2vy[zg],psym=4,color='00ff00'xl,symsize=5.0

   endif

bailout:
   free_lun,dblun ; phot database

end  ; raw2cat.pro
