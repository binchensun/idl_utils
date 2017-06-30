;+
; NAME:
;  stage2cat
; PURPOSE:   (one line only)
;  Generates the Stage2 catalog for a given Raw2 catalog
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  stage2cat,id
; INPUTS:
;  id - String, id for the plphot table in the phot database (ex.'PL2010')
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/12/14, Written by Erin R. George, Southwest Research Institute
;-
pro stage2cat,id,NOSAVE=nosave

   self='stage2cat: '
   if badpar(id,7,0,CALLER=self+'(id) ') then return
   if badpar(nosave,[0,1,2,3],0,CALLER=self+'(NOSAVE) ',default=0) then return

   r2subid='Raw2'
   dcr=1.5/3600.0d0*!pi/180.0d0   ; 1.5 arcsec converted to radians

   openmysql,dblun,'phot'
   ; Query the database to get the list of refids for the
   ;    PL2010 raw1 catalog.
   cmd=['select refid from plphot', $
        'where id='+quote(id), $
        'and subid='+quote(r2subid), $
        'group by refid;']
   mysqlquery,dblun,cmd,refidlist,ngood=nrefids

   for ii=0,nrefids-1 do begin
      ; Query the database for the ra and dec of each object for
      ;    each refid. Build the sets using msrcor, which will
      ;    take out any overlapping objects between the refids.
      cmd=['select ra,decl,rasig,decsig,b,bsig,v,vsig,bmv,bmvsig,'+ $
           'nbobs,nvobs,nbbad,nvbad,nbni,nvni from plphot where', $
           'id='+quote(id), $
           'and subid='+quote(r2subid), $
           'and refid='+quote(refidlist[ii])+';']
      mysqlquery,dblun,cmd,ra0,dec0,rasig0,decsig0,b0,bsig0,v0,vsig0, $
                col0,colsig0,nbobs0,nvobs0,nbbad0,nvbad0,nbni0,nvni0, $
                format='d,d,d,d,f,f,f,f,f,f,i,i,i,i,i,i'
      msrcor,set,ra0,dec0,dcr

      if ii eq 0 then begin
         rasig1=[rasig0]
         decsig1=[decsig0]
         b1=[b0]
         bsig1=[bsig0]
         v1=[v0]
         vsig1=[vsig0]
         col1=[col0]
         colsig1=[colsig0]
         nbobs1=[nbobs0]
         nvobs1=[nvobs0]
         nbbad1=[nbbad0]
         nvbad1=[nvbad0]
         nbni1=[nbni0]
         nvni1=[nvni0]
      endif else begin
         rasig1=[rasig1,rasig0]
         decsig1=[decsig1,decsig0]
         b1=[b1,b0]
         bsig1=[bsig1,bsig0]
         v1=[v1,v0]
         vsig1=[vsig1,vsig0]
         col1=[col1,col0]
         colsig1=[colsig1,colsig0]
         nbobs1=[nbobs1,nbobs0]
         nvobs1=[nvobs1,nvobs0]
         nbbad1=[nbbad1,nbbad0]
         nvbad1=[nvbad1,nvbad0]
         nbni1=[nbni1,nbni0]
         nvni1=[nvni1,nvni0]
      endelse
   endfor  ; ii loop

   for jj=0,max(set.objid) do begin
      z=where(set.objid eq jj)

      meanerr,set.x[z],rasig1[z],mra,sigmamra,sigmadra
      meanerr,set.y[z],decsig1[z],mdec,sigmamdec,sigmaddec
      meanerr,b1[z],bsig1[z],mb,sigmamb,sigmadb
      meanerr,v1[z],vsig1[z],mv,sigmamv,sigmadv
      meanerr,col1[z],colsig1[z],mcol,sigmamcol,sigmadcol

      sra = sigmamra > sigmadra
      sdec = sigmamdec > sigmaddec
      sb = sigmamb > sigmadb
      sv = sigmamv > sigmadv
      scol = sigmamcol > sigmadcol

      bobs=total(nbobs1[z])
      vobs=total(nvobs1[z])
      bbad=total(nbbad1[z])
      vbad=total(nvbad1[z])
      nbn=total(nbni1[z])
      nvn=total(nvni1[z])

      if jj eq 0 then begin
         ra=[mra]
         dec=[mdec]
         rasig=[sra]
         decsig=[sdec]
         b=[mb]
         bsig=[sb]
         v=[mv]
         vsig=[sv]
         col=[mcol]
         colsig=[scol]
         nbobs=[bobs]
         nvobs=[vobs]
         nbbad=[bbad]
         nvbad=[vbad]
         nbni=[nbn]
         nvni=[nvn]
      endif else begin
         ra=[ra,mra]
         dec=[dec,mdec]
         rasig=[rasig,sra]
         decsig=[decsig,sdec]
         b=[b,mb]
         bsig=[bsig,sb]
         v=[v,mv]
         vsig=[vsig,sv]
         col=[col,mcol]
         colsig=[colsig,scol]
         nbobs=[nbobs,bobs]
         nvobs=[nvobs,vobs]
         nbbad=[nbbad,bbad]
         nvbad=[nvbad,vbad]
         nbni=[nbni,nbn]
         nvni=[nvni,nvn]
      endelse
   endfor  ; jj loop

   newsubid='Stage2'   ; IMPORTANT!

   print,strn(set.nobj),' objects in the stage 2 catalog'
   z1=where(nbni ge 1 and nvni ge 1,count1)
   z2=where(nbni ge 2 and nvni ge 2,count2)
   z3=where(nbni ge 3 and nvni ge 3,count3)
   print,count1,' from one or more nights (total)'
   print,count2,' from two or more nights only'
   print,count3,' from three or more nights only'

   ; Delete loop so that data is not replicated.
   if not nosave then begin
      cmddel=['delete from plphot where', $
              'id='+quote(id), $
              'and subid='+quote(newsubid)+';']
      mysqlcmd,dblun,cmddel,answer,nlines
   endif

   c=','
   for kk=0,set.nobj-1 do begin
      cmd=['insert into plphot values', $
           '('+quote(id)+c, $                        ; PL2010
           quote(newsubid)+c, $                      ; Stage1
           'NULL'+c, $                               ; refid
           'NULL'+c, $                               ; jd
           string(ra[kk],format='(f11.9)')+c, $      ; ra
           string(dec[kk],format='(f12.9)')+c, $     ; dec
           string(rasig[kk],format='(f11.9)')+c, $   ; rasig
           string(decsig[kk],format='(f11.9)')+c, $  ; decsig
           string(b[kk],format='(f8.5)')+c, $        ; b mag
           string(bsig[kk],format='(f7.5)')+c, $     ; b mag err
           string(v[kk],format='(f8.5)')+c, $        ; v mag
           string(vsig[kk],format='(f7.5)')+c, $     ; v mag err
           string(col[kk],format='(f8.5)')+c, $      ; b-v col
           string(colsig[kk],format='(f7.5)')+c, $   ; b-v col sig
           string(nbobs[kk],format='(i2.1)')+c, $    ; tot b obs
           string(nvobs[kk],format='(i2.1)')+c, $    ; tot v obs
           string(nbbad[kk],format='(i2.1)')+c, $    ; tot b bad
           string(nvbad[kk],format='(i2.1)')+c, $    ; tot v bad
           string(nbni[kk],format='(i2.1)')+c, $     ; # nights for b
           string(nvni[kk],format='(i2.1)')+');']    ; # nights for v
      if not nosave then $
         mysqlcmd,dblun,cmd,answer,nlines
   endfor  ; kk loop
   free_lun,dblun

   if nosave then begin
      print,'WARNING!  Data were not saved from this run.'
   endif

end
