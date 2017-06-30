;+
; NAME:
;  cloneast
; PURPOSE:   (one line only)
;  Clone astrometric solutions from one night to another
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  cloneast,otherdir,offset
; INPUTS:
;  otherdir - String pointing to the other directory from which to get
;                astrometric solution
;  offset   - Numeric offset of file name.  Files are named YYMMDD.nnn in
;                each directory.  When linked, the root name is changed and
;                the other frame has the offset added to the suffix number.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OVERWRITE - if set, overwrite solution, otherwise skip frames with
;                 pre-existing solutions
;  MAXOTHER  - Largest image number to copy solution from in the other
;                 directory.  Default=199.  This prevents cloning a solution
;                 that has already been cloned.  Set to 0 to suppress this
;                 check.
;  DIGITS    - number of digits for the suffix of the image name, default=3
;  NOSAVE    - flag, if set, prevents making any changes to files or copying
;                 anything.  Use this for a dry run to see if it's going to
;                 work ok.
;  RANGE     - Optional numeric range of things to clone from the other
;                 directory.  This is usually not required if you are providing
;                 values in offset and are using MAXOTHER.  If these two
;                 inputs are set to zero then RANGE is a required input.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/06/23
;  2003/07/03, MWB, added MAXOTHER keyword
;  2004/07/21, MWB, added creation of symbolic links for srd and psf files
;  2009/08/05, MWB, modified for new rd/wrastfc versions
;  2013/09/03, MWB, added DIGITS, NOSAVE, and RANGE keywords, further mods
;                     for even newer rd/wrastfc
;-
pro cloneast,otherdir,offset, $
       OVERWRITE=overwrite,MAXOTHER=maxother,DIGITS=digits,NOSAVE=nosave, $
       RANGE=range

   self='cloneast: '
   if badpar(otherdir,7,0,caller=self+'(otherdir) ') then return
   if badpar(offset,[2,3],0,caller=self+'(offset) ') then return
   if badpar(overwrite,[0,1,2,3],0,caller=self+'(OVERWRITE) ',default=0) then return
   if badpar(maxother,[0,1,2,3],0,caller=self+'(MAXOTHER) ',default=199) then return
   if badpar(digits,[0,1,2,3],0,caller=self+'(DIGITS) ',default=3) then return
   if badpar(nosave,[0,1,2,3],0,caller=self+'(NOSAVE) ',default=0) then return
   if badpar(range,[0,2,3],1,caller=self+'(RANGE) ',default=[-1,-1]) then return

   if offset eq 0 and maxother eq 0 and min(range) lt 0 then begin
      print,'RANGE must be provided with offset=0 and MAXOTHER=0'
      return
   endif

   otherdir = addslash(otherdir)

   ;Check to see if the Refstars directory exists in current directory
   if not exists('Refstars') then begin
      print,'Current directory has no astrometry.  Aborting.'
      return
   endif

   ;Check to see if the Refstars directory exists in other directory
   if not exists(otherdir+'Refstars') then begin
      print,'Other directory, ',otherdir,', has no astrometry.  Aborting.'
      return
   endif

   ;Check for psf directory in both places, if found, will clone the relevant
   ;  srd files and psf files.
   clonepsf = exists('psf') and exists(otherdir+'psf')

   ;Get a list of all solutions in other directory (ie., anything in Refstars)
   fnref = file_search(addslash(otherdir+'Refstars')+'*.ref',count=nref)
   print,strn(nref),' solutions found in ',otherdir

   ;Load the fitcoeff.dat file from the other directory
   rdastfc,otherdir+'fitcoeff.dat',offn,oftype,oxc,oyc,oprot,orenorm, $
      ocra,ocdec,ophotzp,terms,ocoeffarr,oncoeffs,onlines
   print,strn(onlines),' entries in other fitcoeff.dat file.'

   ;Load the centers.dat file from other directory
   ocnlines = file_lines(otherdir+'centers.dat',/noexpand_path)
   octag=strarr(ocnlines)
   ocinfo=strarr(ocnlines)
   openr,lun,otherdir+'centers.dat',/get_lun
   line=''
   for i=0,ocnlines-1 do begin
      readf,lun,line,format='(a)'
      pos=strpos(line,' ')
      octag[i]  = strmid(line,0,pos)
      ocinfo[i] = line
   endfor
   free_lun,lun
   print,strn(ocnlines),' entries in other centers.dat file.'

   ;Load the fitcoeff.dat file from the other directory
   rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renorm, $
      cra,cdec,photzp, $
      flagarr,coeffarr,ncoeffs,nlines
   print,strn(nlines),' entries in local fitcoeff.dat file.'

   ;Load the centers.dat file from current directory
   cnlines = file_lines('centers.dat',/noexpand_path)
   ctag=strarr(cnlines)
   cinfo=strarr(cnlines)
   openr,lun,'centers.dat',/get_lun
   line=''
   for i=0,cnlines-1 do begin
      readf,lun,line,format='(a)'
      pos=strpos(line,' ')
      ctag[i]  = strmid(line,0,pos)
      cinfo[i] = line
   endfor
   free_lun,lun
   print,strn(cnlines),' entries in local centers.dat file.'

   ; figure out the rundate of the other data
   words = strsplit(octag[0],'.',/extract)
   orundate = words[0]
   print,'Run date in other directory is ',orundate

   ; figure out the rundate of the local data
   words = strsplit(ctag[0],'.',/extract)
   rundate = words[0]
   print,'Run date in local directory is ',rundate

   if rundate eq orundate then begin
      print,'Error, current and other rundates must be different!'
      return
   endif

   cdirty=0
   fdirty=0
   ;Loop over fnref and copy files and solutions
   numfmt='i'+strn(digits)+'.'+strn(digits)
   for i=0,nref-1 do begin
      print,fnref[i]
      oldref = strmid(fnref[i],strlen(fnref[i])-6-digits,999)
      tmp = strsplit(oldref,'.',/extract)
      words = strsplit(tmp[0],'x',/extract)
      onum = fix(words[0])
      if onum le maxother or maxother eq 0 then begin
         if min(range) ge 0 and $
            (onum lt range[0] or onum gt range[1]) then continue
         num = onum + offset
         chip = fix(words[1])
         newref = string(num,chip,format='('+numfmt+',"x",i1,".ref")')
         ofn = string(orundate,onum,chip,format='(a,".",'+numfmt+',"x",i1)')
         fn = string(rundate,num,chip,format='(a,".",'+numfmt+',"x",i1)')

         ; copy refstar file
         if not exists('Refstars/'+newref) or overwrite then begin
            print,fnref[i],' --> Refstars/',newref
            if not nosave then $
               file_copy,fnref[i],'Refstars/'+newref, $
                  /noexpand_path,overwrite=overwrite
         endif

         if clonepsf then begin
            if chip eq 1 then begin
               oldpsf = string(orundate,onum,format='(a,".",'+numfmt+',".psf")')
               oldpsf = otherdir+'psf/'+oldpsf
               if strmid(oldpsf,0,2) eq '..' then oldpsf = '../'+oldpsf
               newpsf = string(rundate,num,format='(a,".",'+numfmt+',".psf")')
               newpsf = 'psf/'+newpsf
               if exists(newpsf) and overwrite and not nosave then $
                  file_delete,newpsf,/quiet
               if not exists(newpsf) and not nosave then $
                  file_link,oldpsf,newpsf
            endif
            oldsrd = string(orundate,onum,chip, $
                               format='(a,".",'+numfmt+',".srdx",i1)')
            oldsrd = otherdir+oldsrd
            newsrd = string(rundate,num,chip, $
                               format='(a,".",'+numfmt+',".srdx",i1)')
            if exists(newsrd) and overwrite and not nosave then $
               file_delete,newsrd,/quiet
            if not exists(newsrd) and not nosave then $
               file_link,oldsrd,newsrd
         endif

         ; copy centers.dat entry to internal arrays
         oz=where(strpos(octag,ofn) ge 0,ocount)
         if ocount gt 0 then begin
   ;         print,ofn,ocount,ocinfo[oz[0]]
            z=where(strpos(ctag,fn) ge 0,count)
            if count eq 0 then begin
               print,' add ',ofn,' as ',fn
               newinfo=ocinfo[oz[0]]
               strput,newinfo,fn
               print,newinfo
               ctag = [ctag,fn]
               cinfo = [cinfo,newinfo]
            endif else if count eq 1 and overwrite then begin
               print,' replace ',ofn,' as ',fn
               newinfo=ocinfo[oz[0]]
               strput,newinfo,fn
               print,newinfo
               cinfo[z[0]] = newinfo
               cdirty=1
            endif
         endif else begin
            print,'warning, center not found for ',ofn
         endelse

         ; copy fitcoeff entries to internal arrays
         oz=where(ofn eq offn,ocount)
         if ocount eq 2 then begin
            print,ofn,' fit--> ',fn
            z=where(fn eq ffn,count)
            if count eq 0 then begin
               print,' add ',ofn,' as ',fn
               ffn      = [ffn,     fn,fn          ]
               ftype    = [ftype,   oftype[oz]     ]
               xc       = [xc,      oxc[oz]        ]
               yc       = [yc,      oyc[oz]        ]
               prot     = [prot,    oprot[oz]      ]
               renorm   = [renorm,  orenorm[oz]    ]
               cra      = [cra,     ocra[oz]       ]
               cdec     = [cdec,    ocdec[oz]      ]
               photzp   = [photzp,  ophotzp[oz]    ]
               coeffarr = [coeffarr,ocoeffarr[oz,*]]
               ncoeffs  = [ncoeffs, oncoeffs[oz]   ]
               nlines   = nlines+2
               fdirty=1
            endif else if count eq 2 and overwrite then begin
               print,' replace ',ofn,' as ',fn
               ffn[z]        = fn
               ftype[z]      = oftype[oz]
               xc[z]         = oxc[oz]
               yc[z]         = oyc[oz]
               prot[z]       = oprot[oz]
               renorm[z]     = orenorm[oz]
               cra[z]        = ocra[oz]
               cdec[z]       = ocdec[oz]
               photzp[z]     = ophotzp[oz]
               coeffarr[z,*] = ocoeffarr[oz,*]
               ncoeffs[z]    = oncoeffs[oz]
               fdirty=1
            endif
         endif else if ofn ne 0 then begin
            print,'warning, too many solutions found for ',ofn
         endif else begin
            print,'warning, solution not found for ',ofn
         endelse

      endif else begin
         print,'Skip ',fnref[i]
      endelse

   endfor

   ; write out the revised centers.dat file
   if cnlines ne n_elements(ctag) or cdirty then begin
      print,strn(n_elements(ctag)-cnlines),' lines added to centers.dat'
      z=sort(ctag)
      if exists('centers.dat') and not nosave then $
         file_move,'centers.dat','centers.dat.bak', $
            /noexpand_path,/overwrite,/verbose
      print,'Writing new centers.dat file.'
      if not nosave then begin
         openw,lun,'centers.dat'
         for i=0,n_elements(ctag)-1 do begin
            printf,lun,cinfo[z[i]]
         endfor
         free_lun,lun
      endif
   endif

   ; write out the revised fitcoeff.dat file
   if fdirty then begin
      z=sort(ffn)
      if exists('fitcoeff.dat') and not nosave then $
         file_move,'fitcoeff.dat','fitcoeff.dat.bak', $
            /noexpand_path,/overwrite,/verbose
      print,'Writing new fitcoeff.dat file.'
      if not nosave then begin
         wrastfc,'fitcoeff.dat',ffn[z],ftype[z],xc[z],yc[z],prot[z],renorm[z], $
            cra[z],cdec[z],photzp[z],terms,coeffarr[z,*]
      endif
   endif

   if nosave then begin
      print,self,'NOSAVE flag set, no actual changes were made.'
   endif

end
