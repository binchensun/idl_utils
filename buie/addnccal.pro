;+
; NAME:
;   addnccal
; PURPOSE:  
;   Add calibration frames for entries in the nasacam calib data base
; DESCRIPTION:
;   Searches the database nasacam.calib table for entries with 
;   status=UNKNOWN and attempts to generate appropriate "sub-supers"  which
;   are superbias, dark, or flat frames for the given range of images. The
;   constraints on nasacam flat taking (in particular) are such that these
;   super frames will usually need to be combined with others nearby in time,
;   thus the term "sub-supers." After generating (without interaction) what it
;   can, addnccal will update the calname for the rows processed. Ordinarily
;   biases are set to status "GOOD" or "PENDING" based on automatic criteria
;   and flats are set PENDING prior to inspection via the tool chknccal.
; CATEGORY:
;    CCD data processing
; CALLING SEQUENCE:
;   addnccal
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
; ABSMOMENT - A 2 element vector giving the maximum value for absolute mean and
;             standard deviation of any superbias generated. This is the
;             baseline test for sub super biases. Current default is
;             [ 5.0, 5.0] based on a cropping region.
;  CALROOT  - string giving the path to locate the calibration images referenced
;             in the data base entries, and created by addnccal. 
;             The current default is
;             '/kolvir/data1/buie/rawfits/nasacamcal' (on kolvir).
;  COLDTEMPS- An vector of 4 elements giving, respectively, the expected
;             CCD detector temperature, the expected cold head temperature,
;             and the maximum absolute deviations from those levels for
;             individual bias or flat frames to be considered good. The NASACAM
;             default is [-100.0, -160.0, 20.0, 50.0].
;             This keyword is not implemented and is currently a no-op.
;  CROP     - region of original image to save, [x1,x2,y1,y2]
;             default is 2006 NASACAM default of [34, 2064, 2, 2046].
;  DATABASE - Name of MYSQL database for calibration entries.
;             The default is 'nasacam'.
;  EARLIEST - string in MYSQL date format giving the earliest row in the 
;             data base to process. The default is the beginning of time.
;  IMGROOT  - string giving the path to locate the image files referenced
;             in the data base entries. The default is 
;             '/kolvir/data1/buie/rawfits' (on kolvir).
;  LATEST   - string in MYSQL date format giving the latest row in the 
;             data base to process. The default is the end of time.
;  MASTBIAS - Absolute path to a fits file which contains a superbias
;             considered good. Superbiases generated by addnccal will be
;             subtracted from  this, and the mean and standard deviation
;             of the resulting array is compared with MASTMOMENT. If not
;             specified, this comparison is not made.
;             This keyword is not implemented and is currently a no-op.
; MASTMOMENT- A 2 element vector giving the maximum absolute mean and standard
;             deviation of the difference array (against MASTBIAS) for a 
;             generated sub-super-bias to be considered good. MASTMOMENT is
;             only meaningful if MASTBIAS is specified. The default is
;             [ 300.0, 30.0]
;             This keyword is not implemented and is currently a no-op.
; NOMEDIANFLAT- Flag, if set, the JUSTMEDIAN flag is not set to MKFLAT 
;             when making flats. The default is to set JUSTMEDIAN,
;             which is passed to mkflat and precludes noise clipping
;             and averaging, when making flats.
;  OLEVEL   - a 4 element vector giving the expected overscan level and
;             the overscan standard deviation, and
;             the maximum permitted absolute deviations from those levels for
;             individual bias or flat frames to be considered good. The NASACAM
;             default is [2800.0, 10.0, 300.0, 30.0].
;             This keyword is not implemented and is currently a no-op.
;  OVERSCAN - column overscan region to use for frame bias level,
;             default is 2006 NASACAM default of [2074, 2103].
;  RDNOISE  - Read noise of CCD, DN/pixel, default is 2006 NASACAM default of 
;             9.0
;  SCALE    - For flats, 4 element vector which, if provide, defines the 
;             array dimensions that are used to scale the mean
;             of the arrays before combining.  If combined in this
;             manner, the arrays are combined weighted by the means.
;                 [x1,x2,y1,y2]
;             These coordinates apply to the pixel locations AFTER cropping.
;             The default is to use the center 50% of the image but not any
;             bigger than 200x200 subsection at the center.
;             This keyword is passed transparently to mkflat.
;  TABLENAME- Name of table in MYSQL database for calibration entries.
;             The default is 'calib'. 
;  TEST     - Flag, if set, addnccal prints the mkbias, mkflat, and db 
;             update commands without running them. It makes the cal
;             subdirectories only.
;  
; OUTPUTS:
; No outputs for this procedure.
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;    Data base entries are updated and calibration superframes are placed in
;    CALROOT. New subdirectories will generally be created in CALROOT.
;    The calibration superframe outputs will follow the naming convention
;    YYYYMMDD.suf
;    where suf is  n.b1    for superbiases
;              and n.f1.f2 for superflats
;    where     n represents the ordinal of the entry for that night among
;              raw calibration images of that class, and f1 and f2 are 
;              positions on the NASACAM filter wheel. 
;              FOR EXAMPLE, if 10 biases and 5 flats each in B,V and methane
;              were taken at each twilght for 20060715, then the cal files
;              would be called
;              20060715.1.b1  20060715.1.11.8  20060715.1.12.2  20060715.1.12.3
;              20060715.2.b1  20060715.2.11.8  20060715.2.12.2  20060715.2.12.3
;
; RESTRICTIONS:
;   Assumes images are located in subdirectories named YYYYMMDD for each night.
;   Places calibration products in paths named YYYY/MM/DD for each night.
;   It currently processes data ONLY for the NASACAM CCD on the 31", and there
;   is no support for dark images.
;   Image frame numbers must be in the range 1-999 inclusive.
; 
; PROCEDURE:
; MODIFICATION HISTORY:
;   2006/7/15, Written by Peter L. Collins, Lowell Observatory
;   2006/7/24, PLC, modified after adding new column startime to calib table,
;                   adding "PENDING" as a value for status,
;                   changes to order of bias/flat evaluation- in
;                   particular, designed to make good/bad choice
;                   on sub-super-bias whenever possible automatically.
;   2006/07/26, PLC,added support from empty returns from mysqlquery.
;   2006/07/27, PLC,general cleanup and mark unimplemented keywords in header.
;
;-

pro  addnccal, DATABASE=database,TABLENAME=tablename,IMGROOT=imgroot, $
                CALROOT=calroot,LATEST=latest,EARLIEST=earliest, $
                OVERSCAN=overscan,CROP=crop,RDNOISE=rdnoise, $
                TEST=test,OLEVEL=in_olevel,MASTMOMENT=in_mastmoment, $
                MASTBIAS=mastbias, COLDTEMPS=in_coldtemps, $
                ABSMOMENT=in_absmoment,SCALE=scale,NOMEDIANFLAT=nomedianflat


   self='ADDNCCAL: '

;  NASACAM image defaults
   nasacam2006_overscan=[2069,2103]
   nasacam2006_crop=[21,2064,3,2046]
   nasacam2006_scale=[-1,-1,-1,-1]
   nasacam2006_rdnoise=9.0

;  NASACAM test threshold defaults
   nasacam2006_coldtemps=[-100.0, -160.0, 20.0, 50.0]
   nasacam2006_olevel=[2800.0, 10.0, 300.0, 30.0]
   nasacam2006_mastmoment=[300.0, 30.0]

   if badpar(database,[0,7],0,caller=self+'(DATABASE) ',  $
             default='nasacam') then return
   if badpar(tablename,[0,7],0,caller=self+'(TABLENAME) ',  $
             default='calib') then return
   if badpar(imgroot,[0,7],0,caller=self+'(IMGROOT) ',  $
            default='/kolvir/data1/buie/rawfits') then return
   if badpar(calroot,[0,7],0,caller=self+'(CALROOT) ',  $
             default='/kolvir/data1/buie/rawfits/nasacamcal') then return
   if badpar(latest,[0,7],0,caller=self+'(LATEST) ',  $
             default='') then return
   if badpar(earliest,[0,7],0,caller=self+'(EARLIEST) ',  $
             default='') then return
   if badpar(overscan,[0,2,3],1,caller=self+'(OVERSCAN) ', $
             default=nasacam2006_overscan) then return
   if badpar(crop, [0,2,3],1,caller=self+'(CROP) ', $
             default=nasacam2006_crop) then return
   if badpar(scale, [0,2,3],1,caller=self+'(SCALE) ', $
             default=nasacam2006_scale) then return
   if badpar(rdnoise,[0,2,3,4,5],0,caller=self+'(RDNOISE) ', $
             default=nasacam2006_rdnoise) then return
   if badpar(test,[0,1,2,3,12],0,caller=self+'(TEST) ', $
             default=0) then return
   if badpar(in_olevel,[0,1,2,3,4,5,12],1,caller=self+'(OLEVEL) ', $
             default=nasacam2006_olevel) then return
   if badpar(in_coldtemps,[0,1,2,3,4,5,12],1,caller=self+'(COLDTEMPS) ', $
             default=nasacam2006_coldtemps) then return
   if badpar(mastbias,[0,7],0,caller=self+'(MASTBIAS) ',  $
             default='') then return
   if badpar(in_mastmoment,[0,1,2,3,4,5,12],1,caller=self+'(MASTMOMENT) ', $
             default=nasacam2006_mastmoment) then return
   if badpar(in_absmoment,[0,1,2,3,4,5,12],1,caller=self+'(ABSMOMENT) ', $
             default=[5.0,5.0]) then return
   if badpar(nomedianflat,[0,1,2,3,12],0,caller=self+'(NOMEDIANFLAT) ', $
             default=0) then return

   justmedian = 1
   if nomedianflat then justmedian = 0

   ; test dates.
   sqldtrexp='[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]'
   if earliest ne '' and not stregex(earliest,sqldtrexp,/BOOLEAN) then begin
      print, self + 'EARLIEST keyword ill formed for SQL DATE: (', earliest,')'
      return
   endif
   if latest ne '' and not stregex(latest,sqldtrexp,/BOOLEAN) then begin
      print, self + 'LATEST ill formed for SQL DATE: (', latest,')'
      return
   endif

   ; test root directories.
   if not file_test(imgroot,/READ,/DIRECTORY) then begin
      print, self + 'IMGROOT: (', imgroot, ') must be a read enabled dir.'
      return
   endif
   if not file_test(calroot,/READ,/WRITE,/DIRECTORY) then begin
      print, self + 'CALROOT: (', calroot, ') must be a read/write enabled dir.'
      return
   endif

   ;  Initially locate all entries in the data base with status not 'bad'
   ;  and find entries with startime NULL. These will be given good start
   ;  times from the image table. (The startime values for a sequence of 
   ;  images is taken to be the midtime of the first... )
   ;
   ;  This would also be a place to put internal consistency checking of the
   ;  data base, directories, and files.
   what ='ObsDate,type,num1,num2,calname,status,startime '

   datequalifier= ''
   if earliest ne '' then begin
      datequalifier += ' ObsDate >= ' + quote(earliest)
      if latest ne '' then datequalifier += ' and ObsDate <= ' + quote(latest)
   endif else begin
      if latest ne '' then datequalifier += ' ObsDate <= ' + quote(latest)
   endelse
   if datequalifier eq '' then begin
      datequalifier = ' where '
   endif else begin
      datequalifier = ' where ' + datequalifier +  ' and '
   endelse


   statusqualifier = " status != 'bad' " 

   orderqualifier = " order by ObsDate, num1 "

   semico=';'
   query='select ' + what + ' from ' + tablename +  datequalifier  + $
          statusqualifier + orderqualifier + semico

   if test then print, self, 'frame query: ', query
   ;open database
   openmysql,dblun,database
   mysqlquery,dblun,query,ObsDate,type,frame1,frame2, calname, $
                                status,startime,format='a,a,i,i,a,a,a'
   ; note that this query should always return something..
   ; loop is a placeholder for other checking.
   for i = 0,n_elements(ObsDate) - 1 do begin 
      if startime[i] eq 'NULL' then begin
         framewhat = 'type, midtime'
         root = strjoin(strsplit(ObsDate[i],'-',/extract))
         filequalifier = 'where filename= ' + $
                          quote(root + '.' + string(frame1[i], format="(i3.3)"))
         query='select ' + framewhat + ' from ' + ' image ' + filequalifier $
                + semico
         if test then print, self, 'frame startime query: ', query
         mysqlquery,dblun,query,frametype,startime_val, format='a,a'

         ; checking  to see if query got anything. 
         if  strlen(startime_val) gt 8  then begin
            framequalifier = ' ObsDate= ' + quote(ObsDate[i]) + ' and ' + $
                             ' num1= ' + string(frame1[i], format="(i3.3)")

            updateframe = 'update ' + tablename + ' set startime=' + $
                         quote(startime_val[0])
            updateframe += ' where ' + framequalifier + semico
            if not test then begin
               mysqlcmd, dblun, updateframe,updateresp,nlines
            endif else begin
               print, 'update frame sql cmd: ', updateframe
            endelse
         endif
      endif
   endfor

   ; find all bias entries with an sql query- the biases with status='unknown'
   ; need to get sub-super-biases generated. Biases with status='pending'
   ; are a show stopper till the pending issue is resolved.

   what=' ObsDate,num1,num2,calname,status '
   typequalifier =  " type = 'bias' "
   statusqualifier = " ( status = 'unknown' or status = 'pending' ) "

   orderqualifier = " order by ObsDate, num1 "
    
   query='select ' + what + ' from ' + tablename +  datequalifier  + $
           typequalifier + ' and ' + statusqualifier + orderqualifier + semico


   if test then print, self, 'bias query: ', query
   mysqlquery,dblun,query,ObsDate,frame1,frame2, calname, status, $
              format='a,i,i,a,a'

   ; check to see if query got anything.
   if ObsDate[0] eq ''  then begin
      print, ' there are no bias frames to be processed'
      nbiastodo = 0
   endif else begin
      nbiastodo = n_elements(ObsDate)

      pends = where(status eq 'pending', npends)
      if npends gt 0 then begin
         print, 'there are pending bias frames for: '
         print,  ObsDate[pends]
         print, "Please fix!"
         return
      endif
   endelse

   for i = 0, nbiastodo-1 do begin
      datecomponents = strsplit(Obsdate[i], '-',/extract)
      root = strjoin(datecomponents)
      calpath = strjoin(datecomponents,'/')
      indir = addslash(imgroot) + root
      if not file_test(indir,/READ,/DIRECTORY) then begin
         print, self, 'Directory ', indir, $
                ' is not read accessible or does not exist, aborting.'
         return
      endif

      outdir = addslash(calroot) + calpath
      ; now you have to build outdir and all of its components if they do
      ; not already exist.
      if not file_test(outdir,/READ,/WRITE,/DIRECTORY) then begin
         cd, calroot
         file_mkdir, './' + datecomponents[0],/NOEXPAND_PATH 
         file_mkdir, './' + datecomponents[0] + '/' + datecomponents[1], $
                 /NOEXPAND_PATH 
         file_mkdir, './' + datecomponents[0] + '/' + datecomponents[1] $
                       + '/' + datecomponents[2], /NOEXPAND_PATH 
      endif
      cd, outdir
      if not file_test('.',/READ,/WRITE,/DIRECTORY) then begin
         print, self, 'Failure creating directory ', outdir, ' aborting.'
         return
      endif
      

      f1 = frame1[i]
      nframes = frame2[i] - f1 + 1

      biassuffix=string(f1, format="(i3.3,'.bias')")

      if not test  then begin 
         mkbias,root + '.',biassuffix,f1,nframes,dumyimg, $
                DDIR=indir,CROP=crop,OVERSCAN=overscan,RDNOISE=rdnoise
      m0 = mean( dumyimg[200:1500,200:1500])
      m1 = stddev( dumyimg[200:1500,200:1500],/DOUBLE)
      if abs(m0) < in_absmoment[0] and m1 < in_absmoment[1] then begin
         biasstatus = 'good' 
      endif else begin
         biasstatus = 'pending'
      endelse
      print, ' sub super bias', root + '.' + biassuffix,   '( ', $
               m0, m1, ')-', biasstatus

      ;unload
      dumyimg=0
      endif else begin
         print, 'mkbias,',root + '.' + ', ',biassuffix, ', ', $
                 f1, ', ',nframes, ', ', $
                'dumyimg,', 'DDIR =', indir, ', ', ' CROP=', crop, ', ', $
                'OVERSCAN=',overscan, 'RDNOISE=', rdnoise/gain
      endelse

      calname[i] = root + '.' + biassuffix
      ; update db
      updatecal = 'update ' + tablename + " set type='bias', " + $
                  ' calname=' + quote(root + '.' + biassuffix) + ', ' + $
                   'status = ' + quote(biasstatus) 
      datespec = "ObsDate =" + quote(ObsDate[i]) 
      framespec = ' and num1 = ' + quote(string(f1, format="(i3.3)"))
      framespec += ' and num2 = ' + quote(string(frame2[i], $
                    format="(i3.3)"))
      updatecal += ' where ' + datespec + framespec + semico

      if not test then begin
         mysqlcmd, dblun, updatecal,updateresp,nlines
      endif else begin
         print, 'update sql cmd: ', updatecal
      endelse
      if biasstatus eq 'pending' then begin
         print, 'there is a pending bias frame for: '
         print,  ObsDate[i]
         print, "Please fix!"
         return
      endif
   endfor

   ; go find all flats needing to be processed.
   what=' ObsDate,num1,num2,calname,status,startime,filt1,filt2 '
   typequalifier =  " type = 'flat' "
   statusqualifier = " status = 'unknown' "

   orderqualifier = " order by ObsDate, num1 "
    
   query='select ' + what + ' from ' + tablename +  datequalifier  + $
           typequalifier + ' and ' + statusqualifier + orderqualifier + semico

   if test then print, self, 'flat query: ', query
   mysqlquery,dblun,query,ObsDate,frame1,frame2, calname, status,startime, $
              filt1,filt2,format='a,i,i,a,a,a,i,i'

   ; test to see if the query came back empty.
   if ObsDate[0] eq ''  then begin
      print, ' there are no flat frames to be processed'
      nflatstodo = 0
      return
   endif else begin
      nflatstodo = n_elements( ObsDate)
   endelse

   ; process the flats.
   for i = 0, nflatstodo-1 do begin
      datecomponents = strsplit(Obsdate[i], '-',/extract)
      root = strjoin(datecomponents)
      calpath = strjoin(datecomponents,'/')
      indir = addslash(imgroot) + root
      if not file_test(indir,/READ,/DIRECTORY) then begin
         print, self, 'Directory ', indir, $
                ' is not read accessible or does not exist, aborting.'
         return
      endif

      outdir = addslash(calroot) + calpath
      ; now you have to build outdir and all of its components if they do
      ; not already exist.
      if not file_test(outdir,/READ,/WRITE,/DIRECTORY) then begin
         cd, calroot
         file_mkdir, './' + datecomponents[0],/NOEXPAND_PATH 
         file_mkdir, './' + datecomponents[0] + '/' + datecomponents[1], $
                 /NOEXPAND_PATH 
         file_mkdir, './' + datecomponents[0] + '/' + datecomponents[1] $
                       + '/' + datecomponents[2], /NOEXPAND_PATH 
      endif
      cd, outdir
      if not file_test('.',/READ,/WRITE,/DIRECTORY) then begin
         print, self, 'Failure creating directory ', outdir, ' aborting.'
         return
      endif
      
      flatime = startime[i]

      ; find the nearest (in time)  sub superbias for processing the next flat.
      what=' ObsDate,num1,num2,calname,status,startime '
      typequalifier =  " type = 'bias' "
      statusqualifier = " status = 'good' "
      orderqualifier = ' order by ( '
      orderqualifier += ' ABS( unix_timestamp(startime ) -  unix_timestamp(' $
                          + quote( flatime) + ' ) ) )  '
      scopequalifier = ' limit 1'
      query='select ' + what + ' from ' + tablename +  ' where ' + $
             typequalifier  + ' and ' + statusqualifier +  orderqualifier + $
             scopequalifier +  semico

      if test then print, self, 'closest bias query: ', query
      mysqlquery,dblun,query,ObsDateb,frame1b,frame2b, calnameb, statusb, $
              startimeb, format='a,i,i,a,a,a'

      ; test to see if the query came back empty.
      if  ObsDateb[0] eq '' then begin
         print, 'there are no good biases to process flats, quitting..'
         return
      endif

      biaspath= addslash(calroot) + $
      strjoin(strsplit(ObsDateb[0],'-',/extract), '/') +   '/' + calnameb[0]
      print, ' flat frames ' ,  $
               string( frame1[i], frame2[i], format="(i3.3,'-',i3.3)")
      print, 'for ' , ObsDate[i]
      print, ' with sub super bias', biaspath

      flatsuffix=string(frame1[i], filt1[i],filt2[i], $
                             format="(i3.3,'.', i2, '+', i2)")
      flatsuffix = strjoin(strsplit(flatsuffix,/extract))
      nframes = frame2[i] - frame1[i] + 1
      if not test  then begin 
         if scale[0] eq -1 then begin
            mkflat,root + '.',flatsuffix,frame1[i],nframes, $
                   biaspath,nodark,dumyimg,DDIR=indir, $
                   CROP=crop,OVERSCAN=overscan,RDNOISE=rdnoise, $
                   JUSTMEDIAN=justmedian
         endif else begin
            mkflat,root + '.',flatsuffix,frame1[i],nframes, $
                   biaspath,nodark,dumyimg,DDIR=indir, $
                   CROP=crop,OVERSCAN=overscan,RDNOISE=rdnoise, $
                   SCALE=scale,JUSTMEDIAN=justmedian
         endelse
         ;unload
         dumyimg=0
      endif else begin
         if scale eq [-1,-1,-1,-1] then begin
            print, 'mkflat,',root + '.', flatsuffix, ', ', $
                    frame1[i],' , ', nframes,' , ',  $
                    biaspath,', nodark,dumyimg,', $
                   'DDIR =', indir, ' , ', ' CROP=', crop, ' , ', $
                   'OVERSCAN=',overscan, 'JUSTMEDIAN=',justmedian
         endif else begin
            print, 'mkflat,',root + '.', flatsuffix, ', ', $
                    frame1[i],' , ', nframes,' , ',  $
                    biaspath,', nodark,dumyimg,', $
                   'DDIR =', indir, ' , ', ' CROP=', crop, ' , ', $
                   'OVERSCAN=',overscan, 'SCALE=',scale, $
                   'JUSTMEDIAN=',justmedian
         endelse
      endelse
      ; update db
      updatecal = 'update ' + tablename + $
      " set type='flat', status='pending', "  + " calname=" + $
                  quote(root + '.' + flatsuffix)
      datespec = "ObsDate =" + quote(ObsDate[i]) 
      framespec = ' and num1 = ' + quote(string(frame1[i],format="(i4.4)"))
      framespec += ' and num2 = ' + quote(string(frame2[i],format="(i4.4)"))
      updatecal += ' where ' + datespec + framespec + semico
      if not test then begin
         mysqlcmd, dblun, updatecal,updateresp,nlines
      endif else begin
         print, 'update sql cmd: ', updatecal
      endelse
   endfor
   free_lun, dblun
   return
end
