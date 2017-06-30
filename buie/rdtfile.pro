;+
; NAME:
;  rdtfile
; PURPOSE:
;  Reads the template file, tfile.dat, for a given night.
; DESCRIPTION:
;  This program reads in the first line of the tfile.dat for a specific
;  observing night and returns the information the file contains.
; CATEGORY:
;  File I/O 
; CALLING SEQUENCE:
;  rdtfile
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  LOWELL  - Flag, if set will require the tfile to be in a Lowell format
;               to successfully return information.
; OUTPUTS:
;  object  - the object for the image data
;  tempobj - object name of template for the image
;  marker  - unique identifier
;  fnstack - name of image template
;  outsize - size of output difference image
;  degree  - order of variation
; KEYWORD OUTPUT PARAMETERS:
;  obscode - Observatory code for the data.  This is a guess based on
;               various heuristics and from the file format.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURES:
;  Reads in the tfile.dat from an observing night and returns the first
;  line of information.
; MODIFICATION HISTORY:
; 2012/01/24 Written by Erin R. George, Southwest Research Institute
; 2012/03/27, MWB, added support for NMSU data
;-
pro rdtfile,object,tempobj,marker,fnstack,outsize,degree,error, $
       obscode=obscode,LOWELL=lowell

   error=1
   self='(rdtfile): '
   if badpar(lowell,[0,1,2,3],0,caller=self+'(LOWELL) ',default=0) then return

   error=0

   ; Load the template file information
   if not exists('tfile.dat') then begin
      print,self,'The template file descriptor, tfile.dat, does not exist.', $
                 ' Aborting.'
      error=1
      return
   endif

   ; Open the file and read the first line of data.
   openr,lun,'tfile.dat',/get_lun
   line=''
   readf,lun,line,format='(a)'
   free_lun,lun

   ; Parse the template file information
   line=strtrim(strcompress(line),2)
   if line eq 'bad' then begin
      print,self,'The data in the current directory have been marked bad.', $
                 ' Aborting.'
      error=1
      return
   endif

   words=strsplit(line,' ',/extract)

   ; Here's where it gets tricky, the Lowell data will have exactly
   ;   six fields in the data.   The LCO data will have 4.  If no requirements
   ;   are placed on the file then it will adapt to whatever format it finds.

   if lowell and n_elements(words) ne 6 then begin
      print,self,'tfile is not in Lowell format and this was required.'
      error=1
      return
   endif

   if n_elements(words) eq 6 then begin
      object=words[0]
      tempobj=words[1]
      marker=words[2]
      fnstack=words[3]
      outsize=fix(words[4])
      degree=fix(words[5])
      obscode='688'
   endif else if n_elements(words) eq 4 then begin
      object='Pluto'
      tempobj='Pluto'
      marker=words[0]
      fnstack=words[1]
      outsize=fix(words[2])
      degree=fix(words[3])
      obscode='E10'
   endif else if n_elements(words) eq 5 then begin
      object='Pluto'
      tempobj='Pluto'
      marker=words[0]
      fnstack=words[1]
      outsize=fix(words[2])
      degree=fix(words[3])
      obscode=words[4]
   endif else begin
      print,self,'The contents of tfile.dat are incorrect. Aborting.'
      error=1
      return
   endelse

end
