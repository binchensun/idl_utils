;+
; NAME:
;  im2vid
; PURPOSE:   (one line only)
;  Convert a sequence of images to a video file
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  im2vid,filepatt,outfile
; INPUTS:
;  filepatt - String with a file search pattern that will pick up the files
;                you want.  These files will be loaded in the sorted order.
;                Valid file formats are checked with QUERY_IMAGE
;  outfile  - Name of output video file.  The file type is determined from
;                the file name.  Valid options are .mp4 and .avi and are
;                dictated by IDLffVdeoWrite.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FRAME_RATE - Number of frames per second to display.  (default=15)
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2012/07/30
;-
pro im2vid,filepatt,outfile,FRAME_RATE=fps,VERBOSE=verbose,LOOPCOUNT=loopcount

   self='im2vid: '
   if badpar(filepatt,7,0,caller=self+'(filepatt) ') then return
   if badpar(outfile,7,0,caller=self+'(outfile) ') then return
   if badpar(fps,[0,2,3,4,5],0,caller=self+'(FRAME_RATE) ', $
                               default=15) then return
   if badpar(loopcount,[0,2,3,4,5],0,caller=self+'(LOOPCOUNT) ', $
                               default=1) then return
   if badpar(verbose,[0,1,2,3,4,5],0,caller=self+'(VERBOSE) ', $
                               default=0) then return

   ; Get the list of file names
   fnlist=file_search(filepatt,count=nfiles)
   if nfiles eq 0 then begin
      print,'File search pattern [',filepatt,']'
      print,'did not return any files.'
      return
   endif

   ; Check the first file name for being a support image type
   if not query_image(fnlist[0]) then begin
      print,'The first file, ',fnlist[0],' is not a supported image type.'
      return
   endif

   if verbose then $
      print,strn(nfiles),' files found matching the pattern ',filepatt

   bim=read_image(fnlist[0])

   sz=size(bim,/structure)
   color=sz.n_dimensions eq 3
   if color then begin
      xs=sz.dimensions[1]
      ys=sz.dimensions[2]
   endif else begin
      xs=sz.dimensions[0]
      ys=sz.dimensions[1]
   endelse

   ovid=idlffvideowrite(outfile)
   vidstream=ovid.addvideostream(xs,ys,fps)

   for i=0,loopcount-1 do begin
      for j=0,nfiles-1 do begin
         bim=read_image(fnlist[j])
         sz=size(bim,/structure)
         color=sz.n_dimensions eq 3
         if color then begin
            cim=bim
         endif else begin
            cim=bytarr(3,xs,ys)
            for k=0,2 do cim[k,*,*]=bim
         endelse
         time=ovid.put(vidstream,cim)
         if verbose then print,i,j,time
      endfor
   endfor

   ovid.cleanup

end
