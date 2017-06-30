;+
; NAME:
;  findobji
; PURPOSE:
;  Locate image changes with 3-plane color overlays (array version)
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  findobji,image1,image2[,image3]
;
; INPUTS:
;
;  First input format:
;     image1 - Image #1 (red)
;     image2 - Image #2 (green)
;     image3 - Image #3 to load into the blue image plane (default=image2)
;
;  images are all arrays
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  BINFAC  - binning factor for image display, default=1
;  QUEUE   - Printer queue for hardcopy, default is chani for color and
;              office for b/w
;
; OUTPUTS:
;  No explicit outputs but you are prompted for a final hardcopy in either
;    color or black and white.
;
; KEYWORD OUTPUT PARAMETERS:
;  CCUBE   - If provided, this keyword returns a three dimensional byte array
;              of the final stacked color cube displayed in window 3.
;  FCUBE   - If provided, you get the floating point image stack
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  You must have a 24-bit color display to use this program.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  2012/08/16 - Written by Marc W. Buie, Southwest Research Institute, cloned
;                 from findobj.pro
;-
pro findobji,in_im1,in_im2,in_im3, $
     NOCLEAN=noclean,PATH=path,QUEUE=queue,SAVECLEAN=saveclean,EXTEN=exten, $
     BINFAC=binfac,CCUBE=ccube,NOSCALE=noscale,FCUBE=fcube

   self='findobj: '
   if badpar(in_im1,[2,3,4,5],2,caller=self+'(image1) ') then return
   if badpar(in_im2,[2,3,4,5],2,caller=self+'(image2) ') then return
   if badpar(in_im3,[0,2,3,4,5],[0,2],caller=self+'(image3) ', $
                default=in_im2) then return
   if badpar(queue,[0,7],0,CALLER=self+'(QUEUE) ', $
                           default='[[default]]') then return
   if badpar(binfac,[0,1,2,3],0,CALLER=self+'(BINFAC) ',default=1) then return

   fmt='(a,".",i3.3)'
   cfmt='(a,"c.",i3.3)'

   im1=in_im1
   im2=in_im2
   im3=in_im3

   if binfac ne 1 then begin
      arrsz=size(im1)
      nx=(arrsz[1]/binfac)*binfac
      ny=(arrsz[2]/binfac)*binfac
      im1=rebin(im1[0:nx-1,0:ny-1],nx/binfac,ny/binfac)
      im2=rebin(im2[0:nx-1,0:ny-1],nx/binfac,ny/binfac)
      im3=rebin(im3[0:nx-1,0:ny-1],nx/binfac,ny/binfac)
   endif

   sz=size(im1)
   nx = sz[1]
   ny = sz[2]

   ; Display image and setup coordinate system on display.
   dbinfac = 2
   dim1 = rebin(im1[0:nx/dbinfac*dbinfac-1,0:ny/dbinfac*dbinfac-1], $
                   nx/dbinfac,ny/dbinfac,sample=0)
   dim2 = rebin(im2[0:nx/dbinfac*dbinfac-1,0:ny/dbinfac*dbinfac-1], $
                   nx/dbinfac,ny/dbinfac,sample=0)
   dim3 = rebin(im3[0:nx/dbinfac*dbinfac-1,0:ny/dbinfac*dbinfac-1], $
                   nx/dbinfac,ny/dbinfac,sample=0)

   ls = -3
   hs = 5
   skysclim,dim1,low1,hi1,mean1,sigma1,npts=50000
   low1 = mean1+ls*sigma1  ; 7
   hi1  = mean1+hs*sigma1 ; 16
   skysclim,dim2,low2,hi2,mean2,sigma2,npts=50000
   low2 = mean2+ls*sigma2
   hi2  = mean2+hs*sigma2
   skysclim,dim3,low3,hi3,mean3,sigma3,npts=50000
   low3 = mean3+ls*sigma3
   hi3  = mean3+hs*sigma3

print,mean1,sigma1
print,mean2,sigma2
print,mean3,sigma3

   setwin,0,xsize=nx/dbinfac,ysize=ny/dbinfac,/show
   bim1 = bytscl(dim1,min=low1,max=hi1,top=255)
   tv,bim1
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,sz[1]-1],yr=[0,sz[2]-1],xstyle=5,ystyle=5,/noerase

   print,'Click on a reference star'
   cursor,xloc1,yloc1,3
   if !mouse.button eq 4 then return
   basphote,5.5,im1,1.0,xloc1,yloc1,10,20,40,/nolog,/silent, $
      xcen=x1m,ycen=y1m,flux=fl1
   oplot,[x1m],[y1m],psym=4,color='ff0000'xl
   x1m = fix(x1m+0.5)
   y1m = fix(y1m+0.5)

   setwin,1,xsize=nx/dbinfac,ysize=ny/dbinfac,/show
   bim2 = bytscl(dim2,min=low2,max=hi2,top=255)
   tv,bim2

   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,sz[1]-1],yr=[0,sz[2]-1],xstyle=5,ystyle=5,/noerase
   oplot,[-20,20,20,-20,-20]+x1m,[-20,-20,20,20,-20]+y1m,color='ff2000'xl
   cursor,xloc2,yloc2,3
   if !mouse.button eq 4 then return
   basphote,5.5,im2,1.0,xloc2,yloc2,10,20,40,/nolog,/silent, $
      xcen=x2m,ycen=y2m,flux=fl2
   x2m = fix(x2m+0.5)
   y2m = fix(y2m+0.5)
   oplot,[x2m],[y2m],psym=4,color='ff0000'xl

   setwin,2,xsize=nx/dbinfac,ysize=ny/dbinfac,/show
   bim3 = bytscl(dim3,min=low3,max=hi3,top=255)
   tv,bim3

   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,sz[1]-1],yr=[0,sz[2]-1],xstyle=5,ystyle=5,/noerase
   oplot,[-20,20,20,-20,-20]+x1m,[-20,-20,20,20,-20]+y1m,color='ff2000'xl
   cursor,xloc3,yloc3,3
   if !mouse.button eq 4 then return
   basphote,5.5,im3,1.0,xloc3,yloc3,10,20,40,/nolog,/silent, $
      xcen=x3m,ycen=y3m,flux=fl3
   x3m = fix(x3m+0.5)
   y3m = fix(y3m+0.5)
   oplot,[x3m],[y3m],psym=4,color='ff0000'xl

   dleft  = min([x1m,x2m,x3m])
   dright = min(nx-[x1m,x2m,x3m])-1
   dup    = min(ny-[y1m,y2m,y3m])-1
   ddown  = min([y1m,y2m,y3m])

   print,'Image 2-1 offset ',x2m-x1m,y2m-y1m
   print,'Image 3-1 offset ',x3m-x1m,y3m-y1m
   print,'Image 1 section ',x1m-dleft,x1m+dright,y1m-ddown,y1m+dup
   print,'Image 2 section ',x2m-dleft,x2m+dright,y2m-ddown,y2m+dup
   print,'Image 3 section ',x3m-dleft,x3m+dright,y3m-ddown,y3m+dup
   print,'fluxes ',fl1,fl2,fl3,fl2/fl1,fl3/fl1
   print,'means  ',mean1,mean2,mean3
   print,'sigmas ',sigma1,sigma2,sigma3
   sig=(sigma1+sigma2+sigma3)/3.0
   sig1 = sig
   sig2 = sig*fl2/fl1
   sig3 = sig*fl3/fl1

   xsz=dright+dleft+1
   ysz=dup+ddown+1

   setwin,3,xsize=xsz,ysize=ysz,/show
   fim1 = bytscl(im1,min=mean1+ls*sig1,max=mean1+hs*sig1,top=255)
   fim2 = bytscl(im2,min=mean2+ls*sig2,max=mean2+hs*sig2,top=255)
   fim3 = bytscl(im3,min=mean3+ls*sig3,max=mean3+hs*sig3,top=255)
   fcube=0
   fcube = $
      [[[im1[x1m-dleft:x1m+dright,y1m-ddown:y1m+dup]]], $
       [[im2[x2m-dleft:x2m+dright,y2m-ddown:y2m+dup]]], $
       [[im3[x3m-dleft:x3m+dright,y3m-ddown:y3m+dup]]]]
   ccube=0
   ccube = $
      [[[fim1[x1m-dleft:x1m+dright,y1m-ddown:y1m+dup]]], $
       [[fim2[x2m-dleft:x2m+dright,y2m-ddown:y2m+dup]]], $
       [[fim3[x3m-dleft:x3m+dright,y3m-ddown:y3m+dup]]]]

   tv,ccube,true=3

   ans=''
   read,ans,prompt='Hardcopy? (c/b/n)'
   IF strmid(ans,0,1) eq 'c' THEN BEGIN
      if queue eq '[[default]]' then pqueue = 'chani' else pqueue = queue
      npix = max([xsz,ysz])
      IF npix lt 600 THEN $
         scale = 25.0 $
      ELSE IF npix lt 1064 THEN $
         scale = 56.0 $
      ELSE IF npix lt 2100 THEN $
         scale = 112.0 $
      ELSE $
         scale = float(npix)/15.0
      hardim,[[[fim1[x1m-dleft:x1m+dright,y1m-ddown:y1m+dup]]], $
              [[fim2[x2m-dleft:x2m+dright,y2m-ddown:y2m+dup]]], $
              [[fim3[x3m-dleft:x3m+dright,y3m-ddown:y3m+dup]]]],0,255, $
             autosize=1,queue=pqueue,true_color=3,width=npix/scale, $
             title='R:'+imfile1+' G:'+imfile2+' B:'+imfile3
   ENDIF ELSE IF strmid(ans,0,1) eq 'b' THEN BEGIN
      if queue eq '[[default]]' then pqueue = 'office' else pqueue = queue
      npix = max([xsz,ysz])
      IF npix lt 600 THEN $
         scale = 25.0 $
      ELSE IF npix lt 1064 THEN $
         scale = 56.0 $
      ELSE IF npix lt 2100 THEN $
         scale = 112.0 $
      ELSE $
         scale = float(npix)/15.0
      hardim,fim1,0,255,title=imfile1, $
         autosize=1,queue=pqueue,/negative,width=npix/scale
   ENDIF

end
