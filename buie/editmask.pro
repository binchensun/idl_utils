;+
; NAME:
;  editmask
; PURPOSE:   (one line only)
;  Interactive image-based editing of a bad-pixel mask
; DESCRIPTION:
; CATEGORY:
;  CCD Data Processing
; CALLING SEQUENCE:
;  editmask,image,mask
; INPUTS:
;  image - Array containing image, this argument is not modified.
;  mask  - Array containing initial mask.  This does not need to exist
;            prior to calling this routine.  This argument is modified.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  XSIZE - size of the main image window
;  YSIZE - size of the main image window
;  BITMASK - This is a mask that contains the bit (or bits) to be set
;              (or cleared) in the mask.  Normally, this mask will have
;              a single bit set.  The input must be a byte type scalar.
;              Default='01'xb (bit 0 set).
;  STYLE - This indicates how the bitmask information is to be visually
;            applied to the image.
;              0 - blot out pixel with red (default)
;              1 - replace pixel with sky value
;          This used only for visual purposes.  The image is not actually
;            modified here.
; OUTPUTS:
;  mask  - Edited version of the mask
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2011/09/21
;-
pro editmask_cleanup, tlb

   ; kludge used just to get the modified mask back to the calling routine
   common mwb_editmask,newmask

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   newmask=(*state).mask

   ; Free up any dynamically allocated space with the state structure
;  ptr_free,(*state).something

   ; Free up the state structure itself.
   ptr_free, state

end

pro editmask_updatebitnum,state,bit

   (*state).bitnum=bit
   (*state).bitmask=ishft(1B,(*state).bitnum)
   str='bit '+strn((*state).bitnum)
   widget_control,(*state).bitvalid,set_value=str
   widget_control,(*state).bitnameid, $
      set_value=(*state).bitnames[(*state).bitnum]
end

pro editmask_updatedisplay,state
   editmask_updateimage,state
   editmask_updatezoom,state
   editmask_updatenav,state
end

pro editmask_updateimage,state

   widget_control,(*state).drawwin,get_value=winnum
   wset,winnum
   erase

   ; Making a working copy of the desired corner of the display.
   x0 = (*state).x0
   y0 = (*state).y0

   ; Compute upper extraction boundary from main array
   x1 = x0 + (*state).szx - 1
   y1 = y0 + (*state).szy - 1

   ; Limit upper boundary to edge of array by shifting to edge (if needed).
   xshift = x1 - ( (*state).nx-1 ) > 0
   yshift = y1 - ( (*state).ny-1 ) > 0
   x0 = x0 - xshift
   x1 = x1 - xshift
   y0 = y0 - yshift
   y1 = y1 - yshift

   ; Now check to make sure we aren't pushing past the lower or left edges.
   xshift = x0 < 0
   yshift = y0 < 0
   x0 = x0 - xshift
   x1 = x1 - xshift
   y0 = y0 - yshift
   y1 = y1 - yshift

   ; Final check in case the display image is larger than full image.
   if x1 gt (*state).nx-1 then x1 = (*state).nx-1
   if y1 gt (*state).ny-1 then y1 = (*state).ny-1

   mimage=(*state).image
   z=where((*state).mask ne 0B,count)
   if count ne 0 then begin
      filler=randomn(seed,count)*(*state).skysig+(*state).sky
      mimage[z]=filler
   endif
   (*state).mimage=mimage

   ; bit 0 - Red
   ; bit 1 - orange
   ; bit 2 - light blue
   ; bit 3 - magenta
   ; bit 4 - purple
   ; bit 5 - lavender
   ; bit 6 - green
   ; bit 7 - dark yellow
   cr=[255B,255B,  0B,255B, 80B,137B,  0B,178B]
   cg=[  0B,128B,128B,  0B,  0B,110B,255B,178B]
   cb=[  0B,  0B,255B,255B,255B,255B,  0B,  0B]
   cmaskr=bytarr((*state).nx,(*state).ny)
   cmaskg=bytarr((*state).nx,(*state).ny)
   cmaskb=bytarr((*state).nx,(*state).ny)
   for i=0,7 do begin
      bitmask=ishft(1B,i)
      z=where(((*state).mask and bitmask) ne 0B,count)
      if count ne 0 then begin
         cmaskr[z]=cr[i]
         cmaskg[z]=cg[i]
         cmaskb[z]=cb[i]
      endif
   endfor
   (*state).cmaskr=cmaskr
   (*state).cmaskg=cmaskg
   (*state).cmaskb=cmaskb

   alowval=(*state).sky + (*state).dmsig*(*state).skysig
   ahival =(*state).sky + (*state).dpsig*(*state).skysig
   fullbim=bytscl((*state).mimage,min=alowval,max=ahival,top=255)
   (*state).fullbim=fullbim

   usigstr=strn((*state).dpsig,format='(f10.1)')
   widget_control,(*state).usigid,set_value=usigstr
   lsigstr=strn((*state).dmsig,format='(f10.1)')
   widget_control,(*state).lsigid,set_value=lsigstr
   udnstr=strn(ahival,format='(f10.1)')
   widget_control,(*state).udnid,set_value=udnstr
   ldnstr=strn(alowval,format='(f10.1)')
   widget_control,(*state).ldnid,set_value=ldnstr

   if (*state).style[(*state).bitnum] eq 0 then begin
      z=where((*state).mask ne 0B,count)
      bimr = fullbim
      bimg = fullbim
      bimb = fullbim
      if count ne 0 then begin
         bimr[z]=cmaskr[z]
         bimg[z]=cmaskg[z]
         bimb[z]=cmaskb[z]
      endif
      bimr=bimr[x0:x1,y0:y1]
      bimg=bimg[x0:x1,y0:y1]
      bimb=bimb[x0:x1,y0:y1]
      tv,[[[bimr]],[[bimg]],[[bimb]]],true=3
   endif else if (*state).style[(*state).bitnum] eq 1 then begin
      tv,fullbim[x0:x1,y0:y1]
   endif

   (*state).x0=x0
   (*state).y0=y0

end

pro editmask_updatenav,state

   widget_control,(*state).navwin,get_value=winnum
   wset,winnum
   erase

   ; Make a local copy to save some typing
   sf = (*state).sf

   xnav = round((*state).nx/(*state).sf)<(*state).sznx
   ynav = round((*state).ny/(*state).sf)<(*state).szny

   thumb=congrid((*state).mimage,xnav,ynav)
   alowval=(*state).sky + (*state).dmsig*(*state).skysig
   ahival =(*state).sky + (*state).dpsig*(*state).skysig
   bim=bytscl(thumb,min=alowval,max=ahival,top=255)

   dx=((*state).sznx-xnav)/2
   dy=((*state).szny-ynav)/2
   tv,bim,dx,dy

end

pro editmask_updatezoom,state

   widget_control,(*state).zoomwin,get_value=winnum
   wset,winnum
   erase

   if (*state).zxcen lt 0 or (*state).zycen lt 0 then return

   str=strn((*state).zxcen)+','+strn((*state).zycen)
   widget_control,(*state).zcenid,set_value=str

   ; size of extraction region
   wid = (*state).szzx/(*state).zf
   zsz = wid * (*state).zf

   ; Extract image sub-section around object, watch out for image edges.
   x10 = (*state).zxcen - wid/2
   x20 = x10  + wid - 1
   y10 = (*state).zycen - wid/2
   y20 = y10  + wid - 1

   x1=max([x10,0])
   x2=min([x20,(*state).nx-1])
   y1=max([y10,0])
   y2=min([y20,(*state).ny-1])

   bim=bytarr(wid,wid)

   (*state).zx0 = x10
   (*state).zy0 = y10

   bim[x1-x10,y1-y10] = (*state).fullbim[x1:x2,y1:y2]

   if (*state).style[(*state).bitnum] eq 0 then begin
      z=where((*state).mask ne 0B,count)
      fbimr = (*state).fullbim
      fbimg = (*state).fullbim
      fbimb = (*state).fullbim
      if count ne 0 then begin
         fbimr[z]=(*state).cmaskr[z]
         fbimg[z]=(*state).cmaskg[z]
         fbimb[z]=(*state).cmaskb[z]
      endif
      bimr=bytarr(wid,wid)
      bimg=bytarr(wid,wid)
      bimb=bytarr(wid,wid)

      bimr[x1-x10,y1-y10]=fbimr[x1:x2,y1:y2]
      bimg[x1-x10,y1-y10]=fbimg[x1:x2,y1:y2]
      bimb[x1-x10,y1-y10]=fbimb[x1:x2,y1:y2]
      tv,rebin([[[bimr]],[[bimg]],[[bimb]]], $
                (*state).szzx,(*state).szzx,3,/sample),true=3
   endif else if (*state).style[(*state).bitnum] eq 1 then begin
      tv,rebin(bim,(*state).szzx,(*state).szzx,/sample)
   endif

end

pro editmask_eve, event

   widget_control, event.top, GET_UVALUE=state

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   dirty=0

   case event_name of

      'THE_MENU': begin
         case event.value of

            (*state).bitnames[0]: begin
               editmask_updatebitnum,state,0
               dirty=1
            end

            (*state).bitnames[1]: begin
               editmask_updatebitnum,state,1
               dirty=1
            end

            (*state).bitnames[2]: begin
               editmask_updatebitnum,state,2
               dirty=1
            end

            (*state).bitnames[3]: begin
               editmask_updatebitnum,state,3
               dirty=1
            end

            (*state).bitnames[4]: begin
               editmask_updatebitnum,state,4
               dirty=1
            end

            (*state).bitnames[5]: begin
               editmask_updatebitnum,state,5
               dirty=1
            end

            (*state).bitnames[6]: begin
               editmask_updatebitnum,state,6
               dirty=1
            end

            (*state).bitnames[7]: begin
               editmask_updatebitnum,state,7
               dirty=1
            end

            'Upper DN': begin
               newval=qinput(prompt='New upper DN value for stretch', $
                             group_leader=event.top,cancelled=abort,/float)
               if abort then return
               newval=float(newval)
               (*state).dpsig=(newval-(*state).sky)/(*state).skysig
               dirty=1
            end

            'Lower DN': begin
               newval=qinput(prompt='New lower DN value for stretch', $
                             group_leader=event.top,cancelled=abort,/float)
               if abort then return
               newval=float(newval)
               (*state).dmsig=(newval-(*state).sky)/(*state).skysig
               dirty=1
            end

            'Upper Sigma': begin
               newval=qinput(prompt='New upper sigma value for stretch', $
                             group_leader=event.top,cancelled=abort,/float)
               if abort then return
               (*state).dpsig=float(newval)
               dirty=1
            end

            'Lower Sigma': begin
               newval=qinput(prompt='New lower sigma value for stretch', $
                             group_leader=event.top,cancelled=abort,/float)
               if abort then return
               (*state).dmsig=float(newval)
               dirty=1
            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               return
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Navigate': begin
         if event.press gt 0 then begin
            xm=fix((event.x-(*state).sznx/2.0) * (*state).sf + (*state).nx/2.0)
            ym=fix((event.y-(*state).szny/2.0) * (*state).sf + (*state).ny/2.0)
            (*state).x0 = xm - (*state).szx/2
            (*state).y0 = ym - (*state).szy/2
            dirty=1
         endif
      end

      'Window': begin
         if event.press gt 0 then begin
            (*state).zxcen=event.x+(*state).x0
            (*state).zycen=event.y+(*state).y0
            dirty=1
         endif
      end

      'Zoom': begin
         ; zoom not valid, don't do anything
         if (*state).zxcen lt 0 or (*state).zycen lt 0 then return

         ; on release of mouse button, quit modifying
         if (*state).press ne 0 and event.release ne 0 then begin
            (*state).press =  0
            (*state).lastx = -1
            (*state).lasty = -1
            return
         endif

         if (*state).press eq 0 then begin
            if event.press eq 0 then begin
               if event.release eq 0 then begin
                  return
               endif else if event.release eq 1 then begin
                  print,'cannot happen B'
               endif else if event.release eq 2 then begin
                  (*state).press =  0
                  (*state).lastx = -1
                  (*state).lasty = -1
               endif
            endif else begin
               (*state).press = event.press
            endelse
         endif else begin
            if event.press eq 0 then begin
               if event.release eq 0 then begin
               endif else begin
                  (*state).press =  0
                  (*state).lastx = -1
                  (*state).lasty = -1
               endelse
            endif else begin
               print,'cannot happen A'
            endelse
         endelse
         x=event.x/(*state).zf+(*state).zx0
         y=event.y/(*state).zf+(*state).zy0
         if (*state).lastx ne x or (*state).lasty ne y then begin
            if (*state).press eq 1 then begin
               (*state).mask[x,y] = (*state).mask[x,y] or (*state).bitmask
               dirty=1
            endif else if (*state).press eq 2 then begin
               (*state).mask[x,y] = (*state).mask[x,y] and $
                                    ( not (*state).bitmask )
               dirty=1
            endif
            (*state).lastx=x
            (*state).lasty=y
         endif

      end

;      'Mainbase': begin

         ; Use if you have other widgets on screen, need to take off their
         ;   size from the event x,y size.
;         info=widget_info((*state).colbaseid,/geometry)
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

         ; Use if draw window is only thing in the tool.
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

;        refresh window here
;      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

   if dirty then $
      editmask_updatedisplay,state

end ; end of event handler

pro editmask,image,mask,XSIZE=xsize,YSIZE=ysize,BITNUM=bitnum,STYLE=style, $
       BITNAMES=bitnames

   common mwb_editmask,newmask

   self='editmask: '
   if badpar(image,[1,2,3,4,5,12,13],2,caller=self+'(image) ') then return
   sz=size(image,/dimen)
   if badpar(mask,[0,1],2,caller=self+'(image) ', $
                          default=bytarr(sz[0],sz[1])) then return
   sz2=size(mask,/dimen)
   if sz[0] ne sz2[0] or sz[1] ne sz2[1] then begin
      print,self,'ERROR! Image and mask must be the same size'
      return
   endif
   if badpar(xsize,[0,2,3],0,caller=self+'(XSIZE) ',default=410) then return
   if badpar(ysize,[0,2,3],0,caller=self+'(YSIZE) ',default=410) then return
   if badpar(bitnum,[0,1,2,3],0,caller=self+'(BITNUM) ',default=0) then return
   if badpar(style,[0,2,3],1,caller=self+'(STYLE) ', $
      default=replicate(0,8),npts=numstyles) then return
   if numstyles ne 8 then begin
      print,self,'Number of styles must be exactly 8.'
      return
   endif
   defnames='Bit '+strtrim(string(indgen(8)),2)
   if badpar(bitnames,[0,7],1,caller=self+'(BITNAMES) ', $
             default=defnames,npts=numnames) then return
   if numnames ne 8 then begin
      print,self,'Number of bit names must be exactly 8.'
      return
   endif

   ; prevents more than one copy from running
   if xregistered('editmask') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. EDITMASK cannot be started.'
      return
   endif

   ;Define the main base.
   mainbase = widget_base( TITLE='EDITMASK: Image Mask Editor', $
                           /COLUMN, UVALUE=0, MBAR=bar)

   bitmenu=replicate(0,8)
   bitmenu[7]=2
   bitmenu=string(bitmenu,format='(i1)')+'\'+bitnames

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\action 1',$
                     '0\action 2',$
                     '2\Exit',$
                     '1\Bit',$
                     bitmenu, $
                     '1\Change',$
                     '0\Upper Sigma', $
                     '0\Lower Sigma', $
                     '0\Upper DN', $
                     '0\Lower DN', $
                     '2\change',$
                     '1\Tools',$
                     '0\tool 1', $
                     '0\tool 2', $
                     '2\tool 3'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase)

   b1 = widget_base(base,row=1)

   b2 = widget_base(b1,column=1)

   sznx=200
   szny=200
   szzx=200
   szzy=200
   win2 = widget_draw(b2,XSIZE=sznx,YSIZE=szny,RETAIN=2, $
                       /BUTTON_EVENTS,UVALUE='Navigate' )
   win3 = widget_draw(b2,XSIZE=szzx,YSIZE=szzy,RETAIN=2, $
                       /BUTTON_EVENTS,/MOTION_EVENTS,UVALUE='Zoom' )

   dpsig= +8.0
   dmsig= -3.0


   b3 = widget_base(b2,/row)
   b4 = widget_base(b3,/column,/grid_layout)
   t=widget_label(b4,value='Zoom Center')
   t=widget_label(b4,value='upper sigma limit')
   t=widget_label(b4,value='lower sigma limit')
   t=widget_label(b4,value='upper DN limit')
   t=widget_label(b4,value='lower DN limit')
   bitname=widget_label(b4,value='',/dynamic_resize)

   b4 = widget_base(b3,/column,/grid_layout)
   zcen=widget_label(b4,value='',/dynamic_resize,/align_right)
   usig=widget_label(b4,value='',/dynamic_resize,/align_right)
   lsig=widget_label(b4,value='',/dynamic_resize,/align_right)
   udn=widget_label(b4,value='',/dynamic_resize,/align_right)
   ldn=widget_label(b4,value='',/dynamic_resize,/align_right)
   bitval=widget_label(b4,value='',/dynamic_resize)

   ; compute some sizes
   ; main window
   szx = xsize<sz[0]
   szy = ysize<sz[1]
   ; score window
   xf = float(sz[0])/sznx
   yf = float(sz[1])/szny
   ; The biggest factor is the one we'll use.
   sf = max([xf,yf])

   win1 = widget_draw(b1,XSIZE=szx,YSIZE=szy,RETAIN=2, $
                       /BUTTON_EVENTS,UVALUE='Window')

   robomean,image,3.0,0.5,sky,dummy,skysig
   mimage=image
   z=where(mask ne 0B,count)
   if count ne 0 then begin
      filler=randomn(seed,count)*skysig+sky
      mimage[z]=filler
   endif

   ; redundant except for starting out with something sensible
   bitmask=ishft(1B,bitnum)

   state = ptr_new({ $

      ; Data and information in the widget
      bitnum: bitnum, $
      bitmask: bitmask, $
      bitnames: bitnames, $
      cmaskr: bytarr(sz[0],sz[1]), $
      cmaskg: bytarr(sz[0],sz[1]), $
      cmaskb: bytarr(sz[0],sz[1]), $
      dmsig: dmsig, $
      dpsig: dpsig, $
      fullbim: bytarr(sz[0],sz[1]), $
      image: image, $
      mask: mask, $
      mimage: mimage, $
      lastx: -1, $
      lasty: -1, $
      nx: sz[0], $
      ny: sz[1], $
      press: 0, $
      sf: sf, $                  ; navigation window scale factor
      sky: sky, $
      skysig: skysig, $
      style: style, $                ; 0=blot, 1=sky
      sznx: sznx, $
      szny: szny, $
      szzx: szzx, $
      szzy: szzy, $
      szx: szx, $
      szy: szy, $
      x0: 0, $                   ; LLHC of display window from full image.
      y0: 0, $                   ; LLHC of display window from full image.
      zf: 10, $                   ; Zoom factor
      zxcen: -1, $               ; requested center of zoom
      zycen: -1, $               ; requested center of zoom
      zx0: 0, $                  ; LLHC of zoom display window from full image.
      zy0: 0, $                  ; LLHC of zoom display window from full image.

      ; Widget ids
      bitnameid: bitname, $
      bitvalid: bitval, $
      drawwin: win1, $           ; ID of main draw window
      navwin: win2, $
      zoomwin: win3, $
      zcenid: zcen, $
      usigid: usig, $
      lsigid: lsig, $
      udnid: udn, $
      ldnid: ldn, $

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   editmask_updatebitnum,state,bitnum
   editmask_updatedisplay,state

   ; Give control to the XMANAGER.
   xmanager, 'editmask', mainbase, $
             EVENT_HANDLER='editmask_eve', $
             GROUP_LEADER=mainbase, CLEANUP='editmask_cleanup'

   mask=newmask

end
