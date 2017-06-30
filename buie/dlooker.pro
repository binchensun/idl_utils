;+
; NAME:
;  dlooker
; PURPOSE:
;  Visual identification and measurement of moving objects in digital images.
; DESCRIPTION:
;
;  This program handles visually inspecting large digital images and permiting
;     measuring positions of the tracklets and transients found.  The object
;     data is collected in mysql tables.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  dlooker
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  PATH      - Optional path for root of image directory.
;                default = /net/wixer/raid/buie/
;
;  XSIZE     - x size, in pixels, of the main window (default=1070)
;
;  YSIZE     - y size, in pixels, of the main window (default=820)
;
;  ZXSIZE    - x size, in pixels, of the score window (default=148)
;
;  ZYSIZE    - y size, in pixels, of the score window (default=296)
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;  2012/01/01, Written by Marc W. Buie, Lowell Observatory,
;                 cloned from looker.pro
;  2012/03/10, MWB, added support for shift stacks
;  2012/03/11, MWB, cleaned up tracklet plotting
;                   added tracklet selection edit tool
;                   fixed: zoom update after selecting object
;-

;------------------------------------------------------------------------------
pro dlooker_cleanup, tlb

   widget_control, tlb, get_uvalue=state
   ptr_free,(*state).astinfo
   ptr_free,(*state).bcube
   ptr_free,(*state).blinkidx
   ptr_free,(*state).chiplist
   ptr_free,(*state).cube
   ptr_free,(*state).dimgfwhm
   ptr_free,(*state).dreffwhm
   ptr_free,(*state).etascat
   ptr_free,(*state).exptime
   ptr_free,(*state).fieldlist
   ptr_free,(*state).fileroot
   ptr_free,(*state).fwhm
   ptr_free,(*state).invert
   ptr_free,(*state).jdmid
   ptr_free,(*state).maglim
   ptr_free,(*state).night
   ptr_free,(*state).nrep
   ptr_free,(*state).objrad
   ptr_free,(*state).partlist
   ptr_free,(*state).photzp
   ptr_free,(*state).setlist
   ptr_free,(*state).visitlist
   ptr_free,(*state).xiscat

   ptr_free,(*state).skid
   ptr_free,(*state).snid
   ptr_free,(*state).sx
   ptr_free,(*state).sy
   ptr_free,(*state).nmeas
   ptr_free,(*state).rate
   ptr_free,(*state).dir
   ptr_free,(*state).sfwhm
   ptr_free,(*state).smag

   ptr_free,(*state).objlist
   ptr_free,(*state).xobj
   ptr_free,(*state).yobj
   ptr_free,(*state).x1sig
   ptr_free,(*state).y1sig
   ptr_free,(*state).x3sig
   ptr_free,(*state).y3sig

   ptr_free, state
   setusym,-1

end

;----------------------------------------------------------------------------
pro dlooker_delete,state,tranid

   if tranid le 0 then return

   openmysql,dblun,'gen'

   cmd='delete from transient where id='+strn(tranid)+';'
   mysqlcmd,dblun,cmd

   cmd='delete from tracklet where id='+strn((*state).curtrk)+ $
       ' and tranid='+strn(tranid)+';'
   mysqlcmd,dblun,cmd

   cmd='select count(*) from tracklet where id='+strn((*state).curtrk)+';'
   mysqlquery,dblun,cmd,ncheck,format='i'
   if ncheck eq 0 then (*state).curtrk=-1

   dlooker_trkpop,state

   (*state).curtrn=-1
   
   free_lun,dblun

end

;----------------------------------------------------------------------------
; This takes care of refreshing the visual display in all relevant windows
; based on the current state.  Note that state is a pointer to an anonymous
; structure.
pro dlooker_display,state,NOSCORE=noscore

   if not keyword_set(noscore) then noscore=0

   widget_control, (*state).drawwin, get_value=winnum
   WSET, winnum
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

   (*state).x0 = x0
   (*state).y0 = y0

   if (*state).colormode eq 0 then begin
      tv,(*(*state).bcube)[x0:x1,y0:y1,(*state).curvis]
      tag=''
   endif else begin
      othervis = (*state).curvis+1
      if othervis eq (*state).nvisits then othervis=0
      cube=[[[(*(*state).bcube)[x0:x1,y0:y1,(*state).curvis]]], $
            [[(*(*state).bcube)[x0:x1,y0:y1,othervis]]], $
            [[(*(*state).bcube)[x0:x1,y0:y1,othervis]]]]
      tv,cube,true=3
      dt=(*(*state).jdmid)[othervis] - $
         (*(*state).jdmid)[(*state).curvis]
      if abs(dt) gt 0.5 then begin
         tag=' '+string(dt,format='(f5.1)')+'d'
      endif else begin
         tag=' '+string(dt*24.0d0,format='(f5.1)')+'h'
      endelse
   endelse

   if (*state).nobj gt 0 and not (*state).nomarks then begin
;print,'plot object error ellipse here'
      for i=0,(*state).nobj-1 do begin
;print,i,(*(*state).xobj)[(*state).curvis,i], $
;        (*(*state).yobj)[(*state).curvis,i]

         plots,(*(*state).xobj)[(*state).curvis,i]-(*state).x0, $
               (*(*state).yobj)[(*state).curvis,i]-(*state).y0, $
               color='0000ff'xl,psym=4,/device,symsize=4.0

         plots,(*(*state).xobj)[(*state).curvis,i]+ $
                  (*(*state).x1sig)[*,i]-(*state).x0, $
               (*(*state).yobj)[(*state).curvis,i]+ $
                        (*(*state).y1sig)[*,i]-(*state).y0, $
               color='0000ff'xl,/device
         plots,(*(*state).xobj)[(*state).curvis,i]+ $
                  (*(*state).x3sig)[*,i]-(*state).x0, $
               (*(*state).yobj)[(*state).curvis,i]+ $
                        (*(*state).y3sig)[*,i]-(*state).y0, $
               color='00c0c0'xl,/device
      endfor
   endif

   if (*state).ntrk gt 0 and not (*state).nomarks then begin
      setusym,-1
      for i=0,(*state).ntrk-1 do begin
         ; plot all predicted positions with yellow? circles 
         z=where( (*(*state).snid)[*,i] lt 0, count )
         if count gt 0 then $
            plots,(*(*state).sx)[z,i]-(*state).x0, $
                  (*(*state).sy)[z,i]-(*state).y0, $
                  psym=8,/device,color='00ffff'xl,symsize=2.5
         if (*state).lines then begin
            ; connect all positions with a line
            plots,(*(*state).sx)[*,i]-(*state).x0, $
                  (*(*state).sy)[*,i]-(*state).y0,/device, $
                  linestyle=0,color='ff00ff'xl
         endif

         ; outer circle the current visit with red
         nv=(*state).curvis
         plots,(*(*state).sx)[nv,i]-(*state).x0, $
               (*(*state).sy)[nv,i]-(*state).y0, $
               psym=8,/device,color='0000ff'xl,symsize=3.5

         ; plot all measured positions with green circles
         zm=where( (*(*state).snid)[*,i] ge 0, countm )
         if countm gt 0 then begin
            plots,(*(*state).sx)[zm,i]-(*state).x0, $
                  (*(*state).sy)[zm,i]-(*state).y0, $
               psym=8,/device,color='00ff00'xl,symsize=2.5
         endif

      endfor
      setusym,1
   endif

   str=strn((*state).curvis+1)+' of '+strn((*state).nvisits)
   widget_control,(*state).visitid,set_value=str

   jdstr,(*(*state).jdmid)[(*state).curvis],0,jds
   widget_control,(*state).timeid,set_value=jds+tag
   str='FWHM='+strn((*(*state).fwhm)[(*state).curvis],format='(f10.1)')+ $
       '  Rlim='+strn((*(*state).maglim)[(*state).curvis],format='(f10.2)')
   widget_control,(*state).info1id,set_value=str
   str='Ifwhm='+strn((*(*state).dimgfwhm)[(*state).curvis],format='(f10.1)')+ $
       '  Rfwhm='+strn((*(*state).dreffwhm)[(*state).curvis],format='(f10.1)')
   widget_control,(*state).info2id,set_value=str

   if not noscore then dlooker_updatescore,state,x0,x1,y0,y1

   dlooker_objinfo,state

end

;------------------------------------------------------------------------------
pro dlooker_checkinitials,state

   widget_control, (*state).initialsid, GET_VALUE=initials
   initials=trimrank(initials)

   if initials eq '' then begin
      text = [ $
         'You have not entered your initials next to the "Next/Previous" buttons.  This', $
         'information is needed to track who was responsible for a given measurement.', $
         'You must provide this information before you can proceed with ANY operation', $
         'in this program.']
      result=dialog_message(text,/ERROR)
      (*state).initials=''
   endif

   if initials eq (*state).initials then return

   ; If we get this far there has been a change to the requested initials
   ;   to use.

   ; force to uppercase, keep only first three characters
   initials=strcompress(strupcase(initials),/remove_all)
   initials=strmid(initials,0,3)

   ; Check to see if these initials are already registered
   c=','
   openmysql,dblun,'gen'
   cmd='select name from initials'+ $
       ' where id='+quote(initials)+';'
   mysqlquery,dblun,cmd,name,format='a',ngood=nfound

   ; Initials not found, must query for name to go with it.
   if nfound eq 0 then begin
      name = qinput(PROMPT='Please enter the name to go with ' + $
                           'initials ['+initials+']' )
      name=strtrim(strcompress(name[0]),2)
      if name eq '' then begin
         (*state).initials=''
      endif else begin
         cmd='insert into initials values ('+quote(initials)+c+quote(name)+');'
         mysqlcmd,dblun,cmd
         (*state).initials=initials
      endelse

   ; Initials found, use them
   endif else begin
      (*state).initials=initials
   endelse
   free_lun,dblun

   widget_control,(*state).initialsid,SET_VALUE=(*state).initials

end

;------------------------------------------------------------------------------
pro dlooker_getrunlist,runlist,out_dblist

   dblist=['nh11']

   for i=0,n_elements(dblist)-1 do begin
      openmysql,dblun,'nh11'
      cmd='select rdir from diff group by rdir order by rdir;'
      mysqlquery,dblun,cmd,rdir,format='a',ngood=count
      if i eq 0 then begin
         runlist=rdir
         out_dblist=replicate(dblist[i],count)
      endif else begin
         runlist=[runlist,rdir]
         out_dblist=[out_dblist,replicate(dblist[i],count)]
      endelse
      free_lun,dblun
   endfor

end

;------------------------------------------------------------------------------
pro dlooker_getsubimage,state,xpos,ypos,frame,wid,sky,subim

   ; Extract image sub-section around transient, watch out for image edges.
   x10 = fix(xpos) - fix(wid)/2
   x20 = fix(x10)  + fix(wid) - 1
   y10 = fix(ypos) - fix(wid)/2
   y20 = fix(y10)  + fix(wid) - 1

   x1=max([x10,0])
   x2=min([x20,(*state).nx-1])
   y1=max([y10,0])
   y2=min([y20,(*state).ny-1])

   subim=replicate(sky,wid,wid)

   subim[x1-x10,y1-y10] = (*(*state).cube)[x1:x2,y1:y2,(*state).curvis]

end

;------------------------------------------------------------------------------
pro dlooker_loadset,state,newset
   words=strsplit(newset,'.',/extract)
   field = words[0]
   chip  = fix(words[1])
   if n_elements(words) eq 2 then $
      part='' $
   else $
      part=words[2]
   dlooker_loadset2,state,field,chip,part
end

;------------------------------------------------------------------------------
pro dlooker_loadset2,state,field,chip,part

   if (*state).basis eq 'Delta' then $
      (*state).dpath=(*state).path+'diff/'+(*state).rdir+'.d/' $
   else $
      (*state).dpath=(*state).path+'diff/'+(*state).rdir+'.g/'

   if part ne '' then begin
      newset=field+'.'+strn(chip)
      newset=newset+'.'+part
   endif else begin
      newset=field+'.'+string(chip,format='(i2.2)')
   endelse
   (*state).field = field
   (*state).chip  = chip
   (*state).part=part
   (*state).set=newset
   widget_control,(*state).setid,set_value=newset

   openmysql,dblun,(*state).db
   ; Get the list of files for this visit
   cmd='select diff.file,jdmid,diff.fwhm,diff.maglim,'+ $
       'dimgfwhm,dreffwhm,exptime,stack.photzp,diff.objrad'+ $
       ' from diff,stack'+ $
       ' where diff.file like '+quote(newset+'%')+ $
       ' and diff.rdir='+quote((*state).rdir)+ $
       ' and diff.rdir=stack.rdir'+ $
       ' and diff.img=stack.file'+ $
       ' and basis='+quote((*state).basis)+ $
       ' order by jdmid;'

print,cmd
   mysqlquery,dblun,cmd,file,jdmid,fwhm,maglim,dimgfwhm,dreffwhm, $
      exptime,photzp,objrad, $
      format='a,d,f,f,f,f,f,f,f'

   post=strpos(file,'-t')
   poss=strpos(file,'-s')
   posf=strpos(file,'-f')
   if (*state).type eq 'pair' then begin
      z=where(post lt 0 and poss lt 0 and posf lt 0,nset)
      inv=bytarr(nset)
   endif else if (*state).type eq 'stack' then begin
      z=where(post lt 0 and poss gt 0 and posf lt 0,nset)
      inv=bytarr(nset)
   endif else if (*state).type eq 'shift' then begin
      z=where(post lt 0 and poss lt 0 and posf gt 0,nset)
      inv=bytarr(nset)
   endif else begin
      z=where(post gt 0 and poss lt 0 and posf lt 0,nset)
      inv=bytarr(nset)
   endelse
   file=file[z]
   jdmid=jdmid[z]
   nvisits=n_elements(file)

   if (*state).type eq 'pair' then begin
      cmd=['select setnum,refset from stack', $
           'where rdir='+quote((*state).rdir), $
           'and object='+quote((*state).field), $
           'and ccdnum='+strn((*state).chip), $
           'and part='+quote((*state).part), $
           'and setnum != refset', $
           'order by fwhm limit 1;']
      mysqlquery,dblun,cmd,inv_visit,refset,format='i,i'
      cmd=['select jdmid from stack', $
           'where rdir='+quote((*state).rdir), $
           'and object='+quote((*state).field), $
           'and ccdnum='+strn((*state).chip), $
           'and part='+quote((*state).part), $
           'and setnum=refset;']
print,cmd
      mysqlquery,dblun,cmd,refjdmid,format='d'

      setnum=intarr(nvisits)
      for i=0,nvisits-1 do begin
         words1=strsplit(file[i],'.',/extract)
         words2=strsplit(words1[-2],'-',/extract)
         setnum[i]=fix(words2[0])
      endfor
      z=where(setnum eq inv_visit,/null)
      file=[file,file[z]]
      maglim=[maglim,maglim[z]]
      fwhm=[fwhm,fwhm[z]]
      dimgfwhm=[dimgfwhm,dreffwhm[z]]
      dreffwhm=[dreffwhm,dimgfwhm[z]]
      exptime=[exptime,exptime[z]]
      photzp=[photzp,photzp[z]]
      objrad=[objrad,objrad[z]]
      inv=[inv,1]
      setnum=[setnum,refset]
      jdmid=[jdmid,refjdmid]
      idx=sort(jdmid)
      file=file[idx]
      maglim=maglim[idx]
      fwhm=fwhm[idx]
      dimgfwhm=dimgfwhm[idx]
      dreffwhm=dreffwhm[idx]
      exptime=exptime[idx]
      photzp=photzp[idx]
      objrad=objrad[idx]
      inv=inv[idx]
      setnum=setnum[idx]
      jdmid=jdmid[idx]
      nvisits=n_elements(file)

   endif else begin
      setnum=indgen(nvisits)
   endelse

   free_lun,dblun
print,jdmid

   ; build the file roots
   fileroot=(*state).set+'.'+strcompress(string(setnum),/remove_all)

   ; Figure out the temporal grouping of the image sets to find blink sets
   avger,jdmid,jdmid,replicate(1.0,nvisits),20,4, $
      npts=nrep,group=night,xspread=0.5

   z=where(nrep gt 1,count)
   if count eq 0 then begin
      widget_control,(*state).blinkset,sensitive=0,value=''
   endif else begin
      sets=strcompress(string(indgen(count)),/remove_all)
      widget_control,(*state).blinksetid,sensitive=1,set_value=sets, $
         set_combobox_select=0
      idx=where(night eq z[0],count,/null)
      ptr_free,(*state).blinkidx
      (*state).blinkidx=ptr_new(idx)
   endelse

   ptr_free,(*state).visitlist
   ptr_free,(*state).fileroot
   ptr_free,(*state).invert
   ptr_free,(*state).jdmid
   ptr_free,(*state).fwhm
   ptr_free,(*state).dimgfwhm
   ptr_free,(*state).dreffwhm
   ptr_free,(*state).exptime
   ptr_free,(*state).photzp
   ptr_free,(*state).objrad
   ptr_free,(*state).maglim
   ptr_free,(*state).nrep
   ptr_free,(*state).night
   (*state).nvisits=nvisits
   (*state).invert=ptr_new(inv)
   (*state).visitlist=ptr_new(file)
   (*state).fileroot=ptr_new(fileroot)
   (*state).jdmid=ptr_new(jdmid)
   (*state).fwhm=ptr_new(fwhm)
   (*state).dimgfwhm=ptr_new(dimgfwhm)
   (*state).dreffwhm=ptr_new(dreffwhm)
   (*state).exptime=ptr_new(exptime)
   (*state).photzp=ptr_new(photzp)
   (*state).objrad=ptr_new(objrad)
   (*state).maglim=ptr_new(maglim)
   (*state).nrep=ptr_new(nrep)
   (*state).night=ptr_new(night)
   (*state).curvis=0
   (*state).x0=0
   (*state).y0=0

   if (*state).blink eq -2 then (*state).blink=0 $
   else if (*state).blink gt 0 then (*state).blink=-1

   for i=0,nvisits-1 do begin
      fn=(*state).dpath+(*(*state).visitlist)[i]
      print,i,fn,(*(*state).invert)[i],jdmid[i],night[i], $
         format='(i2,1x,a,1x,i1,1x,f13.5,1x,i2)'
      img=readfits(fn,hdr)
      if i eq 0 then begin
         (*state).gain=sxpar(hdr,'GAIN')
         astinfo,hdr,info
         ptr_free,(*state).astinfo
         (*state).astinfo=ptr_new(info)
         sz=size(img,/dimen)
         (*state).nx=sz[0]
         (*state).ny=sz[1]
         ptr_free,(*state).cube
         (*state).cube=ptr_new(fltarr(sz[0],sz[1],nvisits))
         (*state).bcube=ptr_new(bytarr(sz[0],sz[1],nvisits))
         xedge=[indgen((*state).nx),replicate((*state).nx-1,(*state).ny), $
                reverse(indgen((*state).nx)),replicate(0,(*state).ny)]
         yedge=[replicate(0,(*state).nx),indgen((*state).ny), $
              replicate((*state).ny-1,(*state).nx),reverse(indgen((*state).ny))]
         astcvt,'xy',xedge,yedge,info,'rd',ra,dec
         (*state).rarange=minmax(ra)
         (*state).decrange=minmax(dec)
      endif
      if (*(*state).invert)[i] then img= -1.0*img

      (*(*state).cube)[*,*,i]=img

   endfor

   (*state).curtrk=-1
   (*state).curtrn=-1

   dlooker_trkpop,state
   dlooker_objpop,state

   dlooker_stretch,state,/all

   ; Compute scale factor in each axis to go from full frame to score frame.
   xf = float((*state).nx) / (*state).szsx
   yf = float((*state).ny) / (*state).szsy

   ; The biggest factor is the one we'll use.
   (*state).sf = max([xf,yf])

   dlooker_updatescore,state,/reset

end

;------------------------------------------------------------------------------
pro dlooker_measure,state,visit,x,y,exact,error,xpos,ypos,maxsig,sky,skysig, $
                       NOUPDATE=noupdate,SILENT=silent

; Object management strategy:
; Tables: transient (full data on one frame + type)
;         tracklet  (short-arc set of transients for a single object, these
;                    are used within a run and assumed to be moving in
;                    straight lines on the sky, though there are some cases
;                    where a short-arc object can exhibit non-linear motion
;                    and that's ok, it just won't have good position
;                    predictions)
;         varstar   (variable star, can transcend runs, multiple measurements
;                    can be in database but are not required)
;         object    (one or more tracklets, these are expected to be
;                    associated with an externally managed orbit)
; IDs for transients, tracklets, and varstars are managed by and for the
;   database.  Basically these are just unique long integers.  The ID for
;   objects is more complicated and depends on specific external naming rules
;   that depend on the project.  getnewid.pro is used to manage these IDs.
;   THIS PROGRAM DOES NOT MANAGE OBJECTS, only transients and tracklets.
;   Objects are treated as read-only and used for advisory information to
;   be plotted on images.
;
; Every measurement generates a transient which records the full details
;   of the observation.  This information is stored in the gen.transient
;   table.  When posted it gets a unique measurment id that is important
;   only to the database.  There is a flag here that will identify what
;   type of source this object is.  The transient also carries information
;   that identifies if it has been reported to the MPC or not.
;
; Every transient must be associated with either a tracklet or a varstar.
;   The association is managed prior to measurement.  If the type ends up
;   being incorrect then everything needs to be cleaned up.
;
; All changes and measurements are reflected immediately in the database.
;   there is no separate "save" action.  If something is deleted it is truly
;   deleted.  The database must never lag behind the knowledge inside this
;   program.  This will mean transient ids and tracklet ids may get "wasted"
;   and never reused.  Tough.  It's a long integer and there just aren't that
;   many objects.
;
; Measurements are of either moving objects or variable stars.  From a single
;   click you have a transient and this will go into the gen.transient table.
;   NOTE: variables stars are not implemented yet.
;   For a moving object, every transient must be associated with one and only
;     one tracklet.  Not all transients within a tracklet will be on a single
;     image.
;
; There are four states for dlooker with regard to mose events in the
;   display windows.
;   A) no data/image loaded.  This is the initial state of dlooker.  In this
;        state all clicks and most events are silently ignored.  Once you
;        leave this state you cannot get back to it without exiting and
;        restarting the program.  All the other states assume data are loaded.
;   B) NEW TRACKLET/NEW TRANSIENT   curtrk=-1 curtrn=-1
;   C) OLD TRACKLET/NEW TRANSIENT   curtrk>=0 curtrn=-1
;   D) OLD TRACKLET/OLD TRANSIENT   curtrk>=0 curtrn>=0
;
; Here's what the clicks do in the main image window, state A is not discussed
;   Left click
;      State A - never happens, filtered out
;      State B
;        add new transient to db from measurement, update curtrn
;        add new tracklet to db with this transient, update curtrk
;      State C
;        add new transient to db from measurement, update curtrn
;        update current tracklet to contain this transient
;      State D
;        modify transient information in db
;      for states B, C, and D
;        update tracklet info
;        update zoom (new zoom state=ZC)
;        update main display
;        update score
;        update trk/trn info on widget display
;   Middle click
;      State A - never happens, filtered out
;      State B, C, or D
;        measure from image, do not save any data
;        update zoom display (no circle, new zoom state=ZB)
;   Right click
;      State A - never happens, filtered out
;      State B, C, or D
;        select nearest transient and by association its tracklet is selected
;          too.  This can be either a predicted transient location or an
;          actual measured transient.  The state will change to either C or
;          D depending on the target of the click.  There is a maximum
;          distance threshold to be successful.  If not successful, present
;          a popup screen with this information.
;        if state/selection changed
;          update tracklet info
;          update zoom
;             new zoom state ZA if new curtrn=-1
;             new zoom state ZC if new curtrn>=0
;          update main display
;          update score
;          update trk/trn info on widget display
;
; The zoom window is never active if curtrn=-1, when it is active it has
;   the following states:
;   ZA) zoom window event handling disabled, display is black (empty)
;   ZB) zoom window event handling disabled, display shows last browse
;   ZC) zoom window active, display shows current transient
;   When changing the displayed image (by ANY means), the zoom window will
;     change state according to the value of curtrn.  If the new curtrn
;     is undefined (-1) then the state goes to ZA.  If the new curtrn is
;     defined (>=0) then the state goes to ZC.
;   Thus, zoom events are handled only in state ZC, here's what it does
;     in that case.  Not that the zoom display scaling depends entirely on
;     the center position.
;       Left click
;         re-measure current transient, exact position
;         do not change zoom center
;         update zoom (draw circle for object)
;       Middle click
;         re-measure current transient, exact position
;         zoom center = new transient location
;         update zoom (draw circle for object)
;       Right click
;         re-measure current transient, auto-centroid 
;         zoom center = new transient location
;         update zoom (draw circle for object)
;       After any click
;         modify transient information in db
;         update tracklet info
;         update main display
;         update score
;         update trk/trn info on widget display
;   
; You can select from existing tracklets from a pull-down menu tool
;   When you select a tracklet in this way, you will auto-load the image
;   containing the first valid transient in the tracklet and that transient
;   will be selected.
;
; There is a delete transient tool.  This applies to a single transient
;   and it must first be selected.  Deletion means the transient is marked
;   bad (state='n') in the transient table (not actually deleted).  curtrn is
;   set to -1 a the zoom state will change to ZA when updated.
;   If this is the only transient in the tracklet a special confirmation
;   window comes up to get permission to proceed.  Upon deleting, the 
;   associated tracklet must be deleted.  If this tracklet is linked to
;   an object, put up a detailed warning window.  This program will not
;   fix this problem, only provide a warning.
;
; Linking tracklets to objects and overall object management is handled by
;   separate software.  Object position predictions can be overlain on the tool.
;   This is handled by having a list of objects of interest that is provided
;   via an input file upon starting dlooker or by manually adding names
;   to the list once dlooker is running.  Object position predictions are
;   used across runs to suggest places to look.  If you find something
;   interesting the next step is to create a new tracklet.  This new
;   tracklet can then be linked into the object with the external tools.
;   Positions for an object prediction are advisory only and can be turned
;   off or on and do not influence the operation of dlooker.  They are a
;   visual aid only.
;
; dlooker must display objects on the current image, there are three classes
;   of objects that must be supported.
;   1) varstars - this is relatively simple.  Only those falling on the current
;      set need to be displayed and available for manipulating.  NIY
;   2) Tracklets - by their very nature they are relevant within a run.  It
;      makes sense to load all tracklets for the run and display either the
;      measurements or predictions that fall on the image being displayed.
;   3) Objects - These are things with designations of some kind that have
;      an associated orbit.  The objects under consideration are managed
;      by a list file and populating that list is handled by some external
;      tool.  It can also be manipulated (mostly to add) within dlooker.
;      These objects are used to predict a location with an uncertainty
;      ellipse (or just a line of variations).  But, you don't measure objects
;      per se.  Instead, you create new trackets that are then added to
;      the object later.

   error = 0

   objrad=(*(*state).objrad)[visit]
   sky1=objrad+5
   sky2=objrad+35

   basphote,(*state).gain,(*(*state).cube)[*,*,visit], $
      (*(*state).exptime)[visit],x,y,objrad,sky1,sky2,max=maxsig,exact=exact, $
      /nolog,xcen=xpos,ycen=ypos,mag=mag,skymean=sky,skyerr=skysig,fwhm=fwhm, $
      silent=silent
   mag=mag+(*(*state).photzp)[visit]

   if not keyword_set(noupdate) then begin
      astcvt,'xy',xpos,ypos,(*(*state).astinfo),'rd',ra,dec
      jdstr,(*(*state).jdmid)[visit],0,jds
      jdstr,(*(*state).jdmid)[visit],205,jds2
      rastr,ra,3,ras
      decstr,dec,2,decs
      print,jds,xpos,ypos,ras,decs,mag,format='(a,2(1x,f7.2),2(1x,a),1x,f7.3)'
      print,jds2,xpos,ypos,ras,decs,mag,format='(a,2(1x,f7.2),2(1x,a),1x,f7.3)'
      dlooker_tracklet_addobs,state,xpos,ypos,mag,fwhm
   endif

end

;------------------------------------------------------------------------------
; The job of this routine is to post all relevant information about the
;   current transient/tracklet/object on the display.
pro dlooker_objinfo,state

   if (*state).curtrk ge 0 then begin
      widget_control,(*state).curtrkid,set_value=strn((*state).curtrk)
      tz=trimrank(where((*(*state).skid) eq (*state).curtrk))
      if (*(*state).nmeas)[tz] gt 1 then begin
         ratestr = string((*(*state).rate)[tz],'"', $
                         (*(*state).dir )[tz], $
                         format='(f10.2,a,"/hr, ",f10.1,"dg")')
         ratestr = strcompress(strtrim(ratestr,2))
      endif else begin
         ratestr = 'rate not valid'
      endelse
      nmeasstr='  '+strn((*(*state).nmeas)[tz])+' out of '+ $
                strn((*state).nvisits)+' measured'
      if (*(*state).nmeas)[tz] ge 2 then begin
         fmt='(f4.2)'
         scatstr='  xi,eta scat '+ $
                 strn((*(*state).xiscat)[tz],format=fmt)+','+ $
                 strn((*(*state).etascat)[tz],format=fmt)+' arcsec'
      endif else begin
         scatstr=' '
      endelse
   endif else begin
      widget_control,(*state).curtrkid,set_value='<ready>'
      ratestr = 'rate not valid'
      nmeasstr=' '
      scatstr=' '
   endelse
   widget_control,(*state).rateid,set_value=ratestr
   widget_control,(*state).nmeasid,set_value=nmeasstr
   widget_control,(*state).scatid,set_value=scatstr

   if (*state).curtrn ge 0 then begin
      widget_control,(*state).curtrnid,set_value=strn((*state).curtrn)
      tz=trimrank(where((*(*state).skid) eq (*state).curtrk))
      xpos=(*(*state).sx)[(*state).curvis,tz]
      ypos=(*(*state).sy)[(*state).curvis,tz]
      astcvt,'xy',xpos,ypos,(*(*state).astinfo),'rd',ra,dec
      rastr,ra,3,ras
      decstr,dec,2,decs
      mags=strn((*(*state).smag)[(*state).curvis,tz],format='(f10.1)')
      fwhms=strn((*(*state).sfwhm)[(*state).curvis,tz],format='(f10.1)')
      widget_control,(*state).rawinid,set_value=ras
      widget_control,(*state).decwinid,set_value=decs
      widget_control,(*state).magid,set_value=mags
      widget_control,(*state).fwhmid,set_value=fwhms
   endif else begin
      widget_control,(*state).curtrnid,set_value='<new>'
      widget_control,(*state).rawinid,set_value=' '
      widget_control,(*state).decwinid,set_value=' '
      widget_control,(*state).magid,set_value=' '
      widget_control,(*state).fwhmid,set_value=' '
   endelse

end

; This routine takes care of building the information for the current
;   set of objects and where they are relative to the current set.  The
;   object list is not managed here, just used.
;------------------------------------------------------------------------------
pro dlooker_objpop,state

   if (*state).nobj le 0 then return

   ; number of points for the error ellipse
   npts=301

   ; Destined for the state structure
   xobj=fltarr((*state).nvisits,(*state).nobj)
   yobj=fltarr((*state).nvisits,(*state).nobj)
   x1sig=fltarr(npts,(*state).nobj)
   y1sig=fltarr(npts,(*state).nobj)
   x3sig=fltarr(npts,(*state).nobj)
   y3sig=fltarr(npts,(*state).nobj)

   for i=0,(*state).nobj-1 do begin
      ephem,(*(*state).jdmid),500,2,(*(*state).objlist)[i],eph
      ra=trimrank(eph[0,*])
      dec=trimrank(eph[1,*])
      astcvt,'rd',ra,dec,(*(*state).astinfo),'SN',xi,eta
      astcvt,'rd',ra,dec,(*(*state).astinfo),'xy',x,y
      xobj[*,i]=x
      yobj[*,i]=y

;      jd=(*(*state).jdmid)[0]+[0.0d0,4.0d0]/24.0d0

      ; compute the line of variations position and the error ellipse
      ;   size,  assume that it's constant during the set.
      ephem,(*(*state).jdmid)[0],500,11,(*(*state).objlist)[i],eph
      vra=trimrank(eph[10])
      vdec=trimrank(eph[11])
      erra=trimrank(eph[12])
      errb=trimrank(eph[13])

      z=where(x ge 0 and x lt (*state).nx and $
              y ge 0 and y lt (*state).ny,count)
      if count ne 0 then begin
         print,(*(*state).objlist)[i],erra,errb
         print,z
         print,x[z]
         print,y[z]
      endif

      ; project the line of variations onto the sky for use
      astrd2sn,ra[0]+0.001*vra,dec[0]+0.001*vdec,ra[0],dec[0],vxi,veta,/arcsec
      vang=atan(veta,vxi)

      ; ellipse relative to the object position
      ang=findgen(npts)/((npts-1)/2)*!dpi
      xell = erra*cos(ang)
      yell = errb*sin(ang)
      x1sig[*,i] =  xell*cos(vang) + yell*sin(vang)
      y1sig[*,i] = -xell*sin(vang) + yell*cos(vang)
      x3sig[*,i] = 3*x1sig[*,i]
      y3sig[*,i] = 3*y1sig[*,i]
      
   endfor

   ptr_free,(*state).xobj
   ptr_free,(*state).yobj
   ptr_free,(*state).x1sig
   ptr_free,(*state).y1sig
   ptr_free,(*state).x3sig
   ptr_free,(*state).y3sig
   (*state).xobj=ptr_new(xobj)
   (*state).yobj=ptr_new(yobj)
   (*state).x1sig=ptr_new(x1sig)
   (*state).y1sig=ptr_new(y1sig)
   (*state).x3sig=ptr_new(x3sig)
   (*state).y3sig=ptr_new(y3sig)

end

; This routine takes the pre-existing byte image for the zoom window and
;   puts it on the display.  It doesn't know what it is putting up, it just
;   gets it up there.  Then, if RESET is not set, a circle is drawn on top
;   of the image to indicate the position of the current transient.
;------------------------------------------------------------------------------
pro dlooker_refreshzoom,state,xpos,ypos,radius,RESET=reset

   ; size of extraction region
   wid = (*state).szsx/(*state).zf
   zsz = wid * (*state).zf

   widget_control, (*state).zoomwin, get_value=winnum
   WSET, winnum
   tv,rebin((*state).zim,zsz,zsz,/sample)

   if not keyword_set(reset) then begin

      x = (xpos - (*state).zx0) * (*state).zf
      y = (ypos - (*state).zy0) * (*state).zf

      theta=findgen(361.0)/!radeg
      xcirc=radius*cos(theta)
      ycirc=radius*sin(theta)

      plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
         xr=(*state).zx0+[-0.5,wid-0.5], $
         yr=(*state).zy0+[-0.5,wid-0.5], $
         xstyle=5,ystyle=5,/noerase

;      if (*state).curframe eq 1 then color='0040ff'xl $
;      else color='ff8000'xl

      if (*state).curtrn ge 0 then begin
         oplot,xcirc+xpos,ycirc+ypos,color='00ff00'xl
      endif else begin
         oplot,xcirc+xpos,ycirc+ypos,color='00ffff'xl
      endelse

   endif

end

;------------------------------------------------------------------------------
pro dlooker_setup,state,run,dbname

   (*state).db=dbname
   (*state).rdir=run

   openmysql,dblun,(*state).db
   cmd='select file from diff'+ $
       ' where rdir='+quote((*state).rdir)+ $
       ' and file not like '+quote('SUPA%')+ $
       ' and file not like '+quote('20%')+ $
       ' order by file;'
print,cmd
   mysqlquery,dblun,cmd,files,format='a',ngood=nfiles
   free_lun,dblun

   field=strarr(nfiles)
   chip=intarr(nfiles)
   part=strarr(nfiles)
   visit=intarr(nfiles)
   set=strarr(nfiles)

   for i=0,nfiles-1 do begin
      pos=strpos(files[i],'-')
      set[i]=strmid(files[i],0,pos)
      words=strsplit(set[i],'.',/extract)
      set[i]=strjoin(words[0:-2],'.')
      field[i] = words[0]
      chip[i] = fix(words[1])
      if n_elements(words) eq 3 then begin
         visit[i] = fix(words[2])
      endif else begin
         part[i] = words[2]
         visit[i] = fix(words[3])
      endelse
   endfor

   fieldlist=field[uniq(field,sort(field))]
   chiplist=chip[uniq(chip,sort(chip))]
   partlist=part[uniq(part,sort(part))]
   setlist=set[uniq(set,sort(set))]

   ptr_free,(*state).chiplist
   ptr_free,(*state).fieldlist
   ptr_free,(*state).partlist
   ptr_free,(*state).setlist

   (*state).chiplist=ptr_new(chiplist)
   (*state).fieldlist=ptr_new(fieldlist)
   (*state).partlist=ptr_new(partlist)
   (*state).setlist=ptr_new(setlist)

   (*state).x0=0
   (*state).y0=0

   widget_control,(*state).runid,set_value=(*state).rdir+' ['+(*state).db+']'

   widget_control,(*state).basisid,/sensitive
   widget_control,(*state).typeid,/sensitive

   dlooker_loadset,state,(*(*state).setlist)[0]

end

; This routine helps manage the zoom window.  If called with RESET the
;   window is blanked out and none of the input arguments matter except
;   for state.
; If not resetting, this routine will build the byte image that needs to
;   be maintained in the zoom window.  As a curtesy, at the very end
;   the routine that makes everything show up is called (dlooker_refreshzoom)
;------------------------------------------------------------------------------
pro dlooker_setzoom,state,visit,xpos,ypos,peaksig,sky,skysig, $
                       RESET=reset,CURTRAN=curtran

   if keyword_set(reset) then begin
      (*state).zim[*]=0B
      dlooker_refreshzoom,state,reset=reset
      return
   endif

   ; size of extraction region
   wid = (*state).szsx/(*state).zf
   zsz = wid * (*state).zf

   ; Extract image sub-section around object, watch out for image edges.
   x10 = xpos - wid/2
   x20 = x10  + wid - 1
   y10 = ypos - wid/2
   y20 = y10  + wid - 1

   x1=max([x10,0])
   x2=min([x20,(*state).nx-1])
   y1=max([y10,0])
   y2=min([y20,(*state).ny-1])

   subim=replicate(sky,wid,wid)

   subim[x1-x10,y1-y10] = (*(*state).cube)[x1:x2,y1:y2,visit]

   (*state).zx0 = x10
   (*state).zy0 = y10

   subim = ( (subim-(sky-2.0*skysig) > 0) + 10 )^0.1
   maxv  = ( (peaksig > (sky+2.0*skysig))-(sky-2.0*skysig) + 10 )^0.1
   minv  = 1.0 > min(subim)

   (*state).zim=bytscl(subim,min=minv,max=maxv,top=255)

   dlooker_refreshzoom,state,xpos,ypos,(*(*state).objrad)[visit]

end

;------------------------------------------------------------------------------
pro dlooker_stretch,state,ALL=all

   if not keyword_set(all) then all=0

   if all then todo=indgen((*state).nvisits) else todo=(*state).curvis

   for i=0,n_elements(todo)-1 do begin

      ; set the initial sky scaling levels for the subimages.
      widget_control, (*state).minsigid, GET_VALUE=dm
      widget_control, (*state).maxsigid, GET_VALUE=dp
      dm = float(dm[0])
      dp = float(dp[0])
      skysclim,(*(*state).cube)[*,*,todo[i]],lowval,hival,amean,asig, $
         lowclip=0.1,hiclip=0.9
      alowval=amean + dm*asig
      ahival =amean + dp*asig
      (*(*state).bcube)[*,*,todo[i]]=bytscl((*(*state).cube)[*,*,todo[i]], $
         min=alowval,max=ahival,top=255)

   endfor

end

;------------------------------------------------------------------------------
; This subroutine takes the input measurement.  This input needs to go to
;   two places.  1) the internal arrays and 2) the database.
;
; There are four cases to handle
;   A) no data loaded, cannot happen
;   B) NEW TRACKLET/NEW TRANSIENT   curtrk=-1 curtrn=-1
;        create new transient
;        create new tracklet and link to transient
;   C) OLD TRACKLET/NEW TRANSIENT   curtrk>=0 curtrn=-1
;        create new transient
;        link to tracklet
;   D) OLD TRACKLET/OLD TRANSIENT   curtrk>=0 curtrn>=0
;        update transient
;
; At the end, update the predicted positions for this tracklet
;
pro dlooker_tracklet_addobs,state,x,y,omag,fwhm

   ; to simplify some typing
   visit=(*state).curvis
   if (*state).ntrk gt 0 then $
      tz=trimrank(where((*(*state).skid) eq (*state).curtrk)) $
   else tz = -1

   ; compute the ra,dec for the input measurement
   astcvt,'xy',x,y,(*(*state).astinfo),'rd',ora,odec

   c=','
   openmysql,dblun,'gen'

   ; This is for control of simultaneous input by multiple users, uniqueid
   ;   never persists in the database.
   tag=(*state).uniqueid+':'+(*state).initials

   ; take care of saving/updating the transient first
   if (*state).curtrn lt 0 then begin
      ; create a new entry and then get the transient id
      cmd='insert into transient set initials='+quote(tag)+c+'type=0;'
      mysqlcmd,dblun,cmd
      cmd='select id from transient where initials='+quote(tag)+';'
      mysqlquery,dblun,cmd,newid,format='l'
      initials=(*state).initials
   endif else begin
      newid=(*state).curtrn
      cmd='select initials from transient where id='+strn(newid)+';'
      mysqlquery,dblun,cmd,initials,format='a'
   endelse
   dlooker_updateidstr,initials,(*state).initials
   ; must save the transient data to the database
   cmd=['update transient set', $
        'rdir='+quote((*state).rdir)+c, $
        'fileroot='+quote((*(*state).fileroot)[visit])+c, $
        'file='+quote((*(*state).visitlist)[visit])+c, $
        'invert='+strn((*(*state).invert)[visit])+c, $
        'jdmid='+strn((*(*state).jdmid)[visit],format='(f13.5)')+c, $
        'x='+strn(x,format='(f10.3)')+c, $
        'y='+strn(y,format='(f10.3)')+c, $
        'ra='+strn(ora,format='(f20.10)')+c, $
        'decl='+strn(odec,format='(f20.10)')+c, $
        'fwhm='+strn(fwhm,format='(f10.2)')+c, $
        'mag='+strn(omag,format='(f10.2)')+c, $
        'state='+quote('y')+c, $
        'initials='+quote((*state).initials)+c, $
        'feedback=0', $
        'where id='+strn(newid)+';']
   mysqlcmd,dblun,cmd
   (*state).curtrn = newid

   ; now link to a tracklet

   if (*state).curtrk lt 0 then begin
      ; Create new tracklet in database and save the related id.
      cmd='select max(id) from tracklet;'
      mysqlquery,dblun,cmd,lasttrid,format='l'
      newtrid=lasttrid+1
      cmd='insert into tracklet values ('+strn(newtrid)+c+strn(newid)+');'
      mysqlcmd,dblun,cmd
      (*state).curtrk=newtrid
   endif else begin
      cmd='select id from tracklet where id='+strn((*state).curtrk)+ $
          ' and tranid='+strn((*state).curtrn)+';'
      mysqlquery,dblun,cmd,chkid,ngood=ncheck
      if ncheck eq 0 then begin
         cmd='insert into tracklet values ('+ $
             strn((*state).curtrk)+c+strn(newid)+');'
         mysqlcmd,dblun,cmd
      endif
   endelse

   free_lun,dblun

   dlooker_trkpop,state

end

;------------------------------------------------------------------------------
; This routine queries the database and figures out the list of tracklets that
;   could have a position on this set of visits.
pro dlooker_trkpop,state

   openmysql,dblun,'gen'

   ; Get the complete list of tracklets for this run
   cmd=['select tracklet.id from tracklet,transient', $
       ' where tranid=transient.id', $
       ' and rdir='+quote((*state).rdir), $
       ' group by tracklet.id;']

   mysqlquery,dblun,cmd,trkid,format='l',ngood=nalltrks

   ntrk=0

print,'trkpop: nalltrks ',strn(nalltrks)

   for i=0,nalltrks-1 do begin

      ; This is a place to collect information for just this one tracklet
      ;   (indexed by i) to see if it is relevant to this set.
      ttranid=replicate(-1L,(*state).nvisits)
      tx=replicate(-10.0-(*state).maxdist,(*state).nvisits)
      ty=replicate(-10.0-(*state).maxdist,(*state).nvisits)
      tmag=replicate(99.9,(*state).nvisits)
      tfwhm=replicate(-5,(*state).nvisits)

      ; Get all the positions for this tracklet
      cmd=['select transient.id,fileroot,jdmid,x,y,ra,decl,fwhm,mag', $
           'from transient,tracklet', $
           'where tranid=transient.id', $
           'and tracklet.id='+strn(trkid[i]), $
           'order by jdmid;']
      mysqlquery,dblun,cmd,tranid,fileroot,jdmid,x,y,ra,dec,fwhm,mag, $
         format='l,a,d,f,f,d,d,f,f',ngood=ntran

      if ntran eq 1 then begin
         print,'SINGLETON WARNING!!!!'
         print,'Tracklet ',strn(trkid[i]),' has only one measurement.'
         print,'You can find it on ',fileroot
      endif

      ; If any of these measurements fall on the current set, copy them to
      ;   the temporary array for this tracklet
      for j=0,ntran-1 do begin
         z=trimrank(where(fileroot[j] eq (*(*state).fileroot),count))
         if count eq 1 then begin
            print,fileroot[j],'trk',trkid[i],', trn',tranid[j],x[j],y[j], $
               format='(a-15,1x,a,1x,i6,a,1x,i6,2(1x,f6.1))'
            tx[z]=x[j]
            ty[z]=y[j]
            tmag[z]=mag[j]
            tfwhm[z]=fwhm[j]
            ttranid[z]=tranid[j]
         endif
      endfor

      ; compute rate and direction, anchor time is the time of the first
      ;   image in this set.  This gets computed for all tracklets that have
      ;   two or more positions, regardless of position.
      if ntran ge 2 then begin
         ; convert ra,dec of all positions to xi,eta
         astcvt,'rd',ra,dec,(*(*state).astinfo),'SN',xi,eta
         time=jdmid-(*(*state).jdmid)[0]

         ; fit a polynomial to positions
         if ntran le 3 then nterms=1 else nterms=2
         xicoeff=poly_fit(time,xi,nterms,yfit=xifit)
         etacoeff=poly_fit(time,eta,nterms,yfit=etafit)

         if ntran ge 3 then begin
            xiscat=mean(abs(xi-xifit))
            etascat=mean(abs(eta-etafit))
            if xiscat gt 0.5 or etascat gt 0.5 then $
               print,'Tracklet ',strn(trkid[i]), $
                     ' has high scatter: ',xiscat,etascat
         endif else begin
            xiscat=0.
            etascat=0.
         endelse

         ; compute rate and direction (uses only linear portion of fit)
         rate=sqrt(xicoeff[1]^2+etacoeff[1]^2) / 24.0  ; arcsec/hr
         dir =atan(xicoeff[1],etacoeff[1]) * !radeg
         if dir lt 0.0 then dir=dir+360.0

      endif else begin
         astcvt,'rd',ra,dec,(*(*state).astinfo),'SN',xi,eta
         rate=0.
         dir=0.
         xiscat=0.
         etascat=0.
         xicoeff=[xi,0.]
         etacoeff=[eta,0.]
      endelse

      ; This part will fill in any visits where this object wasn't
      ;   measured with a predicted position
      z=where(ttranid lt 0,count)
      if count gt 0 then begin

         ; compute predicted xi,eta for this set
         time=(*(*state).jdmid)-(*(*state).jdmid)[0]
         xipred=poly(time,xicoeff)
         etapred=poly(time,etacoeff)

         ; lcompute predicted position on the image
         astcvt,'SN',xipred,etapred,(*(*state).astinfo),'xy',px,py
         tx[z] = px[z]
         ty[z] = py[z]
         z1=where(px ge 0 and px lt (*state).nx and $
                  py ge 0 and py lt (*state).ny, count1)
         if count1 gt 0 then begin
            for j=0,count1-1 do begin
               print,'v'+strn(z1[j]),ttranid[j], $
                     px[z1[j]],py[z1[j]],' trk '+strn(trkid[i]), $
                     format='(5x,a3,1x,i6,2(1x,f6.1),1x,a)'
            endfor
         endif

      endif

      ; Does this tracklet fall on this set (measured or predicted)?
      z=where(tx gt 0 and tx lt (*state).nx and $
              ty gt 0 and ty le (*state).ny,count)
      ; if not, go on to the next tracklet
      if count eq 0 then continue

      ; Build up the local copy of tracklet/transient information
      if ntrk eq 0 then begin
         ntrk=1
         skid=trkid[i]
         snid=ttranid
         sntran=ntran
         sx=tx
         sy=ty
         smag=tmag
         sfwhm=tfwhm
         sdir=dir
         srate=rate
         rascat=xiscat
         decscat=etascat
      endif else begin
         ntrk++
         skid=[skid,trkid[i]]
         snid=[[snid],[ttranid]]
         sntran=[sntran,ntran]
         sx=[[sx],[tx]]
         sy=[[sy],[ty]]
         smag=[[smag],[tmag]]
         sfwhm=[[sfwhm],[tfwhm]]
         sdir=[sdir,dir]
         srate=[srate,rate]
         rascat=[rascat,xiscat]
         decscat=[decscat,etascat]
      endelse

   endfor

   free_lun,dblun

   ; store everything in the state structure
   (*state).ntrk=ntrk
   if ntrk gt 0 then begin
      ptr_free,(*state).skid
      ptr_free,(*state).snid
      ptr_free,(*state).nmeas
      ptr_free,(*state).sx
      ptr_free,(*state).sy
      ptr_free,(*state).smag
      ptr_free,(*state).sfwhm
      ptr_free,(*state).dir
      ptr_free,(*state).rate
      ptr_free,(*state).xiscat
      ptr_free,(*state).etascat
      (*state).skid =ptr_new(skid)
      (*state).snid =ptr_new(snid)
      (*state).nmeas =ptr_new(sntran)
      (*state).sx =ptr_new(sx)
      (*state).sy =ptr_new(sy)
      (*state).smag =ptr_new(smag)
      (*state).sfwhm =ptr_new(sfwhm)
      (*state).rate=ptr_new(srate)
      (*state).dir =ptr_new(sdir)
      (*state).xiscat =ptr_new(rascat)
      (*state).etascat =ptr_new(decscat)
   endif

end

;------------------------------------------------------------------------------
pro dlooker_updateidstr,idstr,initials

   words=strsplit(idstr,',',/extract)
   if words[0] eq '' then begin
      idstr=initials
   endif else begin
      if words[n_elements(words)-1] ne initials then $
         idstr = idstr+','+initials
   endelse

end

;------------------------------------------------------------------------------
pro dlooker_updatescore,state,x0,x1,y0,y1,RESET=reset,REFRESH=refresh

   ; Make a local copy to save some typing
   sf = (*state).sf

   if not keyword_set(refresh) then begin
      if keyword_set(reset) then begin

         (*state).srgb[*,*,0] = 20B
         (*state).srgb[*,*,1] = 20B
         (*state).srgb[*,*,2] = 0B

         i0 = fix(( 0-(*state).nx/2.0 ) / sf + (*state).szsx/2.0) > 0
         j0 = fix(( 0-(*state).ny/2.0 ) / sf + (*state).szsy/2.0) > 0
         i1 = fix(i0 + (*state).nx/sf) < ((*state).szsx-1)
         j1 = fix(j0 + (*state).ny/sf) < ((*state).szsy-1)

         (*state).srgb[i0:i1,j0:j1,*] = 0B

      endif else begin

         i0 = fix(( x0-(*state).nx/2.0 ) / sf + (*state).szsx/2.0) > 0
         j0 = fix(( y0-(*state).ny/2.0 ) / sf + (*state).szsy/2.0) > 0
         i1 = fix(( x1-(*state).nx/2.0 ) / sf + (*state).szsx/2.0) < ((*state).szsx-1)
         j1 = fix(( y1-(*state).ny/2.0 ) / sf + (*state).szsy/2.0) < ((*state).szsy-1)

         inc = 50B
         bsub = (*state).srgb[i0:i1,j0:j1,1]
         z=where(bsub lt 185B-inc,count)
         if count ne 0 then begin
            bsub[z] = bsub[z] + inc
            (*state).srgb[i0:i1,j0:j1,1] = bsub
         endif
      endelse
   endif

   widget_control, (*state).scorewin, get_value=winnum
   WSET, winnum

   tv,(*state).srgb,true=3

   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr = (*state).nx/2 + (*state).szsx * [-0.5,0.5] * (*state).sf, $
      yr = (*state).ny/2 + (*state).szsy * [-0.5,0.5] * (*state).sf, $
      xstyle=5,ystyle=5,/noerase

   ; outline current area display in main window
   xv = (*state).x0 + [ 0.0, 1.0, 1.0, 0.0, 0.0 ] * (*state).szx
   yv = (*state).y0 + [ 0.0, 0.0, 1.0, 1.0, 0.0 ] * (*state).szy
   oplot,xv,yv,color='800070'xl

   ; plot tracklets on field
   if (*state).ntrk gt 0 then begin
      for i=0,(*state).ntrk-1 do begin
         ; plot all measured positions with green +
         zm=where( (*(*state).snid)[*,i] ge 0, countm )
         plots,(*(*state).sx)[zm,i], $
               (*(*state).sy)[zm,i], $
            psym=1,/data,color='00ff00'xl,symsize=1.0

         ; plot all predicted positions with yellow + 
         z=where( (*(*state).snid)[*,i] le 0, count )
         plots,(*(*state).sx)[z,i], $
               (*(*state).sy)[z,i], $
               psym=1,/data,color='00ffff'xl,symsize=1.0
         ; connect all positions with a line
         plots,(*(*state).sx)[*,i], $
               (*(*state).sy)[*,i],/data, $
               linestyle=0,color='ff00ff'xl
      endfor
   endif

end
;------------------------------------------------------------------------------

PRO dlooker_eve, event

   widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   widget_control, event.top, GET_UVALUE=state

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   if event_name ne 'Initials' and not exit then begin
      dlooker_checkinitials,state
      if (*state).initials eq '' then return
   endif

   CASE event_name OF

      'THE_MENU': BEGIN
         case event.value OF

            'Copy stretch': begin
               dlooker_stretch,state,/all
               dlooker_display,state,/noscore
            end

            'Delete Transient': begin
               if (*state).curtrn lt 0 then return
               text=['You are about to delete this transient object.', $
                     'This action cannot be undone.  Are you sure?']
               ans=qannounc(text,truelabel='Delete Transient', $
                            falselabel='Cancel',group_leader=event.top, $
                            title='Confirmation of delete operation')
               if ans eq 1 then begin
                  dlooker_delete,state,(*state).curtrn
                  dlooker_setzoom,state,/reset
                  dlooker_display,state,/noscore
               endif
            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               RETURN
            end

            'GoTo tracklet': begin
print,'goto tracklet'
            end

            'GoTo transient': begin
               tranid = qinput(PROMPT='Please enter the transient id', $
                              title='GoTo transient',cancelled=cancelled, $
                              group_leader=event.top,/long)
               if cancelled then return
               if tranid le 0 then return
               openmysql,dblun,'gen'
               cmd='select transient.rdir,fileroot,inst'+ $
                   ' from transient,nh11.runinfo'+ $
                   ' where id='+strn(tranid)+ $
                   ' and transient.rdir=runinfo.rdir;'
               mysqlquery,dblun,cmd,rdir,fileroot,inst, $
                  format='a,a',ngood=ncheck
               free_lun,dblun
               if ncheck ne 1 then begin
                  print,'Transient ',strn(tranid),' not found.'
                  return
               end
               dlooker_setup,state,rdir,'nh11'
               words=strsplit(fileroot,'.',/extract)
               field=words[0]
               chip=fix(words[1])
               if inst eq 'SuPrime Cam' then begin
                  part=words[2]
               endif else begin
                  part=''
               endelse
               dlooker_loadset2,state,field,chip,part
               dlooker_setzoom,state,/reset
               dlooker_display,state

            end
         
;            'Postscript Hardcopy': begin
;               if (*state).lastfn2 eq '' then return
;               hardim,[[[*(*state).r]],[[*(*state).g]],[[*(*state).g]]], $
;                  true=3,queue='chani',file='dlooker.ps',/noprint,autosize=2, $
;                  /landscape ;,title='KPNO/MOSAIC  1998 Nov 18'
;               hardim,*(*state).r, $
;                  queue='chani',file='dlooker1.ps',/noprint,autosize=2, $
;                  /landscape ;,title='KPNO/MOSAIC  1998 Nov 18'
;               hardim,*(*state).g, $
;                  queue='chani',file='dlooker2.ps',/noprint,autosize=2, $
;                  /landscape ;,title='KPNO/MOSAIC  1998 Nov 18'
;            end
;
            'Refresh display': begin
               if (*state).rdir eq '' then return
               dlooker_display,state,/noscore
            end

            'Select chip': begin
               if (*state).rdir eq '' then return
               if n_elements((*(*state).chiplist)) eq 1 then return
               chip=picker(string((*(*state).chiplist)),index=idx, $
                             group=event.top,title='Pick Chip')
               if chip eq '[[[CANCEL]]]' then return
               chip=fix(chip)
               dlooker_loadset2,state,(*state).field, $
                                      chip, $
                                      (*(*state).partlist)[0]
               dlooker_setzoom,state,/reset
               dlooker_display,state
            end

            'Select field': begin
               if (*state).rdir eq '' then return
               field=picker((*(*state).fieldlist),index=idx, $
                             group=event.top,title='Pick Field')
               if field eq '[[[CANCEL]]]' then return
               dlooker_loadset2,state,field, $
                                      (*(*state).chiplist)[0], $
                                      (*(*state).partlist)[0]
               dlooker_setzoom,state,/reset
               dlooker_display,state
            end

            'Select run': begin
               dlooker_getrunlist,runlist,dblist
               rundir=picker(runlist+' ['+dblist+']',index=idx, $
                             group=event.top,title='Pick Extension')
               if rundir eq '[[[CANCEL]]]' then return
               dlooker_setup,state,runlist[idx],dblist[idx]
               dlooker_setzoom,state,/reset
               dlooker_display,state
            end

            'Select Transient': begin

               if (*state).ntrk eq 0 then begin
                  text=['There are no tracklets on, or predicted to be on the current frame.', $
                        'No selection is possible.']
                  result=dialog_message(text,/ERROR)
                  return
               endif

               idlist=strcompress(string((*(*state).skid)),/remove_all)
               result=picker(idlist,index=idx, $
                             group=event.top,title='Pick Extension')
               if result eq '[[[CANCEL]]]' then return

               (*state).curtrk=(*(*state).skid)[idx]
               (*state).curtrn=(*(*state).snid)[(*state).curvis,idx]
               dlooker_display,state,/noscore
               dlooker_setzoom,state,/reset

            end

;            'Snap object': begin
;
;               if (*state).curtrk lt 0 then return

;               ; size of extraction region
;               wid = (*state).szsx/(*state).zf
;               zsz = wid * (*state).zf
;
;               ; offset between frame 2 and reference frame
;               xoff = fix( (*(*state).offvals)[(*state).idxfn2*2]   + 0.5 )
;               yoff = fix( (*(*state).offvals)[(*state).idxfn2*2+1] + 0.5 )
;
;               ; Positions of the objects in their own frames
;               xa = (*(*state).xyvals)[0,(*state).curtrk]
;               ya = (*(*state).xyvals)[1,(*state).curtrk]
;               xb = (*(*state).xyvals)[ ((*state).idxfn2+1)*2,   (*state).curtrk ]
;               yb = (*(*state).xyvals)[ ((*state).idxfn2+1)*2+1, (*state).curtrk ]
;
;               ; position of second image in frame 1
;               xb1 = xb - xoff
;               yb1 = yb - yoff
;
;               ; distance moved between images
;               dist = sqrt((xa-xb1)^2 + (ya-yb1)^2)
;
;               ; mid-point between two object positions in frame 1
;               xmid = (xa+xb1)/2.0
;               ymid = (ya+yb1)/2.0
;
;               fmt='(a,2(1x,f6.1),1x,f7.3,"+/-",f6.3,1x,g10.3,"+/-",g7.3)'
;
;               print,'extract around object',(*state).curtrk
;
;               basphote,(*state).gain,*(*state).im1,(*(*state).info1).exptime, $
;                  xa,ya,(*state).radius,(*state).radius+10.0,(*state).radius+30.0, $
;                  boxmrad = (*state).radius > 5,fwhm=fwhm,max=maxsig1,/exact, $
;                  mag=mag,err=err,/nolog,pscale=(*state).pscale, $
;                  fname=(*(*state).fntrip)[0],name=(*state).curtrkname, $
;                  xcen=xpos,ycen=ypos,skymean=sky1,skyerr=skysig1,/silent
;               print,'frame 1 x,y ',xa,ya,mag,err,sky1,skysig1,format=fmt
;
;               basphote,(*state).gain,*(*state).im2,(*(*state).info1).exptime, $
;                  xb,yb,(*state).radius,(*state).radius+10.0,(*state).radius+30.0, $
;                  boxmrad = (*state).radius > 5,fwhm=fwhm,max=maxsig2,/exact, $
;                  mag=mag,err=err,/nolog,pscale=(*state).pscale, $
;                  fname=(*(*state).fntrip)[(*state).idxfn2+1],name=(*state).curtrkname, $
;                  xcen=xpos,ycen=ypos,skymean=sky2,skyerr=skysig2,/silent
;               print,'frame 2 x,y ',xb,yb,mag,err,sky2,skysig2,format=fmt
;
;               ; Get the sky scaling levels for the subimages.
;               widget_control, (*state).minsigid, GET_VALUE=dm
;               widget_control, (*state).maxsigid, GET_VALUE=dp
;               dm = float(dm[0])
;               dp = float(dp[0])
;
;               ; Probably would use LZW but it seems to need licensing.
;               compression=0
;
;               widget_control, (*state).curtrkid, GET_VALUE=objname
;
;               ; FITS versions, add dw extra pixels to extraction
;               dw=300
;               dlooker_getsubimage,state,xa,ya,1,wid+dw,sky1,subim1
;               hdr1 = (*(*state).hdr1)
;               sxaddpar,hdr1,'NAXIS1',wid+dw
;               sxaddpar,hdr1,'NAXIS2',wid+dw
;               sxaddpar,hdr1,'XLLHC',fix(xa)-fix(wid+dw)/2
;               sxaddpar,hdr1,'YLLHC',fix(ya)-fix(wid+dw)/2
;               writefits,objname+'_1.fits',subim1,hdr1
;
;               dlooker_getsubimage,state,xb,yb,2,wid+dw,sky2,subim2
;               hdr2 = (*(*state).hdr2)
;               sxaddpar,hdr2,'NAXIS1',wid+dw
;               sxaddpar,hdr2,'NAXIS2',wid+dw
;               sxaddpar,hdr2,'XLLHC',fix(xb)-fix(wid+dw)/2
;               sxaddpar,hdr2,'YLLHC',fix(yb)-fix(wid+dw)/2
;               writefits,objname+'_2.fits',subim2,hdr2
;
;               ; Linear stretch versions
;               ; image 1 B/W, centered on object
;               dlooker_getsubimage,state,xa,ya,1,wid,sky1,subim1
;               subim1 = reverse(float(subim1)-sky1,2)
;               bsubim1 = bytscl(subim1,min=(*state).lowval1-sky1, $
;                                       max=(*state).highval1-sky1,top=255)
;               write_tiff,objname+'_1.tiff',bsubim1,compression=compression
;
;               ; image 2 B/W, centered on object
;               dlooker_getsubimage,state,xb,yb,2,wid,sky2,subim2
;               subim2 = reverse(float(subim2)-sky2,2)
;               bsubim2 = bytscl(subim2,min=(*state).lowval2-sky2, $
;                                       max=(*state).highval2-sky2,top=255)
;               write_tiff,objname+'_2.tiff',bsubim2,compression=compression
;
;               ; Power-law stretch versions
;               lsubim1 = ( (subim1+2.0*skysig1 > 0) + 10 )^0.1
;               maxv1   = ( ((maxsig1-sky1) > (2.0*skysig1))+2.0*skysig1 + 10 )^0.1
;               minv1   = 1.0 > min(lsubim1)
;               lsubim1 = bytscl(lsubim1,min=minv1,max=maxv1,top=255)
;               write_tiff,objname+'_1l.tiff',lsubim1,compression=compression
;
;               lsubim2 = ( (subim2+2.0*skysig2 > 0) + 10 )^0.1
;               maxv2   = ( ((maxsig2-sky2) > (2.0*skysig2))+2.0*skysig2 + 10 )^0.1
;               minv2   = 1.0 > min(lsubim2)
;               lsubim2 = bytscl(lsubim2,min=minv2,max=maxv2,top=255)
;               write_tiff,objname+'_2l.tiff',lsubim2,compression=compression
;
;               ; image 1 B/W, centered on object mid-point
;               dlooker_getsubimage,state,xmid,ymid,1,wid+dist,sky1,subim1
;               subim1 = reverse(float(subim1)-sky1,2)
;               bsubim1 = bytscl(subim1,min=(*state).lowval1-sky1, $
;                                       max=(*state).highval1-sky1,top=255)
;               write_tiff,objname+'_1c.tiff',bsubim1,compression=compression
;
;               ; image 2 B/W, centered on object mid-point
;               dlooker_getsubimage,state,xmid+xoff,ymid+yoff,2,wid+dist,sky2,subim2
;               subim2 = reverse(float(subim2)-sky2,2)
;               bsubim2 = bytscl(subim2,min=(*state).lowval2-sky2, $
;                                       max=(*state).highval2-sky2,top=255)
;               write_tiff,objname+'_2c.tiff',bsubim2,compression=compression
;
;               ; color composite of image 1 and 2
;               write_tiff,objname+'_c.tiff',[[[bsubim1]],[[bsubim2]],[[bsubim2]]], $
;                  planarconfig=2
;
;               ; Power-law stretch versions
;               lsubim1 = ( (subim1+2.0*skysig1 > 0) + 10 )^0.1
;               maxv1   = ( ((maxsig1-sky1) > (2.0*skysig1))+2.0*skysig1 + 10 )^0.1
;               minv1   = 1.0 > min(lsubim1)
;               lsubim1 = bytscl(lsubim1,min=minv1,max=maxv1,top=255)
;               write_tiff,objname+'_1lc.tiff',lsubim1,compression=compression
;
;               lsubim2 = ( (subim2+2.0*skysig2 > 0) + 10 )^0.1
;               maxv2   = ( ((maxsig2-sky2) > (2.0*skysig2))+2.0*skysig2 + 10 )^0.1
;               minv2   = 1.0 > min(lsubim2)
;               lsubim2 = bytscl(lsubim2,min=minv2,max=maxv2,top=255)
;               write_tiff,objname+'_2lc.tiff',lsubim2,compression=compression
;
;               write_tiff,objname+'_l.tiff',[[[lsubim1]],[[lsubim2]],[[lsubim2]]], $
;                  planarconfig=2
;
;            end
;
            'Suppress overlay': begin
               if (*state).rdir eq '' then return
               (*state).nomarks = not (*state).nomarks
               dlooker_display,state,/noscore
               dlooker_updatescore,state,/refresh
            end

            'Toggle color mode': begin
               if (*state).rdir eq '' then return
               (*state).colormode = not (*state).colormode
               dlooker_display,state,/noscore
            end

            'Toggle lines': begin
               if (*state).rdir eq '' then return
               (*state).lines = not (*state).lines
               dlooker_display,state,/noscore
               dlooker_updatescore,state,/refresh
            end

            else: begin
               MESSAGE, 'Unknown menu event:', /INFO
               HELP, event, /STRUCTURE
            end

         endcase

      end

      'Blink Set': begin
         if (*state).rdir eq '' then return
         ; Z is the group number
         z=where((*(*state).nrep) gt 1,count)
         if count eq 0 then return
         idx=where((*(*state).night) eq z[event.index],count)
         if count eq 0 then return  ; safety, shouldn't happen
         ptr_free,(*state).blinkidx
         (*state).blinkidx=ptr_new(idx)
      end

      'blink': begin
         if (*state).rdir eq '' then return
         z=where((*(*state).nrep) gt 1,count)
         if count eq 0 then return

         if (*state).blink eq 0 then begin
            (*state).blink = 512
            (*state).blinkframe=0
            widget_control,(*state).blinkid,timer=float((*state).blink)/1000.0
         endif else if (*state).blink eq -1 then begin
            (*state).blink = 0
         endif else if (*state).blink eq -2 then begin
            ; single step to next frame
            newblinkframe=(*state).blinkframe+1
            if newblinkframe eq n_elements((*(*state).blinkidx)) then $
               newblinkframe=0
            (*state).blinkframe = newblinkframe
         endif else if (*state).blink gt 0 then begin
            widget_control,(*state).blinkid,timer=float((*state).blink)/1000.0
            newblinkframe=(*state).blinkframe+1
            if newblinkframe eq n_elements((*(*state).blinkidx)) then $
               newblinkframe=0
            (*state).blinkframe = newblinkframe
         endif else begin
            print,'LOOKER: Illegal blink value! ',(*state).blink
            (*state).blink = 0
         endelse
         if (*state).blink eq 0 then begin
            ; restore normal display
            dlooker_display,state,/noscore
         endif else begin
            ; show blinkframe
            idx=(*state).blinkframe
            visit=(*(*state).blinkidx)[idx]
            (*state).curvis=visit
            dlooker_display,state,/noscore
         endelse
      end

      'blink slower': begin
         if (*state).rdir eq '' then return
         z=where((*(*state).nrep) gt 1,count)
         if count eq 0 then return

         if (*state).blink eq 0 then begin
            return
         endif else if (*state).blink gt 0 then begin
            (*state).blink = (*state).blink*2
            if (*state).blink gt 8192 then (*state).blink=8192
         endif
      end

      'blink faster': begin
         if (*state).rdir eq '' then return
         z=where((*(*state).nrep) gt 1,count)
         if count eq 0 then return

         if (*state).blink eq 0 then begin
            return
         endif else if (*state).blink gt 0 then begin
            (*state).blink = (*state).blink/2
            if (*state).blink lt 128 then (*state).blink=128
         endif else if (*state).blink eq -2 then begin
            (*state).blink = 1024
            widget_control,(*state).blinkid,timer=float((*state).blink)/1000.0
         endif
         ; do nothing if blink -1 or < -2
      end

      'blink pause': begin
         if (*state).rdir eq '' then return
         z=where((*(*state).nrep) gt 1,count)
         if count eq 0 then return

         if (*state).blink eq 0 then $
            return $
         else $
            (*state).blink = -2
      end

      'blink stop': begin
         if (*state).rdir eq '' then return
         z=where((*(*state).nrep) gt 1,count)
         if count eq 0 then return

         if (*state).blink eq 0 then return
         (*state).blink = -1
      end

      'Change Basis': begin
         if event.str eq (*state).basis then return
         (*state).basis=event.str
         dlooker_loadset,state,(*state).set
         dlooker_display,state
      end

      'Change Type': begin
         if event.str eq (*state).type then return
         (*state).type=event.str
         dlooker_loadset,state,(*state).set
         dlooker_display,state
      end

      'Initials': begin
         dlooker_checkinitials,state
      end

      'Next': begin
         if (*state).rdir eq '' then return
         nparts=n_elements((*(*state).partlist))
         nchips=n_elements((*(*state).chiplist))
         nfields=n_elements((*(*state).fieldlist))

         z=trimrank(where((*state).part eq (*(*state).partlist),/null))+1
         if z eq nparts then begin
            z=0
            carry=1
         endif else carry=0
         newpart=(*(*state).partlist)[z]

         if carry then begin
            z=trimrank(where((*state).chip eq (*(*state).chiplist),/null))+1
            if z eq nchips then begin
               z=0
               carry=1
            endif else carry=0
            newchip=(*(*state).chiplist)[z]
         endif else newchip=(*state).chip

         if carry then begin
            z=trimrank(where((*state).field eq (*(*state).fieldlist),/null))+1
            if z eq nfields then begin
               z=0
               carry=1
            endif else carry=0
            newfield=(*(*state).fieldlist)[z]
         endif else newfield=(*state).field

         if carry eq 1 then begin
            print,'Already at last set, cannot move forward'
            return
         endif

         dlooker_loadset2,state,newfield,newchip,newpart
         dlooker_setzoom,state,/reset
         dlooker_display,state

      end

      'Next chip': begin
         if (*state).rdir eq '' then return
         z=where((*state).chip eq (*(*state).chiplist),/null)
         if z[0] eq !NULL then return
         z = trimrank(z)+1
         if z eq n_elements((*(*state).chiplist)) then begin
            print,'Already at the last chip, cannot go forward.'
            return
         endif
         dlooker_loadset2,state,(*state).field, $
                                (*(*state).chiplist)[z], $
                                (*(*state).partlist)[0]
         dlooker_setzoom,state,/reset
         dlooker_display,state
      end

      'Next field': begin
         if (*state).rdir eq '' then return
         z=where((*state).field eq (*(*state).fieldlist),/null)
         if z[0] eq !NULL then return
         z = trimrank(z)+1
         if z eq n_elements((*(*state).fieldlist)) then begin
            print,'Already at the last field, cannot go forward.'
            return
         endif
         dlooker_loadset2,state,(*(*state).fieldlist)[z], $
                                (*(*state).chiplist)[0], $
                                (*(*state).partlist)[0]
         dlooker_setzoom,state,/reset
         dlooker_display,state
      end

      'Next part': begin
         if (*state).rdir eq '' then return
         z=where((*state).part eq (*(*state).partlist),/null)
         if z[0] eq !NULL then return
         z = trimrank(z)+1
         if z eq n_elements((*(*state).partlist)) then begin
            print,'Already at the last part, cannot go forward.'
            return
         endif
         dlooker_loadset2,state,(*state).field, $
                                (*state).chip, $
                                (*(*state).partlist)[z]
         dlooker_setzoom,state,/reset
         dlooker_display,state
      end

      'Next visit': begin
         if (*state).rdir eq '' then return
         newvisit = (*state).curvis+1
         if newvisit eq (*state).nvisits then newvisit=0
         (*state).curvis=newvisit
         if (*state).curtrk gt 0 then begin
            tz=trimrank(where((*(*state).skid) eq (*state).curtrk))
            (*state).curtrn = (*(*state).snid)[newvisit,tz]
            if (*(*state).sx)[(*state).curvis,tz] gt 0 and $
               (*(*state).sy)[(*state).curvis,tz] gt 0 and $
               (*(*state).sx)[(*state).curvis,tz] lt (*state).nx and $
               (*(*state).sy)[(*state).curvis,tz] lt (*state).ny then begin
               x=(*(*state).sx)[newvisit,tz]
               y=(*(*state).sy)[newvisit,tz]
               dlooker_measure,state,(*state).curvis,x,y,1,error, $
                  xpos,ypos,maxsig,sky,skysig,/noupdate,/silent
               dlooker_setzoom,state,(*state).curvis,xpos,ypos,maxsig,sky,skysig
            endif else begin
               dlooker_setzoom,state,/reset
            endelse
         endif else begin
            dlooker_setzoom,state,/reset
         endelse
         dlooker_display,state,/noscore
      end

      'New Tracklet': begin
         if (*state).curtrk lt 0 then return
         (*state).curtrk = -1L
         (*state).curtrn = -1L
         dlooker_objinfo,state
         dlooker_setzoom,state,/reset
      end

      'Previous': begin
         if (*state).rdir eq '' then return
         nparts=n_elements((*(*state).partlist))
         nchips=n_elements((*(*state).chiplist))
         nfields=n_elements((*(*state).fieldlist))

         z=trimrank(where((*state).part eq (*(*state).partlist),/null))-1
         if z lt 0 then begin
            z=nparts-1
            carry=1
         endif else carry=0
         newpart=(*(*state).partlist)[z]

         if carry then begin
            z=trimrank(where((*state).chip eq (*(*state).chiplist),/null))-1
            if z lt 0 then begin
               z=nchips-1
               carry=1
            endif else carry=0
            newchip=(*(*state).chiplist)[z]
         endif else newchip=(*state).chip

         if carry then begin
            z=trimrank(where((*state).field eq (*(*state).fieldlist),/null))-1
            if z lt 0 then begin
               z=nfields-1
               carry=1
            endif else carry=0
            newfield=(*(*state).fieldlist)[z]
         endif else newfield=(*state).field

         if carry eq 1 then begin
            print,'Already at first set, cannot move backward'
            return
         endif

         dlooker_loadset2,state,newfield,newchip,newpart
         dlooker_setzoom,state,/reset
         dlooker_display,state

      end

      'Prev chip': begin
         if (*state).rdir eq '' then return
         z=where((*state).chip eq (*(*state).chiplist),/null)
         if z[0] eq !NULL then return
         z = trimrank(z)-1
         if z lt 0 then begin
            print,'Already at the first chip, cannot go back further.'
            return
         endif
         dlooker_loadset2,state,(*state).field, $
                                (*(*state).chiplist)[z], $
                                (*(*state).partlist)[0]
         dlooker_setzoom,state,/reset
         dlooker_display,state
      end

      'Prev field': begin
         if (*state).rdir eq '' then return
         z=where((*state).field eq (*(*state).fieldlist),/null)
         if z[0] eq !NULL then return
         z = trimrank(z)-1
         if z lt 0 then begin
            print,'Already at the first field, cannot go back further.'
            return
         endif
         dlooker_loadset2,state,(*(*state).fieldlist)[z], $
                                (*(*state).chiplist)[0], $
                                (*(*state).partlist)[0]
         dlooker_setzoom,state,/reset
         dlooker_display,state
      end

      'Prev part': begin
         if (*state).rdir eq '' then return
         z=where((*state).part eq (*(*state).partlist),/null)
         if z[0] eq !NULL then return
         z = trimrank(z)-1
         if z lt 0 then begin
            print,'Already at the first part, cannot go back further.'
            return
         endif
         dlooker_loadset2,state,(*state).field, $
                                (*state).chip, $
                                (*(*state).partlist)[z]
         dlooker_setzoom,state,/reset
         dlooker_display,state
      end

      'Prev visit': begin
         if (*state).rdir eq '' then return
         newvisit = (*state).curvis-1
         if newvisit lt 0 then newvisit=(*state).nvisits-1
         (*state).curvis=newvisit
         if (*state).curtrk gt 0 then begin
            tz=trimrank(where((*(*state).skid) eq (*state).curtrk))
            (*state).curtrn = (*(*state).snid)[newvisit,tz]
            if (*(*state).sx)[(*state).curvis,tz] gt 0 and $
               (*(*state).sy)[(*state).curvis,tz] gt 0 and $
               (*(*state).sx)[(*state).curvis,tz] lt (*state).nx and $
               (*(*state).sy)[(*state).curvis,tz] lt (*state).ny then begin
               x=(*(*state).sx)[newvisit,tz]
               y=(*(*state).sy)[newvisit,tz]
               dlooker_measure,state,(*state).curvis,x,y,1,error, $
                  xpos,ypos,maxsig,sky,skysig,/noupdate,/silent
               dlooker_setzoom,state,(*state).curvis,xpos,ypos,maxsig,sky,skysig
            endif else begin
               dlooker_setzoom,state,/reset
            endelse
         endif else begin
            dlooker_setzoom,state,/reset
         endelse
         dlooker_display,state,/noscore
      end

      'Score': BEGIN
         if (*state).rdir eq '' then return
         case event.press OF
            0: begin
            end

            ; Move to cursor location
            1: begin
               xm = fix(( event.x-(*state).szsx/2.0 ) * (*state).sf + (*state).nx/2.0)
               ym = fix(( event.y-(*state).szsy/2.0 ) * (*state).sf + (*state).ny/2.0)

               (*state).x0 = (xm - (*state).szx/2)>0
               (*state).y0 = (ym - (*state).szy/2)>0
               dlooker_display,state

            end

            ; Move half field either up/down or right/left depending on largest
            ;   distance between current point and cursor point.
            2: begin
               xm = fix(( event.x-(*state).szsx/2.0 ) * (*state).sf + (*state).nx/2.0)
               ym = fix(( event.y-(*state).szsy/2.0 ) * (*state).sf + (*state).ny/2.0)

               newx0 = xm - (*state).szx/2
               newy0 = ym - (*state).szy/2
               xdiff = (*state).x0 - newx0
               ydiff = (*state).y0 - newy0
               if abs(xdiff) gt abs(ydiff) then begin
                  newx0 = (*state).x0 + (xdiff gt 0.0 ? -1.0 : 1.0) * (*state).szx/2
                  newy0 = (*state).y0
               endif else begin
                  newx0 = (*state).x0
                  newy0 = (*state).y0 + (ydiff gt 0.0 ? -1.0 : 1.0) * (*state).szy/2
               endelse
               (*state).x0 = newx0
               (*state).y0 = newy0
               dlooker_display,state
            end

            ; Move full field either up/down or right/left depending on largest
            ;   distance between current point and cursor point.
            4: begin
               xm = fix(( event.x-(*state).szsx/2.0 ) * (*state).sf + (*state).nx/2.0)
               ym = fix(( event.y-(*state).szsy/2.0 ) * (*state).sf + (*state).ny/2.0)

               xm = xm / (*state).szx * (*state).szx
               ym = ym / (*state).szy * (*state).szy

               (*state).x0 = xm
               (*state).y0 = ym
               dlooker_display,state
            end
            else: begin
               MESSAGE, 'Unknown window event:', /INFO
               HELP, event, /STRUCTURE
            end

         endcase

      end

      'Set Max Sig': begin
         widget_control, event.id, GET_VALUE=hivalue
         hivalue = float(fix(hivalue[0]*10.0))/10.0
         widget_control, (*state).minsigid, GET_VALUE=lovalue
         lovalue=float(lovalue[0])
         if lovalue eq hivalue then hivalue=hivalue+0.1
         hivalue = strcompress(string(hivalue,format='(f10.1)'))
         widget_control, (*state).maxsigid, SET_VALUE=hivalue
         dlooker_stretch,state
         dlooker_display,state,/noscore
      end

      'Set Min Sig': begin
         widget_control, event.id, GET_VALUE=lovalue
         lovalue = float(fix(lovalue[0]*10.0))/10.0
         widget_control, (*state).maxsigid, GET_VALUE=hivalue
         hivalue=float(hivalue[0])
         if lovalue eq hivalue then lovalue=lovalue-0.1
         lovalue = strcompress(string(lovalue,format='(f10.1)'))
         widget_control, (*state).minsigid, SET_VALUE=lovalue
         dlooker_stretch,state
         dlooker_display,state,/noscore
      end

      'Window': BEGIN
         if (*state).rdir eq '' then return
         case event.press OF
            0: begin
            end
            1: begin
               if (*state).curtrk lt 0 then begin
               endif
               print,'Window, left button press'
               x=event.x+(*state).x0
               y=event.y+(*state).y0

               if x le 0 or y le 0 or $
                  x ge (*state).nx-1 or y ge (*state).ny-1 then return

               dlooker_measure,state,(*state).curvis,x,y,0,error, $
                  xpos,ypos,maxsig,sky,skysig
               dlooker_setzoom,state,(*state).curvis,xpos,ypos,maxsig,sky,skysig
               dlooker_display,state,/noscore

            end
            2: begin
               if (*state).curtrk lt 0 then return
               if (*state).colormode eq 0 then return
               print,'Window, middle button press'
               print,event.x+(*state).x0,event.y+(*state).y0
            end
            4: begin
               if (*state).ntrk gt 0 then begin
                  x=event.x+(*state).x0
                  y=event.y+(*state).y0
                  if x le 0 or y le 0 or $
                     x ge (*state).nx-1 or y ge (*state).ny-1 then return
                  sep=sqrt(((*(*state).sx)-x)^2+((*(*state).sy)-y)^2)
                  z=where(sep eq min(sep))
                  z=z[0]
                  if sep[z] lt (*state).maxdist then begin
                     tknum=z/(*state).nvisits
                     (*state).curtrk=(*(*state).skid)[tknum]
                     (*state).curtrn=(*(*state).snid)[(*state).curvis,tknum]
                     dlooker_objinfo,state
                     x=(*(*state).sx)[(*state).curvis,tknum]
                     y=(*(*state).sy)[(*state).curvis,tknum]
                     if x le 0 or y le 0 or $
                        x ge (*state).nx-1 or y ge (*state).ny-1 then begin
                        dlooker_setzoom,state,/reset
                     endif else begin
                        dlooker_measure,state,(*state).curvis,x,y,1,error, $
                           xpos,ypos,maxsig,sky,skysig,/noupdate,/silent
                        dlooker_setzoom,state,(*state).curvis,xpos,ypos, $
                           maxsig,sky,skysig
                     endelse
                  endif
               endif
            end
            else: begin
               MESSAGE, 'Unknown window event:', /INFO
               HELP, event, /STRUCTURE
            end
         endcase
      END

      'Zoom': BEGIN
         if (*state).rdir eq '' then return
         case event.press OF
            0: begin
            end
            1: begin
               ; position for measurement
               xc = (float(event.x)+0.5)/(*state).zf+(*state).zx0-0.5
               yc = (float(event.y)+0.5)/(*state).zf+(*state).zy0-0.5

               dlooker_measure,state,(*state).curvis,xc,yc,1,error
               if error then return

               dlooker_refreshzoom,state,xc,yc, $
                  (*(*state).objrad)[(*state).curvis]
               dlooker_display,state,/noscore
            end
            2: begin
               ; position for measurement
               xc = (float(event.x)+0.5)/(*state).zf+(*state).zx0-0.5
               yc = (float(event.y)+0.5)/(*state).zf+(*state).zy0-0.5

               dlooker_measure,state,(*state).curvis,xc,yc,1, $
                  error,xpos,ypos,maxsig,sky,skysig
               if error then return

               radius=(*(*state).objrad)[(*state).curvis]
               dlooker_setzoom,state,(*state).curvis,xpos,ypos, $
                  maxsig,sky,skysig
               dlooker_display,state,/noscore
            end
            4: begin
               ; position for measurement
               xc = (float(event.x)+0.5)/(*state).zf+(*state).zx0-0.5
               yc = (float(event.y)+0.5)/(*state).zf+(*state).zy0-0.5
               dlooker_measure,state,(*state).curvis,xc,yc,0,error, $
                  xpos,ypos,maxsig,sky,skysig
               if error then return
               dlooker_setzoom,state,(*state).curvis,xpos,ypos,maxsig,sky,skysig
               dlooker_display,state,/noscore
            end
            else: begin
               MESSAGE, 'Unknown window event:', /INFO
               HELP, event, /STRUCTURE
            end
         endcase
      END

      ELSE : BEGIN
         print,'EVENT NAME: ',event_name
         MESSAGE, 'Unknown event:', /INFO
         HELP, event, /STRUCTURE
      END

   ENDCASE

END ; end of event handler



;------------------------------------------------------------------------------

PRO dlooker,PATH=path,XSIZE=szx,YSIZE=szy,ZXSIZE=szsx,ZYSIZE=szsy

   if xregistered('dlooker') then return

   IF (!d.flags and 256) eq 0 THEN BEGIN
      print, 'Error. No windowing device. LOOKER cannot be started.'
      return
   ENDIF

   IF !d.n_colors ne 16777216 THEN BEGIN
      print,'Error.  24-bit display device is required.'
      return
   ENDIF

   IF badpar(path,[0,7],0,CALLER='LOOKER: (PATH) ', $
             DEFAULT='/net/wixer/raid/buie/') THEN RETURN
   if path ne '' then path=addslash(path)

   IF badpar(szx,[0,2,3],0, $
         caller='LOOKER: (XSIZE) ',default=1200) THEN RETURN
   IF badpar(szy,[0,2,3],0, $
         caller='LOOKER: (YSIZE) ',default=1100) THEN RETURN
   IF badpar(szsx,[0,2,3],0, $
         caller='LOOKER: (ZXSIZE) ',default=148) THEN RETURN
   IF badpar(szsy,[0,2,3],0, $
         caller='LOOKER: (ZYSIZE) ',default=296) THEN RETURN

   basislist= ['Delta','Gaussian']
   typelist=['pair','template','stack','shift']

   setusym,-1

   ;Define the main base.
   mainbase=widget_base(TITLE='DLOOKER: Visual Inspection and Measuring Tool', $
                           /COLUMN,UVALUE=0,MBAR=bar)

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Select run', $
                     '0\Select field',$
                     '0\Select chip',$
                     '0\GoTo transient', $
                     '0\GoTo tracklet', $
                     '0\Change secondary field', $
                     '0\Autoload Toggle', $
                     '0\Postscript Hardcopy', $
                     '2\Exit',$
                     '1\Edit', $
                     '0\Select Transient', $
                     '0\Delete Transient', $
                     '2\Set all ? to N', $
                     '1\Tools',$
                     '0\Refresh display', $
                     '0\Copy stretch', $
                     '0\Suppress overlay', $
                     '0\Toggle lines', $
                     '0\Toggle color mode', $
                     '2\Snap object'], UVALUE='THE_MENU', /MBAR)

   base = widget_base( mainbase, /ROW )

   win1 = widget_draw( base, XSIZE=szx, YSIZE=szy, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Window' )

   cbase = widget_base( base, /COLUMN )

   win2 = widget_draw( cbase, XSIZE=szsx, YSIZE=szsy, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Score' )

   win3 = widget_draw( cbase, XSIZE=szsx, YSIZE=szsx, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Zoom' )

   b1 = widget_base( cbase, /column, /frame)
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value='Tracklet ID:' )
   curtrk = widget_label( b2, value=' ', /align_left, /dynamic_resize )
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value=' V/Dir:' )
   rate = widget_label( b2, value=' ', /align_left, /dynamic_resize )
   nmeas = widget_label( b1, value=' ', /align_left, /dynamic_resize )
   scat = widget_label( b1, value=' ', /align_left, /dynamic_resize )

   b1 = widget_base( cbase, /column, /frame)
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value='Transient ID:' )
   curtrn=widget_label(b2,value=' ', /align_left, /dynamic_resize )
   b2 = widget_base( b1, /row )
   rawin =widget_label(b2,value=' ',/align_left,/dynamic_resize)
   decwin=widget_label(b2,value=' ',/align_left,/dynamic_resize)
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value='fwhm=' )
   fwhmid =widget_label(b2,value=' ',/align_left,/dynamic_resize)
   t1 = widget_label( b2, value='mag=' )
   magid =widget_label(b2,value=' ',/align_left,/dynamic_resize)
   

   newobj = widget_button( cbase, VALUE='New Tracklet', uvalue='New Tracklet' )

   cbase = widget_base( base, /COLUMN )

   b1 = widget_base( cbase, /row, /frame)
   t1 = widget_label( b1, value='Run:' )
   runid = widget_label( b1, value='',/dynamic_resize)

   b1 = widget_base( cbase, /row, /frame)
   previd = widget_button( b1, VALUE='Previous', uvalue='Previous' )
   nextid = widget_button( b1, VALUE='Next', uvalue='Next' )
   initialsid = widget_text(b1, VALUE='', /EDITABLE, XSIZE=4, uvalue='Initials' )
   b2 = widget_base(cbase,/column,/frame)
   b3 = widget_base(b2,/row)
   t1 = widget_button(b3,VALUE='Field -',uvalue='Prev field')
   t1 = widget_button(b3,VALUE='Field +',uvalue='Next field')
   b3 = widget_base(b2,/row)
   t1 = widget_button(b3,VALUE=' Chip -',uvalue='Prev chip')
   t1 = widget_button(b3,VALUE=' Chip +',uvalue='Next chip')
   b3 = widget_base(b2,/row)
   t1 = widget_button(b3,VALUE=' Part -',uvalue='Prev part')
   t1 = widget_button(b3,VALUE=' Part +',uvalue='Next part')
   b3 = widget_base( b2, /row )
   t1 = widget_label( b3, value=' Basis:' )
   basisid=widget_combobox(b3,value=basislist,sensitive=0, $
                           uvalue='Change Basis',/dynamic_resize )
   b3 = widget_base( b2, /row )
   t1 = widget_label( b3, value='  Type:' )
   typeid=widget_combobox(b3,value=typelist,sensitive=0, $
                           uvalue='Change Type',/dynamic_resize )

   b1 = widget_base( cbase, /column, /frame)
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value='   Set:' )
   setid = widget_label(b2, value='', /dynamic_resize )
   b2 = widget_base( b1, /row )
   t1 = widget_label( b2, value=' Visit:' )
   visitid = widget_label( b2, value='', /dynamic_resize )
   timeid = widget_label( b1, value='', /dynamic_resize )
   info1id = widget_label( b1, value='', /dynamic_resize )
   info2id = widget_label( b1, value='', /dynamic_resize )
   b2 = widget_base(b1,/row)
   t1 = widget_button(b2,VALUE='Visit -',uvalue='Prev visit')
   t1 = widget_button(b2,VALUE='Visit +',uvalue='Next visit')

   b2 = widget_base(cbase,/row,/frame)
   b3 = widget_base(b2,/column)
   b4 = widget_base( b3, /row )
   t1 = widget_label(b4,value='Blink Set:' )
   blinksetid=widget_combobox(b4,value=strn(0),sensitive=0, $
                           uvalue='Blink Set',/dynamic_resize )
   b4 = widget_base(b3,/row)
   t1 = widget_button(b4,VALUE='X',uvalue='blink stop')
   t1 = widget_button(b4,VALUE='-',uvalue='blink slower')
   t1 = widget_button(b4,VALUE='[ ]',uvalue='blink pause')
   t1 = widget_button(b4,VALUE='+',uvalue='blink faster')
   blinkid = widget_button(b4, VALUE='>',uvalue='blink')

   b2 = widget_base( cbase, /column, /frame)
   t1 = widget_label( b2, VALUE='Max Display (sigma)' )
   maxsig = widget_text( b2, VALUE='5', /EDITABLE, XSIZE=5, uvalue='Set Max Sig' )
   b2 = widget_base( cbase, /column, /frame)
   t1 = widget_label( b2, VALUE='Min Display (sigma)' )
   minsig = widget_text( b2, VALUE='-3', /EDITABLE, XSIZE=5, uvalue='Set Min Sig' )

;   objlist=['ANI015','ANI023','ANI024','ANI027', $
;            'ANI1000','ANI1001','ANI1002','ANI1003','ANI1004', $
;            'ANI1005','ANI1006']
;   objlist='A'+['VNH0001','VNH0002','VNH0003','VNH0004','VNH0005','VNH0006']
   watchfile='/net/frakir/raid/buie/astrometry/Working/watchlist.txt'
   if exists(watchfile) then begin
      readcol,watchfile,objlist,format='a',count=nobj
      objlist='A'+objlist
   endif else begin
      objlist=''
      nobj=0
   endelse

   state = ptr_new({ $

      ; Data and information in the widget
      astinfo: ptr_new(), $      ; Astrometry information for current set.
      basis: 'Delta', $          ; Current active diff basis set
      basislist: basislist, $    ; list of basis choices
      bcube: ptr_new(), $        ; pre-scaled images from all visits.
      blink: 0, $                ; Blink rate in milliseconds.  Disabled if 0
      blinkframe: 0B, $          ; Current blinking frame showing.
      blinkid: blinkid, $        ; widget id for timer events and starting.
      blinkidx: ptr_new(), $     ; index of visits to blink
      chip: 0, $                 ; current chip being displayed
      chiplist: ptr_new(), $     ; list of chips
      colormode: 0, $            ; 1=color composite, 0=b/w
      cube: ptr_new(), $         ; Difference images from all visits.
      curtrk: -1L, $             ; Current tracklet number being manipulated.
      curtrn: -1L, $             ; current transient number being manipulated.
      curvis: -1, $              ; current visit number
      db: '', $                  ; Support database for difference data
      decrange: dblarr(2), $     ; range of Dec for current set
      dimgfwhm: ptr_new(), $     ; FWHM of images in current set
      dreffwhm: ptr_new(), $     ; FWHM of images in current set
      dpath: '', $               ; full path
      etascat: ptr_new(), $      ; Scatter in tracklet motion
      exptime: ptr_new(), $      ; exposure time
      field: '', $               ; Name of current field.
      fieldlist: ptr_new(), $    ; List of available fields.
      fileroot: ptr_new(), $     ; List of file roots
      fwhm: ptr_new(), $         ; FWHM of images in current set
      gain: -1.0, $              ; Gain of CCD (e-/ADU).
      initials: '', $            ; String, initials of scanner.
      invert: ptr_new(), $       ; Flag array, true if image is to be flipped
      jdmid: ptr_new(), $        ; Jd of all exposures in visit
      lines:   1B, $             ; Flag, if true connecting lines are drawn.
      maglim: ptr_new(), $       ; FWHM of images in current set
      maxdist: 50, $             ; Maximum distance for click select
      night: ptr_new(), $        ; night number for each visit
      nrep: ptr_new(), $         ; number of visits on each night
      nvisits: 0, $              ; number of visits for the current set.
      nomarks: 0, $              ; Flag, if true suppress overplot in main win
      nx: 0L, $                  ; X size of image(s).
      ny: 0L, $                  ; Y size of image(s).
      objrad: ptr_new(), $
      part: '', $                ; current image part
      partlist: ptr_new(), $     ; List of image parts
      path: path, $              ; Location for raw image data.
      photzp: ptr_new(), $       ; photometric zero-point
      rarange: dblarr(2), $      ; range of RA for current set
      rdir: '', $                ; Run ID
      set: '', $                 ; name of current set
      setlist: ptr_new(), $      ; List of image sets
      sf: 1.0, $                 ; Scale factor between full image and score.
      srgb: bytarr(szsx,szsy,3), $ ; color cube for score
      szsx: szsx, $              ; X-width of score display in pixels.
      szsy: szsy, $              ; Y-width of score display in pixels.
      szx: szx, $                ; X-width of main display in pixels.
      szy: szy, $                ; Y-width of main display in pixels.

      ; complete set of positions for all tracklets for this set
      ; these are arrays
      ntrk: 0L, $                ; number of tracklets loaded
      skid: ptr_new(), $         ; tracklet ids on this set [ntrk]
      snid: ptr_new(), $         ; transient ids [nvisits,ntrk]
      sx: ptr_new(), $           ; x position on this set [nvisits,ntrk]
      sy: ptr_new(), $           ; x position on this set [nvisits,ntrk]
      nmeas: ptr_new(), $        ; Number of measurements for each tracklet
      rate: ptr_new(), $         ; Rate of motion ("/hr)
      dir: ptr_new(), $          ; Direction of motion (degrees)
      sfwhm: ptr_new(), $
      smag: ptr_new(), $

      ; complete set of support information for all objects that are being
      ;   tracked for overplotting
      objlist: ptr_new(objlist), $ ; list of objects to watch
      nobj: nobj, $              ; number of objects to watch
      xobj: ptr_new(), $         ; predicted object position [nvisits,nobj]
      yobj: ptr_new(), $         ; predicted object position [nvisits,nobj]
      x1sig: ptr_new(), $        ; 1-sigma ellipse contour [npts,nobj]
      y1sig: ptr_new(), $        ; 1-sigma ellipse contour [npts,nobj]
      x3sig: ptr_new(), $        ; 3-sigma ellipse contour [npts,nobj]
      y3sig: ptr_new(), $        ; 3-sigma ellipse contour [npts,nobj]

      type: 'template', $        ; style of difference to view
      typelist: typelist, $      ; list of types
      uniqueid: uniqueid(), $
      visitlist: ptr_new(), $    ; ordered list of files in the visit.
      x0: 0, $                   ; LLHC of display window from full image.
      y0: 0, $                   ; LLHC of display window from full image.
      xiscat: ptr_new(), $       ; Scatter in tracklet motion
      zim: bytarr(szsx/4,szsx/4), $ ; Zoom image for display.
      zf: 4, $                   ; Zoom factor for zoom window display.
      zx0: 0, $                  ; LLHC of zoom display window from full image.
      zy0: 0, $                  ; LLHC of zoom display window from full image.

      ; Widget ids
      blinksetid: blinksetid, $  ; ID showing current blink set
      curtrkid: curtrk, $        ; ID of label that shows current tracklet id.
      curtrnid: curtrn, $        ; ID of label that shows current tracklet id.
      drawwin:  win1, $          ; ID of main draw window
      fwhmid:   fwhmid, $
      setid:    setid, $         ; ID of label that shows current set name.
      visitid:  visitid, $       ; ID of label that shows current image #1
      basisid:  basisid, $       ; ID of label that shows current image #2
      decwinid: decwin, $        ; ID showing Dec of transient
      initialsid: initialsid, $  ; ID of label that shows initials of scanner
      info1id:  info1id, $       ; Image info line 1
      info2id:  info2id, $       ; Image info line 1
      mainbase: mainbase, $      ; ID of top level base.
      magid:    magid, $
      maxsigid: maxsig, $        ; ID of text widget with display max
      minsigid: minsig, $        ; ID of text widget with display max
      nextid:   nextid, $        ; ID of Next button
      nmeasid:  nmeas, $         ; ID for number of measurement info
      previd:   previd, $        ; ID of Previous button
      rateid:   rate, $          ; ID of label that shows rate/dir of current
      rawinid:  rawin, $         ; ID showing RA of transient
      runid:    runid, $         ; ID of label that shows current run
      scatid:   scat, $          ;
      scorewin: win2, $          ; ID of score and pan control window
      timeid:   timeid, $        ; Where to time of exposure for display
      typeid:   typeid, $        ; ID of difference type
      zoomwin:  win3 $           ; ID of zoom window

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   XMANAGER, 'dlooker', mainbase, $
             EVENT_HANDLER='dlooker_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='dlooker_cleanup'

end
