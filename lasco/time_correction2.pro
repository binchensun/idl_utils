; $Id: time_correction2.pro,v 1.5 2012/12/12 22:03:48 nathan Exp $
;
; TIME_CORRECTION2
;+
; Name: TIME_CORRECTION2
;
; Purpose:
;    To return the LASCO OBE - SOHO LOBT time difference that is equal to or right before the
;    the input obe-time from the values in TIME_DIFFERENCE_DB. 
;    This pro was renamed time_correction2.pro for the 2012/11/28 modification, which changed the output.
;
; Input Parameters:
;    OBE_TIME           -       An input LASCO obe_time for which the time offset is to be obtained.
;
; Output:
;    DELTA_ERROR        -       FLOAT 	Stdev of 3 closest time deltas
;
; RETURN VALUE:
;    DT                 -       FLOAT	OBE - LOBT time offset as seconds.
;
; Keywords:
;    CORRECTION_STRING	-	Used to return an ASCII string with the
;				time difference.
;    VERBOSE            -       If set, print out time selection info.
;   DATFILE=	Set equal to file to use for TIME_DIFF.DAT, if not default.
;
; Calling Sequence:
;    dt = TIME_CORRECTION2(obe_time, delta_error, CORRECTION_STRING = CORRECTION_STRING, /VERBOSE)
; 
; Restrictions:
;    If large jumps in the difference occur between realtime contacts, this
;	routine could return inaccurate values.
;
; Calls: swap_endian.pro, 
;
; History:
;    1997 April 17  - D.M. fecit.
;    1997 August 27 - Added CORRECTION_STRING keyword		D.M. fecit.
;    1999 Feb 7     - Added binary search and indexing to speed finding
;                     the right record, created unix version for Solaris -  DW
;    2002 Jul 10    - Added DELTA_ERROR parameter                        -  Ed Esfandiari
;    2002 Jul 10    - Also added a true binary search                    -  Ed Esfandiari
;
; Jul 10, 2002 - AEE
; Note: according th Jeff Newmark, data in TIME_DIFFERENCE_DB before Sep 1, 1997 are invalid
;       and should NOT be used (starts from 1997-04-11). This new routine (unlike the older
;       eit_time_correction modified by DW, returns time-offsets prior to Sep 1997 which 
;       should not be used; But, once C2 data from Jan 1996 to Sep 3, 1997 is added to the
;       TIME_DIFFERENCE_DB, then everything would be fine. 
;
;    2003 Mar 11    - Add REDUCE_HISTORY common block; 
;			change datafile calls; delta_error=N/A for c2_offsets - NRich
;    2004 Sep 15 - Change OS_version check to 'endian-ness' check, since
;			not all unix platforns are necessarily big-endian - GR Lawrence
;    2007 Sep 26 =  Modified to use vms time_diff.dat file, TIME_DIFF.DAT_vms, through
;                   2007-02-08T14:52:52.944 and unix time_diff.dat file, TIME_DIFF.DAT,
;                   after that. The unix version has newer offsets appended to vms records
;                   with no byte-swap.
;   2010.09.16, nbr - renamed c2*.sav files
;   2010.11.04, nbr - Added common las_time_correction (applies only if DATE-OBS before 1997/12/31)
;   	    	    Added file_date_mod for TIME_DIFF.DAT in version=cmnver
;   2010.11.16, nbr - Bug fix
;   2012.03.05, nbr - In late 2011 the endianness of computer where TIME_DIFF.DAT is stored changed, 
;   	    	    thus changing the file. Also discontinued use of TIME_DIFF.DAT_vms for earlier dates.
;
; @(#)time_correction.pro	1.11 03/05/12  NRL IDL LIBRARY
;  
; $Log: time_correction2.pro,v $
; Revision 1.5  2012/12/12 22:03:48  nathan
; Add DATFILE= input; fix C2_FILES query; always compute newer in get_delta_time
; for byteswap flag
;
; Revision 1.4  2012/12/10 21:02:14  nathan
; auto-correct endian problems
;
; Revision 1.3  2012/11/30 21:01:20  nathan
; extend new output to c2_offset case; accomodate array input
;
; Revision 1.2  2012/11/28 21:04:58  nathan
; Major revision of how this works. Found that time_correction.pro was giving incorrect
; results for times between first time of the day and last time of previous day. Took
; opportunity to return more meaningful error values and better estimate of delta_time if bad
; value was found in TIME_DIFF.DAT. Also return result as scalar seconds instead of
; [days,seconds], and rename procedure.
;
;
;-
; swap words and bytes 
; VMS data base -> Sun Solaris
; 12345678  -> 7856 3412
function word_swap,long_int
b1 = long_int and 'FF000000'XL
b1 = b1 / '1000000'XL
b1 = b1 and 'FF'XL
b2 = long_int and '00FF0000'XL
b2 = b2 / '100'XL
b3 = long_int and '0000FF00'XL
b3 = b3 * '100'XL
b4 = long_int and '000000FF'XL
b4 = b4 * '1000000'XL
;print,b1,b2,b3,b4,format='(4z10)'
new = b1+b2+b3+b4

return,new
end


function get_delta_time,time_unit,rec_num,newer, BASE_TIME=base_time, VERBOSE=verbose

obe_time ='1998/6/1 10:58:10'
image_time = anytim2utc(obe_time) 
;help,time_unit,rec_num,new_w

;print,'rec_num= ',rec_num

;;  Mods GRL 14/09/2004 to check 'endian-ness' rather than OS family to see
;;  whether word_swap should be run. Solves specific issue with x86/Linux
;;  platforms, which are little-endian - as is VMS, hence use original code)

;endi=''
;j=1 & byteorder, j,/swap_if_big_endian				;; GRL 14/09/2004
;if j EQ 1 THEN endi = 'little' else endi = 'big'			;; GRL 14/09/2004
;
;help,endi
;
;endi = 'little'  ; AEE 06/11/07
;
;help,endi

wl = assoc(time_unit, lonarr(4))
wl_0 = wl(0)

;help,wl,wl_0
;stop

;; if (!VERSION.OS_FAMILY eq 'unix') then rec = word_swap(wl_0(0)) $
;;  else rec = word_swap(wl_0(0))
;if (endi EQ 'big') then rec = word_swap(wl_0(0))  else rec =wl_0(0)	;; GRL 14/09/2004

;rec = swap_endian(wl_0(0),/SWAP_IF_BIG_ENDIAN)

;IF (unix_file) THEN BEGIN 
;  ; unix time_diff.dat file is big_endian. Swap if running on a little_endian machine
;  rec = swap_endian(wl_0(0),/SWAP_IF_LITTLE_ENDIAN)
;ENDIF ELSE BEGIN ; vms time_diff.dat file
;  ; vms time_diff.dat file is litte-endian. Swap if running on a big_endian machine
;  rec = swap_endian(wl_0(0),/SWAP_IF_BIG_ENDIAN)
;ENDELSE

;First part of unix time_diff.dat file is the same as vms time_diff.dat file
;and are both vms generated little_endians. Only later parts of unix time_diff
;file is unix generated and is big_endian. So, to check for record of file to
;get total # or records, must only swap the bytes if running on a unix (big_endian) machine:

rec = swap_endian(wl_0(0),/SWAP_IF_BIG_ENDIAN)

;print,'Number of Records',rec,' Finding Rec ',rec_num

this_time = [image_time, image_time]
w = assoc(time_unit, this_time)

if(rec_num lt 0) then rec_num = rec

IF keyword_set(VERBOSE) THEN help,rec_num
first=1

if(rec_num le rec) then begin
     new_w0 = w(rec_num)
     ;if(rec_num ne rec) then begin

    ; handle vax/unix boundry:
    ; don't use few records at start of new unix format with dates before last vax record
    ; (313887) so binary search does not get confused:

    ;if (rec_num gt 313887L and rec_num lt 313893L) then begin
    ;  print,'Changed ren_num= ', rec_num, 'to 313894'
    ;  rec_num= 313894L
    ;endif


    ; if(rec_num le 313887L) then begin
    ;   ; new time_diff.dat file seems to have wrong byte-order (up to rec 313887)
    ;   ; and requires a byte-swap:
    ;   print,' *** byte swaped ***' 
    ;   w_rec = swap_endian(w_rec,/SWAP_IF_BIG_ENDIAN)
    ;   new_w = swap_endian(new_w,/SWAP_IF_BIG_ENDIAN)
    ; end

    ; In late 2011, soc started storing data on new computer. This reversed the endianness of the
    ; time_diff.dat file.
    
    IF keyword_set(VERBOSE) THEN print,'new_w=',new_w0

    newer= (rec_num GT 313887)	; = 2005-12-23T16:53:10.541

    IF (newer) THEN BEGIN 
       new_w = swap_endian(new_w0,/SWAP_IF_BIG_ENDIAN)
    ENDIF ELSE BEGIN 
       new_w = swap_endian(new_w0,/SWAP_IF_LITTLE_ENDIAN)
    ENDELSE

    ;help,/st,w_rec,new_w
    IF keyword_set(VERBOSE) THEN print,'new_w=',new_w
    ;stop
    ;
    ; Check result and try again if invalid
    ;
    IF (new_w[0].mjd gt 60676 or new_w[0].mjd LT 49718) THEN BEGIN
    	IF ~keyword_set(VERBOSE) THEN print,'new_w=',new_w
	IF ~(newer) THEN BEGIN 
	   new_w = swap_endian(new_w0,/SWAP_IF_BIG_ENDIAN)
	ENDIF ELSE BEGIN 
	   new_w = swap_endian(new_w0,/SWAP_IF_LITTLE_ENDIAN)
	ENDELSE
    	print,'Trying different byteswap: '
	print,'new_w=',new_w
	stop
    ENDIF

    ;if (endi EQ 'big') then begin					;; GRL 14/09/2004
    ;;; if (!VERSION.OS_FAMILY eq 'unix') then begin
    ; new_w(0).mjd = word_swap(w_rec(0).mjd)  
    ; new_w(0).time = word_swap(w_rec(0).time)  
    ; new_w(1).mjd = word_swap(w_rec(1).mjd)  
    ; new_w(1).time = word_swap(w_rec(1).time)  
    ;;print,new_w(0).mjd,new_w(0).time
    ;;print,new_w(1).mjd,new_w(1).time
    ;endif else begin
    ; new_w(0).mjd = w_rec(0).mjd
    ; new_w(0).time = w_rec(0).time  
    ; new_w(1).mjd = w_rec(1).mjd  
    ; new_w(1).time = w_rec(1).time  
    ;endelse
endif ELSE BEGIN
    message,'WARNING: '+trim(rec_num)+ ' out of range of TIME_DIFF.DAT.',/info
    print,'Returning zero; no time correction.'
    return,0
ENDELSE

; handle vax/unix boundry:
;if (last_rec(0).mjd gt image_time.mjd) then begin
;  convert_time,time_unit,313887L,last_rec, unix_file
;  rec_ind= 313893L
;endif

base_time=new_w[1]

delta_time = utc2tai(new_w[0])-utc2tai(base_time)

return,delta_time

end



FUNCTION MODIFIED_BINARY_SEARCH,img_time,time_unit,unix_file,n, before=before


  v = img_time.mjd*8.64d4 + 1.e-3*img_time.time

  b=1 ; since index 0, w(0), is the first line containg the record numbers n.
  if unix_file then begin
    ;b= 353615
    b= 320000  ; start index in unix file search
  endif
  t= n
 
  s_ind= -1 
  found= 0

  WHILE ((t GE b) AND (NOT found)) DO BEGIN 
    m= (t+b)/2
    diff=get_delta_time (time_unit,m,base_time=mrec0, unix_file )
    ;print,mrec0,m
    mrec= mrec0.mjd*8.64d4 + 1.e-3*mrec0.time
    IF (mrec EQ v) THEN BEGIN 
      found= -1
      s_ind= m
    ENDIF ELSE BEGIN 
      IF (v GT mrec) THEN $ 
        b= m + 1             $
      ELSE                   $ 
        t= m - 1
    ENDELSE 
  ENDWHILE 


;  print,'t,b,m= ',t,b,m
;  print,'bin_ind= ',s_ind

  IF (KEYWORD_SET(before) AND (NOT found)) THEN s_ind= t

;  print,'returned_ind= ',s_ind
;stop

  RETURN,s_ind

END


FUNCTION TIME_CORRECTION2, obe_time_in, delta_error_out, correction_string = correction_string,verbose=verbose, $
    	DATFILE=datfile
;
COMMON reduce_history, version, prev_a, prev_hdr, zblocks0
common las_time_correction, c2_offsets, c2_utc_dates, c2_dates, c2datafile
; NOTE: this common block only applies where date-obs before 1997/12/30
 
version = strarr(2)
version[0] = '$Id: time_correction2.pro,v 1.5 2012/12/12 22:03:48 nathan Exp $' ;  NRL IDL LIBRARY

nin=n_elements(obe_time_in)
delta_error_out=fltarr(nin)
correction_string=strarr(nin)
delta_time_out=fltarr(nin)
firsttime=1

FOR i=0,nin-1 DO BEGIN
; ################################################################################
image_time = anytim2utc(obe_time_in[i]) 

; July 22, 02: ;AEE
; Jeff Newmark has not added C2 data through '1997/09/03 07:28:00.000' (previous c2 image that
; was processed by star program is for '1997/08/29 18:27:29.462') to the EIT TIME_DIFF.DAT
; so, I will read them from C2, instead:

obe_ecs= anytim2utc(obe_time_in[i],/ecs)
IF (KEYWORD_SET(verbose)) THEN print,'Input obe time = ',obe_ecs 

IF datatype(c2datafile) EQ 'UND' THEN c2datafile=''

IF (obe_ecs LT '1997/09/03 07:28:00.000' OR                                          $
    obe_ecs GE '1997/10/19 18:28:29.682' AND obe_ecs LE '1997/10/27 22:41:37.680' OR $
    obe_ecs GE '1997/12/06 00:27.37.672' AND obe_ecs LE '1997/12/30 20:23:22.848'    ) THEN BEGIN
;
; Use C2 offsets
;
; ################################################################################
;
 IF (obe_ecs LT '1997/09/03 07:28:00.000') THEN $
   datafile='c2_012496t1544_082997t1827_timeoffsets.sav'
 IF (obe_ecs GE '1997/10/19 18:28:29.682' AND obe_ecs LE '1997/10/27 22:41:37.680') THEN $
   datafile='c2_101997t1828_102797t2241_timeoffsets.sav'
 IF (obe_ecs GE '1997/12/06 00:27.37.672' AND obe_ecs LE '1997/12/30 20:23:22.848') THEN $
   datafile='c2_120697t0027_123097t2023_timeoffsets.sav'

    IF c2datafile NE datafile THEN BEGIN

	c2datafile=datafile   
	restore,filepath(datafile,ROOT_DIR=getenv('NRL_LIB'),SUBDIR=['idl','data','calib'])

	;save files contain C2_OFFSETS, C2_UTC_DATES, C2_DATES ( str in /ecs format)

	; extend  last value:

	nelem= n_elements(C2_UTC_DATES)

	lutc= tai2utc(utc2tai(c2_utc_dates(nelem-1))+60.0) ; ; add 60 sec to the last time.
	ldate=anytim2utc(lutc,/ecs)

	C2_DATES=[C2_DATES,ldate]
	C2_OFFSETS=[C2_OFFSETS,C2_OFFSETS[nelem-1]]
	C2_UTC_DATES=[C2_UTC_DATES,lutc]
	;C2_UTC_DATES(nelem-1).mjd= lutc.mjd
	;C2_UTC_DATES(nelem-1).time= lutc.time 
	
    ENDIF
    nelem= n_elements(C2_UTC_DATES)
  ; Return offset for the time less than or equal to image_time and also a stdev in seconds
  ; to match the stuff for the times ge '1997/09/03 07:28:00.000'.  

    list= where(c2_dates le obe_ecs, cnt)
    delta_error_out[i]= -1
if (cnt lt 1) then begin
    print,''
    print,'Warning - image_time is outside the range of the C2 time_file.'
    print,'          image_time = '+anytim2utc(image_time,/ecs)
    print,'          time_file range= '+c2_dates(0)+' - '+c2_dates(n_elements(c2_dates)-2)
    print,'          Returning 0 offset.'
    print,''
    correction_string[i] = 'image_time is outside the range of the time_file.'
    delta_time_out[i]=0.
endif ELSE BEGIN
 
  cnt = cnt-1 
  delta_time= c2_offsets(cnt)
  cnt1=cnt+1
  cnt2=cnt-1
  if (cnt1 ge nelem-2) then cnt1= cnt-2
  if (cnt2 LT 1) THEN cnt2=cnt+2
  delta_error_out[i]=stdev([c2_offsets[cnt2],c2_offsets[cnt],c2_offsets[cnt1]])
  
  ;delta_time_2= (c2_offsets(cnt1) - delta_time) > (-15)
  ;delta_error = string([-15,delta_time_2 > 15]) ; AEE added; NBR removed
  
  
  delta_time_m = fix(delta_time/60) & delta_time_s = delta_time mod 60
  delta_time_m = strtrim(delta_time_m, 2)
  delta_time_s = string(delta_time_s, format = '$(f7.3)')
  delta_time_s = strtrim(delta_time_s, 2)
  if delta_time gt 15 then begin
     correction_string_i = 'OBE is running ahead of LOBT by ' + delta_time_m + $
        ' m ' + delta_time_s + ' s.'
  endif else begin
     correction_string_i = 'OBE is within 15 s of LOBT.'
  end
 
  if(keyword_set(verbose)) then begin
    print,''
    print,'Using C2 offsets from C2 file (not TIME_DIFF.DAT):'
    print,'num_records= ',n_elements(C2_DATES)
    print,'record_ind= ',cnt
    print,'image_time= ',anytim2utc(obe_time,/ecs)
    print,'prev_time & offset= ',c2_dates(cnt), c2_offsets(cnt)
    print,'next_time & offset= ',c2_dates(cnt1), c2_offsets(cnt1)
    print,correction_string_i
;help,cnt,cnt1
;stop
  end
  
  version[0] = version[0]+': using median-filtered star values from'
  version[1] = datafile
  
  ;RETURN,[0L,long(c2_offsets(cnt)*1000)]
  delta_time_out[i]=delta_time
  correction_string[i]=correction_string_i
ENDELSE

ENDIF ELSE BEGIN; EIT gaps, using star median values
;
; ################################################################################
;

;
;print,image_time.mjd,image_time.time
;
current_record = image_time.mjd*8.64d4 + 1.e-3*image_time.time
;print, '%TIME_DIFFERENCE-D-CURRENT, current_record = ' + $
;   strtrim(current_record, 2)
;
; The TIME_DIFF.DAT is provided by EIT and is a VMS data file
; 1st record contains number of records
; succeeding records are data records with 2 time records of 2 long ints each
; in CDS time format (mjd and time in millisecs)
;
;path = getenv('NRL_LIB')+'/lasco/data/calib/'
;datafile='TIME_DIFF.DAT'
;datafile='TIME_DIFF.DAT_unix'
;datafile='TIME_DIFF.DAT'

;print,'obe_ecs= '+obe_ecs

;IF (obe_ecs LE '2007/02/08 14:52:52.944') THEN BEGIN ; last record in vms file

IF obe_ecs LT '2005/12/23 16:53:11' THEN BEGIN
;IF obe_ecs LT '2005/05/30 04:43:26.900' THEN BEGIN
  unix_file= 0 
  ;datafile='TIME_DIFF.DAT_vms' 
ENDIF ELSE BEGIN
  unix_file= 1 
  ;datafile='TIME_DIFF.DAT_unix'
  ;datafile='TIME_DIFF.DAT'
ENDELSE

IF (firsttime) THEN BEGIN

    datafile='TIME_DIFF.DAT'
    IF keyword_set(DATFILE) THEN rdfile=datfile ELSE $
    rdfile = FILEPATH(datafile,ROOT_DIR=getenv('NRL_LIB'),SUBDIR=['idl','data','calib'])
    ;rdfile = '~/idl/sandbox/lasco/time_diff.dat.120102'

    if(keyword_set(verbose)) then message,'Using '+rdfile,/info

    version[1] = datafile + ' '+utc2str(tai2utc(file_date_mod(rdfile)),/date)

    openr, time_unit, rdfile, /get_lun 
    ;
    num_recs= -1  ; to read first line of file containing number of records to follow
                      ; and also return the last record in last_rec and number of records
                      ; in num_recs 
    deltalast=get_delta_time(time_unit,num_recs,1,verbose=verbose, BASE_TIME=obtend)
    firsttime=0
ENDIF
rec_ind= MODIFIED_BINARY_SEARCH(image_time,time_unit,unix_file,num_recs,/before)

;help,image_time
;if (utc2str(image_time) gt '2005-12-23T16:53:10.000' and
;    utc2str(image_time) lt '2005-12-27T20:58:14.000') then begin
;  rec_ind= 313887L 
;endif else begin
;  rec_ind= MODIFIED_BINARY_SEARCH(image_time,time_unit,/before)
;endelse

if (rec_ind lt 1 or rec_ind GE num_recs) then begin
  deltafirst=get_delta_time(time_unit,1, 0,verbose=verbose, base_time=obtsta)
  print,''
  print,'Warning - image_time is outside the range of the TIME_DIFF.DAT.'
  print,'          image_time = '+utc2str(image_time)
  print,'          time_file range= '+utc2str(obtsta)+' - '+utc2str(obtend)
  print,'          Returning 0 offset.'
  print,''
  correction_string[i] = 'image_time is outside the range of the time_file.'
  delta_error_out[i]= -1
  wait,2
  ;free_lun, time_unit
  delta_time_out[i]=0.
endif ELSE BEGIN


IF keyword_set(VERBOSE) THEN message,'convert_time rec_ind-1:',/info
delta_time_0 = get_delta_time(time_unit,rec_ind-1,unix_file,BASE_TIME=obt0, verbose=verbose)
IF keyword_set(VERBOSE) THEN message,'convert_time rec_ind:',/info
delta_time= get_delta_time(time_unit,rec_ind,unix_file,BASE_TIME=obt1, verbose=verbose)
IF keyword_set(VERBOSE) THEN message,'convert_time rec_ind+1:',/info
delta_time_2 = get_delta_time(time_unit,rec_ind+1,unix_file,BASE_TIME=obt2, verbose=verbose)
IF keyword_set(VERBOSE) THEN help,delta_time_0,delta_time,delta_time_2
;
; If the offset is Gt 0 or LT -500, then there is a problem; use local median 
;
alldelt=[delta_time_0,delta_time,delta_time_2]
IF delta_time GT 500 or delta_time LT -15 THEN BEGIN
    sall=sort(abs(alldelt))
    IF keyword_set(VERBOSE) THEN message,'Error detected in delta_time; using median value.',/info
    delta_time=median(alldelt)
    derr=abs(alldelt[1])-abs(alldelt[0])
ENDIF ELSE $
    derr=stdev(alldelt)
    
;error = [-15,max(delta_time_2,15)]
delta_error_out[i] = derr 

if(keyword_set(verbose)) then begin
  print,'num_records= ',num_recs
  print,'record_ind= ',rec_ind
  print,'image_time= ',utc2str(image_time)
;help,image_time,last_rec(0),delta_time
;stop
  print,'prev_time & offset = ',utc2str(obt1), delta_time
  print,'next_time & offset = ',utc2str(obt2), delta_time_2
  help,derr
;help,rec_ind
;stop
endif

;
delta_time_m = fix(delta_time/60) 
delta_time_s = delta_time mod 60
delta_time_m = strtrim(delta_time_m, 2)
delta_time_s = string(delta_time_s, format = '$(f7.3)')
delta_time_s = strtrim(delta_time_s, 2)
;
; So far, all deltas have been positive, so treat larger negative deltas
; as bogus data.
;
;if (delta_time lt (-15) then begin
;if (delta_time lt (-15) or delta_time gt 500) then begin ; AEE added
;   delta_time = 0.0
;   delta_mjd = 0l & delta_ms = 0.0
;end
;
; if delta_time lt (-15) then begin
;    correction_string = 'OBE is running behind LOBT by ' + delta_time_m + $
;       ' m ' + delta_time_s + ' s.'
; endif else
if delta_time gt 15 then begin
   correction_string[i] = 'OBE is running ahead of LOBT by ' + delta_time_m + $
      ' m ' + delta_time_s + ' s.'
endif else begin
   correction_string[i] = 'OBE is within 15 s of LOBT.'
end

;
; print, anytim2utc(last_rec(0), /ecs), '	', anytim2utc(last_rec(1), /ecs)
;
delta_time_out[i]=delta_time

ENDELSE
ENDELSE
;##########################################################################
ENDFOR

IF datatype(time_unit) NE 'UND' THEN  free_lun, time_unit


return, delta_time_out

end

