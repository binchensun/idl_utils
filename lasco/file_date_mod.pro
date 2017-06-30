FUNCTION FILE_DATE_MOD, File,DATE_ONLY=date_only, UTC=utc, SIZE=fsize
;+
; $Id: file_date_mod.pro,v 1.1 2008/04/17 20:44:50 nathan Exp $
; NAME:
;	FILE_DATE_MOD
;
; PURPOSE:
;	This function returns the modification date of a file as (local) TAI
;
; CATEGORY:
;	LASCO UTILITY
;
; CALLING SEQUENCE:
;
;	Result = FILE_DATE_MOD(File)
;
; INPUTS:
;	File:	The name of the file to be processed
;
; KEYWORD PARAMETERS:
;	DATE_ONLY	If set, return date only as a string
; 	UTC		If set, returns GMT/UTC time (Not implemented for Windows)
;   	SIZE=var    	Will return file size from ls -l
;
; OUTPUTS:
;	This function returns the date the file was modified as TAI
;
; PROCEDURE:
;	The procedure spawns a command to the OS to get the modification date
;
; EXAMPLE:
;	date = FILE_DATE_MOD(sample_file.dat,/date_only)
;		returns the date as a string such as 1996/10/15
;
; MODIFICATION HISTORY:
; $Log: file_date_mod.pro,v $
; Revision 1.1  2008/04/17 20:44:50  nathan
; moved from lasco/idl/util
;
; 	10 Feb 1999 - Nathan Rich, NRL
;			fixed year rollover problem  e.g Dec 21 was producing a
; 			file mod of Dec 21, 1999 since the ls command produced
;			just Dec 21 and not Dec 21 1998
;
; 			From the ls man page
; 			If  the  time  of last modification is greater than six
; 			months ago, it is shown in the format `month date year'
; 			for the POSIXlocale.  When the LC_TIME locale category
; 			is not set to the POSIX locale, a different format of
; 			the time field may  be  used.  Files modified within
; 			six months show 'month date time'.
;
; 			Still not sure if this is 6 months to the day or six
; 			months by months.  We will try six months by months - DW
;
;  	7 Oct 1999 - Fixed error in determining and using mon_index - NBR
;    	Jan 2000 - Correct definition of mm and dd; fix year determination (again) - NBR
; 	14 Aug 2000, NBR - Added /SH to spawn call
;  	2 Apr 2001, NBR - Return date-only in ECS format
; 	30 Nov 2001, NBR - Add mods for Windows compatibility for GSV
; 	21 Jun 2002, NBR - Use /bin/ls instead of /usr/ucb/ls and change method of parsing string
;  	9 Jun 2005, NBR - Add UTC option
;	5 Oct 2006,	F. Auchère - replace close, file1 by free_lun, file1 (frees the allocated unit)
;   7 Oct 2006, F. Auchere - more robust way to extract date from spawn output under windows
;
;	04/09/07 @(#)file_date_mod.pro	1.12 LASCO IDL LIBRARY
;-
    months=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

if (!version.os eq 'windows') or (!version.os eq 'Win32') then begin
    SPAWN, 'dir /4 '+file+' > dir.tmp', /hide
    pos1 = strpos(file, '/', /reverse_search)
    pos2 = strpos(file, '\', /reverse_search)
    pos = 1 + max([pos1, pos2])
    name = strmid(file, pos)
    repeat begin
      openr, file1, 'dir.tmp', /get_lun
      str1=''
      repeat begin
        readf, file1, str1
        pos = strpos(str1, name)
      endrep until (pos ne -1) or eof(file1)
      free_lun, file1
    endrep until pos ne -1
    SPAWN, 'del dir.tmp', /hide
    str2=strmid(str1, strpos(str1, '/')-2,17)
    yy=strmid(str2,6,4)
    mm=strmid(str2,3,2)
    dd=strmid(str2,0,2)
    tt=strmid(str2,12,5)+':00'
    str=yy+'-'+mm+'-'+dd+' '+tt
 endif ELSE BEGIN
   IF keyword_set(UTC) THEN BEGIN
	tzorig=getenv('TZ')
	setenv,'TZ=GMT'
   	SPAWN, '/bin/ls -lad '+file, result,/SH
	setenv,'TZ='+tzorig
   ENDIF ELSE $
   SPAWN, '/bin/ls -lad '+file, result,/SH

   result=result(0)
   IF (result(0) EQ '') THEN BEGIN
      PRINT, '%%FILE_DATE_MOD: Error file not accessible: ', file
      RETURN, -1
   ENDIF
   sep=STR_SEP(result,' ')
   whernotsp=where(sep)
   result1=sep[whernotsp]
   fsize=long(result1[4])
   mm = result1[5]
   dd = result1[6]
   yyyyort = result1[7]

   pos = STRPOS(yyyyort, ':')
   IF (pos(0) LT 0) THEN BEGIN	;** yyyyort is year
     	yyyy=yyyyort
	t = '12:00'
	str = yyyy+'-'+mm+'-'+dd+' '+t
   ENDIF ELSE BEGIN			;** yyyyort is time
	t = yyyyort
    	GET_UTC, utc
    	yyyy = STRMID(UTC2STR(utc), 0, 4)
	str = yyyy+'-'+mm+'-'+dd+' '+t
	utcdt = anytim2utc(str)
	IF utcdt.mjd - utc.mjd GT 0 THEN $
	 str = strtrim(string(fix(yyyy)-1),2) +'-'+mm+'-'+dd+' '+t
   ENDELSE
ENDELSE

   utcdt = ANYTIM2UTC(str)
   IF keyword_set(DATE_ONLY) THEN return,UTC2STR(utcdt,/ECS,/DATE_ONLY)
   RETURN, UTC2TAI(utcdt)


END
