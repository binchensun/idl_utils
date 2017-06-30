;+
; NAME:
;  getnewid
; PURPOSE:   (one line only)
;  Obtain an id that can be used for a new object based on database information
; DESCRIPTION:
;  When a new object is discovered (or you think it is), it needs a unique
;    name for identification.  The scheme implemented here tries to accomodate
;    other methods for doing this that were used in the past that did not
;    rely on a central database to hold the information.  It also has to work
;    with the restrictions imposed by Larry Wasserman's various ephemeris tools.
;
;  An object ID is thus defined to be a two character alphabetic (upper case
;    only) prefix followed by 1 or more numeric digits.  There have been cases
;    in the past where the number was not padded to a constant length.  For
;    example, there is an object defined as MA1 in the same sequence where there
;    is an object named MA1001.  In this case the number of digits in the
;    sequence varies.  This type of case is NOT supported by this program.
;    Instead, a sequence is defined by its prefix, and how many zero-padded
;    digits it contains.  In principle there is no upper limit to the number
;    of digits you can use but in practice there is a limit imposed by other
;    programs and data file structures.  I think the real upper limit is
;    six digits but this limit is not imposed by this program.
;
;  Therefore, a sequence of IDs is always finite and the number of IDs in
;    a sequence depends on the number of digits.  As long as the prefix was
;    not used for the older, obsolete scheme, you can have multiple sequences
;    for the same prefix.  The difference will be in length.  Thus MA100 would
;    be in a different sequence from MA1000 (3 digits vs. 4 digits).
;
;  A sequence supported by this routine is defined by the prefix and the
;    number of digits.  Optionally, a lower bound to the numeric tag can
;    be specified but this does not change the definition of a sequence.  It
;    is just provided as a helpful tool for working in different sections of
;    a sequence.
;
;  When asking for a new id, the sequence definition is used to query for the
;    currently defined objects.  From this and the minimum value requested,
;    this program will find the lowest number that is not already used.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  getnewid,prefix,digits,newid
; INPUTS:
;  prefix - Two character uppercase alphabetic string
;  digits - Number of digits in the id
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  MINVAL - Lowest number to consider in the seqeunce (default=0)
;  DBNAME - Name of database to use (default='gen')
;  TBLNAME - Name of tabe to use (default='ast')
; OUTPUTS:
;  newid  - The new ID to use (string).  This does not actually get put into
;              the database so it doesn't really exist until you use it and
;              save it to the database.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2012/01/08, Written by Marc W. Buie, Southwest Research Institute
;-
pro getnewid,prefix,digits,newid,MINVAL=minval,DBNAME=dbname,TBLNAME=tblname

   self='getnewid: '
   if badpar(prefix,7,0,caller=self+'(prefix) ') then return
   if badpar(digits,[2,3],0,caller=self+'(digits) ') then return
   if badpar(minval,[0,2,3],0,caller=self+'(MINVAL) ',default=0) then return
   if badpar(dbname,[0,7],0,caller=self+'(DBNAME) ',default='gen') then return
   if badpar(tblname,[0,7],0,caller=self+'(TBLNAME) ',default='ast') then return

   openmysql,dblun,dbname

   srch=''
   for i=0,digits-1 do srch=srch+'_'

   cmd='select id from ast'+ $
       ' where id like '+quote(prefix+srch)

   if minval gt 0 then begin
      minid=prefix+strn(minval,len=digits,padchar='0')
      cmd=cmd+' and id>'+quote(minid)
   endif

   cmd=cmd+' order by id desc limit 1;'

   mysqlquery,dblun,cmd,oldid,format='a',ngood=ncheck

   if ncheck eq 1 then begin
      num=long(strmid(oldid,2))
      num=num+1
   endif else begin
      num=minval
   endelse

   newid=prefix+strn(num,len=digits,padchar='0')

   free_lun,dblun

end
