pro ut2mjd_vla, datetime, type=type, format=format
;+
; NAME:
; 	UT2MJD_VLA
; PURPOSE:
;       Procedure to convert datetime format in UTC to MJD to be used in the VLA observing script
; EXPLANATION:
; 	The following datetime formats are allowed
;
;       format 1: real*8 scalar encoded as:
;               year*1000 + day + hour/24. + min/24./60 + sec/24./60/60
;               where day is the day of year (1 to 366)
;       format 2: Vector encoded as:
;               date[0] = year (eg. 2005)
;               date[1] = day of year (1 to 366)
;               date[2] = hour
;               date[3] = minute
;               date[4] = second
;               To indicate a date only, set a negative hour.
;       format 3: string (ascii text) encoded as
;               DD-MON-YEAR HH:MM:SS.SS
;               (eg.  14-JUL-2005 15:25:44.23)
;            OR
;               YYYY-MM-DD HH:MM:SS.SS  (ISO standard)
;               (eg.  1987-07-14 15:25:44.23 or 1987-07-14T15:25:44.23)
;
;            OR 
;               DD/MM/YY (pre-2000 option for FITS DATE keywords)
;            Time of day segment is optional in all of these.
;       
;       format 4: three element vector giving spacecraft time words
;       from a Hubble Space Telescope (HST) telemetry packet.   Based on
;       total number of secs since midnight, JAN. 1, 1979
;
;       format 5: Julian day. As this is also a scalar, like format 1, 
;       	the distinction between the two on input is made based on their
;       	value. Numbers > 2300000 are interpreted as Julian days.
;
; CALLING SEQUENCE
;       ut2mjd_vla, datetime, 'vla'
;
; INPUTS:
;       DATE - input date in one of the possible formats. Must be scalar.
;       TYPE - type of output format desired.  If not supplied then
;               format 3 (real*8 scalar) is used.
;                       valid values:
;                       'REAL'  - format 1
;                       'VECTOR' - format 2
;                       'STRING' - format 3
;                       'FITS' - YYYY-MM-DDTHH:MM:SS.SS'
;                       'JULIAN' - Julian date
;                       'MODIFIED' - Modified Julian date (JD-2400000.5)
;               TYPE can be abbreviated to the single character strings 'R',
;               'V', 'S', 'F', 'J', and 'M'.
;               Nobody wants to convert TO spacecraft time (I hope!)
; OUTPUTS:
;       The converted date is returned as the function value.
;       Output is -1 if date is unrecognisable. 
;
;       If the time of day is omitted from the input, it will also
;       be omitted from any output string (format STRING or FITS). 
;       Note that date-only strings are allowed by the FITS standard. 
;       For other output formats any missing time of day is set to 
;       00:00:00.0
; EXAMPLES:
;       IDL> ut2mjd_vla,'2006-03-13 19:58:00.00', type='vla', format='(f17.11)'
;       IDL> ut2mjd_vla,'2006-03-13T19:58:00', type='vla', format='(f17.11)'
;
if ~keyword_set(type) then type='MODIFIED'
if ~keyword_set(format) then format='(f17.11)'
print,date_conv(datetime,type),format=format
end
 
