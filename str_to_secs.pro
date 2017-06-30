function str_to_secs,str
hour=strmid(str,0,2)
minute=strmid(str,3,2)
sec=strmid(str,6,2)
secnum=float(hour)*3600.+float(minute)*60.+float(sec)
return,secnum
end