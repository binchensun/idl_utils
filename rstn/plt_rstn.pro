pro plt_rstn,rstnfile,freqs,timestr,fluxs,obs=obs,timeran=timeran,basetime=basetime
; NAME: 
;       plt_rstn
; PURPOSE: 
;       read ngdc/rstn 8 frequency data from ascii file and convert to idl variables
; INPUTS:
;       rstnfile - name of the ascii file to open, find it at 
;               ftp://ftp.ngdc.noaa.gov/STP/space-weather/solar-data/solar-features/solar-radio/rstn-1-second/
;       Obs - 4-letter string from PALE (Palehua), LEAR (Learmonth),
;              SVTO (San Vito), SGMR (Sagamore-hill)
;              Typical time ranges SVTO 5-16, HOLL 13-24, PALE 17-04, 
;              LEAR 23-10, SGMR 11-22
; OUTPUTS:
;       freqs - frequencies of the input data
;       times - string array of times in ssw format (convertable by anytim)
;       fluxs - array of fluxs for all the frequency bands, format is float array (nfreqs, ntimes)

;frequency values for each column, in GHz
freqs=[0.245,0.410,0.610,1.415,2.695,4.995,8.800,15.400] ;
;find out the data format
;if not specified, find out from file extension
if n_elements(obs) eq 0 then begin
    pos=strpos(rstnfile,'.')
    ext=strmid(rstnfile,pos+1,3)
    case 1 of
        (ext eq 'k7o') or (ext eq 'K7O'): begin 
                print, 'Input data is from Sagamore-hill.'
                obs='SGMR'
                end
        (ext eq 'apl') or (ext eq 'APL'): begin
                print, 'Input data is from Learmonth.'
                obs='LEAR'
                end
        (ext eq 'lis') or (ext eq 'LIS'): begin
                print, 'Input data is from San-vito.'
                obs='SVTO'
                end
        (ext eq 'phf') or (ext eq 'PHF'): begin
                print, 'Input data is from Palehua.'
                obs='PALE'
                end
        else: begin 
                print, 'Input data is not recognized. Abort...'
                close,1
                break
            end
    endcase
endif
;read the data
openr,1,rstnfile
;fmt='A,I,I,I,I,I,I,I,I'
;readcol,rstnfile,format=fmt,timestr,f1,f2,f3,f4,f5,f6,f7,f8
cols=8
rows=100000L
timestr=strarr(rows)
f1=intarr(rows) & f2=f1 & f3=f1 & f4=f1
f5=f1 & f6=f1 & f7=f1 & f8=f1
fluxs=intarr(cols,rows)
s=''
n=0L
while ~ EOF(1) do begin
    readf,1,s
    ;time format:yyyy-mm-dd hh:mm:ss
    timestr_=strmid(s,4,4)+'-'+strmid(s,8,2)+'-'+strmid(s,10,2)+' '+$
            strmid(s,12,2)+':'+strmid(s,14,2)+':'+strmid(s,16,2)
    timestr[n]=timestr_
    ;fluxs
    res=strnumber(strmid(s,18,7),f1_) & f1[n]=f1_
    res=strnumber(strmid(s,25,7),f2_) & f2[n]=f2_
    res=strnumber(strmid(s,32,7),f3_) & f3[n]=f3_
    res=strnumber(strmid(s,39,7),f4_) & f4[n]=f4_
    res=strnumber(strmid(s,46,7),f5_) & f5[n]=f5_
    res=strnumber(strmid(s,53,7),f6_) & f6[n]=f6_
    res=strnumber(strmid(s,60,7),f7_) & f7[n]=f7_
    res=strnumber(strmid(s,67,7),f8_) & f8[n]=f8_
    fluxs=[[f1],[f2],[f3],[f4],[f5],[f6],[f7],[f8]]
    n=n+1
endwhile
timestr=timestr[0:n-1]
times=anytim(timestr,/time_only)
fluxs=fluxs[0:n-1,*]
close,1
;
set_plot,'ps'
device,file='rstn.eps',xs=5,ys=10,/inches,/encaps
nwin=8
!p.multi=[0,1,nwin]
if ~keyword_set(basetime) then basetime='2012-03-03 00:00'
if ~keyword_set(timeran) then timeran=['2012-03-03 18:00','2012-03-03 19:30']
ledg=0.13 & redg=0.96 ;left and right edge
bedg=0.04 & tedg=0.99 ;bottom and top edge
ghgt=0.03 & dhgt=(tedg-bedg-(nwin-1)*ghgt)/nwin ;gap height and data window height
x1s=fltarr(nwin) & x2s=x1s & y1s=x1s & y2s=x1s
positions=fltarr(4,nwin)
x1s[0]=ledg & y1s[0]=bedg & x2s[0]=redg & y2s[0]=y1s[0]+dhgt
positions[*,0]=[x1s[0],y1s[0],x2s[0],y2s[0]]
for i=1,nwin-1 do begin
    x1s[i]=ledg
    x2s[i]=redg
    y1s[i]=y1s[i-1]+dhgt+ghgt
    y2s[i]=y1s[i]+dhgt
    positions[*,i]=[x1s[i],y1s[i],x2s[i],y2s[i]]
endfor
finds=[7,6,5,4,3,2,1,0]
;yrans=[[520,650],[280,450],[160,350],[80,440],[50,450],[30,200],[30,160],[0,160]]
yrans=[[520,600],[280,400],[160,230],[115,150],[50,80],[30,200],[30,160],[0,160]]
for i=0,nwin-1 do begin
    flux=fluxs[*,finds[i]]
    position=positions[*,i]
    yran=yrans[*,i]
    if i eq 0 then begin
        utplot,times,flux,basetime,timeran=timeran,position=position,$
            yran=yran,/xsty,/ysty,xminor=3,chars=1.5,ytit='Flux (SFU)',thick=2
    endif else begin
        utplot,times,flux,basetime,timeran=timeran,position=position,$
            yran=yran,/xsty,/ysty,xminor=3,chars=1.5,xtit='',ytit='Flux (SFU)',thick=2
    endelse
    xyouts,position[2]-0.3,position[3]-0.02, 'RSTN '+$
        string(freqs[finds[i]],format='(F6.3)')+' GHz',chars=0.9,/norm
endfor
device,/close
set_plot,'x'
!p.multi=[0,1,1]
end
