pro sprev,filenm,avf,avt,amp,phs
;
; simple task to read an SPFLG gridded scratch file - returns a cube
; containing the (averaged) amplitude and phase cross spectrum for
; each baseline
;    filenm = name of FITS file containing SPFLG data
;    avf = number bins to average in frequency (e.g., 2)
;    avt = number of bins to average in time
;    amp = cube containing dynamic cross spectrum of baseline
;    amplitudes
;    phs = cube containing dynamic cross spectrum of baseline phases
;
; comments: modified by B. Chen to allow average with any integer number of frequency channels/time points
; (due to the restriction of rebin). Simply removed the last few pixels.

a=readfits(filenm)
s=size(a)
nf=s(1) & nt=s(2) & np=s(3) & nbl=s(4)
nfreq=(nf-3)/3
rpt=fltarr(nfreq,nt,np,nbl) & ipt=rpt & wgt=ipt
for i=0,nfreq-1 do begin
   rpt(i,*,*,*)=a(3+3*i,*,*,*)
   ipt(i,*,*,*)=a(4+3*i,*,*,*)
   wgt(i,*,*,*)=a(5+3*i,*,*,*)
endfor
nfa=nfreq/avf & nta=nt/avt
rpt=rebin(rpt[0:nfa*avf-1,0:nta*avt-1,*,*],nfa,nta,np,nbl)
ipt=rebin(ipt[0:nfa*avf-1,0:nta*avt-1,*,*],nfa,nta,np,nbl)
amp=sqrt(rpt^2.+ipt^2.) 
phs=atan(rpt,ipt)*180.0/!pi 
end
