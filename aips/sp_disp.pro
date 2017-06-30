pro sp_disp,spfile=spfile,avf=avf,avt=avt,figdir=figdir
; task to select amplitude/phase, polarization, and baseline to display dynamic spectrum
; simple functions such as advancing to the previous/next baseline, save current display as a figure, and
; return to initial selection can be used through keyboard readouts
; subroutine needed: sprev.pro 
; inputs: spfile -- AIPS SPFLG catalog fits file, if not set, pick one from a standard IDL dialogue widget
;         avf    -- frequency channels to average
;         avt    -- integration time points to average
;         figdir -- output figure directory, if not set, pick one from a standard IDL dialogue widget

defpath='/media/My Book/Projects/20120303/EVLA/FITS/SPFLG/' ;default SPFLG fits file path
if ~keyword_set(spfile) then spfile=dialog_pickfile(path=defpath,$
    title='Select AIPS SPFLG fits file', filter='*.FITS')
if ~keyword_set(avf) then avf=1
if ~keyword_set(avt) then avt=1
print, 'Selected AIPS SPFLG fits file: ',spfile
sprev,spfile,avf,avt,amp,pha
sz=size(amp)
init: sel='' 
print,'Make selection for display. Type "ap" for selecting amplitude or phase;', $
      '"pol" for selecting polarization; "bl" for selecting baseline,',$
      '"c" to finish selection and continue to drawing'
read,sel,prompt='type "ap", "pol", "bl" or "c": '
case sel of
    'ap':     begin 
                    print, 'Selecting amplitude or phase...'
                    ap=''
                    repeat begin
                        read,ap,prompt='Type "amp" or "pha": '
                        if ap ne 'amp' and ap ne 'pha' then $
                            print, 'Not recognized, please type "amp" or "pha"'
                    endrep until ap eq 'amp' or ap eq 'pha'
                    case ap of
                        'amp': data=alog10(amp)
                        'pha': data=pha
                    endcase
                    print,'Displaying data of ',ap
                    goto,init
              end
    'pol':    begin
                    print,'Selecting polarization...'
                    rl=''
                    repeat begin
                        read,rl,prompt='Type "r" or "l": '
                        if rl ne 'R' and rl ne 'r' and rl ne 'L' and rl ne 'l' then $
                            print,'Not recognized, please type "R" or "L"'
                    endrep until rl eq 'R' or rl eq 'r' or rl eq 'L' or rl eq 'l'
                    case rl of 
                        'r': pol=0
                        'l': pol=1
                    endcase
                    print,'Displaying polarization: ', rl
                    goto,init
              end
    'bl':     begin
                    print,'Selecting baseline...'
                    blstr=''
                    repeat begin
                        read,blstr,prompt='Type a number, starting from 0: '
                        chk=strnumber(blstr,bl)
                        bl=fix(bl)
                        if chk eq 0 then begin
                            print,'Baseline input is not a number! Retry...'
                        endif
                        if bl gt sz[4]-1 then begin
                            print,'Baseline input is larger than that is available! Retry...'
                        endif
                    endrep until chk eq 1 and bl le sz[4]-1
                    print,'Displaying baseline number: ', bl
                    goto,init
              end      
    'c':      begin
                    if n_elements(ap) eq 0 then begin
                        ap='amp'
                        data=alog10(amp)
                    endif
                    if n_elements(pol) eq 0 then begin
                        pol=0
                        rl='r'
                    endif
                    if n_elements(bl) eq 0 then bl=0
                    print,'Done selection, displaying spectrum for: '+ap+', '+rl+', '+string(bl+1,format='(I3)')
              end
    else:     begin
                    print,'Not recognized, please type "ap", "pol", "bl" or "c"'
                    goto, init
              end
endcase

;--------------display the dynamic spectrum------------------------
window,0,xs=sz[1],ys=sz[2]
print,'Type "r" to reselect, "n" to advance to the next baseline, ', $
      '"b" to go back to the last baseline, "s" to save image, ', $
      '"q" to quit: '
disp: print,'Displaying spectrum for: '+ap+', '+rl+', '+string(bl+1,format='(I3)')
tvscl,data[*,*,pol,bl]
;------------------------------------------------------------------
next: nxt=''
read,nxt,prompt='Type "r", "n", "b", "s" or "q": '
case nxt of
    'r': goto, init
    'n': begin
                    bl=bl+1
                    if bl gt sz[4]-1 then begin 
                        ;print, 'Baseline number is larger than that is available!, displaying bl=0'
                        bl=0
                        goto, disp
                    endif else begin
                        goto, disp
                    endelse
         end
    'b': begin
                    bl=bl-1
                    if bl lt 0 then begin 
                        ;print, 'Baseline number is negative, displaying the last baseline'
                        bl=sz[4]-1
                        goto, disp
                    endif else begin
                        goto, disp
                    endelse
         end
    's': begin  
                    if ~keyword_set(figdir) then begin
                        print,'Figure output directory not set, pick up one: '
                        figdir=dialog_pickfile(/directory)
                    endif
                    len=strlen(spfile)
                    pos=strpos(spfile,'SPF_')
                    fign=strmid(spfile,pos,len-5-pos)+'_'+ap+'_'+rl+'_bl'+string(bl+1,format='(I03)')+'.png'
                    print, 'Saving in the directory: '+figdir+' as '+fign
                    write_png,figdir+fign,tvrd(/true)
                    goto, next
         end
    'q': break
    else: begin
                print, 'Not recognized, please type "r", "n", "b", "s", or "q"'
                goto, next
          end
endcase
end
