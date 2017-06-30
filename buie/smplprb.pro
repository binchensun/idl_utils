;+
; NAME:
;  smplprb
; PURPOSE:   (one line only)
;  Sample a probability function to facilitate drawing random numbers from it.
; DESCRIPTION:
;  This program is intended to take a probability function, and return
;    an array that if plotted as a histogram will look like the probability
;    function.  The array that is produced is meant to be used to draw
;    random numbers that will be characterized by the probility function.
; CATEGORY:
;  Mathematical
; CALLING SEQUENCE:
;  xval=SMPLPRB(func,x1,x2,n)
; INPUTS:
;  func   -String. Function name. ex: 'sqrt' or 'alog'
;              Any positive definite function can be used and must take
;               one and only one argument.
;  x1     - Min of x range.
;  x2     - Max of x range. 
; OPTIONAL INPUT PARAMETERS:
;  n      - Size of output array.  Determines sampling size.
;           DEFAULT=10000.  As this number is increased the resolution of
;           the sampled probability function is improved.  Successful usage
;           of this routine will depend on tuning this value.
; KEYWORD INPUT PARAMETERS:
; PLOT    - Flag, if set will cause a plots to be generated to show
;             a histogram derived from xval with the function overplotted.
; DEBUG   - Flag, if set will print some internal debugging information.
; RANDOMIZE - Flag, if set will force the returned array to be in random
;               order.  The default is to turn them in an order sorted by
;               increasing function value.
; OUTPUTS:
;  returns an array of n x-values between x1 and x2.  Each discrete value of
;     X appears in the output array as many times as is needed to represent
;     the probability of that value.  This is a relative number since
;     increasing N will increase the number of times that every value of X
;     will appear.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Can only take probability functions, i.e. the function cannot be
;  less than zero.
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/07/13, Written by Maureen Teyssier, REU Lowell Observatory
;  2004/07/15, MWB, incorporated into library.
;  2013/08/14, MWB, slight algorithmic improvements, on average the results
;                     are nearly the same but the sampling is slightly more
;                     robust and cleaner now.  Also added the DEBUG and
;                     RANDOMIZE keyword.
;-
function smplprb,func,x1,x2,n,PLOT=plot,DEBUG=debug,RANDOMIZE=randomize

   self='SMPLPRB: '
   if badpar(func,[7],0,CALLER=self+'(func) ') then return,-1
   if badpar(x1,[1,2,3,4,5],0,CALLER=self+'(x1) ') then return,-1
   if badpar(x2,[1,2,3,4,5],0,CALLER=self+'(x2) ') then return,-1

   if badpar(n,[0,1,2,3,4,5],0,CALLER=self+'(n) ',DEF=10000) then return,-1

   if badpar(plot,[0,1,2,3],0,CALLER=self+'(PLOT) ',DEF=0) then return,-1
   if badpar(debug,[0,1,2,3],0,CALLER=self+'(DEBUG) ',DEF=0) then return,-1
   if badpar(randomize,[0,1,2,3],0,CALLER=self+'(RANDOMIZE) ',DEF=0) then return,-1

   if debug then print,'input range',x1,x2,' with ',strn(n),' values'

   ;m is used to sample x.  Making m a large number will sample x finely.  
   ;It is necessary to make m large enough so that you get a representative 
   ;sample, (this prevents skipping over values). However, if m is overly 
   ;large, there are a lot of repeated values-you're not gaining any more 
   ;information. Sqrt(n) is a heuristic guess for m that seems to work in 
   ;most cases.
                                
   m=ceil(sqrt(n))
   if debug then print,'bins',m

   ; Divide the given range into M discrete values
   dx=(x2-x1)/m
   if debug then print,'bin size',dx

   ;Creating an array of discrete x values over the given range from x1 to x2.  
   x=x1+(indgen(m)+0.5)*dx
   if debug then print,'x array runs from ',min(x),' to ',max(x)

   ;evaluate the function at the bin value (x)
   fxn=call_function(func,x)

   ;normalize the resulting sampled function 
   fcn=fxn/total(fxn)
   if debug then print,'Normalized function range',minmax(fcn)
   if debug then print,'Scaled function range',minmax(fcn)*n

   ;when you divide the range into m pieces of size dx, it isn't divided
   ;exactly. So, when you multiply m pieces by dx, and add x1, you don't
   ;get x2 exactly. 

   xval=fltarr(n)
   j=0L

   ;for each value of x that is evaluated for the normalized function, 
   ;find out how many times x should appear in the output array
   for i=0L,m-1 do begin
      ;Assigning the probablility, of the function at x, an integer.  This
      ;integer is also the number of times that x will be placed in xval.
      count=fix(fcn[i]*n+.5)
      if count gt 0 then begin
         ;Placing the replicated value of x in xval
         i0=j
         i1=(j+count-1) < (n-1)
         xval[i0:i1]=replicate(x[i],count)
         j+=count 
      endif
   endfor
   if debug then print,strn(j),' values generated'

   ; If the output array is not quite filled up, do so from the last value
   if j lt n then begin
      i0=j
      i1=n-1
      xval[i0:i1]=xval[j-1]
      if debug then $
         print,'Padding out from ',strn(i0),' to ',strn(i1),' with ',xval[j-1]
   endif

   if debug then print,'range of result',minmax(xval)

   ;check the histogram against the plot
   if plot then begin
      h=histogram(xval,nbins=m+1,min=x1,max=x2)
      plot,x,fcn*n
      oplot,x,h,color='0000ff'xl,psym=10
   endif

   if randomize then begin
      idx=sort(randomu(seed,n))
      return,xval[idx]
   endif else begin
      return,xval
   endelse

end
