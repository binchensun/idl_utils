;+
; NAME:
;  astcvt
; PURPOSE:   (one line only)
;  Convert between different astrometric coordinate systems.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astcvt,srcnam,srcval1,srcval2,info,dstnam,dstval1,dstval2
; INPUTS:
;  srcnam - String that identifies the type of input data
;              xy - raw image coordinates
;              mn - normalized monolith coordinates
;              sn - tangent plane coordinates (radians)
;              SN - tangent plane coordinates (arcsec)
;              rd - right ascension, declination (radians)
;  srcval1,srcval2 - input pair of values (scalar or vector),
;                       must have the same length
;  info   - full astrometric support structure (see astinfo)
;  dstnam - String that identifies the type of output data, same list as srcnam
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  dstval1,dstval2 - output pair of values
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/11/20 - Written by Marc W. Buie, Southwest Research Institute
;-
pro astcvt,srcnam,srcval1,srcval2,info,dstnam,dstval1,dstval2

   self='ASTCVT: '
   if badpar(srcnam,7,0,caller=self+'(srcnam) ') then return
   if badpar(srcval1,[2,3,4,5],[0,1,2,3],caller=self+'(srcval1) ') then return
   if badpar(srcval2,[2,3,4,5],[0,1,2,3],caller=self+'(srcval2) ') then return
   if badpar(info,8,1,caller=self+'(info) ') then return
   if badpar(dstnam,7,0,caller=self+'(dstnam) ') then return

   validnames=['xy','mn','sn','SN','rd']

   z=where(srcnam eq validnames,count)
   if count ne 1 then begin
      print,'srcnam value of [',srcnam,'] is not valid.'
      dstval1=!null
      dstval2=!null
      return
   endif

   z=where(dstnam eq validnames,count)
   if count ne 1 then begin
      print,'dstnam value of [',dstnam,'] is not valid.'
      dstval1=!null
      dstval2=!null
      return
   endif

   if srcnam eq dstnam then begin
      print,'srcnam and dstnam must be different.'
      dstval1=!null
      dstval2=!null
      return
   endif

   case srcnam of
      'xy': begin
         case dstnam of
            'mn': begin
               ; offset then rotate
               tmpval1 = srcval1-info.xcref
               tmpval2 = srcval2-info.ycref
               dstval1 = ( tmpval1*cos(info.prot) + $
                           tmpval2*sin(info.prot) ) / info.renormfac
               dstval2 = (-tmpval1*sin(info.prot) + $
                           tmpval2*cos(info.prot) ) / info.renormfac
            end
            'sn': begin
               astcvt,'xy',srcval1,srcval2,info,'mn',tmpval1,tmpval2
               astcvt,'mn',tmpval1,tmpval2,info,'sn',dstval1,dstval2
            end
            'SN': begin
               astcvt,'xy',srcval1,srcval2,info,'mn',tmpval1,tmpval2
               astcvt,'mn',tmpval1,tmpval2,info,'SN',dstval1,dstval2
            end
            'rd': begin
               astcvt,'xy',srcval1,srcval2,info,'sn',tmpval1,tmpval2
               astcvt,'sn',tmpval1,tmpval2,info,'rd',dstval1,dstval2
            end
         endcase
      end
      'mn': begin
         case dstnam of
            'xy': begin
               ; rotate then offset
               tmpval1 = srcval1*info.renormfac
               tmpval2 = srcval2*info.renormfac
               dstval1 = (tmpval1*cos(info.prot)-tmpval2*sin(info.prot))
               dstval2 = (tmpval1*sin(info.prot)+tmpval2*cos(info.prot))
               dstval1 = dstval1+info.xcref
               dstval2 = dstval2+info.ycref
            end
            'sn': begin
               astcvt,'mn',srcval1,srcval2,info,'SN',tmpval1,tmpval2
               astcvt,'SN',tmpval1,tmpval2,info,'sn',dstval1,dstval2
            end
            'SN': begin
               dstval1 = asteval(srcval1,srcval2,info.cxi,info.terms)
               dstval2 = asteval(srcval1,srcval2,info.ceta,info.terms)
            end
            'rd': begin
               astcvt,'mn',srcval1,srcval2,info,'sn',tmpval1,tmpval2
               astcvt,'sn',tmpval1,tmpval2,info,'rd',dstval1,dstval2
            end
         endcase
      end
      'sn': begin
         case dstnam of
            'xy': begin
               astcvt,'sn',srcval1,srcval2,info,'mn',tmpval1,tmpval2
               astcvt,'mn',tmpval1,tmpval2,info,'xy',dstval1,dstval2
            end
            'mn': begin
               astcvt,'sn',srcval1,srcval2,info,'SN',tmpval1,tmpval2
               astcvt,'SN',tmpval1,tmpval2,info,'mn',dstval1,dstval2
            end
            'SN': begin
               dstval1 = srcval1*180.0d0/!dpi*3600.0d0
               dstval2 = srcval2*180.0d0/!dpi*3600.0d0
            end
            'rd': begin
               cosdeccen = cos(info.decref)
               sindeccen = sin(info.decref)
               tandeccen = tan(info.decref)
               dstval1 = atan(srcval1,(cosdeccen-srcval2*sindeccen))+info.raref
               dstval2 = atan((cos(dstval1-info.raref)*(srcval2+tandeccen)) / $
                               (1.0d0-srcval2*tandeccen))
            end
         endcase
      end
      'SN': begin
         case dstnam of
            'xy': begin
               astcvt,'SN',srcval1,srcval2,info,'mn',tmpval1,tmpval2
               astcvt,'mn',tmpval1,tmpval2,info,'xy',dstval1,dstval2
            end
            'mn': begin
               xi  = srcval1
               eta = srcval2
               ; pull out the linear coefficients for use in computing the
               ; starting guess.
               z=where(strlowcase(info.terms) eq 'const',/null)
               a0=trimrank(info.cxi[z])
               b0=trimrank(info.ceta[z])
               z=where(strlowcase(info.terms) eq 'x',/null)
               a1=trimrank(info.cxi[z])
               b1=trimrank(info.ceta[z])
               z=where(strlowcase(info.terms) eq 'y',/null)
               a2=trimrank(info.cxi[z])
               b2=trimrank(info.ceta[z])

               ; first guess for the mn coordinates
               dx = trimrank((a2*(eta-b0)-b2*(xi-a0))/(b1*a2-b2*a1))
               dy = trimrank((a1*(eta-b0)-b1*(xi-a0))/(b2*a1-b1*a2))

               ; compute new xi,eta
               astcvt,'mn',dx,dy,info,'SN',new_xi,new_eta

               ; compute error i xi,eta
               dxi = xi - new_xi
               deta = eta - new_eta

               pass=0
               while max(abs(dxi)) gt 0.001 or max(abs(deta)) gt 0.001 do begin

                  ; compute tweak
                  if abs(info.ceta[1]) gt abs(info.cxi[1]) then $
                     delx = deta/info.ceta[1] $
                  else $
                     delx = dxi/info.cxi[1]

                  if abs(info.ceta[2]) gt abs(info.cxi[2]) then $
                     dely = deta/info.ceta[2] $
                  else $
                     dely = dxi/info.cxi[2]

                  ; next approximation to mn coordinates
                  dx = dx + delx
                  dy = dy + dely

                  ; compute new xi,eta and error
                  astcvt,'mn',dx,dy,info,'SN',new_xi,new_eta
                  dxi = xi - new_xi
                  deta = eta - new_eta

if pass eq 90 then begin
   print,self,'early exit',max(abs(dxi)),max(abs(deta))
   break
endif
                  pass++
               endwhile
               dstval1 = dx
               dstval2 = dy
            end
            'sn': begin
               dstval1 = srcval1/180.0d0*!dpi/3600.0d0
               dstval2 = srcval2/180.0d0*!dpi/3600.0d0
            end
            'rd': begin
               astcvt,'SN',srcval1,srcval2,info,'sn',tmpval1,tmpval2
               astcvt,'sn',tmpval1,tmpval2,info,'rd',dstval1,dstval2
            end
         endcase
      end
      'rd': begin
         case dstnam of
            'xy': begin
               astcvt,'rd',srcval1,srcval2,info,'mn',tmpval1,tmpval2
               astcvt,'mn',tmpval1,tmpval2,info,'xy',dstval1,dstval2
            end
            'mn': begin
               astcvt,'rd',srcval1,srcval2,info,'sn',tmpval1,tmpval2
               astcvt,'sn',tmpval1,tmpval2,info,'mn',dstval1,dstval2
            end
            'sn': begin
               beta      = srcval1 - info.raref
               cosbeta   = cos(beta)
               tandec    = tan(srcval2)
               tandeccen = tan(info.decref)
               s   = cosbeta + tandec*tandeccen
               dstval1 = sin(beta)/cos(info.decref)/s
               dstval2 = (tandec - tandeccen*cosbeta)/s
            end
            'SN': begin
               astcvt,'rd',srcval1,srcval2,info,'sn',tmpval1,tmpval2
               astcvt,'sn',tmpval1,tmpval2,info,'SN',dstval1,dstval2
            end
         endcase
      end
   endcase

end
