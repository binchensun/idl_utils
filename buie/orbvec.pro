;+
; NAME:
;  orbvec
; PURPOSE:   (one line only)
;  Computes orbit orientation vectors from angular orbital elements
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  orbvec,inc,node,peri,p,q,r
; INPUTS:
;  inc  - Orbit inclination [scalar, radians]
;  node - Longitude of the ascending node [scalar, radians]
;  peri - Longitude of periapse [scalar, radians]
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  p,q,r - 3-element orientation vectors
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/11/29, Written by Marc W. Buie, Southwest Research Institute
;                guided by PQR.F written by David J. Tholen
;-
pro orbvec,inc,node,peri,pv,qv,rv

   self='orbvec: '
   if badpar(inc,[4,5],0,caller=self+'(inc) ') then return
   if badpar(node,[4,5],0,caller=self+'(node) ') then return
   if badpar(peri,[4,5],0,caller=self+'(peri) ') then return

   cosi=cos(inc)
   sini=sin(inc)
   cosn=cos(node)
   sinn=sin(node)
   cosw=cos(peri)
   sinw=sin(peri)

   cncw=cosn*cosw
   cnsw=cosn*sinw
   sncw=sinn*cosw
   snsw=sinn*sinw

   pv = [ -cosi*snsw+cncw, $
           cosi*cnsw+sncw, $
           sini*sinw ]

   qv = [ -cosi*sncw-cnsw, $
           cosi*cncw-snsw, $
           sini*cosw ]

   rv = [  sini*sinn, $
          -sini*cosn, $
           cosi ]

end
