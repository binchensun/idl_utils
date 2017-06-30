;+
Pro fieldlines_fff_bc, bx_in, by_in, bz_in, ix_in, iy_in, iz_in, x_in, y_in, z_in, $
                    angx = angx, angz = angz, $
                    thick = thick, maxiter = maxiter, $
                    oplot = oplot, linecolor = linecolor, $
                    title = title, pick_lines = pick_lines, $
                    n_lines = n_lines, window_number = window_number, $
                    boxqs = boxqs, _extra = _extra, $
                    psym = psym, symsize = symsize, $
                    nolines = nolines, nopoints = nopoints
;
; traces field lines for a specified magnetic field
; configuration. Uses the bi-linear & tri-linear interpolation routines
; lin2interp and lin3interp, as well as the routine
; differential2. Added maxiter to get rid of hangups, 28-feb-2004, jmm
; rewritten for multiple fields, 20-may-2004, jmm
; Added oplot, 24-may-2004, jmm
;
; The input is a magnetic field, bx, by, bz
;
; Inputs ix_in, iy_in, iz_in are x, y, and z subscripts of field lines, you can 
; set them yourself. Ix_in, iy_in and iz_in must have the same number of
; elements...
;
; Use the /pick_lines to choose field lines, and return ix, iy, If
; n_lines is set, then there will be this many lines, the default for
; n_lines is 20
;
; You can pass in the spatial grids, x_in, y_in, and z_in
; This program requires uniform grids, and probably
; the same number of points in the x and y grids.
;
; angx and angz are angles for orientation, the defaults are
; angx = 30.0, and angz = 30.0, unless /pick_lines is set, then
; angx = 90.0 and angz = 0.0, regardless of the inputs
;
; Thick controls the thickness of the plotted line, the default is
; 1.0,
;
; If oplot is set, this oplots on whatever the current window is
;
; linecolor controls the color of the lines.
;
; title is a title for the plot
;
; Use the keyword window_number to set a new window.
;-

  If(n_params() Lt 6) Then Begin
    print, 'fieldlines_fff, bx, by, bz, ix, iy, iz'
    Return
  Endif

;sniff out the device, to see if you can use window commands
  If(!d.name Eq 'X') Then x_yes = 1b Else x_yes = 0b
  If(keyword_set(thick)) Then thick_x = thick Else thick_x = 1.0
  If(not keyword_set(pick_lines)) Then Begin
    if(n_elements(angx) eq 0) then angx = 30.
    if(n_elements(angz) eq 0) then angz = 30.
  Endif Else Begin
    angx = 90.0
    angz = 0.0
  Endelse 

  common shared, bx, by, bz, x, y, z, x2, y2, z2, sign
    
  bx = bx_in & by = by_in & bz = bz_in 
  bmag=sqrt(bx^2.+by^2.+bz^2.)
;x, y, and z have to be < 1, i think
  If(n_elements(x_in) Eq 0) Then Begin
    nx = n_elements(bx[*, 0, 0])
    ny = n_elements(bx[0, *, 0])
    nz = n_elements(bx[0, 0, *])
    x = findgen(nx)/float(nx-1) 
    y = findgen(ny)/float(ny-1) 
    z = findgen(nz)/float(nz-1)
  Endif Else Begin
;Grids are defined from [0,0,0] 
    x = x_in-min(x_in)
    y = y_in-min(y_in)
    z = z_in-min(z_in)
    max_xy = max(abs([x, y]))
    x = x/max_xy & y = y/max_xy & z = z/max_xy
  Endelse
  If(keyword_set(boxqs)) Then Begin
    xb0 = boxqs[0] & xb1 =  boxqs[1]
    yb0 = boxqs[2] & yb1 =  boxqs[3]
    zb0 = 0 & zb1 = max([(xb1-xb0), (yb1-yb0)]) < (n_elements(z)-1)
    x = x[xb0:xb1] & y = y[yb0:yb1] & z = z[0:zb1]
    bx = bx[xb0:xb1, yb0:yb1, 0:zb1]
    by = by[xb0:xb1, yb0:yb1, 0:zb1]
    bz = bz[xb0:xb1, yb0:yb1, 0:zb1]
;Reset grids to start at[0,0,0]
    x = x-x[0] & y = y-y[0] & z = z-z[0]
    max_xy = max(abs([x, y]))
    x = x/max_xy & y = y/max_xy & z = z/max_xy
  Endif
; begin calculation
  nx = n_elements(x)
  ny = n_elements(y)
  nz = n_elements(z)
  magb = sqrt(bx^2+by^2+bz^2)
; define grid 
  magfac = 2.*float(nx)/float(ny)
  x2 = magfac*0.5d+0*x
  y2 = magfac*0.5d+0*y
  z2 = magfac*0.5d+0*z
  ;x2=x
  ;y2=y
  ;z2=z
  xmin = min(x2, max = xmax)
  ymin = min(y2, max = ymax)
  zmin = min(z2, max = zmax)
; define grid spacing
  dx = x[1:*]-x
  dy = y[1:*]-y
  dz = z[1:*]-z
; open square window (otherwise box is distorted)
  If(x_yes) Then Begin
    If(keyword_set(window_number)) Then wno = window_number Else wno = 0
    If(Not keyword_set(oplot)) Then window, wno, xsize = 512, ysize = 512
  Endif
; set 3D coordinate system
  scale3, xrange = [0, 1], yrange = [0, 1], zrange = [0, 1], $
    ax = angx, az = angz
; specify color table (rainbow + white)
  loadct, 0
  white = 255
  flcol = 255
; draw image of bz in boundary
  print, 'regridding and drawing image..... '
  xorig = [0, nx-1, 0, nx-1]
  yorig = [0, 0, ny-1, ny-1]
  s = size(bz[*, *, 0])
  xp = (xorig*!x.s[1]+!x.s[0])/(1.*(nx-1))
  yp = (yorig*!y.s[1]+!y.s[0])/(1.*(ny-1))
  p = [[xp], [yp], [fltarr(4)], [replicate(1, 4)]]#!P.T
;
  u = p[*, 0]/p[*, 3]*!d.x_vsize
  v = p[*, 1]/p[*, 3]*!d.y_vsize
  u0 = min(u) & v0 = min(v)
  su = max(u)-u0+1 & sv = max(v)-v0+1
;
  fact = 1                      ;one pixel/output coordinate
  miss = 0                      ;missing is black
  c_color = [150, 200, 250]
;
  miss = 0                      ;Get polynomial coeff for warp 
  if !d.n_colors gt 2 then top = !d.n_colors-1 else top = 255
;
  u2 = (u-u0)/fact
  v2 = (v-v0)/fact
  m = [[u2[0], v2[0], 0, 0, 1, 0], [0, 0, u2[0], v2[0], 0, 1], $
       [u2[1], v2[1], 0, 0, 1, 0], [0, 0, u2[1], v2[1], 0, 1], $
       [u2[2], v2[2], 0, 0, 1, 0], [0, 0, u2[2], v2[2], 0, 1]]
;rhs=[xorig(0),yorig(0),xorig(1),yorig(1),xorig(2),yorig(2)]
  rhs = [xorig[0], yorig[0], xorig[1]+1, yorig[1], xorig[2], yorig[2]+1]
;
  ludc, m, index, /double
  sol = lusol(m, index, rhs, /double)
;
  kx = fltarr(2, 2)
  ky = fltarr(2, 2)
  kx[0, 1] = sol[0]
  kx[1, 0] = sol[1]
  ky[0, 1] = sol[2]
  ky[1, 0] = sol[3]
  kx[0, 0] = sol[4]
  ky[0, 0] = sol[5]
;
  interp = 2
; rebin boundary values on a uniform grid
  image = dblarr(nx, ny)
  xu = indgen(nx)/(1.0*(nx-1))
  yu = indgen(ny)/(1.0*(ny-1))
  If(Not keyword_set(oplot)) Then Begin
;    For i = 0, nx-1 Do For j = 0, ny-1 Do Begin
;      image[i, j] = lin2interp(bz[*, *, 0], x2, y2, xu[i], yu[j])
;    Endfor
    image = interpu_2d(bz[*, *, 0], x2, y2, xu, yu)
    a = poly_2d(bytscl(image, top = top), kx, ky, interp, su/fact, sv/fact, $
                missing = miss) ;Warp it
    tv, a, u0, v0, xsize = su, ysize = sv, /device
; outline box by drawing edges (dashed)
    plots, [xmax, xmax], [ymin, ymax], [zmin, zmin], /t3d, /data, $
      linestyle = 2, color = white, thick = thick_x
    plots, [xmax, xmin], [ymax, ymax], [zmin, zmin], /t3d, /data, $
      linestyle = 2, color = white, thick = thick_x
  Endif
  print, 'drawing fieldlines...'
; field line tracing - loop over starting coords
  loadct, 39
  bzmax = max(abs(reform(bz[*, *, 0])))
  if (bzmax le 0.) then begin
    print, 'bzmax lt 0.'
    return
  endif
  bzb = bytscl(reform(abs(bz[*, *, 0])), min = 0., max = bzmax, top = 254)
  If(Not keyword_set(maxiter)) Then maxiter = 1000
;Do we pick the lines??
  If(keyword_set(pick_lines)) Then Begin
    pick_em = 1b
    If(keyword_set(n_lines)) Then nix = n_lines Else nix = 20
    ix_in = lonarr(nix) & iy_in = ix_in & iz_in=ix_in
    print, 'Pick ', nix, ' Fieldline starting points:'
  Endif Else Begin
    pick_em = 0b
    nix = n_elements(ix_in)
  Endelse
  For rr = 0l, nix-1l Do Begin
    If(pick_em) Then Begin
      cursor, x00, y00
      wait, 0.5
      x0 = (x00-.211)/(.789-.211) ;this should be easier to do...
      y0 = (y00-.211)/(.789-.211)
      ix = value_locate(x2, x0)+1
      iy = value_locate(y2, y0)+1
      iz = 0
      z0 = zmin
      ix_in[rr] = ix & iy_in[rr] = iy & iz_in[rr] = iz
    Endif Else Begin
      ix = ix_in[rr] & iy = iy_in[rr] & iz = iz_in[rr]
      If(ix Ne -1) And (iy Ne -1) And (iz Ne -1) $ 
        And (ix Le nx-1) And (iy Le ny-1) And (iz Le nz-1) Then Begin
        ;x0 = x2[ix] & y0 = y2[iy] & z0 = zmin
        x0 = x2[ix] & y0 = y2[iy] & z0 = z2[iz]
      Endif Else Begin
        x0 = 0 & y0 = 0 & z0 = zmin
        message, /info, 'Bad Coordinates detected'
      Endelse
    Endelse
; determine whether field is up or down
; h is stepsize, by default 1/8 of minimum gridsize
    If(ix Ne -1) And (iy Ne -1) And (iz Ne -1) $ 
        And (ix Le nx-1) And (iy Le ny-1) And (iz Le nz-1) Then Begin
        If(keyword_set(linecolor)) Then flcol = linecolor[rr] $
        Else flcol = bzb[ix, iy]
        If(not keyword_set(nopoints)) Then Begin
            ;print,'ploting coordinate points...'
            if (keyword_set(psym)) then pysm=psym else psym=1
            if (keyword_set(symsize)) then symsize=symsize else symsize=2
            plots, x0, y0, z0, $
            /t3d, /data, thick = thick_x, color = flcol,psym=psym,syms=symsize
        endif
            ;   plots, [x0,x0], [y0,y0], [zmin,z0], $
            ;         /t3d, /data, thick = thick_x, color = 250,psym=2,syms=2
        ;sign = float(0)
        ;bznearby = lin3interp(bz, x2, y2, z2, x0, y0, z0)
        ;                          ; always integrate into the box
        ;if (bznearby gt 0.0) then sign = 1.0 else sign = -1.0 
                                   ; integrate until field line leaves box 
        ;print, bz[ix, iy, 0], flcol
	print,'bmag, bz, iz:'
        print,bmag[ix,iy,iz],bz[ix,iy,iz],iz
        ;print,bz[ix,iy,iz],flcol
      if(not keyword_set(nolines)) Then Begin
        signs=[-1.0,1.0] ;both directions along the field line
        for i=0,1 do begin
          sign=signs[i]  
          h = min(dx)/8.0 & s = 0.0 & coords = [x0, y0, z0]
          count = 0l
          while (coords[0] ge xmin) and (coords[0] le xmax) and $
            (coords[1] ge ymin) and (coords[1] le ymax) and $
            (coords[2] ge zmin) and (coords[2] le zmax) and $
            count lt maxiter do begin
            ; RHS of ODEs determined by linear
            ; interpolation
            count = count+1l
            dydx = differential2(s, coords)
            result = rk4(coords, dydx, s, h, 'differential2')
                                ; plot increment in field line
            plots, [coords[0], result[0]], [coords[1], result[1]], $
              [coords[2], result[2]], /t3d, /data, thick = thick_x, color = flcol
              coords = result
           endwhile
         endfor
       endif
    Endif
  Endfor
  loadct, 0
  If(keyword_set(title)) Then ttl = title Else ttl = '!3B!dz!n(0)'
  xyouts, 0.07, 0.89, ttl, charsize = 2.0
; draw axes
  thick = thick_x
  plots, [xmin, xmin], [ymin, ymax], [zmin, zmin], /t3d, /data, $
    color = white, thick = thick
  plots, [xmin, xmin], [ymin, ymin], [zmin, zmax/2], /t3d, /data, $
    color = white, thick = thick
;
; label axes
;
  charthick = 1.75
  xyouts, xmax+.05, ymin, z = zmin, 'X', /t3d, /data, size = 4, $
    color = white, charthick=charthick
  xyouts, xmin, ymax+.05, z = zmin, 'Y', /t3d, /data, size = 4, $
    color = white, charthick=charthick
  xyouts, xmin, ymin, z = zmax/2+0.05, 'Z', /t3d, /data, size = 4, $
    color = white, charthick=charthick

  Return
End
