FUNCTION func_anti_symmetricarr,nx,nz,arrin
  arrout = DBLARR(2*nx-3,nz)
  FOR m = 0,nz-1-1 DO BEGIN
    FOR n = 0,nx-1 DO BEGIN
        ni = nx-1-n
        arrout(n,m)     = -arrin(ni,m)
    ENDFOR
    FOR n = 2,nx-1 DO BEGIN
        ni = n+nx-1-2
        arrout(ni,m)    = arrin(n,m)
    ENDFOR
  ENDFOR
  RETURN, arrout
END