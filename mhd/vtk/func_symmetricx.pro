FUNCTION func_symmetricx,nx,arrin
  arrout = DBLARR(2*nx-3)
  FOR n = 0,nx-1 DO BEGIN
        ni = nx-1-n
        arrout(n)   = -arrin(ni)
  ENDFOR
  FOR n = 2,nx-1 DO BEGIN
        ni = n+nx-1-2
        arrout(ni)  = arrin(n)
  ENDFOR
  RETURN, arrout
END