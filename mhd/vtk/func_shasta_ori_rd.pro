;========================================================================
;    Name: func_shasta_ori_rd
;    Purpose: read the data
;    Input: filename, including full path & filename
;    Output: 
;           Type(1) nx,nz,x,z,time,rho,rhovx,rhovz,bx,bz,a,p
FUNCTION func_shasta_ori_rd,datfile
  common com_var,nx,nz,x,z,time,dt,rho,rhovx,rhovz,bx,bz,a,u
  nx = 1
  nz = 1
  OPENR,1,datfile,/f77_unFORmatted
  READU,1,nx
  READU,1,nz
  x    = DBLARR(nx)
  z    = DBLARR(nz)
  time = DBLARR(1)
  dt   = DBLARR(1)
  rho  = DBLARR(nx,nz)
  rhovx =DBLARR(nx,nz)
  rhovz =DBLARR(nx,nz)
  bx =   DBLARR(nx,nz)
  bz =   DBLARR(nx,nz)
  u =    DBLARR(nx,nz)
  a =    DBLARR(nx,nz)
  READU,1,x
  READU,1,z
  READU,1,rho
  READU,1,rhovx
  READU,1,rhovz
  READU,1,bx
  READU,1,bz
  READU,1,u
  READU,1,time,dt
  CLOSE,1
;   get a new A from BX and BZ
  a(nx-1,nz-1) = 0.0
  FOR mz = 1,nz-1 DO BEGIN
      kz = nz-1-mz
      a(nx-1,kz) = a(nx-1,kz+1) + 0.5*(bx(nx-1,kz)+bx(nx-1,kz+1))*(z(kz)-z(kz+1))
  ENDFOR
  FOR mz = 0,nz-1 DO BEGIN
      kz = nz-1-mz
      FOR mx = 0,nx-1-1 DO BEGIN
        kx = nx-1-mx
        a(kx-1,kz) = a(kx,kz) - 0.5*(bz(kx,kz)+bz(kx-1,kz))*(x(kx-1)-x(kx))
      ENDFOR
  ENDFOR
END