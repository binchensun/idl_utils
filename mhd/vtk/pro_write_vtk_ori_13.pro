pro pro_write_vtk_ori_13
  ;  Purpose:
  ;      read the output file form 'pro_pectral_anals.pro'
  ;      and plot. Also plot the gamma with time.
  ;  Input:
  ;      path, which includes all SAV files.
  ;  Output:
  ;      PS format figure.
  ;  Update:
  ;      2012-04-04
  ;      2012-05-07, add psfile name.
  ;      2015-01-29, plot vz
  ;----------------------------------------------------------------------
  ; (0) define envionments and path
  ;----------------------------------------------------------------------
  hightime = 1
  if(!version.os_family eq 'unix')then begin
    path='/media/data/bchen/work/MHD/20120303/run_20150203/run_shasta_20150203h/'
  endif else begin
    path='C:\Users\ccai\Works\calculations\SHASTA\SHASTA_ORI_13\SHASTA_ORI_13\Release\'
  endelse

  ;----------------------------------------------------------------------
  ; (1) start main part
  ;----------------------------------------------------------------------
  ; common blocks
  common com_var,nx,nz,x,z,time,dt,rho,rhovx,rhovz,bx,bz,a,u
  common com_vtk2d, x1,x2,rho2d,vx2d,vz2d,bx2d,bz2d,a2d,u2d

  ; define file name
  filestr = FILE_SEARCH(path,'sb*.dat')
  nfile = N_ELEMENTS(filestr)

  ; time range
  time_sta = 29.0
  time_end = 49.0

  print,'Print1, file number:',nfile

  ; enter file cycle
  for ip = 0,nfile-1 do begin
    fname = filestr(ip)
    print,'ifile = ', ip

    ; read all variables
    readdat = func_shasta_ori_rd(fname)

    ;
    if((time ge time_sta) and (time le time_end))then begin
      ; data (vz)
      x1 = func_symmetricx(nx,x)
      x2 = z
      ; magnetic
      a2d  = func_symmetricarr(nx,nz,a)
      bx2d = func_symmetricarr(nx,nz,bx)
      bz2d = func_anti_symmetricarr(nx,nz,bz)
      ; density
      rho2d = func_symmetricarr(nx,nz,rho)
      ; velocity
      vx = rhovx/rho
      vx2d = func_anti_symmetricarr(nx,nz,vx)
      vz = rhovz/rho
      vz2d = func_symmetricarr(nx,nz,vz)
      u2d = func_symmetricarr(nx,nz,u)

      ; call write vtk
      strtime = string(time,'(f07.3)')
      vtkfilename = strmid(strtime,0,3)+strmid(strtime,4,3)
      call_procedure,'WRITE_VTK', path+vtkfilename+'.vtk', x=x1, y=x2, z=0.0d0
    endif ; end time range
  endfor
  ;----------------------------------------------------------------------
  ; (2) normal stop
  ;----------------------------------------------------------------------
  print,'normal stop'
end
