; $Id$
;+
; NAME:
; WRITE_VTK
;
; PURPOSE:
; This procedure converts the shasta_ori_13 Code data into legacy VTK
; format.
;
;
; ARGUMENTS:
; File: File name including the full path (if any) of the output
; VTK file. If this argument is omitted, the file name
; 'work.vtk' is used.
;
; KEYWORDS:
; X: A 1D array containing the x-coordinates of the grid.
; Y: A 1D array containing the y-coordinates of the grid.
; Z: A 1D array containing the z-coordinates of the grid.
; SPACING: Three-element array containing the spacing in
; each dimension.
; GRID: Set this keyword for writing non-uniform grid.
;
; MODIFICATION HISTORY:
; Written by: Chengcai Shen.
;-

pro write_vtk, file, x=x, y=y, z=z, spacing=spacing, grid=grid
  compile_opt idl2
  common com_vtk2d, x1,x2,rho2d,vx2d,vz2d,bx2d,bz2d,a2d,u2d
  
  ; Determine the coordinates and the precision.
  rectilinear_grid = 1
  data_type = strlowcase(size(x, /tname))

  ; Find the dimensions of the data.
  nx = n_elements(x)
  ny = n_elements(y)
  nz = n_elements(z)
  ntot = nx * ny * nz
  dimensions = [nx, ny, nz]
  origin = [x[0], y[0], z[0]]
  ndim = n_elements(where(dimensions gt 1))

  ; Open the VTK file for write.
  print, 'Writing ', strtrim(file), '...'
  openw, lun, file, /get_lun, /swap_if_little_endian

  ; Write the header information.
  printf, lun, '# vtk DataFile Version 2.0'
  printf, lun, 'Shasta Code Data'
  printf, lun, 'BINARY'
  if rectilinear_grid then begin
    printf, lun, 'DATASET RECTILINEAR_GRID'
    printf, lun, 'DIMENSIONS ', dimensions
    printf, lun, 'X_COORDINATES ', nx, ' ', data_type
    writeu, lun, x
    printf, lun, 'Y_COORDINATES ', ny, ' ', data_type
    writeu, lun, y
    printf, lun, 'Z_COORDINATES ', nz, ' ', data_type
    writeu, lun, z
  endif else begin
    printf, lun, 'DATASET STRUCTURED_POINTS'
    printf, lun, 'DIMENSIONS ', dimensions
    printf, lun, 'ORIGIN ', origin
    printf, lun, 'SPACING ', spacing
  endelse
  printf, lun, 'POINT_DATA ', ntot

  ; Write out each data field.

  ; Write scalar field.
  ; magnetic
  printf, lun, 'SCALARS ', 'vector_A ', data_type
  printf, lun, 'LOOKUP_TABLE default'
  writeu, lun, swap_endian(a2d, /swap_if_big_endian)
  ; density
  printf, lun, 'SCALARS ', 'density ', data_type
  printf, lun, 'LOOKUP_TABLE default'
  writeu, lun, swap_endian(rho2d, /swap_if_big_endian)
  ; pressure
  printf, lun, 'SCALARS ', 'pressure ', data_type
  printf, lun, 'LOOKUP_TABLE default'
  writeu, lun, swap_endian(u2d, /swap_if_big_endian)

  ; Write vector field.
  ; velocity
  vv = dblarr(3,ntot)
  for j = 0,ny-1 do begin
    for i =0,nx-1 do begin
      k = i+j*nx
      vv[0,k]=vx2d[i,j]
      vv[1,k]=vz2d[i,j]
      vv[2,k]=0.0
    endfor
  endfor
  printf, lun, 'VECTORS ', 'velocity ', data_type
  writeu, lun, swap_endian(vv, /swap_if_big_endian)
  ; B
  bb = dblarr(3,ntot)
  for j = 0,ny-1 do begin
    for i =0,nx-1 do begin
      k = i+j*nx
      bb[0,k]=bx2d[i,j]
      bb[1,k]=bz2d[i,j]
      bb[2,k]=0.0
    endfor
  endfor
  printf, lun, 'VECTORS ', 'magnetic_B ', data_type
  writeu, lun, swap_endian(bb, /swap_if_big_endian)
  close, lun
  free_lun, lun
end