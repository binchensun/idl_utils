function ngauss, x, p
    ni=n_elements(p)/3
    if n_elements(p) mod 3 ne 1 then begin
        print,'number of coefficients must be 3*n+1'
    endif else begin
        ni=n_elements(p)/3
    endelse
    y=fltarr(n_elements(x))
    for i=0,ni-1 do y=y+gauss1(x,p[i*3+1:i*3+3],/peak)
    return,y+p[0]
end
