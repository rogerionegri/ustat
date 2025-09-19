function remove_nonfinites, img
  dims = size(img,/dimensions)
  for b = 0, dims[0]-1 do begin
    temp = reform(img[b,*,*], dims[1],dims[2])
    pos = where(finite(temp) ne 1)
    if pos[0] ne -1 then temp[pos] = 0
    img[b,*,*] = temp[*,*]
  endfor

  return, img
end