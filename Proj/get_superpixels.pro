;=======================================
function  get_superpixels
  return, 0 ;just a caller
end

;=======================================
function  get_context_K, img, att, k
  t0 = systime(/seconds)

  na = n_elements(att)
  dims = size(img,/dimension)
  superpixels = ptrarr(dims[1], dims[2])

  _img = congrid(img, dims[0], dims[1]+k, dims[2]+k)
  ;_img[*,k/2:dims[1]-(k/2)+1,k/2:dims[2]-(k/2)+1] = img[*,*,*]
  _img[*,k/2:dims[1]-1+(k/2) , k/2:dims[2]-1+(k/2)] = img[*,*,*]

  ii=0L
  for i = (k/2), dims[1]-1-(k/2) do begin
    jj=0
    for j = (k/2), dims[2]-1-(k/2) do begin
        
      ;neigh = img[att , i:(i+k-1) , j:(j+k-1)]
      neigh = _img[att , (i-(k/2)):(i+(k/2)) , (j-(k/2)):(j+(k/2))]

      superpixels[ii,jj] = ptr_new( reform(neigh,na,k*k) )
      jj++
    endfor
    ii++
  endfor

  Print, 'Superpixel bulding run-time (seconds): ', systime(/seconds)-t0
  return, superpixels
end
