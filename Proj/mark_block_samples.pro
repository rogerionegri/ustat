function mark_block_samples, img, att, ptrROIs, k

  dims = size(img,/dimensions)
  ptrBlocks = ptrarr( n_elements(ptrROIs) )
  ptrBlocksImg = ptrarr( n_elements(ptrROIs) )
  sampleN = intarr( n_elements(ptrROIs) )
  
  for r = 0, n_elements(ptrROIs)-1 do begin
    mask = intarr(dims[1],dims[2])
    blocks = intarr(dims[1],dims[2])
    
    roi = *ptrROIs[r]
    mask[roi.roilex] = -1 ;a dummy value pointing for 'availability'
    
    ;Identify blocks kxk inside the current sampels
    index = 1
    for i = 0, dims[1]-1 do begin
      for j = 0, dims[2]-1 do begin
        
        if ((i-k/2) ge 0) AND ((i+k/2) LT dims[1]) AND ((j-k/2) ge 0) AND ((j+k/2) LT dims[2]) then begin
          neigh = mask[ (i-k/2):(i+k/2) , (j-k/2):(j+k/2) ]
          ;if min(neigh) eq -1 then begin
          if max(neigh) eq -1 then begin
            blocks[ (i-k/2):(i+k/2) , (j-k/2):(j+k/2) ] = index
            mask[ (i-k/2):(i+k/2) , (j-k/2):(j+k/2) ] = index
            index++
          endif
        endif
        
      endfor
    endfor
    
    indBlocks = blocks[UNIQ(blocks, SORT(blocks))]
    if max(indBlocks) ne 0 then begin ;exists at least one valid block...
      listBlocks = ptrarr(max(indBlocks))
      listBlocksImg = ptrarr(max(indBlocks))
      
      for b = 1L, max(indBlocks) do begin
        pos = where(blocks eq b)
        
        V = make_array(n_elements(att), n_elements(pos), type = size(img,/type))
        for a = 0, n_elements(att)-1 do begin
          temp = reform(img[att[a],*,*],dims[1],dims[2])
          V[a , *] = temp[pos]
        endfor
        
        listBlocks[b-1] = ptr_new( pos )
        listBlocksImg[b-1] = ptr_new( V )
      endfor
      
      sampleN[r] = n_elements(listBlocks) 
      ptrBlocks[r] = ptr_new( listBlocks ) ;a list of pointers that represent lists of position inside blocks
      ptrBlocksImg[r] = ptr_new( listBlocksImg )
      
    endif else begin
      ptrBlocks[r] = ptr_new( ptr_new() )
      ptrBlocksImg[r] = ptr_new( ptr_new() )
      sampleN[r] = 0
    endelse
    
    
  endfor

  return, {N: sampleN, ptrBlocks: ptrBlocks, ptrBlocksImg: ptrBlocksImg} 
end


;-----------------------------
function limit_block_samples, __structBlocks, limit, seed

  structBlocks = __structBlocks ;safe copy
  for r = 0, n_elements(structBlocks.N)-1 do begin
    N = structBlocks.N[r]
    
    if N gt limit then begin
      ;randomIndex = long( randomu(seed, limit,  /UNIFORM) * 10^fix(alog10(N)+1) ) mod N
      randomIndex = sort( randomu(seed, N,  /UNIFORM) )
      
      tempBlock = *structBlocks.ptrBlocks[r]
      tempBlockImg = *structBlocks.ptrBlocksImg[r]

      structBlocks.N[r] = limit
      structBlocks.ptrBlocks[r] = ptr_new( tempBlock[ randomIndex[0:limit-1] ] )
      structBlocks.ptrBlocksImg[r] = ptr_new( tempBlockImg[ randomIndex[0:limit-1] ] )  
    endif
    
  endfor

  return, structBlocks
end