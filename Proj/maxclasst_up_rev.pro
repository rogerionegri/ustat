function maxClassT_up_rev, superpixels, structBlocks, kernelType, kernelPar


  ;'T base' before including/testing new elements
  matT_base = compute_T(structBlocks, kernelType, kernelPar)
  Tobs_base = total( matT_base[where(matT_base ne -1)] )
  ;----------------------------------------------------------------

  ;Superblock structure/format (to be substituted by a segmentation)
  dims = size(superpixels,/dimensions)
  N = total(structBlocks.N)
  C = n_elements(structBlocks.N)
  ;----------------------------------------------------------------

  ;Support variables for the classification process
  vecClass = dblarr(C)
  mapClass = intarr(dims)
  ;----------------------------------------------------------------


  ;Main process (classification)-----------------------------------
  for i = 0, dims[0]-1 do begin
    for j = 0, dims[1]-1 do begin

      vecClass[*] *= 0
      
      if ptr_valid(superpixels[i,j]) then object = *superpixels[i,j] else object = *superpixels[0,0]

      ;Computing the distance between 'object' and the 'groups'
      vecDistGroups = dblarr(C)
      for k = 0, C-1 do begin
        blocksGk = *structBlocks.ptrBlocksImg[k]
        for kin = 0, structBlocks.N[k]-1 do begin
          Xj = *blocksGk[kin]
          val = kernel(object, Xj, kernelType, kernelPar)
          vecDistGroups[k] += val
        endfor
      endfor

      ;Testing the inclusion of 'object' into each group
      for k = 0, C-1 do begin

        ng = structBlocks.N[k]
        Tin = 0.0D
        Tout = 0.0D

        for kth = 0, C-1 do begin
          if kth eq  k then Tin += vecDistGroups[kth] $
          else Tout += vecDistGroups[kth]
        endfor

        vecClass[k] = Tobs_base + 2*Tout - (N-(ng+1))/((ng+1)-1.0)*Tin
      endfor

      ;Argmax decision
      pos = where(vecClass eq max(vecClass))
      if n_elements(pos) gt 0 then begin
        print, 'draw...'
      endif
      mapClass[i,j] = pos[0]

      print, '(i,j) --> ', i,j , ' of ', dims

    end
  end

  return, mapClass
end
