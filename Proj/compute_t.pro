function compute_T, structBlocks, kernelType, kernelPar

  dims = size(img,/dimensions)
  N = total(structBlocks.N)
  matT = dblarr(n_elements(structBlocks.N) , n_elements(structBlocks.N)) -1 ;store pairwise comparisions [-1 acts as dummy value]
  
  for gi = 0, n_elements(structBlocks.N)-1 do begin    ;pairwise comparision
    for gj = gi, n_elements(structBlocks.N)-1 do begin ;repetitions allowed!
      
      T = 0D
      
      ni = structBlocks.N[gi]
      nj = structBlocks.N[gj]
      if (gi ne gj) then eta = 1 else eta = -(N-ni)/(ni-1) ;TODO: Correct? 
      
      if (total(ptr_valid(*structBlocks.ptrBlocksImg[gi])) eq n_elements(*structBlocks.ptrBlocksImg[gi])) AND (total(ptr_valid(*structBlocks.ptrBlocksImg[gj])) eq n_elements(*structBlocks.ptrBlocksImg[gj])) then begin
      
      
        Zi = *structBlocks.ptrBlocksImg[gi]
        Zj = *structBlocks.ptrBlocksImg[gj]
      
      ;if (Zi ne !NULL) AND (Zj ne !NULL) then begin
      ;if (total(ptr_valid(Zi)) eq n_elements(Zi)) AND (total(ptr_valid(Zj)) eq n_elements(Zj)) then begin
        
        
        for bi = 0, n_elements(Zi)-1 do begin
          Vi = *Zi[bi]
          
          for bj = 0, n_elements(Zj)-1 do begin
            Vj = *Zj[bj]
            
            val = kernel(Vi, Vj, kernelType, kernelPar)
            T += (eta*val)
            
            ;print, '(gi,gj | ni,nj --> bi,bj)', gi,gj,ni,nj,bi,bj
          endfor
        endfor
        
        matT[gi,gj] = T
        matT[gj,gi] = T   ;simmetry... 
      endif
      
      ;print, '(gi,gj) --> ', gi,gj , ' of ', n_elements(structBlocks.N)
      
    endfor
  endfor
  
  return, matT
end



;-------------------------------
;Geral
function kernel, X, Y, kernelType, kernelPar
  case kernelType of
    1: val = kernel_dif(X, Y, kernelPar)
    2: val = kernel_I(X, Y, kernelPar)
    3: val = kernel_I__(X, Y, kernelPar)
  endcase
  return, val
end 


;-------------------------------
;Phi1
function kernel_dif, X, Y, kernelPar
  ;val = total( abs( X-Y )^kernelPar )
  val = mean( abs( X-Y )^kernelPar )
  ;val = total( norm( X-Y )^kernelPar )
  return, val
end

;-------------------------------
;Phi2
function kernel_I, X, Y, kernelPar
  ;implemenbtar...
  ;val = mean( abs( X-Y )^kernelPar )
  val = max( abs( X-Y )) gt kernelPar ? 1:0
  return, val
end


;-------------------------------
;Phi2
function kernel_I__, X, Y, kernelPar
  ;implemenbtar...
  ;val = mean( abs( X-Y )^kernelPar )
  val = max( abs( X-Y )) ;gt kernelPar ? 1:0
  return, val
end






function compute_T_V0, img, att, blocks, kernelType, kernelPar

  dims = size(img,/dimensions)
  N = total(blocks.N)
  matT = dblarr(n_elements(blocks.N) , n_elements(blocks.N)) -1 ;store pairwise comparisions [-1 acts as dummy value]

  for gi = 0, n_elements(blocks.N)-1 do begin    ;pairwise comparision
    for gj = gi, n_elements(blocks.N)-1 do begin ;repetitions allowed!

      T = 0D

      ni = blocks.N[gi]
      nj = blocks.N[gj]
      if (gi ne gj) then eta = 1 else eta = -(N-ni)/(ni-1) ;TODO: Correct?

      Zi = *blocks.ptrBlocks[gi]
      Zj = *blocks.ptrBlocks[gj]

      if (Zi ne !NULL) AND (Zj ne !NULL) then begin

        for bi = 0, n_elements(Zi)-1 do begin

          ;Pack I
          Xi = *Zi[bi]
          Vi = make_array(n_elements(att), n_elements(Xi), type = size(img,/type))
          for a = 0, n_elements(att)-1 do begin
            temp = reform(img[att[a],*,*],dims[1],dims[2])
            Vi[a , *] = temp[Xi]
          endfor

          for bj = 0, n_elements(Zj)-1 do begin

            ;Pack J
            Xj = *Zj[bj]
            Vj = make_array(n_elements(att), n_elements(Xj), type = size(img,/type))
            for a = 0, n_elements(att)-1 do begin
              temp = reform(img[att[a],*,*],dims[1],dims[2])
              Vj[a , *] = temp[Xj]
            endfor

            val = kernel(Vi, Vj, kernelType, kernelPar)
            T += (eta*val)

            print, '(gi,gj | ni,nj --> bi,bj)', gi,gj,ni,nj,bi,bj
          endfor
        endfor

        matT[gi,gj] = T
        matT[gj,gi] = T   ;simmetry...
      endif

    endfor
  endfor

  return, matT
end




;------------------------------------------
function compute_T_pack, structBlocks, kernelType, kernelPar

  dims = size(img,/dimensions)
  N = total(structBlocks.N)
  
  matT = ptrarr(n_elements(structBlocks.N) , n_elements(structBlocks.N))

  for gi = 0, n_elements(structBlocks.N)-1 do begin    ;pairwise comparision
    for gj = gi, n_elements(structBlocks.N)-1 do begin ;repetitions allowed!

      T = [0D]

      ni = structBlocks.N[gi]
      nj = structBlocks.N[gj]
      
      ;if (gi ne gj) then eta = 1 else eta = -(N-ni)/(ni-1) ;TODO: Correct?

      Zi = *structBlocks.ptrBlocksImg[gi]
      Zj = *structBlocks.ptrBlocksImg[gj]

      if (Zi ne !NULL) AND (Zj ne !NULL) then begin

        for bi = 0, n_elements(Zi)-1 do begin
          Vi = *Zi[bi]

          for bj = 0, n_elements(Zj)-1 do begin
            Vj = *Zj[bj]

            val = kernel(Vi, Vj, kernelType, kernelPar)
            T = [T , val]     ;T += (eta*val)
          endfor

        endfor

        matT[gi,gj] = ptr_new(T[1:-1])
      endif

    endfor
  endfor

  return, matT
end


