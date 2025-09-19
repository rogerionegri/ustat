;@RoiFunctions.pro
@ascii_read_roi.pro

PRO IMG2ASCII_SKLEARN
  
  path_img = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/EXP_DATASET/PALSAR_MATCOV_2009__x10-8__TIFF.tif'
  path_out_img = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/EXP_DATASET/PALSAR_MATCOV_2009__x10-8__TIFF_finite-normalized-global_ASCII.txt'

  path_roi_treino = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/EXP_DATASET/amostrasTreino7Classes.txt'
  path_roi_teste = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/EXP_DATASET/amostrasTeste7Classes.txt'

  path_out_set_treino = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/conv_ascii/amostrasTreino7Classes_ASCII_finite-normalized-global.txt'
  path_out_set_teste = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/conv_ascii/amostrasTeste7Classes_ASCII_finite-normalized-global.txt'

  img = read_tiff(path_img,channels=[0,3,5])
  dims = size(img,/dimensions)

  for i = 0, dims[0]-1 do begin
    temp = reform(img[i,*,*], dims[1], dims[2])
    pos = where(finite(temp) ne 1)
    temp[pos] = 0 
    ;a = 1.0/(max(temp)-min(temp))
    ;b = -a*min(temp)
    ;img[i,*,*] = a*temp[*,*] + b
    img[i,*,*] = temp[*,*]
  endfor
  
  ;remover nao finitos + normalizar
  a = 1.0/(max(img)-min(img))
  b = -a*min(img)
  for i = 0, dims[0]-1 do begin
    ;temp = reform(img[i,*,*], dims[1], dims[2])
    ;pos = where(finite(temp) ne 1)
    ;temp[pos] = 0
    ;a = 1.0/(max(temp)-min(temp))
    ;b = -a*min(temp)
    img[i,*,*] = a*img[i,*,*] + b
  endfor
  
  
  ;Conversão da imagem para ASCII
  OpenW, Arq, path_out_img, /get_lun
  for i = 0, dims[1]-1 do begin
    for j = 0, dims[2]-1 do begin
      line = '-1'
      for k = 0, dims[0]-1 do line += ',' + strtrim(string( float(img[k,i,j]) ),1)
      PrintF, Arq, line
    endfor
  endfor
  Close, Arq
  Free_lun, Arq
	
	
  ;Conversão das amostras para ASCII - Treino
  ptrROIs = ascii_read_roi(path_roi_treino)
  OpenW, Arq, path_out_set_treino, /get_lun
  for r = 0, n_elements(PtrROIs)-1 do begin
     roi = *PtrROIs[r]
     for ind = 0, n_elements(roi.roilex)-1 do begin
        line = strtrim(string(r),1)
        
        pos = roi.roilex[ind]
        col = pos mod dims[1]
        lin = pos / dims[1]
        
        for k = 0, dims[0]-1 do line += ',' + strtrim(string(float(img[k,col,lin])),1)
        PrintF, Arq, line
     endfor
  endfor
  Close, Arq
  Free_lun, Arq
  
  
  ;Conversão das amostras para ASCII - Teste
  ptrROIs = ascii_read_roi(path_roi_teste)
  OpenW, Arq, path_out_set_teste, /get_lun
  for r = 0, n_elements(PtrROIs)-1 do begin
     roi = *PtrROIs[r]
     for ind = 0, n_elements(roi.roilex)-1 do begin
        line = strtrim(string(r),1)
        
        pos = roi.roilex[ind]
        col = pos mod dims[1]
        lin = pos / dims[1]
        
        for k = 0, dims[0]-1 do line += ',' + strtrim(string(float(img[k,col,lin])),1)
        PrintF, Arq, line
     endfor
  endfor
  Close, Arq
  Free_lun, Arq
  
  
  Print, 'End...'
END
