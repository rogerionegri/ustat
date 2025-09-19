@RoiFunctions.pro

PRO ASCII2IMAGE_SKLEARN
 
  vecNames = ['AB','MLC','KNN','MLP','CART','RF','SVM-Lin','SVM-RBF']
  ;vecNames = ['MLC_2']
 
  path_base = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/EXP_DATASET/PALSAR_MATCOV_2009__x10-8__TIFF.tif'
  ;path_ascii = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/conv_ascii/output_29nov24/SVM-RBF.csv'
  ;path_out = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/conv_ascii/output_29nov24/SVM-RBF.tif'
  path_res = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/conv_ascii/output_finitenormalized/'
  
  Result = QUERY_TIFF(path_base, Info, GEOTIFF=geoVar)
  dimRef = info.DIMENSIONS
  mapIndex = intarr(dimRef[0], dimRef[1])
  
  for k = 0, n_elements(vecNames)-1 do begin
  
    path_ascii = path_res+vecNames[k]+'.csv'
    path_out = path_res+vecNames[k]+'.tif'
  
    OpenR, Arq, path_ascii, /get_lun
    line = ''
    ;while ~eof(Arq) do begin
    for i = 0, dimRef[0]-1 do begin
      for j = 0, dimRef[1]-1 do begin
        ReadF, Arq, line
        mapIndex[i,j] = long(line)
      endfor
    endfor
    ;endwhile
    Close, Arq

    ;Salvando os resultados----------------------------------------
    write_tiff, path_out, geotiff=geoVar, mapIndex  
    
  endfor
  
  
  
  
  Print, 'End...'
END
