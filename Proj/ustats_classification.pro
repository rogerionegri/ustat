  @ascii_read_roi.pro
  @mark_block_samples.pro
  @compute_t.pro
  @get_superpixels.pro
  @maxclasst_up_rev.pro
  @MeasureFunctions.pro
  @ClassificationFunctions.pro


PRO USTATS_CLASSIFICATION

  ;-------------------------------------------
  path_img = '...' ;input image
  path_roi = '...' ;training sampels
  path_test = '...' ;test sampels
  path_out = '.../' ;output dir

  att = [0,3,5]    ;the intensities along the diagonal of the cov. matrix
  limit = 50
  kernelType = 1  
  seed = 0L
  prefix = 'phi1'
  k = 9                   ;block size
  kernelPar = 1.1  ;kernel parameter

  ;-------------------------------------------
  filename = path_out + 'claUst_k='+strtrim(string(long(k)),1)+'_p='+strtrim(string(kernelPar),1)+'.tif'
  
  head = 'k;par;OverAccuracy;Tau;VarTau;Kappa;VarKappa;Time'
  OpenW,Arq,path_out+'report.txt',/get_lun,/append
  PrintF,Arq,head & Close,Arq & Free_lun,Arq
  ;-------------------------------------------
  

  ;Read data----------------------------------
  img = read_tiff(path_img)
  Result = QUERY_TIFF(path_img, Info, GEOTIFF=geoVar)

  dims = size(img,/dimensions)
  ptrROIs = ascii_read_roi(path_roi)
  testROIs = ascii_read_roi(path_test)

  ;Remove 'non-finites' from the input image
  img = remove_nonfinites(img)

  t0 = systime(/seconds)

  ;Superpixel extraction----------------------
  superpixels = get_context_K(img, att, k)

  t1 = systime(/seconds)
  print, 'superpixel time: ', t1-t0

  ;Extract samples----------------------------
  structBlocks = mark_block_samples(img,att,ptrROIs,k)
  ;Limit sample size--------------------------
  structBlocks = limit_block_samples(structBlocks,limit,seed)

  ;Classification module----------------------
  res = maxClassT_up_rev(superpixels, structBlocks, kernelType, kernelPar)

  t1 = systime(/seconds)

  claImg = COLORIZE_INDEX(res, ptrROIs)

  ;Assessment
  matconf = CONFUSION_MATRIX( congrid(res, dims[1], dims[2]) , testROIs)
  measures = CONCORDANCE_MEASURES(matconf)

  reportLine = strtrim(string(long(k)),1)+';'+strtrim(string(kernelPar),1)+';'+$
  strtrim(string(measures[0]),1)+';'+strtrim(string(measures[1]),1)+';'+$
  strtrim(string(measures[2]),1)+';'+strtrim(string(measures[3]),1)+';'+$
  strtrim(string(measures[4]),1)+';'+strtrim(string(t1-t0),1)

  OpenW,Arq,path_out+'report.txt',/get_lun,/append
  PrintF,Arq,reportLine & Close,Arq & Free_lun,Arq
  write_tiff, filename, geotiff=geoVar, claImg

  print, '...end of process'
END