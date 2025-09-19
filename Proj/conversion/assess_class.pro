@ClassificationFunctions.pro
@ascii_read_roi.pro
@MeasureFunctions.pro

PRO ASSESS_CLASS
 
 paths = ['/home/rogerio/Colabs/colab.Aluisio/U-Stat/resClass/mlc_envi__.tif', $
 '/home/rogerio/Colabs/colab.Aluisio/U-Stat/resClass/svm_envi__.tif', $
 '/home/rogerio/Colabs/colab.Aluisio/U-Stat/resClass/mahala_envi__.tif', $
 '/home/rogerio/Colabs/colab.Aluisio/U-Stat/resClass/mindist_envi__.tif', $
 '/home/rogerio/Colabs/colab.Aluisio/U-Stat/resClass/nn_l4_log-09-01-05-01__.tif'] 

  path_roi_treino = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/EXP_DATASET/amostrasTreino7Classes.txt'
  path_roi_teste = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/EXP_DATASET/amostrasTeste7Classes.txt'


  ptrRoi = ascii_read_roi(path_roi_treino)
  ptrTest = ascii_read_roi(path_roi_teste)


  for i = 0, n_elements(paths)-1 do begin
    img = read_tiff(paths[i])
    img = (img > 1)-1
    ;index = CLASSIF_INDEX(img,ptrRoi)
    mat = CONFUSION_MATRIX(img,ptrTest)
    meas = CONCORDANCE_MEASURES(mat)
    print, paths[i], meas
  endfor

END