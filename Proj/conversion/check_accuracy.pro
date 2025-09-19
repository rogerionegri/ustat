@ascii_read_roi.pro

PRO check_accuracy
  
  path_class = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/conv_ascii/output_29nov24/MLC.tif'
  path_test = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/EXP_DATASET/amostrasTeste7Classes.txt'
  
  img = read_tiff(path_class)
  ptr_test = ascii_read_roi(path_test)
  
  stop


END