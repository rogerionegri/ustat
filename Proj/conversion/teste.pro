pro teste

path = '/home/rogerio/Colabs/colab.Aluisio/PolSAR/conv_ascii/amostrasTreino7Classes_ASCII_finite-normalized.txt'

OpenR, Arq, path, /get_lun

line=''
data = [0.0,0.0,0.0,0.0]
while ~eof(Arq) do begin
   ReadF, Arq, line
  data = [[data] , [float(strsplit(line,',',/extract))] ]
endwhile

  data = data[*,1:-1]
  stop

end