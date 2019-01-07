# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                   #
#             Job: Novo modelo CredLeader 2018                      #
#                  análise variáveis                                #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Carregando pacotes
  library(dplyr)
  library(stringr)
  library(haven)
  library(dplyr)
  library(cluster)
  library(fpc)
  library(NbClust)
  library(readxl)
  # library(psych)  
  # library(mclust)
  library(ggplot2)

# Pasta de trabalho
  wd_base<-'C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Modelo Ago18 v01 - base toda/Base original'
  setwd(wd_base)

# Lendo a base:
  base_ago18_variaveis<-as.data.frame(read_spss('Elegíveis 201807 normais sem Seller sem BLACKLIST.sav'))
  head(base_ago18_variaveis)
  nrow(base_ago18_variaveis) # 392782
  summary(base_ago18_variaveis)

# Quantidades:
  data.frame(cbind(count(base_ago18_variaveis,GRUPO_MODELAGEM),
                   count(base_ago18_variaveis,GRUPO_MODELAGEM)[,2]/nrow(base_ago18_variaveis)))
    # GRUPO_MODELAGEM      n       n.1
    # 1              GA 314764 0.8013707
    # 2              GC  78018 0.1986293
  
  
# Pegando variáveis para correlação:
  variaveis_correlacao<-c('BS', 'LIMITE','IDADE','COMUNIS_16m', 'RENDA_LIQUIDA', 'DIA_VENCIMENTO',
                          'LIMITE_ATUAL','LIMITE_DISPONIVEL_SR', 'LIMITE_ANTERIOR', 'QTD_ATRASO_ATE_10_DIAS',
                          'QTD_LIMITE_EXCEDIDO','CEP_RESIDENCIAL', 'LOJ_NUMERO_LOJA', 'LIMITE_UTILIZADO',
                          'VLR_DEBT_A_FATURAR','MALA_SMS_EMAIL_16m', 'SALARIO_CORRIGIDO_VDD',
                          'TEMPO_ULTMA_COMPRA', 'TEMPO_CADASTRO_em_anos','TEMPO_ALTER_LIMITE',
                          'DELTA_LIMITE','TEMPO_ULTIM_SR','TEMPO_ULTIM_CP', 'Contrato_SR',
                          'Contrato_CP', 'contratos_AGOSET18', 'Pegou_AGOSET18','Extratando',
                          'TEM_EMAIL','TEM_CELULAR')
  
# Pegando apenas variáveis que interessam:
  base_ago18_variaveis<-subset(base_ago18_variaveis,
                               select=variaveis_correlacao)
  
# Fazendo a correlação:
  summary(base_ago18_variaveis) # TEM VÁRIOS NAS
  # Corrigindo NAs:
  base_ago18_variaveis$COMUNIS_16m[which(is.na(base_ago18_variaveis$COMUNIS_16m))]<-0
  base_ago18_variaveis$RENDA_LIQUIDA[which(is.na(base_ago18_variaveis$RENDA_LIQUIDA))]<-0
  base_ago18_variaveis$VLR_DEBT_A_FATURAR[which(is.na(base_ago18_variaveis$VLR_DEBT_A_FATURAR))]<-0
  base_ago18_variaveis$TEMPO_ULTMA_COMPRA[which(is.na(base_ago18_variaveis$TEMPO_ULTMA_COMPRA))]<-max(base_ago18_variaveis$TEMPO_ULTMA_COMPRA)
  base_ago18_variaveis$IDADE[which(is.na(base_ago18_variaveis$IDADE))]<-0
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Olhando mais de perto algumas variáveis:
  data.frame(cbind(count(base_ago18_variaveis,QTD_LIMITE_EXCEDIDO),
                   format(count(base_ago18_variaveis,QTD_LIMITE_EXCEDIDO)[,2]/nrow(base_ago18_variaveis),scientific=F)))
    # QTD_LIMITE_EXCEDIDO      n            n.1
    # 1                    0 237124 0.603703835715
    # 2                    1  50343 0.128170333671
    # 3                    2  28707 0.073086343061
    # 4                    3  18430 0.046921702115
  
  data.frame(cbind(count(base_ago18_variaveis,QTD_ATRASO_ATE_10_DIAS),
                   format(count(base_ago18_variaveis,QTD_ATRASO_ATE_10_DIAS)[,2]/nrow(base_ago18_variaveis),scientific=F)))
    # QTD_ATRASO_ATE_10_DIAS     n            n.1
    # 1                        0 39091 0.099523399748
    # 2                        1 21136 0.053811019853
    # 3                        2 22185 0.056481712502
    # 4                        3 16638 0.042359374920

  data.frame(cbind(count(base_ago18_variaveis,UF_RESIDENCIAL),
                   format(count(base_ago18_variaveis,UF_RESIDENCIAL)[,2]/nrow(base_ago18_variaveis),scientific=F)))
    # RJ 308696 0.785921961801
    # BA  21657 0.055137455382
    # ES  15949 0.040605221217
    # MG  22192 0.056499534093
  
  
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  summary(base_ago18_variaveis) # TEM VÁRIOS NAS
  
  matriz_corr<-cor(base_ago18_variaveis)
  matriz_corr2<-cor(na.omit(base_ago18_variaveis))
  
  nrow(base_ago18_variaveis)-nrow(na.omit(base_ago18_variaveis)) # 205.804 muito alto! # 32.642 melhor!
  
# Salvando a matriz de correlação:  
  wd_correlacao<-'C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Matriz de correlação'
  setwd(wd_correlacao)
  
  write.csv2(matriz_corr,"matriz_corr33.csv")
  write.csv2(matriz_corr2,"matriz_corr23.csv")

  
# Visualização:
  library(corrplot)
  corrplot(matriz_corr2, method="circle")
  corrplot(matriz_corr2, method="color",type='lower',number.font = 1,tl.cex=1)
  corrplot(matriz_corr2, method="number",type='lower',number.font = 1,tl.cex=1)
  
  
  