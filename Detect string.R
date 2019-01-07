# # # # # # # # # # # # # # # # # # # # # # # # # 
#                                               #
#      Job: como % TXD é calculado, afinal?     #
#                                               #
# # # # # # # # # # # # # # # # # # # # # # # # #

# Pacotes:
  library(haven)
  library(dplyr)  
  library(stringr)

# Pasta de trabalho:
  setwd('C:/Users/gboni/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Separações/Antigos/201704_05_CREDLEADER_ABR_MAI/201704_05_CREDLEADER_ABR_MAI')
  
# Lendo a base:
  base_fo<-as.data.frame(read_spss('20170418_CP_Refresh_GA.sav'))

# Panz:
  head(base_fo)
  
# Quantificando:
  # Identificando quais linhas possuem
  base_fo[which(str_detect(base_fo$ACAO_ABR_MAI,'TXD')),'TAXA']<-'TXD'
  base_fo[which(str_detect(base_fo$ACAO_ABR_MAI,'TXN')),'TAXA']<-'TXN'
  
  data.frame(count(base_fo,TAXA))
  
  

  base_fo$NOVOS<-0
  base_fo[which(str_detect(base_fo$ACAO_ABR_MAI,'NOVOS')),'NOVOS']<-1      

  data.frame(count(base_fo,NOVOS))
    
  
  data.frame(count(base_fo,NOVOS,TAXA))
  