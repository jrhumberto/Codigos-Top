# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                   #
#             Job: Novo modelo CredLeader 2018                      #
#                  cluster análises - silhouete                     #                                                                 #
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

# Pasta de trabalho (receita com conversão)
  wd<-'C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Estudo qual base/Por clusters/Resultados/Clusterização com conversão'
  setwd(wd)

# Lendo as receitas:
  receita_sil_jan18_com_conv<-as.data.frame(read_excel('Todos com todas - com conversão.xlsx',
                                                        sheet='Clusters comuni jan18'))
  receita_sil_mar18_com_conv<-as.data.frame(read_excel('Todos com todas - com conversão.xlsx',
                                                       sheet='Clusters comuni mar18'))
  receita_sil_jun18_com_conv<-as.data.frame(read_excel('Todos com todas - com conversão.xlsx',
                                                       sheet='Clusters comuni jun18'))
  receita_sil_ago18_com_conv<-as.data.frame(read_excel('Todos com todas - com conversão.xlsx',
                                                       sheet='Clusters comuni ago18'))

# Pegando apenas variáveis que interessam:
  # removendo linhas com NA:
    receita_sil_jan18_com_conv<-receita_sil_jan18_com_conv[1:20,1:8]
    receita_sil_mar18_com_conv<-receita_sil_mar18_com_conv[1:23,1:8]
    receita_sil_jun18_com_conv<-receita_sil_jun18_com_conv[1:27,1:8]
    receita_sil_ago18_com_conv<-receita_sil_ago18_com_conv[1:27,1:8]
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    
# Diagnõstico clusters:
    # como foi kmeans, calculo a distância quadrática
    # e calculo a distância quadrática das variáveis que utilizei para clusterizar:
    # comunis 16 meses e conversão
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Silhoueta de cada clusterização:
    
  # jan18
    sil_jan18_conv<-silhouette(receita_sil_jan18_com_conv$cluster_jan18,
                               (dist(receita_sil_jan18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_jan18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6677346
    
  # mar18
    sil_mar18_conv<-silhouette(receita_sil_mar18_com_conv$Cluster_mar18,
                               (dist(receita_sil_mar18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_mar18_conv) 
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6650924
    
  # jun18
    sil_jun18_conv<-silhouette(receita_sil_jun18_com_conv$Cluster_jun18,
                               (dist(receita_sil_jun18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_jun18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6471032
    
  # ago18
    sil_ago18_conv<-silhouette(receita_sil_ago18_com_conv$Cluster_ago18,
                               (dist(receita_sil_ago18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_ago18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6157379
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Silhoueta de cada receita:
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
# # # jan18 # # # 
    sil_jan18_mar18_conv<-silhouette(receita_sil_jan18_com_conv$Cluster_mar18,
                               (dist(receita_sil_jan18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_jan18_mar18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.5959158
    
    
    sil_jan18_jun18_conv<-silhouette(receita_sil_jan18_com_conv$Cluster_jun18,
                                     (dist(receita_sil_jan18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_jan18_jun18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6394625
    
    
    sil_jan18_ago18_conv<-silhouette(receita_sil_jan18_com_conv$Cluster_ago18,
                                     (dist(receita_sil_jan18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_jan18_ago18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.648353

        
# # # mar18 # # # 
    sil_ma18_jan18_conv<-silhouette(receita_sil_mar18_com_conv$Cluster_jan18[1:20],
                                     (dist(receita_sil_mar18_com_conv[1:20,c(1,4)]))^2)
    si.sum <- summary(sil_ma18_jan18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6677344
    
    
    sil_ma18_jun18_conv<-silhouette(receita_sil_mar18_com_conv$Cluster_jun18,
                                    (dist(receita_sil_mar18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_ma18_jun18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.5054284
    
    
    sil_ma18_ago18_conv<-silhouette(receita_sil_mar18_com_conv$Cluster_ago18,
                                    (dist(receita_sil_mar18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_ma18_ago18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6158987
    
    
# # # jun18 # # # 
    # lidando com fatores:
    str(receita_sil_jun18_com_conv)
    # as.numeric(levels(receita_sil_jun18_com_conv$Cluster_jan18[1:20]))[receita_sil_jun18_com_conv$Cluster_jan18[1:20]]
    receita_sil_jun18_com_conv$Cluster_jan18<-as.numeric(as.character(receita_sil_jun18_com_conv$Cluster_jan18))
    receita_sil_jun18_com_conv$Cluster_mar18<-as.numeric(as.character(receita_sil_jun18_com_conv$Cluster_mar18))
# # # 
    
    sil_jun18_jan18_conv<-silhouette(receita_sil_jun18_com_conv$Cluster_jan18[1:20],
                                    (dist(receita_sil_jun18_com_conv[1:20,c(1,4)]))^2)
    si.sum <- summary(sil_jun18_jan18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6677345
    
    
    sil_jun18_mar18_conv<-silhouette(receita_sil_jun18_com_conv$Cluster_mar18[1:23],
                                     (dist(receita_sil_jun18_com_conv[1:23,c(1,4)]))^2)
    si.sum <- summary(sil_jun18_mar18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6650931
    
    
    sil_jun18_jun18_conv<-silhouette(receita_sil_jun18_com_conv$Cluster_jun18[1:23],
                                     (dist(receita_sil_jun18_com_conv[1:23,c(1,4)]))^2)
    si.sum <- summary(sil_jun18_jun18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.5054245
    
    
    
# # # ago18 # # # 
    # lidando com fatores:
    str(receita_sil_ago18_com_conv)
    # as.numeric(levels(receita_sil_ago18_com_conv$Cluster_jan18[1:20]))[receita_sil_ago18_com_conv$Cluster_jan18[1:20]]
    receita_sil_ago18_com_conv$Cluster_jan18<-as.numeric(as.character(receita_sil_ago18_com_conv$Cluster_jan18))
    receita_sil_ago18_com_conv$Cluster_mar18<-as.numeric(as.character(receita_sil_ago18_com_conv$Cluster_mar18))
    receita_sil_ago18_com_conv$Cluster_jun18<-as.numeric(as.character(receita_sil_ago18_com_conv$Cluster_jun18))
    # # # 
    
    sil_ago18_jan18_conv<-silhouette(receita_sil_ago18_com_conv$Cluster_jan18[1:20],
                                     (dist(receita_sil_ago18_com_conv[1:20,c(1,4)]))^2)
    si.sum <- summary(sil_ago18_jan18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! #  0.6677343
    
    
    sil_ago18_mar18_conv<-silhouette(receita_sil_ago18_com_conv$Cluster_mar18[1:23],
                                     (dist(receita_sil_ago18_com_conv[1:23,c(1,4)]))^2)
    si.sum <- summary(sil_ago18_mar18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6650929
    
    
    sil_ago18_jun18_conv<-silhouette(receita_sil_ago18_com_conv$Cluster_jun18,
                                     (dist(receita_sil_ago18_com_conv[,c(1,4)]))^2)
    si.sum <- summary(sil_ago18_jun18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.647103
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
# Pasta de trabalho (receita com conversão e iguais)
  wd<-'C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Estudo qual base/Por clusters/Resultados/Clusterização com conversão e iguais'
  setwd(wd)
    
 # Lendo as receitas:
   receita_sil_jan18_com_conv_iguais<-as.data.frame(read_excel('Todos com todas - com conversão iguais.xlsx',
                                                               sheet='Clusters comuni jan18'))
   receita_sil_mar18_com_conv_iguais<-as.data.frame(read_excel('Todos com todas - com conversão iguais.xlsx',
                                                               sheet='Clusters comuni mar18'))
   receita_sil_jun18_com_conv_iguais<-as.data.frame(read_excel('Todos com todas - com conversão iguais.xlsx',
                                                               sheet='Clusters comuni jun18'))
   receita_sil_ago18_com_conv_iguais<-as.data.frame(read_excel('Todos com todas - com conversão iguais.xlsx',
                                                               sheet='Clusters comuni ago18'))
    
# Pegando apenas variáveis que interessam:
  # removendo linhas com NA:
    receita_sil_jan18_com_conv_iguais<-receita_sil_jan18_com_conv_iguais[1:20,1:8]
    receita_sil_mar18_com_conv_iguais<-receita_sil_mar18_com_conv_iguais[1:23,1:8]
    receita_sil_jun18_com_conv_iguais<-receita_sil_jun18_com_conv_iguais[1:27,1:8]
    receita_sil_ago18_com_conv_iguais<-receita_sil_ago18_com_conv_iguais[1:29,1:8]
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
    
# Diagnõstico clusters:
  # como foi kmeans, calculo a distância quadrática
  # e calculo a distância quadrática das variáveis que utilizei para clusterizar:
  # comunis 16 meses e conversão
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # Silhoueta de cada clusterização:
    
    str(receita_sil_jan18_com_conv_iguais)
    
  # jan18
    sil_jan18_conv_iguais<-silhouette(receita_sil_jan18_com_conv_iguais$Cluster_jan18,
                                (dist(receita_sil_jan18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_jan18_conv_iguais)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6245329
    
  # mar18
    sil_mar18_conv_iguais<-silhouette(receita_sil_mar18_com_conv_iguais$Cluster_mar18,
                               (dist(receita_sil_mar18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_mar18_conv_iguais) 
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6260604
    
  # jun18
    sil_jun18_conv_iguais<-silhouette(receita_sil_jun18_com_conv_iguais$Cluster_jun18,
                               (dist(receita_sil_jun18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_jun18_conv_iguais)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6469142
    
  # ago18
    sil_ago18_conv_iguais<-silhouette(receita_sil_ago18_com_conv_iguais$Cluster_ago18,
                               (dist(receita_sil_ago18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_ago18_conv_iguais)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6395822
    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Silhoueta de cada receita:
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    
# # # jan18 # # # 
    sil_jan18_mar18_conv<-silhouette(receita_sil_jan18_com_conv_iguais$Cluster_mar18,
                                     (dist(receita_sil_jan18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_jan18_mar18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.552213
    
    
    sil_jan18_jun18_conv<-silhouette(receita_sil_jan18_com_conv_iguais$Cluster_jun18,
                                     (dist(receita_sil_jan18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_jan18_jun18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6129841
    
    
    sil_jan18_ago18_conv<-silhouette(receita_sil_jan18_com_conv_iguais$Cluster_ago18,
                                     (dist(receita_sil_jan18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_jan18_ago18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.5502628
    
    
    # # # mar18 # # # 
    sil_ma18_jan18_conv<-silhouette(receita_sil_mar18_com_conv_iguais$Cluster_jan18[1:20],
                                    (dist(receita_sil_mar18_com_conv_iguais[1:20,c(1,4)]))^2)
    si.sum <- summary(sil_ma18_jan18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6677344
    
    
    sil_ma18_jun18_conv<-silhouette(receita_sil_mar18_com_conv_iguais$Cluster_jun18,
                                    (dist(receita_sil_mar18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_ma18_jun18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.5054284
    
    
    sil_ma18_ago18_conv<-silhouette(receita_sil_mar18_com_conv_iguais$Cluster_ago18,
                                    (dist(receita_sil_mar18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_ma18_ago18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6158987
    
    
    # # # jun18 # # # 
    # lidando com fatores:
    str(receita_sil_jun18_com_conv_iguais)
    # as.numeric(levels(receita_sil_jun18_com_conv_iguais$Cluster_jan18[1:20]))[receita_sil_jun18_com_conv_iguais$Cluster_jan18[1:20]]
    receita_sil_jun18_com_conv_iguais$Cluster_jan18<-as.numeric(as.character(receita_sil_jun18_com_conv_iguais$Cluster_jan18))
    receita_sil_jun18_com_conv_iguais$Cluster_mar18<-as.numeric(as.character(receita_sil_jun18_com_conv_iguais$Cluster_mar18))
    # # # 
    
    sil_jun18_jan18_conv<-silhouette(receita_sil_jun18_com_conv_iguais$Cluster_jan18[1:20],
                                     (dist(receita_sil_jun18_com_conv_iguais[1:20,c(1,4)]))^2)
    si.sum <- summary(sil_jun18_jan18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6677345
    
    
    sil_jun18_mar18_conv<-silhouette(receita_sil_jun18_com_conv_iguais$Cluster_mar18[1:23],
                                     (dist(receita_sil_jun18_com_conv_iguais[1:23,c(1,4)]))^2)
    si.sum <- summary(sil_jun18_mar18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6650931
    
    
    sil_jun18_jun18_conv<-silhouette(receita_sil_jun18_com_conv_iguais$Cluster_jun18[1:23],
                                     (dist(receita_sil_jun18_com_conv_iguais[1:23,c(1,4)]))^2)
    si.sum <- summary(sil_jun18_jun18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.5054245
    
    
    
    # # # ago18 # # # 
    # lidando com fatores:
    str(receita_sil_ago18_com_conv_iguais)
    # as.numeric(levels(receita_sil_ago18_com_conv_iguais$Cluster_jan18[1:20]))[receita_sil_ago18_com_conv_iguais$Cluster_jan18[1:20]]
    receita_sil_ago18_com_conv_iguais$Cluster_jan18<-as.numeric(as.character(receita_sil_ago18_com_conv_iguais$Cluster_jan18))
    receita_sil_ago18_com_conv_iguais$Cluster_mar18<-as.numeric(as.character(receita_sil_ago18_com_conv_iguais$Cluster_mar18))
    receita_sil_ago18_com_conv_iguais$Cluster_jun18<-as.numeric(as.character(receita_sil_ago18_com_conv_iguais$Cluster_jun18))
    # # # 
    
    sil_ago18_jan18_conv<-silhouette(receita_sil_ago18_com_conv_iguais$Cluster_jan18[1:20],
                                     (dist(receita_sil_ago18_com_conv_iguais[1:20,c(1,4)]))^2)
    si.sum <- summary(sil_ago18_jan18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! #  0.6677343
    
    
    sil_ago18_mar18_conv<-silhouette(receita_sil_ago18_com_conv_iguais$Cluster_mar18[1:23],
                                     (dist(receita_sil_ago18_com_conv_iguais[1:23,c(1,4)]))^2)
    si.sum <- summary(sil_ago18_mar18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.6650929
    
    
    sil_ago18_jun18_conv<-silhouette(receita_sil_ago18_com_conv_iguais$Cluster_jun18,
                                     (dist(receita_sil_ago18_com_conv_iguais[,c(1,4)]))^2)
    si.sum <- summary(sil_ago18_jun18_conv)
    si.sum$avg.width # average sil da clusterização como um todo! # 0.647103