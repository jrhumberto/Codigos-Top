# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                   #
#             Job: Novo modelo CredLeader 2018                      #
#                  escolhendo clusterização da base escolhida       #                                                                 #
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
  wd<-'C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Base'
  setwd(wd)

# Lendo a base:
  base_ago18<-as.data.frame(read_spss('G0 a G7 - 201807.sav'))
  head(base_ago18)
  nrow(base_ago18)
  
  count(base_ago18,COMUNIS_16m)

# Pegando apenas variáveis que interessam:
  head(ago18_cluster)
    # MALA_SMS_EMAIL_16m   conversao
    # 1                  0 0.005229735
    # 2                  1 0.007869059
    # 3                  2 0.011514069
    # 4                  3 0.012504466
    # 5                  4 0.009392403
    # 6                  5 0.012577361
  
# Retirando cluster 0
  ago18_cluster_zero<-ago18_cluster[1,]
  head(ago18_cluster_zero)
  # MALA_SMS_EMAIL_16m   conversao
  # 1                  0 0.005229735
  
  ago18_cluster_semzero<-ago18_cluster[-1,]
  head(ago18_cluster_semzero)
  # MALA_SMS_EMAIL_16m   conversao
  # 2                  1 0.007869059
  # 3                  2 0.011514069
  # 4                  3 0.012504466
  # 5                  4 0.009392403
  # 6                  5 0.012577361
  # 7                  6 0.010270846
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #   
# Clusterização ago18
# sem zero
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # v3:
    set.seed(10)
    ago18_cluster_feito_sem_zero_3<-kmeans(ago18_cluster_semzero[,1],
                                          6,
                                          1000, nstart = 1)
    ago18_cluster_feito_sem_zero_3$centers
    ago18_cluster_feito_sem_zero_3$size
  
  # Preparando cluster sem zero para salvar:
    ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg[-1,], 
                                                 cluster_sem0 = ago18_cluster_feito_sem_zero_3$cluster,
                                                 qtd_comunis=rep(1,nrow(ago18_agg)-1))
    
    # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v3.csv',
                 row.names = F)
     
# - # - # - # - # 
    # v3_2:
      set.seed(10)
      ago18_cluster_feito_sem_zero_3_2<-kmeans(ago18_cluster_semzero,
                                             6,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_3_2$centers
      ago18_cluster_feito_sem_zero_3_2$size
      
    # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg[-1,], 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_3_2$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)-1))
      
    # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v3_2.csv',
                 row.names = F)
 
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
    # v4:
      set.seed(10)
      ago18_cluster_feito_sem_zero_4<-kmeans(ago18_cluster_semzero[,1],
                                             7,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_4$centers
      ago18_cluster_feito_sem_zero_4$size
      
    # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg[-1,], 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_4$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)-1))
      
    # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v4.csv',
                 row.names = F)

  # - # - # - # -
            
      # v4_2:
      set.seed(10)
      ago18_cluster_feito_sem_zero_4_2<-kmeans(ago18_cluster_semzero[,1],
                                             7,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_4_2$centers
      ago18_cluster_feito_sem_zero_4_2$size
      
      # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg[-1,], 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_4_2$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)-1))
      
      # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v4_2.csv',
                 row.names = F)
      
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
    # v5:
      set.seed(10)
      ago18_cluster_feito_sem_zero_5<-kmeans(ago18_cluster_semzero[,1],
                                             4,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_5$centers
      ago18_cluster_feito_sem_zero_5$size
      
    # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg[-1,], 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_5$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)-1))
      
    # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v5.csv',
                 row.names = F)
  
# - # - # - #
    # v5_2:
      set.seed(10)
      ago18_cluster_feito_sem_zero_5_2<-kmeans(ago18_cluster_semzero[,1],
                                             4,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_5_2$centers
      ago18_cluster_feito_sem_zero_5_2$size
      
    # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg[-1,], 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_5_2$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)-1))
      
    # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v5_2.csv',
                 row.names = F)
      
      
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
    # v6: - quantidade comuni e conversão
      set.seed(10)
      ago18_cluster_feito_sem_zero_6<-kmeans(ago18_cluster_semzero,
                                             4,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_6$centers
      ago18_cluster_feito_sem_zero_6$size
      
    # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg[-1,], 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_6$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)-1))
      
    # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v6.csv',
                 row.names = F)
      
      
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
    # v7: - quantidade comuni e conversão e com zero
      set.seed(10)
      ago18_cluster_feito_sem_zero_7<-kmeans(ago18_cluster,
                                             4,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_7$centers
      ago18_cluster_feito_sem_zero_7$size
      
    # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg, 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_7$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)))
      
   # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v7.csv',
                 row.names = F)
      
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
    # v8: -  conversão e com zero
      set.seed(10)
      ago18_cluster_feito_sem_zero_8<-kmeans(ago18_cluster[,1],
                                             4,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_8$centers
      ago18_cluster_feito_sem_zero_8$size
      
    # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg, 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_8$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)))
      
    # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v8.csv',
                 row.names = F)
      
# - # - # - # - #
    # v9: -  conversão e com zero
      set.seed(10)
      ago18_cluster_feito_sem_zero_9<-kmeans(ago18_cluster[,2],
                                             4,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_9$centers
      ago18_cluster_feito_sem_zero_9$size
      
    # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg, 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_9$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)))
      
    # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v9.csv',
                 row.names = F) 
      
      
# - # - # - # - #
    # v10: -  conversão e sem zero
      set.seed(100)
      ago18_cluster_feito_sem_zero_10<-kmeans(ago18_cluster_semzero[,2],
                                             4,
                                             1000, nstart = 1)
      ago18_cluster_feito_sem_zero_10$centers
      ago18_cluster_feito_sem_zero_10$size
      
      # Preparando cluster sem zero para salvar:
      ago18_cluster_feito_sem_zero_salvar <- cbind(ago18_agg[-1,], 
                                                   cluster_sem0 = ago18_cluster_feito_sem_zero_10$cluster,
                                                   qtd_comunis=rep(1,nrow(ago18_agg)-1))
      
      # Salvando:
      setwd('C:/Users/phohberg/Desktop/Jobs/Jobs/AÇÕES/CREDLeader/Modelo/Novo modelo/Ago18/Cluster')
      write.csv2(ago18_cluster_feito_sem_zero_salvar,
                 file='Clusters comuni ago18 clusterizado sem 0 para modelagem -v10.csv',
                 row.names = F) 
  
      
      
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 
#  Gráfico solicitado pela Aline - comuni16m vs clusters  
#      
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
      
      # Agg - x = clusters, y= comunis16
      ggplot(ago18_cluster_feito_sem_zero_salvar, 
                  aes(x=cluster_sem0, y=MALA_SMS_EMAIL_16m)) +
              geom_point(aes(colour=as.factor(cluster_sem0)),size=4)+
              labs(x="Clusters de conversão", 
                   y='# comunicações últimos 16 meses')+
              geom_hline(yintercept = 1.5:28.5, color='gray')
      
      # Agg - x = comunis16, y= clusters
      ggplot(ago18_cluster_feito_sem_zero_salvar, 
             aes(x=MALA_SMS_EMAIL_16m, y=cluster_sem0)) +
        geom_point(aes(colour=as.factor(MALA_SMS_EMAIL_16m)),size=4)+
        labs(x="# comunicações últimos 16 meses", 
             y='Clusters de conversão')+
        geom_vline(xintercept = 1.5:28.5, color='gray')
      
      # Agg - x = comunis16, y= CONVERSÃO
      ggplot(ago18_cluster_feito_sem_zero_salvar, 
             aes(x=COMUNIS_16m, y=conversao)) +
        geom_point(aes(colour=as.factor(COMUNIS_16m)),size=4)+
        labs(x="# comunicações últimos 16 meses", 
             y='Conversão')+
        geom_vline(xintercept = 1.5:28.5, color='gray')
      
      # Agg - x = clusters, y= CONVERSÃO
      ggplot(ago18_cluster_feito_sem_zero_salvar, 
             aes(x=cluster_sem0, y=conversao)) +
        geom_point(aes(colour=as.factor(cluster_sem0)),size=4)+
        labs(x="Clusters de conversão", 
             y='Conversão')
      

      # Base full
        # Preparando:
          names(ago18_cluster_feito_sem_zero_salvar)[1]<-"COMUNIS_16m"
          
          base_ago18_cluster<-merge(x=base_ago18,
                                    y=ago18_cluster_feito_sem_zero_salvar[,c(1,5)],
                                    by="COMUNIS_16m",
                                    all.x=T)
          
          base_ago18_cluster$cluster_sem0[which(is.na(base_ago18_cluster$cluster_sem0))]<-0
          
          base_ago18_cluster$conversao
          
      # Gráfico:
        ggplot(base_ago18_cluster, 
               aes(x=COMUNIS_16m, y=cluster_sem0)) +
          geom_point(aes(colour=as.factor(COMUNIS_16m)),size=4)+
          labs(x="# comunicações últimos 16 meses", 
               y='Clusters de conversão')+
          geom_vline(xintercept = 1.5:28.5, color='gray')
        
        
        
        
        
      
# Diagnõstico clusters:
# como foi kmeans, calculo a distância quadrática
# e calculo a distância quadrática das variáveis que utilizei para clusterizar:
# comunis 16 meses e conversão

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Silhoueta de cada clusterização:

# jan18
sil_jan18_conv<-silhouette(receita_sil_jan18_com_conv$cluster_jan18,
                           (dist(receita_sil_jan18_com_conv[,c(1,4)]))^2)




