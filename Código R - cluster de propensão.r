# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                         #
#       Job: clusterização com a propensão                #
#                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Carregando pacotes
  library(dplyr)
  library(cluster)
  library(psych)  
  library(mclust)
  library(digest)
  library(ggplot2)
  library(gridExtra)
  library(ggfortify)
  library(graphics)
  library(foreign)
  library(fpc)
  library(data.table)
  library(xlsx)
  library(scales)
  
  
  # Pasta de trabalho #
    wd<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Modelos\\Job modelo Propensão à compra\\Aptos"
    setwd(wd)
  
  # Lendo a base de dados  #
    base<-"Aptos - vc 201604 - sem outliers - limpo nomes teste - 80% - v12.sav"
  # essa base já tem o modelo estimado e rodado #
    base2=read.spss(base)
    base.dat<-as.data.table(base2)
    head(base.dat)
    
  # Identificando onde estão as variáveis que eu quero
    which(names(base.dat)=="Probabilidade_estimada_M5") #132
    which(names(base.dat)=="PL") #15
    
    base.dat2<- subset(base.dat,select=c("PL","Probabilidade_estimada_M5"))
    head(base.dat2)
    base.dat2<-cbind(base.dat2,qtd_clientes=rep(1,nrow(base.dat2)))
    base.dat3<-subset(base.dat2,select = Probabilidade_estimada_M5)

  # Agregando por faixa de propensão: somando conversão e qtd clientes
    base_agg<-aggregate(cbind(PL,qtd_clientes) ~ Probabilidade_estimada_M5,
                                                 base.dat2,
                                                 sum)
    nrow(base_agg)
    nrow(base.dat2)
    head(base.dat3)
    
    # Reduzi a base em
      nrow(base_agg)/ nrow(base.dat2) * 100  # 27,75%
    
    
    # Verificando
    head(base_agg)
    
    min(base_agg$Probabilidade_estimada_M5) #iguais
    min(base.dat2$Probabilidade_estimada_M5)
    
    nrow(base.dat2)
    sum(base_agg$qtd_clientes)
  # # # # # # # # # # # # # C L U S T E R S # # # # # # # # # # # # # # # # # # # # #
  
  # K-means - com 5 clusters:
    n_clusters_base_agg<-5
    set.seed(5)
    cl_base5_agg<-kmeans(base_agg,n_clusters_base_agg,1000, nstart = 5)
    cl_base5_agg$centers
    cl_base5_agg$size
    
    n_clusters_base<-5
    set.seed(5)
    cl_base5<-kmeans(base.dat2,n_clusters_base,1000, nstart = 5)
    cl_base5$centers
    cl_base5$size
    
# # 5 clusters
    n_clusters_base3<-5
    set.seed(5)
    cl_base3_5<-kmeans(base.dat3,n_clusters_base3,1000, nstart = 1)
    cl_base3_5$centers
    cl_base3_5$size
    
# # 7 clusters
    n_clusters_base3_7<-7
    set.seed(7)
    cl_base3_7<-kmeans(base.dat3,n_clusters_base3_7,1000, nstart = 1)
    cl_base3_7$centers
    cl_base3_7$size

# # 9 clusters
    n_clusters_base3_9<-9
    set.seed(9)
    cl_base3_9<-kmeans(base.dat3,n_clusters_base3_9,1000, nstart = 1)
    cl_base3_9$centers
    cl_base3_9$size
    cl_base3_9$cluster
    
          
# # 10 clusters
    n_clusters_base3_10<-10
    set.seed(10)
    cl_base3_10<-kmeans(base.dat3,n_clusters_base3_10,100, nstart = 1)
    cl_base3_10$centers
    cl_base3_10$size
    
########################################################
    
# Colocando os clusters na base original    
  head(base.dat2)
  base.dat4<-cbind(base.dat2,
                   cluster5=cl_base3_5$cluster,
                   cluster7=cl_base3_7$cluster,
                   cluster9=cl_base3_9$cluster,
                   qtd_clientes=rep(1,nrow(base.dat2)))
  
  head(base.dat4)
  nrow(base.dat4)
  nrow(base.dat2)
  
  
########################################################
  
# Anotando os dados dos clusters
  #cluster5
  base_cluster5<-aggregate(cbind(PL,qtd_clientes) ~ cluster5, 
                                                    base.dat4,
                                                    sum)
  base_cluster5<-cbind(base_cluster5,
                             conversao=base_cluster5$PL/base_cluster5$qtd_clientes)
   
  head(base_cluster5)
  names(base_cluster5)<-c("clusters","PL","# clientes","% conversao")
   
  #cluster7
  base_cluster7<-aggregate(cbind(PL,qtd_clientes) ~ cluster7, 
                                                   base.dat4,
                                                   sum)
  base_cluster7<-cbind(base_cluster7,
                       conversao=base_cluster7$PL/base_cluster7$qtd_clientes)
  
  head(base_cluster7)
  names(base_cluster7)<-c("clusters","PL","# clientes","% conversao")
  
  #cluster9
  base_cluster9<-aggregate(cbind(PL,qtd_clientes) ~ cluster9, 
                                                   base.dat4,
                                                   sum)
  base_cluster9<-cbind(base_cluster9,
                       conversao=base_cluster9$PL/base_cluster9$qtd_clientes)
  
  head(base_cluster9)
  names(base_cluster9)<-c("clusters","PL","# clientes","% conversao")
  
  


write.table(x=base_cluster9,
             file="Cluster9.csv",
              row.names = F)




fwrite(base_cluster9,
       "C:\\Users\\phohberg\\Desktop\\Jobs\\Modelos\\Job modelo Propensão à compra\\Mães - R\\Cluster9.csv")




  # Selecionando apenas: #
  # indicador de conversão (ComprouFora_ou_Ativou_3m)
  # propensão estimada (Probabilidade_estimada_MODELO1)
  base.dat2<-subset(base.dat,select = c(ComprouFora_ou_Ativou_3m,Probabilidade_estimada_MODELO1))
  head(base.dat2)
  
  #tendo certeza de que tá tudo bem:
  summary(base.dat2$ComprouFora_ou_Ativou_3m)
  #Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  #0.000000 0.000000 0.000000 0.001085 0.000000 1.000000 
  summary(base.dat2$Probabilidade_estimada_MODELO1)
  #Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
  #1.126e-05 3.580e-04 6.796e-04 1.088e-03 1.295e-03 1.180e-01 
  
  #Fazendo clusters
  
  # transformando a base em data.table (o Diego disse que é melhor)
  # library(data.table)
  base.dat3 <- data.table(base.dat2)
  # verificando se deu certo
  head(base.dat3)
  base.dat3<-cbind(base.dat3,qtd_clientes=rep(1,nrow(base.dat3)))
  # unificando por qtde basecação:
  base.dat4<-aggregate(cbind(ComprouFora_ou_Ativou_3m,qtd_clientes) ~ Probabilidade_estimada_MODELO1,
                         base.dat3,
                         sum)
  base.dat4<-as.data.table(base.dat4)
  head(base.dat4)
  nrow(base.dat4)
  
  ## essa base contém faixas de basecação e qtde de clientes por faixa ##
  wd2<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Job Migrado\\Proposta"
  setwd(wd2)
  write.xlsx(base.dat4, file = "Faixas de propensão e qtde de clientes por faixa.xlsx", row.names = FALSE)
  
  head(base.dat4)
  
  # verificando as classes dos elementos desse data table, pra ver se faz sentido:
  class(base.dat4$ComprouFora_ou_Ativou_3m)
  class(base.dat4$Probabilidade_estimada_MODELO1)
  
  base.dat5<-cbind(base.dat4,conversao=(base.dat4$ComprouFora_ou_Ativou_3m/base.dat4$qtd_clientes))
  head(base.dat5)
  head(base.dat4)
  # !!Problema: 58 linhas com qtd cliente = 1 = soma (ComprouFora_ou_Ativou_3m)
  length(which(base.dat5$ComprouFora_ou_Ativou_3m == base.dat5$qtd_clientes))
  ##$#$#$#$##$#$#$#$#$#$#$#
  #Solução: retirar esses caras e depois analisar individualmente!!! #########
  ##$#$#$#$##$#$#$#$#$#$#$#
  ##$#$#$#$##$#$#$#$#$#$#$#
  base.dat5.2<-base.dat5[-which(base.dat5$ComprouFora_ou_Ativou_3m == base.dat5$qtd_clientes),]
  nrow(base.dat5.2)
  nrow(base.dat5)
  head(base.dat5.2)
  #Verificando se deu tudo certo (lembrando que conversão 100% é bom evitar!!)
  summary(base.dat5.2$conversao)
  
  # quero fazer os clusters baseada nas conversões por faixa de basecação, então:
  base.dat6<-subset(base.dat5.2,select=c(Probabilidade_estimada_MODELO1,conversao))
  is.data.table(base.dat6)
  is.data.frame(base.dat6)
  head(base.dat6)
  
  
  # Rerodando clusters!!! ####
  sum(base.dat5$qtd_clientes)
  # 380.647
  
  base.dat7<- base.dat5[,1]
  
  #criando um vetor independente: 
  # conversao=(base.dat4$ComprouFora_ou_Ativou_3m/base.dat4$qtd_clientes)
  # summary(conversao)
  
  # K-means2 - com 5 clusters:
  n_clusters.base5<-5
  set.seed(5)
  cl_base5<-kmeans(base.dat7,n_clusters.base5,1000, nstart = 5)
  cl_base5$centers
  cl_base5$size
  
  # K-means2 - com 3 clusters:
  # n_clusters.base3<-3
  # set.seed(3)
  # cl_base3<-kmeans(base.dat6,n_clusters.base3,1000, nstart = 3)
  # cl_base3$centers
  # cl_base3$size
  
  # Qtd clientes por faixa prob
  qtd_clientes_por_faixa<-base.dat5[,3]
  sum(qtd_clientes_por_faixa)
  # [1] 380.647
  
  # K-means2 - com 6 clusters:
  n_clusters.base6<-6
  set.seed(6)
  cl_base6<-kmeans(base.dat7,n_clusters.base6,1000, nstart = 6)
  cl_base6$centers
  cl_base6$size
  
  # K-means2 - com 7 clusters:
  n_clusters.base7<-7
  set.seed(7)
  cl_base7<-kmeans(base.dat7,n_clusters.base7,1000, nstart = 7)
  cl_base7$centers
  cl_base7$size
  
  # K-means2 - com 8 clusters:
  n_clusters.base8<-8
  set.seed(8)
  cl_base8<-kmeans(base.dat7,n_clusters.base8,1000, nstart = 8)
  cl_base8$centers
  cl_base8$size
  
  # K-means2 - com 10 clusters:
  n_clusters.base10<-10
  set.seed(10)
  cl_base10<-kmeans(base.dat7,n_clusters.base10,1000, nstart = 10)
  cl_base10$centers
  cl_base10$size
  
  # K-means2 - com 9 clusters:
  n_clusters.base9<-9
  set.seed(9)
  cl_base9<-kmeans(base.dat7,n_clusters.base9,1000, nstart = 9)
  cl_base9$centers
  cl_base9$size
  
  # Gostei mais do 8, 7, 6, 10. <3
  base.dat_6cl <- cbind(base.dat7, 
                          cluster = cl_base6$cluster,
                          qtd_bases=rep(1,nrow(base.dat7)),
                          qtd_clientes_por_faixa,
                          qtd_migrado=base.dat5$ComprouFora_ou_Ativou_3m,
                          propensao=base.dat5$Probabilidade_estimada_MODELO1)
  base.dat_7cl <- cbind(base.dat7, 
                          cluster = cl_base7$cluster,
                          qtd_bases=rep(1,nrow(base.dat7)),
                          qtd_clientes_por_faixa,
                          qtd_migrado=base.dat5$ComprouFora_ou_Ativou_3m,
                          propensao=base.dat5$Probabilidade_estimada_MODELO1)
  base.dat_8cl <- cbind(base.dat7, 
                          cluster = cl_base8$cluster,
                          qtd_bases=rep(1,nrow(base.dat7)),
                          qtd_clientes_por_faixa,
                          qtd_migrado=base.dat5$ComprouFora_ou_Ativou_3m,
                          propensao=base.dat5$Probabilidade_estimada_MODELO1)
  base.dat_10cl <- cbind(base.dat7, 
                           cluster = cl_base10$cluster,
                           qtd_bases=rep(1,nrow(base.dat7)),
                           qtd_clientes_por_faixa,
                           qtd_migrado=base.dat5$ComprouFora_ou_Ativou_3m,
                           propensao=base.dat5$Probabilidade_estimada_MODELO1)
  base.dat_9cl <- cbind(base.dat7, 
                          cluster = cl_base9$cluster,
                          qtd_bases=rep(1,nrow(base.dat7)),
                          qtd_clientes_por_faixa,
                          qtd_migrado=base.dat5$ComprouFora_ou_Ativou_3m,
                          propensao=base.dat5$Probabilidade_estimada_MODELO1)
  #is.data.table(base.dat_6cl)
  # head(base.dat_6cl)
  
  # Pasta de trabalho #
  wd3<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Job Migrado\\Proposta\\Separando PARA base com prop"
  setwd(wd3)
  write.xlsx(base.dat_6cl, file = "base.dat_6cl.xlsx", row.names = FALSE)
  write.xlsx(base.dat_7cl, file = "base.dat_7cl.xlsx", row.names = FALSE)
  write.xlsx(base.dat_8cl, file = "base.dat_8cl.xlsx", row.names = FALSE)
  write.xlsx(base.dat_9cl, file = "base.dat_9cl.xlsx", row.names = FALSE)
  write.xlsx(base.dat_10cl, file = "base.dat_10cl.xlsx", row.names = FALSE)  
  
  # unificando por cluster
  base.dat_6cl.agg<-aggregate(cbind(qtd_clientes,qtd_bases,qtd_migrado) ~ cluster,
                                base.dat_6cl,sum)
  base.dat_7cl.agg<-aggregate(cbind(qtd_clientes,qtd_bases,qtd_migrado) ~ cluster,
                                base.dat_7cl,sum)
  base.dat_8cl.agg<-aggregate(cbind(qtd_clientes,qtd_bases,qtd_migrado) ~ cluster,
                                base.dat_8cl,sum)
  base.dat_9cl.agg<-aggregate(cbind(qtd_clientes,qtd_bases,qtd_migrado) ~ cluster,
                                base.dat_9cl,sum)
  base.dat_10cl.agg<-aggregate(cbind(qtd_clientes,qtd_bases,qtd_migrado) ~ cluster,
                                 base.dat_10cl,sum)
  
  write.xlsx(base.dat_6cl.agg, file = "base.dat_6cl agg por cluster.xlsx", row.names = FALSE)
  write.xlsx(base.dat_7cl.agg, file = "base.dat_7cl agg por cluster.xlsx", row.names = FALSE)
  write.xlsx(base.dat_8cl.agg, file = "base.dat_8cl agg por cluster.xlsx", row.names = FALSE)
  write.xlsx(base.dat_10cl.agg, file = "base.dat_10cl agg por cluster.xlsx", row.names = FALSE) 
  
  
  
  ############################################
  #   Como identificar os clusters?          #
  ############################################
  
  # Identificar máximos e mínimos da propensão estimada de cada um #
  # vou fazer para o 9 clusters e 10 clusters
  ### 9 clusters ##
  head(base.dat_9cl)
  summary(base.dat_9cl$cluster) # ok, 9 clusters!
  
  # Descobrindo os limites de propensã de cada cluster:
  #######     cluster 1   #####
  length(which(base.dat_9cl$cluster==1)) # linhas do cluster 1
  cl_base9$size #bateu
  length(base.dat_9cl$propensao[which(base.dat_9cl$cluster==1)]) # bateu
  
  # mínimo e máximo
  Prop_min<-NULL
  Prop_max<-NULL
  Cluster_9<-NULL
  for (i in 1:9)
  {
    Prop_min[i]<-min(base.dat_9cl$propensao[which(base.dat_9cl$cluster==i)])
    Prop_max[i]<-max(base.dat_9cl$propensao[which(base.dat_9cl$cluster==i)])
    Cluster_9[i]<-i
  }
  
  Limites_cluster9 <- data.frame(Cluster_9,
                                 Prop_min,
                                 Prop_max,
                                 qtd_clientes=base.dat_9cl.agg$qtd_clientes,
                                 qtd_migrados_9=base.dat_9cl.agg$qtd_migrado)
  names(Limites_cluster9)<- c("Cluster 9", "Propensão mínima","Propensão máxima", 
                              "# clientes", "# migrado")
  
  
  ### 10 clusters ##
  head(base.dat_10cl)
  summary(base.dat_10cl$cluster) # ok, 10 clusters!
  # Descobrindo os limites de propensã de cada cluster:
  #######     cluster 1   #####
  length(which(base.dat_10cl$cluster==1)) # linhas do cluster 1
  cl_base10$size #bateu
  length(base.dat_10cl$propensao[which(base.dat_10cl$cluster==1)]) # bateu
  
  # mínimo e máximo
  Prop_min<-NULL
  Prop_max<-NULL
  Cluster_10<-NULL
  for (i in 1:10)
  {
    Prop_min[i]<-min(base.dat_10cl$propensao[which(base.dat_10cl$cluster==i)])
    Prop_max[i]<-max(base.dat_10cl$propensao[which(base.dat_10cl$cluster==i)])
    Cluster_10[i]<-i
  }
  
  # # # # # # # # # # Verificações # # # # # # # # #
  #cluster1
  i=1
  length(which(base.dat_10cl$cluster==i)) #mas essa base não tem a qtde total clientes
  sum(base.dat_10cl$qtd_clientes[which(base.dat_10cl$cluster==i)])
  # mínimo:
  min_teste_prop<-min(base.dat_10cl$propensao[which(base.dat_10cl$cluster==i)])
  min_teste_conv<-min(base.dat_10cl$conversao[which(base.dat_10cl$cluster==i)])
  
  
  # máximo:
  max_teste_prop<-max(base.dat_10cl$propensao[which(base.dat_10cl$cluster==i)]) 
  max_teste_conv<-max(base.dat_10cl$conversao[which(base.dat_10cl$cluster==i)])
  
  posicoes_clientes_cluster1<-which(base.dat_10cl$propensao >= min_teste_prop &
                                      base.dat_10cl$propensao <= max_teste_prop & 
                                      base.dat_10cl$conversao <= max_teste_conv & 
                                      base.dat_10cl$conversao >= min_teste_conv)
  
  posicoes_clientes_cluster1<-which(base.dat_10cl$propensao >= min_teste_prop &
                                      base.dat_10cl$propensao <= max_teste_prop)
  
  
  
  sum(base.dat_10cl$qtd_clientes[posicoes_clientes_cluster1])
  
  
  
  
  
  
  
  Limites_cluster10 <- data.frame(Cluster_10,
                                  Prop_min,
                                  Prop_max,
                                  qtd_clientes=base.dat_10cl.agg$qtd_clientes,
                                  qtd_migrados_9=base.dat_10cl.agg$qtd_migrado)
  names(Limites_cluster10)<- c("Cluster 10", "Propensão mínima","Propensão máxima", 
                               "# clientes", "# migrado")
  
  # Pasta de trabalho #
  wd3<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Job Migrado\\Proposta\\Separando PARA base com prop"
  setwd(wd3)
  write.xlsx(Limites_cluster10, file = "Limites prop cluster 10.xlsx", row.names = FALSE)
  write.table(Limites_cluster10, file = "Limites prop cluster 10.csv",sep=";",dec=",", row.names = FALSE)
  write.xlsx(Limites_cluster9, file = "Limites prop cluster 9.xlsx", row.names = FALSE)
  