## Apenas estudando os comuni3dígitos ##

#Carregando pacotes
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

# Pasta de trabalho #
wd<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Job Breno\\Repensando o CP\\Separando por comunicações"
setwd(wd)

# Lendo a base de dados  #
Base_elegíveis<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Job Fábio\\Mai16\\201605_Elegiveis + comunicação-v8.sav"
# essa base tem os totais de comunicação #
comuni=read.spss(Base_elegíveis)
comuni.dat<-as.data.frame(comuni)
head(comuni.dat)
# Selecionando apenas: #
  # a soma das comunicações de 15 meses (MALA_SMS_EMAIL_15m)
  # qtd contratos (QTD_contratos_JUNouJUL16)
    comuni.dat2<-subset(comuni.dat,select = c(QTD_contratos_JUNouJUL16,MALA_SMS_EMAIL_15m))
    head(comuni.dat2)
    
#Fazendo clusters

# transformando a base em data.table (o Diego disse que é melhor)
# library(data.table)
  comuni.dat3 <- data.table(comuni.dat2)
  # verificando se deu certo
  head(comuni.dat3)
  comuni.dat3<-cbind(comuni.dat3,qtd_clientes=rep(1,nrow(comuni.dat3)))
  # unificando por qtde comunicação:
  comuni.dat4<-aggregate(cbind(QTD_contratos_JUNouJUL16,qtd_clientes) ~ MALA_SMS_EMAIL_15m,
                          comuni.dat3,
                          sum)
  comuni.dat4<-as.data.table(comuni.dat4)
  ## essa base contém faixas de comunicação e qtde de clientes por faixa ##
  wd<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Job Breno\\Repensando o CP\\Separando por comunicações"
  setwd(wd)
  write.csv(comuni.dat4, file = "Faixas de comunicação e qtde de clientes por faixa.csv", row.names = FALSE)
  
  head(comuni.dat4)
  attach(comuni.dat4)
  
  # verificando as classes dos elementos desse data table, pra ver se faz sentido:
  class(comuni.dat4$QTD_contratos_JUNouJUL16)
  class(comuni.dat4$MALA_SMS_EMAIL_15m)

  comuni.dat5<-cbind(comuni.dat4,conversao=(comuni.dat4$QTD_contratos_JUNouJUL16/comuni.dat4$qtd_clientes))
  head(comuni.dat5)
  head(comuni.dat4)
  
  # quero fazer os clusters baseada nas conversões por faixa de comunicação, então:
  comuni.dat6<-subset(comuni.dat5,select=c(MALA_SMS_EMAIL_15m,conversao))
  is.data.table(comuni.dat6)
  
  #criando um vetor independente: 
  conversao=(comuni.dat4$QTD_contratos_JUNouJUL16/comuni.dat4$qtd_clientes)
  conversao
  
# K-means2 - com 5 clusters:
  n_clusters.comuni5<-5
  set.seed(5)
  cl_comuni5<-kmeans(comuni.dat6,n_clusters.comuni5,1000, nstart = 5)
  cl_comuni5$centers
  cl_comuni5$size
  
# K-means2 - com 3 clusters:
  n_clusters.comuni3<-3
  set.seed(3)
  cl_comuni3<-kmeans(comuni.dat6,n_clusters.comuni3,1000, nstart = 3)
  cl_comuni3$centers
  cl_comuni3$size
  
# K-means2 - com 4 clusters:
  n_clusters.comuni4<-4
  set.seed(4)
  cl_comuni4<-kmeans(comuni.dat6,n_clusters.comuni4,1000, nstart = 4)
  cl_comuni4$centers
  cl_comuni4$size

comuni.dat_3cl <- cbind(comuni.dat6, cluster = cl_comuni3$cluster,qtd_comunis=rep(1,nrow(comuni.dat6)))
comuni.dat_4cl <- cbind(comuni.dat6, cluster = cl_comuni4$cluster,qtd_comunis=rep(1,nrow(comuni.dat6)))
comuni.dat_5cl <- cbind(comuni.dat6, cluster = cl_comuni5$cluster,qtd_comunis=rep(1,nrow(comuni.dat6)))
#is.data.table(comuni.dat_5cl)

# Pasta de trabalho #
wd<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Job Breno\\Repensando o CP\\Separando por comunicações"
setwd(wd)
  write.csv(comuni.dat_3cl, file = "comuni.dat_3cl.csv", row.names = FALSE)
  write.csv(comuni.dat_4cl, file = "comuni.dat_4cl.csv", row.names = FALSE)
  write.csv(comuni.dat_5cl, file = "comuni.dat_5cl.csv", row.names = FALSE)

# unificando por cluster
  comuni.dat_3cl.agg<-aggregate(cbind(QTD_contratos_JUNouJUL16,qtd_comunis) ~ cluster,
                              comuni.dat_3cl,sum)
  comuni.dat_4cl.agg<-aggregate(cbind(QTD_contratos_JUNouJUL16,qtd_comunis) ~ cluster,
                             comuni.dat_4cl,sum)
  comuni.dat_5cl.agg<-aggregate(cbind(QTD_contratos_JUNouJUL16,qtd_comunis) ~ cluster,
                             comuni.dat_5cl,sum)
#   
# ############## total clientes em cada faixa de qtde comunicação #########
#   clientes_por_qtde_comuni<-c(26032,	9800,	21438,	43351,	66260,	73901,	
#                               61680,	52213,	49478,	47442,	42705,	33385,	
#                               22716,	14018,	7760,	4358,	2241,	874,	278,	57,5)
#   

# ### Retirando a primeira linha do comuni.dat5 ###
     comuni.dat7<-comuni.dat6[-1,]
     nrow(comuni.dat6)
     nrow(comuni.dat7)

     # K-means2 - com 5 clusters:
     n_clusters.comuni5<-5
     set.seed(5)
     cl_comuni5<-kmeans(comuni.dat7,n_clusters.comuni5,1000, nstart = 5)
     cl_comuni5$centers
     cl_comuni5$size
     
     # K-means2 - com 3 clusters:
     n_clusters.comuni3<-3
     set.seed(3)
     cl_comuni3<-kmeans(comuni.dat7,n_clusters.comuni3,1000, nstart = 3)
     cl_comuni3$centers
     cl_comuni3$size
     
     # K-means2 - com 4 clusters:
     n_clusters.comuni4<-4
     set.seed(4)
     cl_comuni4<-kmeans(comuni.dat7,n_clusters.comuni4,1000, nstart = 4)
     cl_comuni4$centers
     cl_comuni4$size
     
     comuni.dat_3cl <- cbind(comuni.dat7, cluster = cl_comuni3$cluster,qtd_comunis=rep(1,nrow(comuni.dat7)),qtd_clientes=comuni.dat4$qtd_clientes[-1])
     comuni.dat_4cl <- cbind(comuni.dat7, cluster = cl_comuni4$cluster,qtd_comunis=rep(1,nrow(comuni.dat7)),qtd_clientes=comuni.dat4$qtd_clientes[-1])
     comuni.dat_5cl <- cbind(comuni.dat7, cluster = cl_comuni5$cluster,qtd_comunis=rep(1,nrow(comuni.dat7)),qtd_clientes=comuni.dat4$qtd_clientes[-1])
     #is.data.table(comuni.dat_5cl)
     
     # Pasta de trabalho #
     wd<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Job Breno\\Repensando o CP\\Separando por comunicações"
     setwd(wd)
     write.csv(comuni.dat_3cl, file = "comuni.dat_3cl.csv", row.names = FALSE)
     write.csv(comuni.dat_4cl, file = "comuni.dat_4cl.csv", row.names = FALSE)
     write.csv(comuni.dat_5cl, file = "comuni.dat_5cl.csv", row.names = FALSE)
     
     # unificando por cluster
     comuni.dat_3cl.agg<-aggregate(cbind(QTD_contratos_JUNouJUL16,qtd_comunis) ~ cluster,
                                   comuni.dat_3cl,sum)
     comuni.dat_4cl.agg<-aggregate(cbind(QTD_contratos_JUNouJUL16,qtd_comunis) ~ cluster,
                                   comuni.dat_4cl,sum)
     comuni.dat_5cl.agg<-aggregate(cbind(QTD_contratos_JUNouJUL16,qtd_comunis) ~ cluster,
                                   comuni.dat_5cl,sum)  
     
     
     # para ver 
     
     head(comuni.dat4)
     
     # K-means2 - com 5 clusters:
     n_clusters.comuni5<-5
     set.seed(5)
     cl_comuni5l<-kmeans(comuni.dat6,n_clusters.comuni5,1000, nstart = 5)
     cl_comuni5l$centers
     cl_comuni5l$size
     
     # K-means2 - com 3 clusters:
     n_clusters.comuni3<-3
     set.seed(3)
     cl_comuni3<-kmeans(comuni.dat7,n_clusters.comuni3,1000, nstart = 3)
     cl_comuni3$centers
     cl_comuni3$size
     
     # K-means2 - com 4 clusters:
     n_clusters.comuni4<-4
     set.seed(4)
     cl_comuni4<-kmeans(comuni.dat7,n_clusters.comuni4,1000, nstart = 4)
     cl_comuni4$centers
     cl_comuni4$size
     
     comuni.dat_3cl <- cbind(comuni.dat7, cluster = cl_comuni3$cluster,qtd_comunis=rep(1,nrow(comuni.dat7)))
     comuni.dat_4cl <- cbind(comuni.dat7, cluster = cl_comuni4$cluster,qtd_comunis=rep(1,nrow(comuni.dat7)))
     comuni.dat_5cl <- cbind(comuni.dat7, cluster = cl_comuni5$cluster,qtd_comunis=rep(1,nrow(comuni.dat7)))
     #is.data.table(comuni.dat_5cl)
     
     
     
     
     
     
#     
#     
#     
#     
#     
# 
#     # verificando a distribuição de contratos
# teste <- comuni.dat4[, .N,
#                   by = QTD_contratos_JUNouJUL16_sum][order(QTD_contratos_JUNouJUL16_sum)]
# total <- teste[, sum(N)]
# teste[, N_PERC := N/total]
# 
# # Quantidade total de contratos:
# total_contratos=sum(comuni.dat_10cl$QTD_contratos_JUNouJUL16_sum)
# 
# # verificando a distribuição de contratos 
# ## analisando regiões 4 (BA e SE), 5 (PA, AL, PB, RN), 6 (CE, PI, MA, PA, AP, AM, RR, AC)
# # FAZER DEPOIS!!!
# 
# head(comuni.dat_7cl)
# is.factor(cl_comuni$cluster)
# 
# #plotando clusters7 vs comuni_2DIGITOS:
# ggplot(comuni.dat3, aes(x = comuni_2DIGITOS, y = QTD_contratos_JUNouJUL16_sum,color = as.factor(cl_comuni$cluster), label=comuni_2DIGITOS))  + geom_point() +geom_text(hjust=0, vjust=0)
# 
# #plotando clusters5 vs comuni_2DIGITOS:
# ggplot(comuni.dat3, aes(x = comuni_2DIGITOS, y = QTD_contratos_JUNouJUL16_sum,color = as.factor(cl_comuni3$cluster), label=comuni_2DIGITOS)) + geom_point()+geom_text(hjust=0, vjust=0)
# 
# 
# # Vendo qual comuni é de qual cluster:
# # Clusters7: usarei o df comuni.dat_7cl.
# comuni_clusters7.7<-c(subset(comuni.dat_7cl, cluster==7, comuni_2DIGITOS))
# comuni_clusters7.1<-c(subset(comuni.dat_7cl, cluster==1, comuni_2DIGITOS))
# comuni_clusters7.2<-c(subset(comuni.dat_7cl, cluster==2, comuni_2DIGITOS))
# comuni_clusters7.3<-c(subset(comuni.dat_7cl, cluster==3, comuni_2DIGITOS))
# comuni_clusters7.4<-c(subset(comuni.dat_7cl, cluster==4, comuni_2DIGITOS))
# comuni_clusters7.5<-c(subset(comuni.dat_7cl, cluster==5, comuni_2DIGITOS))
# comuni_clusters7.6<-c(subset(comuni.dat_7cl, cluster==6, comuni_2DIGITOS))
# # Clusters7: usarei o df comuni.dat_7cl.
# comuni.dat_5cl[cluster==1, comuni_2DIGITOS]
# comuni_clusters5.1<-c(subset(comuni.dat_5cl, cluster==1, comuni_2DIGITOS))
# comuni_clusters5.2<-c(subset(comuni.dat_5cl, cluster==2, comuni_2DIGITOS))
# comuni_clusters5.3<-c(subset(comuni.dat_5cl, cluster==3, comuni_2DIGITOS))
# comuni_clusters5.4<-c(subset(comuni.dat_5cl, cluster==4, comuni_2DIGITOS))
# comuni_clusters5.5<-c(subset(comuni.dat_5cl, cluster==5, comuni_2DIGITOS))
# 
# 
# 
# .subset2(comuni.dat_5cl, "comuni_2DIGITOS")
# 
# # remove(list = c("comuni_clusters5.1", "comuni_clusters5.2",
# #                 "comuni_clusters5.3", "comuni_clusters5.4",
# #                 "comuni_clusters5.5"))
# 
# for (i in 1:5) {
#   assign(x = paste0("comuni_clusters5.", i),
#          value = comuni.dat_5cl[cluster == i, comuni_2DIGITOS])
# }
# 
# 
# library("microbenchmark")
# 
# setkey(comuni.dat_5cl, cluster)
# 
# t0 <- proc.time()
# for (i in 1:100000) a[i] <- 1
# t1 <- proc.time()
# 
# t1 - t0
# 
# 
# microbenchmark(
#   c(subset(comuni.dat_5cl, cluster==i, comuni_2DIGITOS)),
#   comuni.dat_5cl[i, comuni_2DIGITOS]
# )
# 
# comuni.dat_5cl$comuni_2DIGITOS[88]
# 
# length(QTD_contratos_JUNouJUL16_sum)
# length(comuni_3DIGITOS)
# 
# help("clusplot")
# 
# clusplot(comuni.dat2, cl_comuni$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
# 
# df=iris
# m=as.matrix(cbind(df$Petal.Length, df$Petal.Width),ncol=2)
# cl=(kmeans(m,3))
# cl$size
# cl$withinss
# df$cluster=factor(cl$cluster)
# centers=as.data.frame(cl$centers)
# ggplot(data=df, aes(x=Petal.Length, y=Petal.Width, color=cluster )) + 
#   geom_point() + 
#   #, size=52, alpha=.3, legend=FALSE)
# 
# length(df$Petal.Length)
# summary(df)
