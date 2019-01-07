## Apenas estudando os cluster13dígitos ##

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
wd<-"C:\\Users\\phohberg\\Desktop\\Jobs\\Job Breno\\Repensando o CP\\Final - modelos prontos\\Bases finais"
setwd(wd)

# Lendo as bases de dados  #
# cluster1 #
cluster1<-"201605 - elegíveis - cluster1 - comunicações - v6.sav"
# essa base tem os totais de cluster1cação #
cluster1.base=read.spss(cluster1)
c1.dat<-as.data.frame(cluster1.base)

c1.dat.agg<-aggregate(cbind(ContratouJUNouJUL,qtd) ~ PRE_1,
                         cbind(c1.dat,qtd=rep(1,nrow(c1.dat))),sum)

# Ajeitando a questão do percentil #
  prob.c1<-c1.dat$PRE_1
  max(prob.c1)
  min(prob.c1)
  # Percentis - definindo os percentis #
    percentis.valores<-quantile(prob.c1, probs = seq(0, 1, by= 0.1))
    percentis.nomes<-c(names(quantile(prob.c1, probs = seq(0, 1, by= 0.1))))
    length(percentis)
  # which(x<=2 & x<=1)
  # # Valores #
  #   # p1
  #     which(prob.c1>=percentis.valores[1] & prob.c1<=percentis.valores[2])
  #     qtd.p1<-length(which(prob.c1>=percentis.valores[1] & prob.c1<=percentis.valores[2]))
  #     qtd.p1
  #     qtd.p1/length(prob.c1)
  
      porcentagens<-NULL
      quantidades<-NULL
      i=1
      for (i in 1:10)
      {
        quantidades<-c(quantidades,
                       length(which(c1.dat.agg$PRE_1>=percentis.valores[i] & base1.dat.agg$PRE_1<percentis.valores[i+1])))
        # which(prob.c1>=percentis.valores[i] & prob.c1<=percentis.valores[i+1])
        porcentagens<-c(porcentagens,quantidades[i]/nrol(prob.c1))
      }
      quantidades
      porcentagens
      length(quantidades)
      length(porcentagens)
      
  # Criando uma tabela #
      # nomes #
      nomes<-NULL
      valores<-NULL
      i=1
      for (i in 1:10)
      {
        nomes<-c(nomes,paste(percentis.nomes[i],percentis.nomes[i+1], sep="-"))
        valores<-c(valores,paste(round(percentis.valores[i],4),round(percentis.valores[i+1],4),sep="-"))
      }
      # length(nomes)
      # length(valores)
      tabela.percentis.c1<-cbind(nomes,valores,quantidades,porcentagens)
      colnames(tabela.percentis.c1)<-c("Percentis","Faixas","Quantidades","%")
      class(tabela.percentis.c1)
      write.csv2(tabela.percentis.c1,"cluster1.csv", row.names=F)
#------------------------------------------------------------------------------------------------------------------#
      # cluster2 #
      cluster2<-"201605 - elegíveis - cluster2 - comunicações - v6.sav"
      # essa base tem os totais de cluster2cação #
      cluster2.base=read.spss(cluster2)
      c2.dat<-as.data.frame(cluster2.base)
      head(c2.dat)
      
      # Ajeitando a questão do percentil #
      prob.c2<-c2.dat$PRE_1
      max(prob.c2)
      min(prob.c2)
      # Percentis #
      percentis.valores<-quantile(prob.c2, probs = seq(0, 1, by= 0.1))
      percentis.nomes<-c(names(quantile(prob.c2, probs = seq(0, 1, by= 0.1))))
      length(percentis)

      porcentagens<-NULL
      quantidades<-NULL
      i=1
      for (i in 1:10)
      {
        quantidades<-c(quantidades,length(which(prob.c2>=percentis.valores[i] & prob.c2<percentis.valores[i+1])))
        # which(prob.c2>=percentis.valores[i] & prob.c2<=percentis.valores[i+1])
        porcentagens<-c(porcentagens,quantidades[i]/length(prob.c2))
      }
      quantidades
      porcentagens
      length(quantidades)
      length(porcentagens)
      
      # Criando uma tabela #
      # nomes #
      nomes<-NULL
      valores<-NULL
      i=1
      for (i in 1:10)
      {
        nomes<-c(nomes,paste(percentis.nomes[i],percentis.nomes[i+1], sep="-"))
        valores<-c(valores,paste(round(percentis.valores[i],4),round(percentis.valores[i+1],4),sep="-"))
      }
      # length(nomes)
      # length(valores)
      tabela.percentis.c2<-cbind(nomes,valores,quantidades,porcentagens)
      colnames(tabela.percentis.c2)<-c("Percentis","Faixas","Quantidades","%")
      class(tabela.percentis.c2)
      write.csv2(tabela.percentis.c2,"cluster2.csv", row.names=F)
      #------------------------------------------------------------------------------------------------------------------#
      # cluster3 #
      cluster3<-"201605 - elegíveis - cluster3 - comunicações - v6.sav"
      # essa base tem os totais de cluster3cação #
      cluster3.base=read.spss(cluster3)
      c3.dat<-as.data.frame(cluster3.base)
      head(c3.dat)
      
      # Ajeitando a questão do percentil #
      prob.c3<-c3.dat$PRE_1
      max(prob.c3)
      min(prob.c3)
      # Percentis #
      percentis.valores<-quantile(prob.c3, probs = seq(0, 1, by= 0.1))
      percentis.nomes<-c(names(quantile(prob.c3, probs = seq(0, 1, by= 0.1))))
      length(percentis)

      porcentagens<-NULL
      quantidades<-NULL
      i=1
      for (i in 1:10)
      {
        quantidades<-c(quantidades,length(which(prob.c3>=percentis.valores[i] & prob.c3<percentis.valores[i+1])))
        # which(prob.c3>=percentis.valores[i] & prob.c3<=percentis.valores[i+1])
        porcentagens<-c(porcentagens,quantidades[i]/length(prob.c3))
      }
      quantidades
      porcentagens
      length(quantidades)
      length(porcentagens)
      
      # Criando uma tabela #
      # nomes #
      nomes<-NULL
      valores<-NULL
      i=1
      for (i in 1:10)
      {
        nomes<-c(nomes,paste(percentis.nomes[i],percentis.nomes[i+1], sep="-"))
        valores<-c(valores,paste(round(percentis.valores[i],4),round(percentis.valores[i+1],4),sep="-"))
      }
      # length(nomes)
      # length(valores)
      tabela.percentis.c3<-cbind(nomes,valores,quantidades,porcentagens)
      colnames(tabela.percentis.c3)<-c("Percentis","Faixas","Quantidades","%")
      class(tabela.percentis.c3)
      write.csv2(tabela.percentis.c3,"cluster3.csv", row.names=F)
      #------------------------------------------------------------------------------------------------------------------#

      
      
