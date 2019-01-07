

# Limpando a memória
rm(list = ls(all.names = T))

# Instalando e carregando pacotes
#install.packages("install.load")
library(install.load)

install_load("readxl","rgdal","leaflet","RColorBrewer","dplyr","curl", "ggplot2","gridExtra", "lme4", 
             "lattice","lmtest", "stargazer", "car", "plm")

# Indicando os diretórios
wd1 <- "E:/TCC/Dados/SES/"
wd2 <- "E:/TCC/Dados/SIAB/"
wd3 <- "E:/TCC/Dados/SNIS/" 
wd4 <- "E:/TCC/Dados/rj_municipios/"
wd5 <- "E:/TCC/Dados/sociodemo/"
wd6 <- "E:/TCC/Dados/censo_2010/"


#--------------------------------------- Carregando arquivos  --------------------------------------#
#####

# SES-RJ

filenames <- list.files(wd1, pattern="*.csv", full.names = T)
ldf <- lapply(filenames, function(x) read.csv2(x, stringsAsFactors = F))
for(i in 1:length(ldf)){
  ldf[[i]] <- ldf[[i]][-c(93:107),-16]
  ldf[[i]][,1] <- toupper(chartr("áéíóúâêôãõçÁÉÍÓÚÂÊÔÃÕÇ",
                                 "aeiouaeoaocAEIOUAEOAOC",ldf[[i]][,1]))
}

# Faltantes
ldf[[1]][,-1] <- data.frame(t(apply(ldf[[1]][,-1], 1, function(x) 
  round(ifelse(x == 0, mean(subset(x, x > 0)), x),1))))
ldf[[2]][,-1] <- data.frame(t(apply(ldf[[2]][,-1], 1, function(x) 
  round(ifelse(x == 0, mean(subset(x, x > 0)), x),1))))


morb <- (ldf[[1]][,-1]+ldf[[2]][,-1])/100 # taxa por mil habitantes
morb$Municipio <- ldf[[1]][,1]


# SIAB

filenames <- list.files(wd2, pattern="*.csv", full.names = T)
ldf <- lapply(filenames, function(x) read.csv2(x, stringsAsFactors = F))
for(i in 1:length(ldf)){
  ldf[[i]] <- ldf[[i]][-c(93:95),]
  ldf[[i]][,1] <- toupper(chartr("áéíóúâêôãõçÁÉÍÓÚÂÊÔÃÕÇ",
                                 "aeiouaeoaocAEIOUAEOAOC",
                                 substr(ldf[[i]][,1],8,nchar(ldf[[i]][,1]))))
}
for(i in c(1,2,4)){
  ldf[[i]][,-1] <- 100*ldf[[i]][,-1]/ldf[[3]][,-1]
  ldf[[i]][,-1] <- data.frame(apply(ldf[[i]][,-1],2,
                                    function(x) ifelse(is.na(x),0,x)), stringsAsFactors = F)
}

por_ano <- list()
for(i in 2:ncol(ldf[[1]])){
  por_ano[[i-1]] <- data.frame(Municipio = ldf[[1]][,1], stringsAsFactors = F)
  por_ano[[i-1]] <- data.frame(cbind(por_ano[[i-1]],ldf[[1]][,i],ldf[[2]][,i],ldf[[4]][,i]),
                               stringsAsFactors = F)
  names(por_ano[[i-1]]) <- c("Municipio","Agua","Esgoto","Lixo")
}


# SNIS
# 
# filenames <- list.files(wd3, pattern="*.csv", full.names = T)
# ldf <- lapply(filenames, function(x) read.csv2(x, stringsAsFactors = F))
# for(i in 1:length(ldf)){
#   ldf[[i]] <- ldf[[i]][-c(93:95),]
#   ldf[[i]][,2] <- toupper(chartr("áéíóúâêôãõçÁÉÍÓÚÂÊÔÃÕÇ",
#                                  "aeiouaeoaocAEIOUAEOAOC",ldf[[i]][,2]))
#   names(ldf[[i]])[-c(1:7)] <- c("Pop_Tot","Pop_urb","Pop_agua","agua_prod","agua_trat","Pop_urb_agua",
#                                 "Pop_esgoto","esgoto_colet","esgoto_trat","Pop_urb_esgoto","Receita_agua",
#                                 "Receita_esgoto","Receita_agua_esgoto","Despesa_agua_esgoto","Reclamacoes",
#                                 "Servicos","Pop-urb_residuos","Residuos_colet","Pop_diaria","Pop_2.3_sem",
#                                 "Pop_1_sem","Pop_residuos","Coleta_sel","Despesa_residuos","Receita_residuos")
#   ldf[[i]][-c(1:7,30)] <- data.frame(apply(ldf[[i]][-c(1:7,30)],2,
#                                         function(x) ifelse(is.na(x),0,x)), stringsAsFactors = F)
#   names(ldf[[i]])[2] <- "Municipio"
#   ldf[[i]][87,2] <- "TRAJANO DE MORAES"
# }
# 
# despesas_2010 <- data.frame(apply(ldf[[10]][,c(8,21,31)],1,function(x) sum(x[c(2,3)])/x[1]))
# despesas_2010$Municipio <- ldf[[10]]$Municipio
# names(despesas_2010) <- c("Despesas","Municipio")

# IBGE - Censo 2010

# filenames <- list.files(wd6, pattern="*.csv", full.names = T)
# sanea_2010 <- lapply(filenames, function(x) read.csv2(x, stringsAsFactors = F))
# sanea_2010 <- data.frame(Municipio = toupper(chartr("áéíóúâêôãõçÁÉÍÓÚÂÊÔÃÕÇ",
#                                                     "aeiouaeoaocAEIOUAEOAOC",sanea_2010[[1]][,1])),
#                          Agua = sanea_2010[[1]]$Total/sanea_2010[[2]]$Total,
#                          Esgoto = sanea_2010[[3]]$Total/sanea_2010[[2]]$Total,
#                          Lixo = sanea_2010[[4]]$Total/sanea_2010[[2]]$Total,
#                          stringsAsFactors = F)

# Socioeconômicos

setwd(wd5)
socec <- read_excel("atlas2013_dadosbrutos_pt.xlsx",2)
socec <- filter(socec, ANO == 2010, UF == 33)
socec <- select(socec, Município, FECTOT, T_ANALF18M, PREN20, GINI, PMPOB, 
                RMPOB, IDHM, T_BANAGUA)
socec$Município <- toupper(chartr("áéíóúâêôãõçÁÉÍÓÚÂÊÔÃÕÇ",
                        "aeiouaeoaocAEIOUAEOAOC",socec$Município))
names(socec)[1] <- "Municipio"


#fns <- read.csv2("PlanilhaPesquisaSimplificadaTodosEstados.csv", stringsAsFactors = F)
#fns <- select(fns, UF.MUNICIPIO,ENTIDADE, Componente, Valor.Total,Valor.Liquido)

##### 
#------------------------------------- Construindo dataframes --------------------------------------#
#####

# Dados Longitudinais
dados_long <- data.frame(Municipio = rep(por_ano[[1]]$Municipio,14), 
                         Ano = c(rep(2001,92),rep(2002,92),rep(2003,92),rep(2004,92),
                         rep(2005,92),rep(2006,92),rep(2007,92),rep(2008,92),rep(2009,92),
                         rep(2010,92),rep(2011,92),rep(2012,92),rep(2013,92),rep(2014,92)),
                         Morbidade = unlist(lapply(morb[,-15],function(x) x)),
                         Agua = unlist(lapply(por_ano, function(x) x$Agua)),
                         Esgoto = unlist(lapply(por_ano, function(x) x$Esgoto)),
                         Lixo = unlist(lapply(por_ano, function(x) x$Lixo)),
                         Subject = rep(1:92,14), 
                         stringsAsFactors = F)
dados_long$Time <- dados_long$Ano - 2000
dados_long$Morb_transf <- log(dados_long$Morbidade)

#####
# Imputação de dados
#####

# Faltantes
zeros <- unique(filter(dados_long, Agua == 0)$Subject)
for(i in 1:length(zeros)){
  sub <- filter(dados_long, Subject == zeros[i])
  model <- lm(Agua ~ Morbidade + Time, data=sub, subset = Agua > 0)
  dados_long[which(dados_long$Subject == zeros[i]),"Agua"] <- ifelse(sub$Agua == 0, 
                                                                     predict (model, sub), 
                                                                     sub$Agua)
  sub <- filter(dados_long, Subject == zeros[i])
  model <- lm(Esgoto ~ Morbidade + Time, data=sub, subset = Esgoto > 0)
  dados_long[which(dados_long$Subject == zeros[i]),"Esgoto"] <- ifelse(sub$Esgoto == 0, 
                                                                     predict (model, sub), 
                                                                     sub$Esgoto)
  sub <- filter(dados_long, Subject == zeros[i])
  model <- lm(Lixo ~ Morbidade + Time, data=sub, subset = Lixo > 0)
  dados_long[which(dados_long$Subject == zeros[i]),"Lixo"] <- ifelse(sub$Lixo == 0, 
                                                                     predict (model, sub), 
                                                                     sub$Lixo)
}

#Anos 2009 e 2010 para o Rio de Janeiro
sub <- filter(dados_long, Subject == 68)
model <- lm(Agua ~ Morbidade + Time, data=sub, subset = Ano != c(2009,2010))
dados_long[which(dados_long$Subject == 68),"Agua"] <- ifelse(sub$Ano == c(2009,2010), 
                                                             predict (model, sub), 
                                                             sub$Agua)
sub <- filter(dados_long, Subject == 68)
model <- lm(Esgoto ~ Morbidade + Time, data=sub, subset = Ano != c(2009,2010))
dados_long[which(dados_long$Subject == 68),"Esgoto"] <- ifelse(sub$Ano == c(2009,2010), 
                                                               predict (model, sub), 
                                                               sub$Esgoto)
sub <- filter(dados_long, Subject == 68)
model <- lm(Lixo ~ Morbidade + Time, data=sub, subset = Ano != c(2009,2010))
dados_long[which(dados_long$Subject == 68),"Lixo"] <- ifelse(sub$Ano == c(2009,2010), 
                                                             predict (model, sub), 
                                                             sub$Lixo)
#####
# Dados em cross section  - 2010
#####

dados_2010 <- full_join(socec, morb[,c(10,15)], by = "Municipio")
dados_2010$Morb_transf <- log(dados_2010$X2010)
sanea_2010 <- select(filter(dados_long, Ano == 2010),Municipio, Agua, Esgoto, Lixo)
dados_2010 <- full_join(dados_2010, sanea_2010, by="Municipio")

setwd("E:/TCC/Dados/")
write.csv2(dados_2010,"dados_crossection.csv",row.names = F)
write.csv2(dados_long,"dados_longitudinais.csv",row.names = F)

rm(list=c("morb","sanea_2010","socec","sub","ldf","por_ano"))
#####
#--------------------------------------- Análises descritivas --------------------------------------#

# --------------- 2010 -------------------------

# Summary
resumo <- t(data.frame(sapply(dados_2010[,-1],summary)))
stargazer(resumo)

#####
# Histogramas e Boxplots
#####
grid.arrange(qplot(data= dados_2010, x = X2010, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = X2010, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)


grid.arrange(qplot(data= dados_2010, x = Morb_transf, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = Morb_transf, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)


grid.arrange(qplot(data= dados_2010 , x = Agua, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = Agua, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

grid.arrange(qplot(data= dados_2010, x = Esgoto, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = Esgoto, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

grid.arrange(qplot(data= dados_2010, x = Lixo, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = Lixo, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

grid.arrange(qplot(data= dados_2010, x = IDHM, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = IDHM, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

grid.arrange(qplot(data= dados_2010, x = RMPOB, ylab="", xlab="") +
               geom_histogram(colour = "black", fill="slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = RMPOB, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

grid.arrange(qplot(data= dados_2010, x = T_ANALF18M, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = T_ANALF18M, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

grid.arrange(qplot(data= dados_2010, x = PREN20, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = PREN20, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

grid.arrange(qplot(data= dados_2010, x = T_BANAGUA, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = T_BANAGUA, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

grid.arrange(qplot(data= dados_2010, x = GINI, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = GINI, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

grid.arrange(qplot(data= dados_2010, x = PMPOB, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= dados_2010, y = PMPOB, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)

#####
# Scatter plot vs original
#####
ggplot(data = dados_2010, aes(y = X2010, x = Agua)) + xlab("Abastecimento de água") + ylab("Morbidade") +
  geom_point()  +  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = X2010, x = Esgoto)) + xlab("Esgotamento sanitário") + ylab("Morbidade") +
    geom_point() +
  theme_bw() + theme(panel.grid.major = element_blank())
  
ggplot(data = dados_2010, aes(y = X2010, x = Lixo)) + xlab("Coleta de lixo") + ylab("Morbidade") +
    geom_point() +
  theme_bw() + theme(panel.grid.major = element_blank())
  
ggplot(data = dados_2010, aes(y = X2010, x = PMPOB)) + xlab("Proporção da população pobre") + ylab("Morbidade") +
  geom_point() + 
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = X2010, x = RMPOB)) + xlab("Renda média da população pobre") + ylab("Morbidade") +
  geom_point() +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = X2010, x = IDHM)) + xlab("IDHM") + ylab("Morbidade") +
  geom_point() + 
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = X2010, x = T_BANAGUA)) + xlab("Proporção com banheiro e encanamento") + ylab("Morbidade") +
  geom_point() +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = X2010, x = PREN20)) + xlab("Proporção da renda apropriada pelos 20% mais pobres") + ylab("Morbidade") +
  geom_point() +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = X2010, x = T_ANALF18M)) + xlab("Taxa de analfabetismo") + ylab("Morbidade") +
  geom_point()  +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = X2010, x = GINI)) + xlab("Índice de Gini") + ylab("Morbidade") +
  geom_point() +
  theme_bw() + theme(panel.grid.major = element_blank())

#####
# Scatter plot vs transformada
#####
ggplot(data = dados_2010, aes(y = Morb_transf, x = Agua)) + xlab("Abastecimento de água") + ylab("Log(Morbidade)") +
  geom_point() + 
theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = Morb_transf, x = Esgoto)) + xlab("Esgotamento sanitário") + ylab("Log(Morbidade)") +
  geom_point() + 
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = Morb_transf, x = Lixo)) + xlab("Coleta de lixo") + ylab("Log(Morbidade)") +
  geom_point() +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = Morb_transf, x = PMPOB)) + xlab("Proporção da população pobre") + ylab("Log(Morbidade)") +
  geom_point() + 
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = Morb_transf, x = RMPOB)) + xlab("Renda média da população pobre") + ylab("Log(Morbidade)") +
  geom_point() +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = Morb_transf, x = IDHM)) + xlab("IDHM") + ylab("Log(Morbidade)") +
  geom_point() + 
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = Morb_transf, x = T_BANAGUA)) + xlab("Proporção com banheiro e encanamento") + ylab("Log(Morbidade)") +
  geom_point() +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = Morb_transf, x = PREN20)) + xlab("Proporção da renda apropriada pelos 20% mais pobres") + ylab("Log(Morbidade)") +
  geom_point() + 
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = Morb_transf, x = T_ANALF18M)) + xlab("Taxa de analfabetismo") + ylab("Log(Morbidade)") +
  geom_point() + 
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(data = dados_2010, aes(y = Morb_transf, x = GINI)) + xlab("Índice de Gini") + ylab("Log(Morbidade)") +
  geom_point() + 
  theme_bw() + theme(panel.grid.major = element_blank())


#####
# Matriz de correlaçao em 2010
#####
correl <- cor(dados_2010[,-1])
levelplot(abs(correl),
          main = list(label = "Matriz de correlação", cex = 2),
          xlab = "", ylab="",
          col.regions = colorRampPalette(c("blue","yellow","red","darkred")))

#####
# --------------- 2001 a 2014 -----------------------------------------------------

resumo <- t(data.frame(sapply(dados_long[,-1],summary)))
row.names(resumo) <- dados_long$Municipio
stargazer(resumo)

#####
# Múltiplas séries temporais
#####
ggplot(dados_long, aes(y = Morbidade, x = Ano)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morb_transf, x = Ano)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())

#####
# Scatter plot vs originais
#####
# ggplot(subset(dados_long, Subject %in% 1:16), aes(y = Morbidade, x = Lixo)) + 
#   geom_point() + facet_wrap(~Subject) +
#   theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morbidade, x = Agua)) + geom_point() + 
  facet_wrap(~Subject) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morbidade, x = Esgoto)) + geom_point() + 
  facet_wrap(~Subject) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morbidade, x = Lixo)) + geom_point() + 
  facet_wrap(~Subject) +
  theme_bw() + theme(panel.grid.major = element_blank())



ggplot(dados_long, aes(y = Morbidade, x = Agua)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morbidade, x = Esgoto)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morbidade, x = Lixo)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())
 


#####
# Scatter plot vs modificadas
#####
# ggplot(subset(dados_long, Subject %in% 1:16), aes(y = Morbidade, x = Lixo)) + 
#   geom_point() + facet_wrap(~Subject) +
#   theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morb_transf, x = Agua)) + geom_point() + 
  facet_wrap(~Subject) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morb_transf, x = Esgoto)) + geom_point() + 
  facet_wrap(~Subject) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morb_transf, x = Lixo)) + geom_point() + 
  facet_wrap(~Subject) +
  theme_bw() + theme(panel.grid.major = element_blank())



ggplot(dados_long, aes(y = Morb_transf, x = Agua)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morb_transf, x = Esgoto)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long, aes(y = Morb_transf, x = Lixo)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())

#####
# Added Scater plot
#####

dados_long_modif <- mutate(dados_long, 
                           Morb_media = Morbidade - mean(Morbidade),
                           Agua_media = Agua - mean(Agua),
                           Esgoto_media = Esgoto - mean(Esgoto),
                           Lixo_media = Lixo - mean(Lixo))

ggplot(dados_long_modif, aes(y = Morb_media, x = Agua_media)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long_modif, aes(y = Morb_media, x = Esgoto_media)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())

ggplot(dados_long_modif, aes(y = Morb_media, x = Lixo_media)) + 
  geom_point(aes(group=Subject, color=Subject)) + 
  geom_line(aes(group=Subject, color=Subject)) +
  theme_bw() + theme(panel.grid.major = element_blank())


#####
#--------------------------------------------- Mapas  ----------------------------------------------#
#####
#--------------------------- Casos de dengue de 2001 a 2016  ----------------------------------#
# Carregando shapefile
setwd(wd4)

mapa_rj <- readOGR("33MUE250GC_SIR.shp",layer = "33MUE250GC_SIR", 
                  verbose = FALSE, encoding = "windows1252")

mapa_rj$NM_MUNICIP <- toupper(chartr("áéíóúâêôãõçÁÉÍÓÚÂÊÔÃÕÇ",
                                    "aeiouaeoaocAEIOUAEOAOC",mapa_rj$NM_MUNICIP))
#mapa_rj$NM_MUNICIP[87] <- "TRAJANO DE MORAIS"

# em 2000 não tinha a cidade de mesquita
mapa_rj_2001 <- merge(mapa_rj,morb[,c(1,15)], by.x="NM_MUNICIP", by.y="Municipio")
mapa_rj_2001 <- merge(mapa_rj_2001,por_ano[[1]], by.x="NM_MUNICIP", by.y="Municipio")
mapa_rj_2001 <- merge(mapa_rj_2001,ldf[[1]], by.x="NM_MUNICIP", by.y="Município")

mapa_rj_2014 <- merge(mapa_rj,morb[,c(14,15)], by.x="NM_MUNICIP", by.y="Municipio")
mapa_rj_2014 <- merge(mapa_rj_2014,por_ano[[14]], by.x="NM_MUNICIP", by.y="Municipio")
mapa_rj_2014 <- merge(mapa_rj_2014,ldf[[14]], by.x="NM_MUNICIP", by.y="Município")

#--------------------------- Domicílios atendidos - 2001  ----------------------------------#
#água
pal <- colorNumeric(palette = "Greys", 
                    domain = mapa_rj_2001$Agua, 6)
pal1 <- pal(mapa_rj_2001$Agua)
colorData <- mapa_rj_2001$Agua
titulo <- "Percentual"

#esgoto
pal <- colorNumeric(palette = "Greys", 
                    domain = mapa_rj_2001$Esgoto, 6)
pal1 <- pal(mapa_rj_2001$Esgoto)
colorData <- mapa_rj_2001$Esgoto
titulo <- "Percentual"

#resíduos
pal <- colorNumeric(palette = "Greys", 
                    domain = mapa_rj_2001$Lixo, 6)
pal1 <- pal(mapa_rj_2001$Lixo)
colorData <- mapa_rj_2001$Lixo
titulo <- "Percentual"

#morbidade
pal <- colorNumeric(palette = "Greys", 
                    domain = mapa_rj_2001$X2001, 6)
pal1 <- pal(mapa_rj_2001$X2001)
colorData <- mapa_rj_2001$X2001
titulo <- "Percentual"

#mapa
leaflet(mapa_rj_2001) %>%
  addPolygons(
    stroke = T, weight = 0.6, color = "#051A21",fillOpacity = 0.9, 
    smoothFactor = 0, fill = T, fillColor = ~pal1)  %>%  
  addLegend("bottomright", pal=pal, values= ~colorData, title = titulo)



#--------------------------- Domicílios atendidos - 2014  ----------------------------------#
#água
pal <- colorNumeric(palette = "Greys", 
                    domain = mapa_rj_2014$Agua, 6)
pal1 <- pal(mapa_rj_2014$Agua)
colorData <- mapa_rj_2014$Agua
titulo <- "Percentual"

#esgoto
pal <- colorNumeric(palette = "Greys", 
                    domain = mapa_rj_2014$Esgoto, 6)
pal1 <- pal(mapa_rj_2014$Esgoto)
colorData <- mapa_rj_2014$Esgoto
titulo <- "Percentual"

#resíduos
pal <- colorNumeric(palette = "Greys", 
                    domain = mapa_rj_2014$Lixo, 6)
pal1 <- pal(mapa_rj_2014$Lixo)
colorData <- mapa_rj_2014$Lixo
titulo <- "Percentual"

#morbidade
pal <- colorNumeric(palette = "Greys", 
                    domain = mapa_rj_2014$X2014, 6)
pal1 <- pal(mapa_rj_2014$X2014)
colorData <- mapa_rj_2014$X2014
titulo <- "Percentual"

#mapa
leaflet(mapa_rj_2014) %>%
  addPolygons(
    stroke = T, weight = 0.8, color = "#051A21",fillOpacity = 0.9, 
    smoothFactor = 0, fill = T, fillColor = ~pal1)  %>%  
  addLegend("bottomright", pal=pal, values= ~colorData, title = titulo)




#--------------------------- Populações atendidas - 2001  ----------------------------------#
#água
pal <- colorNumeric(palette = "Greys", 
                    domain = (100000*mapa_rj_2001$Despesa_agua_esgoto/mapa_rj_2001$Pop_Tot), 6)
pal1 <- pal((100000*mapa_rj_2001$Despesa_agua_esgoto/mapa_rj_2001$Pop_Tot))
colorData <- (100000*mapa_rj_2001$Despesa_agua_esgoto/mapa_rj_2001$Pop_Tot)
titulo <- "Percentual"

#esgoto
pal <- colorNumeric(palette = "Greys", 
                    domain = (100*mapa_rj_2001$Receita_agua_esgoto/mapa_rj_2001$Pop_Tot), 6)
pal1 <- pal((100*mapa_rj_2001$Receita_agua_esgoto/mapa_rj_2001$Pop_Tot))
colorData <- (100*mapa_rj_2001$Receita_agua_esgoto/mapa_rj_2001$Pop_Tot)
titulo <- "Percentual"

#resíduos
pal <- colorNumeric(palette = "YlGnBu", 
                    domain = (100*mapa_rj_2001$Pop_residuos/mapa_rj_2001$Pop_Tot), 6)
pal1 <- pal((100*mapa_rj_2001$Pop_residuos/mapa_rj_2001$Pop_Tot))
colorData <- (100*mapa_rj_2001$Pop_residuos/mapa_rj_2001$Pop_Tot)
titulo <- "Percentual"


#mapa
leaflet(mapa_rj_2001) %>%
  addPolygons(
    stroke = T, weight = 0.6, color = "#051A21",fillOpacity = 0.9, 
    smoothFactor = 0, fill = T, fillColor = ~pal1)  %>%  
  addLegend("bottomright", pal=pal, values= ~colorData, title = titulo)



#--------------------------- Populações atendidas - 2014  ----------------------------------#
#água
pal <- colorNumeric(palette = "Greys", 
                    domain = (100000*mapa_rj_2014$Despesa_agua_esgoto/mapa_rj_2014$Pop_Tot), 6)
pal1 <- pal((100000*mapa_rj_2014$Despesa_agua_esgoto/mapa_rj_2014$Pop_Tot))
colorData <- (100000*mapa_rj_2014$Despesa_agua_esgoto/mapa_rj_2014$Pop_Tot)
titulo <- "Percentual"

#esgoto
pal <- colorNumeric(palette = "Greys", 
                    domain = (100*mapa_rj_2014$Receita_agua_esgoto/mapa_rj_2014$Pop_Tot), 6)
pal1 <- pal((100*mapa_rj_2014$Receita_agua_esgoto/mapa_rj_2014$Pop_Tot))
colorData <- (100*mapa_rj_2014$Receita_agua_esgoto/mapa_rj_2014$Pop_Tot)
titulo <- "Percentual"

#resíduos
pal <- colorNumeric(palette = "Greys", 
                    domain = (100*mapa_rj_2014$Pop_residuos/mapa_rj_2014$Pop_Tot), 6)
pal1 <- pal((100*mapa_rj_2014$Pop_residuos/mapa_rj_2014$Pop_Tot))
colorData <- (100*mapa_rj_2014$Pop_residuos/mapa_rj_2014$Pop_Tot)
titulo <- "Percentual"


#mapa
leaflet(mapa_rj_2014) %>%
  addPolygons(
    stroke = T, weight = 0.6, color = "#051A21",fillOpacity = 0.9, 
    smoothFactor = 0, fill = T, fillColor = ~pal1)  %>%  
  addLegend("bottomright", pal=pal, values= ~colorData, title = titulo)


#####
#------------------------------------- Componentes principais --------------------------------------#

#Ranking
#####
cp <- princomp(dados_2010[,-c(1,5)], cor=T)
summary(cp, loadings = TRUE)
qplot(y=as.numeric((cp$sdev)^2),x=factor(c(1:12)),main="Scree plot",
      xlab = "Componentes",ylab="Variância explicada (%)") + 
  geom_bar(stat="identity", fill = "slategray1") + 
  geom_point() + geom_line(aes(x=c(1:12)), size=1)+
  theme_bw() + theme(panel.grid.major = element_blank())

# Calculo dos escores (componentes principais)
escores <- data.frame(cp$scores[,1],stringsAsFactors = F)
escores[,2] <- dados_2010$Municipio
names(escores) <- c("Escore", "Municipio")
ranking <- arrange(escores, Escore)

qplot(y = escores$Escore, x=factor(""),ylab="",xlab="", 
      main="Componente principal 1") + 
  geom_boxplot(colour = "black", fill ="gray88" )+
  theme_bw() + theme(panel.grid.major = element_blank())

#####
#Redução de dimensão
#####

# Calculo dos escores (componentes principais)
escores <- data.frame(cp$scores[,1:4],stringsAsFactors = F)
escores[,5] <- dados_2010$Municipio
names(escores) <- c("Escore1","Escore2","Escore3","Escore4", "Municipio")
escores$saude <- dados_2010$X2010
escores$saude_modif <- dados_2010$Morb_transf

# Modelo com as componentes principais
model1 <- lm(saude_modif ~ Escore1 + Escore2 + Escore3 + Escore4, data = escores)
summary(model1)
AIC(model1)

# Retornando os coeficientes
loadings <- cp$loadings[,1:4]
coef_model <- model1$coefficients[-1]
coef <- loadings%*%coef_model

coef_model <- model2$coefficients[-1]
coef <- loadings%*%coef_model

#####
#----------------------------------- Modelo Linear Generalizado ------------------------------------#
#####

# model <- glm(X2010 ~ Agua + Esgoto + Lixo + RDPC + IDHM + PMPOB, 
#              family = poisson(link="log"), data = dados_2010)
# summary(model)
# 
# model <- glm(X2010 ~ Agua + Esgoto + Lixo + RDPC + IDHM, 
#              family = poisson(link="log"), data = dados_2010)
# summary(model)
# 
# model <- glm(X2010 ~ Esgoto + Lixo + RDPC + IDHM, 
#              family = poisson(link="log"), data = dados_2010)
# summary(model)
#####

#####
# Modelo 1 - Poisson
#####

model1 <- glm(round(X2010,0) ~  Lixo + IDHM - 1, 
              family = poisson(link="log"), data = dados_2010)
summary(model1)


rsd <- data.frame(residuals(model1))
ggplot(data=rsd,aes(y=residuals.model1.,x=rownames(rsd))) + geom_point()
ggplot(data=rsd,aes(y=residuals.model1.,x=factor(""))) + geom_boxplot()
ggplot(data=rsd,aes(x=residuals.model1.)) + geom_histogram(colour="black")

av.plots(model1)
cutoff <- 4/((nrow(dados_2010)-length(model1$coefficients)-2)) 
plot(model1, which=4, cook.levels=cutoff)
plot(cooks.distance(model1))
sqrt(vif(model1))

##### Não ficou bom
# Modelo 2 - Gamma
#####
model2 <- glm(X2010 ~  Lixo   - 1, 
             family = Gamma(link="inverse"), data = dados_2010)
summary(model2)


rsd <- data.frame(residuals(model2))
ggplot(data=rsd,aes(y=residuals.model2.,x=rownames(rsd))) + geom_point()
ggplot(data=rsd,aes(y=residuals.model2.,x=factor(""))) + geom_boxplot()
ggplot(data=rsd,aes(x=residuals.model2.)) + geom_histogram(colour="black")

av.plots(model2)
cutoff <- 4/((nrow(dados_2010)-length(model2$coefficients)-2)) 
plot(model2, which=4, cook.levels=cutoff)
plot(cooks.distance(model2))
# Influence Plot 
sqrt(vif(model2))

#####
# Modelo 3 - Normal
#####
model3 <- lm(Morb_transf ~ Lixo , data= dados_2010)
summary(model3)

# Resíduos
rsd <- data.frame(residuals(model3))
ggplot(data=rsd,aes(y=residuals.model3.,x=rownames(rsd))) + geom_point()
ggplot(data=rsd,aes(y=residuals.model3.,x=factor(""))) + geom_boxplot()
ggplot(data=rsd,aes(x=residuals.model3.)) + geom_histogram(colour="black")
shapiro.test(rsd$residuals.model3.)
qqnorm(rsd$residuals.model3.)

# Pontos influentes
av.plots(model3)
rcutoff <- 4/((nrow(dados_2010)-length(model3$coefficients)-2)) 
plot(model3, which=4, cook.levels=cutoff)
plot(cooks.distance(model3))

#####
#--------------------------------------- Dados Longitudinais ---------------------------------------#
#####

pmodel1 <- plm(Morb_transf ~ Lixo + Esgoto, data = dados_long, model="within")
fixef(pmodel1, type = 'dmean')
summary(fixef(pmodel1, type = 'dmean'))
summary(pmodel1)
