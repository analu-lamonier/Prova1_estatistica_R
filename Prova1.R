#///////////////////////// PROVA 1 ////////////////////////////////////////

#1)
RoletaAmericana
FinalMoney <- c()
for (j in 1:1000){
  Money <- 10000 
  RoletaAmericana <- c(1:38)
  
for (k in 1:200){
    jogada <- sample (RoletaAmericana, size = 1, replace = T)
    if (jogada <= 18){
      Money <- Money + 100
      
    }else{
      Money <- Money - 100
    }
    
}
  FinalMoney[j] <- Money
}
FinalMoney  
mean(FinalMoney)


#2)a
papagaio <- read.csv(file = "papagaio.csv", header = TRUE)
str(papagaio)
summary(papagaio)

#b
papagaio_embaralhado = papagaio[sample(1:nrow(papagaio)),]
papagaio_embaralhado

#c
papagaio$especie <- as.factor(papagaio$especie)

#d
table(papagaio$especie)
barplot (table(papagaio$especie))
#O conjunto possui mais papagaios da especie arctica, depois da especie
#cornicuklata e por fim papagaios da especie cirrhata

#e
arctica <- papagaio[papagaio$especie == "arctica",]
cirrhata <- papagaio[papagaio$especie == "cirrhata",]
corniculata <-papagaio[papagaio$especie == "corniculata",]
hist(arctica$peso)
#na especie arctica os papagaios pesam entre 300 e 440

#f
mean(arctica$envergadura)
mean(cirrhata$envergadura)
mean(corniculata$envergadura)

sd(arctica$envergadura)
sd(cirrhata$envergadura)
sd(corniculata$envergadura)

100*sd(arctica$envergadura)/mean(arctica$envergadura)
100*sd(cirrhata$envergadura)/mean(cirrhata$envergadura)
100*sd(corniculata$envergadura)/mean(corniculata$envergadura)

#g
par(mfrow = c(1,3))
boxplot(arctica$peso, na.rm = T, ylim = c(250,920), main = "arctica")
boxplot(cirrhata$peso, na.rm = T, ylim = c(250,920), main = "cirrhata")
boxplot(corniculata$peso, na.rm = T, ylim = c(250,920), main = "corniculata")
#os papagaios da especie cirrhata sao os mais pesados se comparados com os das
#outras especies e existe uma variança maior do peso entre os papagaios cirrhata

#h
par(mfrow = c(1,1))
plot (x = papagaio$peso, y = papagaio$envergadura, type = "n") 

points(x = arctica$peso, y = arctica$envergadura, col = "pink", pch = 16)
points(x = cirrhata$peso, y = cirrhata$envergadura, col = "purple", pch = 16)
points(x = corniculata$peso, y = corniculata$envergadura, col = "blue", pch = 16)

#i
n <- round(0.8*nrow(papagaio_embaralhado)) 
n

treinamento <- papagaio_embaralhado[1:n,]
teste <- papagaio_embaralhado[(n+1):nrow(papagaio_embaralhado), ]


#j
classificacao <- c()
for(j in 1:nrow(teste)){
  if(teste$peso[j] < 500 && teste$envergadura < 55){
    classificacao[j] <- "arctica"
  }else{
    if(teste$peso[j] < 700 && teste$envergadura < 60){
      classificacao[j] <- "corniculata"
    }else{
      classificacao[j] <- "cirrhata"
    }
  }
  
  
}

classificacao

#k
mean(classificacao == teste$especie) 
