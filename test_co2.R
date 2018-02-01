setwd("C:/Users/Emilie/Downloads/WDI_csv")

library(readr)
library(dplyr)
library(tidyr)
library(missMDA)
library(FactoMineR)
library(ggplot2)
library(factoextra)
library(corrplot)

data <- read_csv("WDIData.csv")
data<-data[,-63]

str(data)
summary(data)

# Observations au niveau mondial

## Globalement
data_W<-data[data$`Country Name`=="World",]

data_W$individu<-with(data_W, paste(`Country Code`,`Indicator Code`, sep="."))
data_W_indiv<-data_W[,-c(1,2,3,4)]

data_W_indiv<-data.frame(data_W_indiv, row.names = data_W_indiv$individu)
as.data.frame(data_W_indiv)
str(data_W_indiv)
data_W_indiv<-data_W_indiv[,-59]

data_W_indiv_t<-t(data_W_indiv)
data_W_indiv_t<-as.data.frame(data_W_indiv_t, col.names=data_W_indiv$individu)
str(data_W_indiv_t)
data_W_indiv_t$t<-1960:2017

plot(data_W_indiv_t$t,data_W_indiv_t$WLD.EN.ATM.CO2E.KT/10^6, type="l", col="blue", main="Evolution mondiale du CO2 émis", ylab="CO2 émis (Million de kilo-tonnes)", xlab="Années")

which(colnames(data_W_indiv_t)=="WLD.EN.ATM.CO2E.KT")
which(colnames(data_W_indiv_t)=="WLD.EN.ATM.GHGT.KT.CE")
data_W_GHG<-data_W_indiv_t[,c(213,1480)]
matplot(data_W_indiv_t$t,data_W_GHG/10^6, type="l", col=c("blue","dark green"), main="Evolution mondiale du Gaz à effet de serre émis", ylab="Gaz à effet de serre émis (Million de kilo-tonnes)", xlab="Années")
legend(x="topleft", legend=c("Emissions CO2","Emissions Gaz à effet de serre"), cex=0.8,fill=c("blue","dark green"),bty="n")       


data_W_indiv_t$WLD.EN.CO2.GHGT<-data_W_indiv_t$WLD.EN.ATM.CO2E.KT/data_W_indiv_t$WLD.EN.ATM.GHGT.KT.CE*100
plot(data_W_indiv_t$t,data_W_indiv_t$WLD.EN.CO2.GHGT, type="l", col="green", main="Evolution mondiale de la concentration de CO2 parmi les Gaz à effet de serre", ylab="Concentration de CO2 parmi les GES (%)", xlab="Années")


data_W_indiv_t$WLD.EN.CO2.GHGT.GROWTH <- NA
for(i in 2:2014) data_W_indiv_t$WLD.EN.CO2.GHGT.GROWTH[i] <- 100*(data_W_indiv_t$WLD.EN.CO2.GHGT[i]-data_W_indiv_t$WLD.EN.CO2.GHGT[i-1])/data_W_indiv_t$WLD.EN.CO2.GHGT[i-1]

par(mar=c(4,4,3,5))
plot(data_W_indiv_t$t,data_W_indiv_t$WLD.EN.CO2.GHGT.GROWTH, axes=F, pch=16,ylim=c(-10,10), xlab="", ylab="", type="l",col="blue", main="Evolutions mondiales des taux de croissance de la concentration en CO2 et du PIB")
axis(2, ylim=c(-10,10),col="blue")
mtext("Taux de croissance de la concentration de CO2 (%)",side=2,line=2.5)
par(new=T)
plot(data_W_indiv_t$t[10:46],data_W_indiv_t$WLD.NY.GDP.MKTP.KD.ZG[10:46], type="l", col="red", xlim=c(1960,2017), pch=15,  xlab="", ylab="", ylim=c(0,7), axes=F)
mtext("Taux de croissance du PIB (%)",side=4,line=2.5)
axis(4, ylim=c(0,7), col="red",col.axis="red")
axis(1,pretty(range(1960:2015),10)) 
mtext("Années",side=1,col="black",line=2.5)

## Secteurs

data_W_indiv_t$WLD.EN.CO2.ETOT.KT<-data_W_indiv_t$WLD.EN.CO2.ETOT.ZS*data_W_indiv_t$WLD.EN.ATM.CO2E.KT/100
data_W_indiv_t$WLD.EN.CO2.BLDG.KT<-data_W_indiv_t$WLD.EN.CO2.BLDG.ZS*data_W_indiv_t$WLD.EN.ATM.CO2E.KT/100
data_W_indiv_t$WLD.EN.CO2.MANF.KT<-data_W_indiv_t$WLD.EN.CO2.MANF.ZS*data_W_indiv_t$WLD.EN.ATM.CO2E.KT/100
data_W_indiv_t$WLD.EN.CO2.OTHX.KT<-data_W_indiv_t$WLD.EN.CO2.OTHX.ZS*data_W_indiv_t$WLD.EN.ATM.CO2E.KT/100
data_W_indiv_t$WLD.EN.CO2.TRAN.KT<-data_W_indiv_t$WLD.EN.CO2.TRAN.ZS*data_W_indiv_t$WLD.EN.ATM.CO2E.KT/100

annee<-1960:2014

data.sector<-data_W_indiv_t[-c(58,57,56),1579:1583]
data.sector
barplot(t(data.sector/10^6),beside=F,col=c("#F5BCA9","#F7D358","#D8F781","#E6E6FA","#57D4FF"),xlab="Années", ylab="CO2 émis par secteur (Million de kilo-tonnes)",main="Répartition du CO2 émis par secteur", names=annee,las=1.5,horiz=F,space=0.5)
legend(x="topleft", legend=c("Production d'électricité et de chaleur","Bâtiments et services commerciaux et publics","Manufacturier et construction", "Autres", "Transports"), cex=0.8,fill=c("#F5BCA9","#F7D358","#D8F781","#E6E6FA","#57D4FF"),bty="n")       

# Observation au niveau des revenus

## Participation dans le monde

EN<- filter(data,grepl('^EN.', `Indicator Code`))

EN_INC<-EN[EN$`Country Code`=="HIC"|EN$`Country Code`=="LIC"|EN$`Country Code`=="LMC"|EN$`Country Code`=="UMC",]

EN_INC$individu<-with(EN_INC, paste(`Country Code`,`Indicator Code`, sep="."))
EN_INC_indiv_59<-EN_INC[,-c(1,2,3,4)]

EN_INC_indiv_59<-data.frame(EN_INC_indiv_59, row.names = EN_INC_indiv_59$individu)
as.data.frame(EN_INC_indiv_59)
str(EN_INC_indiv_59)
EN_INC_indiv<-EN_INC_indiv_59[,-59]

EN_INC_indiv_t<-t(EN_INC_indiv)
EN_INC_indiv_t<-as.data.frame(EN_INC_indiv_t, col.names=EN_INC_indiv$individu)
str(EN_INC_indiv_t)
EN_INC_indiv_t$t<-1960:2017

x<-EN_INC_indiv_t$t
y<-EN_INC_indiv_t[,grepl("EN.ATM.CO2E.KT$", names(EN_INC_indiv_t))]
as.matrix(y)

matplot(x,y/10^6, col=c("blue","red","orange","green"), type="l", main="Evolution du CO2 émis par niveau de revenu", ylab="CO2 émis (Million de kilo-tonnes)", xlab="Année")
legend(x="topleft", legend=c("Revenu élevé (>12 476$)","Revenu faible (<1 025$)","Revenu moyen inférieur (>1 025$ et <4 035$)", "Revenu moyen supérieur (>4 035$ et <12 476$)"), cex=0.8,fill=c("blue","red","orange","green"),bty="n")       


EN_INC_indiv_59_CO2_EMIS<-filter(EN_INC_indiv_59,grepl("EN.ATM.CO2E.KT$",EN_INC_indiv_59$individu))

EN_INC_indiv_59_CO2_EMIS<-data.frame(EN_INC_indiv_59_CO2_EMIS, row.names = EN_INC_indiv_59_CO2_EMIS$individu)
as.data.frame(EN_INC_indiv_59_CO2_EMIS)
str(EN_INC_indiv_59_CO2_EMIS)
EN_INC_indiv_CO2_EMIS<-EN_INC_indiv_59_CO2_EMIS[,-59]

ind_inc<-c("Revenu élevé (>12 476$)","Revenu faible (<1 025$)","Revenu moyen inférieur (>1 025$ et <4 035$)", "Revenu moyen supérieur (>4 035$ et <12 476$)")
pct_98 <- round(EN_INC_indiv_CO2_EMIS$X1998/sum(EN_INC_indiv_CO2_EMIS$X1998)*100)
lbls_98<- paste(ind_inc, pct_98,"%", sep=" ")
pie(EN_INC_indiv_CO2_EMIS$X1998, labels=lbls_98, main="Répartition du CO2 émis par groupe de niveau de revenu (1998)")
pct_06 <- round(EN_INC_indiv_CO2_EMIS$X2006/sum(EN_INC_indiv_CO2_EMIS$X2006)*100)
lbls_06<- paste(ind_inc, pct_06,"%", sep=" ")
pie(EN_INC_indiv_CO2_EMIS$X2006, labels=lbls_06, main="Répartition du CO2 émis par groupe de niveau de revenu (2006)")
pct_14 <- round(EN_INC_indiv_CO2_EMIS$X2014/sum(EN_INC_indiv_CO2_EMIS$X2014)*100)
lbls_14<- paste(ind_inc, pct_14,"%", sep=" ")
pie(EN_INC_indiv_CO2_EMIS$X2014, labels=lbls_14, main="Répartition du CO2 émis par groupe de niveau de revenu (2014)")

# Observation au niveau des pays

EN_PY<- read_csv("54a8bd76-74ad-46cb-b824-24486dcc7cd9_Data.csv", na="..")
EN_PY_CO2_EMIS<-EN_PY[EN_PY$`Series Code`=="EN.ATM.CO2E.KT",]

par(mfrow=c(2,3))
hist(EN_PY_CO2_EMIS$`1960 [YR1960]`, main="Histogramme du CO2 émis en 1960", xlab="CO2 émis", ylab="Effectif")
hist(EN_PY_CO2_EMIS$`1970 [YR1970]`, main="Histogramme du CO2 émis en 1970", xlab="CO2 émis", ylab="Effectif")
hist(EN_PY_CO2_EMIS$`1980 [YR1980]`, main="Histogramme du CO2 émis en 1980", xlab="CO2 émis", ylab="Effectif")
hist(EN_PY_CO2_EMIS$`1990 [YR1990]`, main="Histogramme du CO2 émis en 1990", xlab="CO2 émis", ylab="Effectif")
hist(EN_PY_CO2_EMIS$`2000 [YR2000]`, main="Histogramme du CO2 émis en 2000", xlab="CO2 émis", ylab="Effectif")
hist(EN_PY_CO2_EMIS$`2010 [YR2010]`, main="Histogramme du CO2 émis en 2010", xlab="CO2 émis", ylab="Effectif")
par(mfrow=c(1,1))
summary(EN_PY_CO2_EMIS)

EN_PY_CO2_EMIS$individu<-with(EN_PY_CO2_EMIS, paste(`Country Code`,`Series Code`, sep="."))
EN_PY_CO2_EMIS_indiv<-EN_PY_CO2_EMIS[,-c(1,2,3,4)]

EN_PY_CO2_EMIS_indiv<-data.frame(EN_PY_CO2_EMIS_indiv, row.names = EN_PY_CO2_EMIS_indiv$individu)
as.data.frame(EN_PY_CO2_EMIS_indiv)
str(EN_PY_CO2_EMIS_indiv)
EN_PY_CO2_EMIS_indiv<-EN_PY_CO2_EMIS_indiv[,-59]

head(EN_PY_CO2_EMIS_indiv[order(EN_PY_CO2_EMIS_indiv[,55],decreasing=T), ])

EN_PY_CO2_EMIS_indiv_t<-t(EN_PY_CO2_EMIS_indiv)
EN_PY_CO2_EMIS_indiv_t<-as.data.frame(EN_PY_CO2_EMIS_indiv_t, col.names=EN_PY_CO2_EMIS_indiv$individu)
str(EN_PY_CO2_EMIS_indiv_t)
EN_PY_CO2_EMIS_indiv_t$t<-1960:2017

x2<-EN_PY_CO2_EMIS_indiv_t$t
y2<-EN_PY_CO2_EMIS_indiv_t[,grepl("EN.ATM.CO2E.KT$", names(EN_PY_CO2_EMIS_indiv_t))]
as.matrix(y2)

matplot(x2,y2/10^6, type="l", main="Evolution du CO2 émis national", ylab="CO2 émis (Million de kilo-tonnes)", xlab="Années")

EN_PY_CO2_EMIS_indiv_t$RDM.EN.ATM.CO2E.KT<-0
which(colnames(EN_PY_CO2_EMIS_indiv_t)=="CHN.EN.ATM.CO2E.KT")
which(colnames(EN_PY_CO2_EMIS_indiv_t)=="USA.EN.ATM.CO2E.KT")
which(colnames(EN_PY_CO2_EMIS_indiv_t)=="IND.EN.ATM.CO2E.KT")
which(colnames(EN_PY_CO2_EMIS_indiv_t)=="RUS.EN.ATM.CO2E.KT")
which(colnames(EN_PY_CO2_EMIS_indiv_t)=="JPN.EN.ATM.CO2E.KT")
which(colnames(EN_PY_CO2_EMIS_indiv_t)=="DEU.EN.ATM.CO2E.KT")
which(colnames(EN_PY_CO2_EMIS_indiv_t)=="IRN.EN.ATM.CO2E.KT")
which(colnames(EN_PY_CO2_EMIS_indiv_t)=="t")

EN_PY_CO2_EMIS_indiv_t$RDM.EN.ATM.CO2E.KT<-apply(EN_PY_CO2_EMIS_indiv_t[,-c(42,207,89,161,98,73,91,218)], 1, sum, na.rm=T)
EN_RDM_CO2_EMIS_t<-EN_PY_CO2_EMIS_indiv_t[,c(218,42,207,89,161,98,91,73,219)]
EN_RDM_CO2_EMIS<-t(EN_RDM_CO2_EMIS_t)
EN_RDM_CO2_EMIS<-as.data.frame(EN_RDM_CO2_EMIS, col.names=EN_RDM_CO2_EMIS_t$t)
EN_RDM_CO2_EMIS<-EN_RDM_CO2_EMIS[-1,]
str(EN_RDM_CO2_EMIS)

x3<-EN_RDM_CO2_EMIS_t$t[EN_RDM_CO2_EMIS_t$t<=2014]
y3<-EN_RDM_CO2_EMIS_t[EN_RDM_CO2_EMIS_t$t<=2014,grepl("EN.ATM.CO2E.KT$", names(EN_RDM_CO2_EMIS_t))]
as.matrix(y3)

matplot(x3,y3/10^6, type="l",col=c("orange","blue","magenta","red","white","green","black","dark violet"), main="Evolution du CO2 émis par les importants émetteurs et le reste du monde", ylab= "CO2 émis (Millions de kilo-tonnes)", xlab="Années")
legend(x="topleft", legend=c("Chine","Etats Unis","Inde", "Russie", "Japon", "Iran", "Allemagne", "Reste du monde"), cex=0.8,fill=c("orange","blue","magenta","red","white","green","black","dark violet"),bty="n")       


EN_PY_CO2_PC<-EN_PY[EN_PY$`Series Code`=="EN.ATM.CO2E.PC",]

EN_PY_CO2_PC$individu<-with(EN_PY_CO2_PC, paste(`Country Code`,`Series Code`, sep="."))
EN_PY_CO2_PC_indiv<-EN_PY_CO2_PC[,-c(1,2,3,4)]

EN_PY_CO2_PC_indiv<-data.frame(EN_PY_CO2_PC_indiv, row.names =EN_PY_CO2_PC_indiv$individu)
as.data.frame(EN_PY_CO2_PC_indiv)
str(EN_PY_CO2_PC_indiv)
EN_PY_CO2_PC_indiv<-EN_PY_CO2_PC_indiv[,-59]

head(EN_PY_CO2_PC_indiv[order(EN_PY_CO2_PC_indiv[,55],decreasing=T), ])

EN_PY_CO2_PC_indiv_t<-t(EN_PY_CO2_PC_indiv)
EN_PY_CO2_PC_indiv_t<-as.data.frame(EN_PY_CO2_PC_indiv_t, col.names=EN_PY_CO2_PC_indiv$individu)
str(EN_PY_CO2_PC_indiv_t)
EN_PY_CO2_PC_indiv_t$t<-1960:2017

x4<-EN_PY_CO2_PC_indiv_t$t
y4<-EN_PY_CO2_PC_indiv_t[,grepl("EN.ATM.CO2E.PC$", names(EN_PY_CO2_PC_indiv_t))]
as.matrix(y4)

matplot(x4,y4, type="l", main="Evolution du CO2 émis national par habitant", ylab="CO2 émis par habitant", xlab="Années")

EN_PY_CO2_PC_indiv_t$RDM.EN.ATM.CO2E.PC<-0
which(colnames(EN_PY_CO2_PC_indiv_t)=="CHN.EN.ATM.CO2E.PC")
which(colnames(EN_PY_CO2_PC_indiv_t)=="USA.EN.ATM.CO2E.PC")
which(colnames(EN_PY_CO2_PC_indiv_t)=="IND.EN.ATM.CO2E.PC")
which(colnames(EN_PY_CO2_PC_indiv_t)=="RUS.EN.ATM.CO2E.PC")
which(colnames(EN_PY_CO2_PC_indiv_t)=="JPN.EN.ATM.CO2E.PC")
which(colnames(EN_PY_CO2_PC_indiv_t)=="DEU.EN.ATM.CO2E.PC")
which(colnames(EN_PY_CO2_PC_indiv_t)=="IRN.EN.ATM.CO2E.PC")
which(colnames(EN_PY_CO2_PC_indiv_t)=="t")

EN_PY_CO2_PC_t<-EN_PY_CO2_PC_indiv_t[,c(218,42,207,89,161,98,91,73)]

x5<-EN_PY_CO2_PC_t$t[EN_PY_CO2_PC_t$t<=2014]
y5<-EN_PY_CO2_PC_t[EN_PY_CO2_PC_t$t<=2014,grepl("EN.ATM.CO2E.PC$", names(EN_PY_CO2_PC_t))]
as.matrix(y5)

matplot(x5,y5, type="l",col=c("orange","blue","magenta","red","white","green","black","dark violet"), main="Evolution du CO2 émis national par habitant des importants émetteurs", ylab="CO2 émis par habitant", xlab="Années")
legend(x="left", legend=c("Chine","Etats Unis","Inde", "Russie", "Japon", "Iran", "Allemagne"), cex=0.8,fill=c("orange","blue","magenta","red","white","green","black"),bty="n")

ind_RDM<-c("Chine","Etats-Unis","Inde","Russie","Japon","Iran","Allemagne","Reste du Monde")
pct_RDM_00 <- round(EN_RDM_CO2_EMIS$X2000..YR2000./sum(EN_RDM_CO2_EMIS$X2000..YR2000.)*100)
lbls_RDM_00<- paste(ind_RDM, pct_RDM_00,"%", sep=" ")
pie(EN_RDM_CO2_EMIS$X2000..YR2000., labels = lbls_RDM_00, col=c("orange","blue","magenta","red","white","green","black","dark violet"), main= "Répartition du CO2 émis par les importants émetteurs et le reste du monde (2000)")
pct_RDM_14 <- round(EN_RDM_CO2_EMIS$X2014..YR2014./sum(EN_RDM_CO2_EMIS$X2014..YR2014.)*100)
lbls_RDM_14<- paste(ind_RDM, pct_RDM_14,"%", sep=" ")
pie(EN_RDM_CO2_EMIS$X2014..YR2014., labels = lbls_RDM_14, col=c("orange","blue","magenta","red","white","green","black","dark violet"), main= "Répartition du CO2 émis par les importants émetteurs et le reste du monde (2014)")


data_pca<- read_csv("70bf36c1-1814-4e12-b823-a71cd7a0f205_Data.csv",na = "NA")
summary(data_pca)

data_pca_2010 <- data.frame(country = data_pca$`Country Name`,Indic = data_pca$`Series Code`,valeur=data_pca$`2010 [YR2010]`)  
str(data_pca_2010)
data_pca_2010<-data_pca_2010[-(2822:2826),]
str(data_pca_2010)

data_pca_2010<-spread(data_pca_2010, Indic, valeur)
plot(data_pca_2010$NY.GDP.PCAP.CD, data_pca_2010$EN.ATM.CO2E.PC)

data_pca_2010$NY.GDP.PCAP.CD_2<-(data_pca_2010$NY.GDP.PCAP.CD)^2
reg<-lm( data_pca_2010$EN.ATM.CO2E.PC~data_pca_2010$NY.GDP.PCAP.CD+data_pca_2010$NY.GDP.PCAP.CD_2)
summary(reg)

data_pca_fin<-as.data.frame(data.frame(data_pca_2010, row.names = data_pca_2010$country))
data_pca_fin<-data_pca_fin[,-c(1,9,11,15)]
str(data_pca_fin)
summary(data_pca_fin)

mat.cor<-cor(data_pca_fin, use="complete.obs")
corrplot(mat.cor, type="upper", diag=F)

imputePCA(data_pca_fin)
res.pca<-PCA(data_pca_fin, scale.unit = T, graph = F)

fviz_eig(res.pca, addlabels = TRUE, ylab="Pourcentage d'inertie expliquée", xlab="Axes factoriels",main="Pourcentage d'inertie expliquée par les valeurs propres")
inertie_cum<-barplot(res.pca$eig[,3],main="Inerties relatives cumulées", xlab="Axes factoriels", ylab="Parts cumulées", col="light blue")
lines(inertie_cum,res.pca$eig[,3])

coordvar12<-round(res.pca$var$coord[,1:2],2)
colnames(coordvar12)<-c("F1","F2")
ctrvar12<-round(res.pca$var$contrib[,1:2],2)
colnames(ctrvar12)<-c("contribution F1","contribution F2")
qltvar12<-round(res.pca$var$cos2[,1:2],3)
colnames(qltvar12)<-c("qualité F1","qualité F2")
var12<-cbind(coordvar12,ctrvar12,qltvar12)
var12

fviz_pca_var(res.pca, axes=c(1,2), title="Nuage des variables sur le plan (F1,F2)",repel = TRUE)


coordvar13<-round(res.pca$var$coord[,c(1,3)],2)
colnames(coordvar13)<-c("F1","F3")
ctrvar13<-round(res.pca$var$contrib[,c(1,3)],2)
colnames(ctrvar13)<-c("contribution F1","contribution F3")
qltvar13<-round(res.pca$var$cos2[,c(1,3)],3)
colnames(qltvar13)<-c("qualité F1","qualité F3")
var13<-cbind(coordvar13,ctrvar13,qltvar13)
var13

fviz_pca_var(res.pca, axes=c(1,3), title="Nuage des variables sur le plan (F1,F3)",repel = TRUE)

fviz_contrib(res.pca, choice = "ind", axes = 1:2, title="Contribution des individus à F1, F2", top=20)
fviz_contrib(res.pca, choice = "ind", axes = c(1,3), title="Contribution des individus à F1, F3", top=20)

coordind12<-round(res.pca$ind$coord[,1:2],2)
colnames(coordind12)<-c("F1","F2")
ctrind12<-round(res.pca$ind$contrib[,1:2],2)
colnames(ctrind12)<-c("contribution F1","contribution F2")
qltind12<-round(res.pca$ind$cos2[,1:2],3)
colnames(qltind12)<-c("qualité F1","qualité F2")
ind12<-cbind(coordind12,ctrind12,qltind12)
ind12

fviz_pca_ind(res.pca, axes=c(1,2), title="Nuage des individus sur le plan (F1,F2)",repel = TRUE, select.ind=list(cos2=0.8), col.ind.sup=NULL)

coordind13<-round(res.pca$ind$coord[,c(1,3)],2)
colnames(coordind13)<-c("F1","F2")
ctrind13<-round(res.pca$ind$contrib[,c(1,3)],2)
colnames(ctrind13)<-c("contribution F1","contribution F2")
qltind13<-round(res.pca$ind$cos2[,c(1,3)],3)
colnames(qltind13)<-c("qualité F1","qualité F2")
ind13<-cbind(coordind13,ctrind13,qltind13)
ind13

fviz_pca_ind(res.pca, axes=c(2,1), title="Nuage des individus sur le plan (F2,F1)",repel = TRUE, select.ind=list(cos2=0.8))
fviz_pca_ind(res.pca, axes=c(1,3), title="Nuage des individus sur le plan (F1,F3)",repel = TRUE, select.ind=list(cos2=0.8))

