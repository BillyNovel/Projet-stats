vins=read.table(file='TP_vins.csv', skip=0, header=TRUE, sep = ';', row.names =1 )

summary(vins$nombre_declarations);summary(vins$superficie);summary(vins$total)


str(vins)
summary(vins)
vinsac=vins[-81,-22]
vinsacp=vinsac[-80,]
summary(vinsacp$total)

#ACP sur les donn�es
library(FactoMineR)
res.pca=PCA(vinsacp,scale.unit=TRUE,graph=FALSE,quali.sup=13)
plot(res.pca,cex=0.75,cex.lab=0.75,cex.axis=0.75)
plot.PCA(res.acp,choix="var",1:2,col.hab=c("forestgreen","blue","brown","yellow4","black","cyan2","darkgoldenrod1","deepskyblue","darkorange","firebrick1","darkorchid1","deeppink","mediumvioletred","navyblue","palegreen","salmon","green","peru","black","gray","seagreen"),lim.cos2.var = 0.7)
plot.PCA(res.acp,choix="ind",1:2,)

#PRODUCTION NON COMMERCIALE
#Tests des indications g�ographiques
vins=vins[,-81]

p.aop=(vins$qte_aop_blanc+vins$qte_aop_rouge+vins$qte_aop_rose)/vins$total
p.noncom=vins$prod_noncommerciale/vins$total
test.aop=cor.test(p.aop,p.noncom)
test.aop

p.igp=(vins$qte_igp_blanc+vins$qte_igp_rouge+vins$qte_igp_rose+vins$qte_igp_vci)/vins$total
test.igp=cor.test(p.igp,p.noncom)
test.igp

p.vsig=(vins$qte_vsig_blanc+vins$qte_vsig_rouge+vins$qte_vsig_rose)/vins$total
test.vsig=cor.test(p.vsig,p.noncom)
test.vsig

p.aop.rouge=vins$qte_aop_rouge/vins$total
test.aop.rouge=cor.test(p.aop.rouge,p.noncom)
test.aop.rouge

p.aop.blanc=vins$qte_aop_blanc/vins$total
test.aop.blanc=cor.test(p.aop.blanc,p.noncom)
test.aop.blanc

p.aop.rose=vins$qte_aop_rose/vins$total
test.aop.rose=cor.test(p.aop.rose,p.noncom)
test.aop.rose

p.vsig.rouge=vins$qte_vsig_rouge/vins$total
test.vsig.rouge=cor.test(p.vsig.rouge,p.noncom)
test.vsig.rouge

p.vsig.rose=vins$qte_vsig_rose/vins$total
test.vsig.rose=cor.test(p.vsig.rose,p.noncom)
test.igp.rose

p.vsig.blanc=vins$qte_igp_blanc/vins$total
test.vsig.blanc=cor.test(p.vsig.blanc,p.noncom)
test.igp.blanc

#Biblioth�ques pour la carte
library(factoextra)
library(NbClust)
library('rgdal')      # Lire et reprojeter les cartes
library('plotrix')    # Cr�er des �chelles de couleurs
library('classInt')   # Affecter ces couleurs aux donn�es

## Mise en forme
TP=vins[-81,-23]
df = scale(na.omit(TP[,-1]))

#Determination du nombre optimum de clusters
fviz_nbclust(df, kmeans, method = c("silhouette", "wss", "gap_stat"))

#Clusterisation
set.seed(20)
TPCluster <- kmeans(TP_vins[1:79,2:21], 5, nstart = 100)
TPCluster

TableCarte=cbind(TP_vins[1:79,],TPCluster$cluster)

# Lecture des d�partements
departements <- readOGR(dsn="IGN",  layer="DEPARTEMENT")

frontieres <- readOGR(dsn="IGN",  layer="LIMITE_DEPARTEMENT")
frontieres <- frontieres[frontieres$NATURE %in% c('Fronti\xe8re internationale','Limite c\xf4ti\xe8re'),]

#Jointure des deux BDD
CODE_DEPT=sub(" .*","",TP_vins[1:79,1])
TP_vins_esub=cbind(TableCarte[1:79,],CODE_DEPT)
deptclass <- merge(departements, TP_vins_esub, by.x="CODE_DEPT", by.y="CODE_DEPT")


# Tra�age de la carte
pdf('france.pdf',width=6,height=4.7)
par(mar=c(0,0,0,0))

plot(frontieres,  col="#FFFFFF")
plot(frontieres,  col="#D8D6D4", lwd=6, add=TRUE)
plot(departements,col="#FFFFFF", border="#CCCCCC",lwd=.7, add=TRUE)
plot(frontieres,  col="#666666", lwd=1, add=TRUE)
plot(deptclass,   col=deptclass$`TPCluster$cluster`,border = col,  lwd=.1, add=TRUE)


dev.off()

#Test sur les Clusters
Data_Vins=read.table("Data_Vins.csv", skip=0, header=TRUE, sep = ';', row.names =1 )
anC<-lm(formula=total~Cluster,data=Data_Vins)
an0<-lm(formula=total~1,data= Data_Vins)
anova(anC,an0)
summary(an)

#AIC excluant les productions
total.lm=lm(total~nombre_declarations+superficie+superficie_aop+superficie_cognac+superficie_igp+superficie_vsig,data=Data_Vins)
par(mfrow=c(2,2))
plot(total.lm)
step(total.lm)

#V�rification
an2=lm(formula=total~nombre_declarations+superficie,data=Data_Vins)
an3=lm(formula=total~nombre_declarations+superficie_aop+superficie_igp+superficie_cognac+superficie_vsig,data=Data_Vins)
anova(an2,an3)

#Graphes de diagnostic
par(mfrow=c(2,2))
plot(an3)

#Passage au log
logtotal=log(vins$total)
logdecla=log(vins$nombre_declarations)
logsaop=log(vins$superficie_aop)
logsigp=log(vins$superficie_igp)
logscognac=log(vins$superficie_cognac)
logsvsig=log(vins$superficie_vsig)
an4=lm(formula=logtotal~logdecla+logsaop+logsigp+logscognac+logsvsig,data=Data_Vins)

