vins=read.table(file='C:/Users/Olhagaray/Desktop/AgroParisTech/2A/Projet stats/TP_vins.csv', skip=0, header=TRUE, sep = ';', row.names =1 )

summary(vins$nombre_declarations);summary(vins$superficie);summary(vins$total)


str(vins)
summary(vins)
vinsac=vins[-81,-22]
vinsacp=vinsac[-80,]
summary(vinsacp$total)

#ACP sur les données
library(FactoMineR)
res.pca=PCA(vinsacp,scale.unit=TRUE,graph=FALSE,quali.sup=13)
plot(res.pca,cex=0.75,cex.lab=0.75,cex.axis=0.75)
plot.PCA(res.acp,choix="var",1:2,col.hab=c("forestgreen","blue","brown","yellow4","black","cyan2","darkgoldenrod1","deepskyblue","darkorange","firebrick1","darkorchid1","deeppink","mediumvioletred","navyblue","palegreen","salmon","green","peru","black","gray","seagreen"),lim.cos2.var = 0.7)
plot.PCA(res.acp,choix="ind",1:2,)

#PRODUCTION NON COMMERCIALE
#Tests des indications géographiques
p.aop=(vins$qte_aop_blanc+vins$qte_aop_rouge+vins$qte_aop_rose)/vins$total
test.aop=cor.test(p.aop,vins$prod_noncommerciale)
test.aop

p.igp=(vins$qte_igp_blanc+vins$qte_igp_rouge+vins$qte_igp_rose+vins$qte_igp_vci)/vins$total
test.igp=cor.test(p.igp,vins$prod_noncommerciale)
test.igp

p.vsig=(vins$qte_vsig_blanc+vins$qte_vsig_rouge+vins$qte_vsig_rose)/vins$total
test.vsig=cor.test(p.vsig,vins$prod_noncommerciale)
test.vsig


library(factoextra)
library(NbClust)
library('rgdal')      # Lire et reprojeter les cartes
library('plotrix')    # Créer des échelles de couleurs
library('classInt')   # Affecter ces couleurs aux données

## Mise en forme
TP=TP_vins[-81,-23]
df = scale(na.omit(TP[,-1]))

#Determination du nombre optimum de clusters
fviz_nbclust(df, kmeans, method = c("silhouette", "wss", "gap_stat"))

#Clusterisation
set.seed(20)
TPCluster <- kmeans(TP_vins[1:79,2:21], 5, nstart = 100)
TPCluster

TableCarte=cbind(TP_vins[1:79,],TPCluster$cluster)

# Lecture des départements
departements <- readOGR(dsn="IGN",  layer="DEPARTEMENT")

frontieres <- readOGR(dsn="IGN",  layer="LIMITE_DEPARTEMENT")
frontieres <- frontieres[frontieres$NATURE %in% c('Fronti\xe8re internationale','Limite c\xf4ti\xe8re'),]

CODE_DEPT=sub(" .*","",TP_vins[1:79,1])
TP_vins_esub=cbind(TableCarte[1:79,],CODE_DEPT)
deptclass <- merge(departements, TP_vins_esub, by.x="CODE_DEPT", by.y="CODE_DEPT")


# Traçage de la carte
pdf('france.pdf',width=6,height=4.7)
par(mar=c(0,0,0,0))

plot(frontieres,  col="#FFFFFF")
plot(frontieres,  col="#D8D6D4", lwd=6, add=TRUE)
plot(departements,col="#FFFFFF", border="#CCCCCC",lwd=.7, add=TRUE)
plot(frontieres,  col="#666666", lwd=1, add=TRUE)
plot(deptclass,   col=deptclass$`TPCluster$cluster`,border = col,  lwd=.1, add=TRUE)


dev.off()

