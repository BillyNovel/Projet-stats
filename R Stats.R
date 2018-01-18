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

