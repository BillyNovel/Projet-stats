#vins=read.table(file='E:/TP_vins.csv', skip=0, header=TRUE, sep = ';')
#Quelques boxplot pour commencer
vins=read.table(file='C:/Users/Olhagaray/Desktop/AgroParisTech/2A/Projet stats/TP_vins.csv', skip=0, header=TRUE, sep = ';', row.names =1 )
vins_appel=read.table(file='C:/Users/Olhagaray/Desktop/AgroParisTech/2A/Projet stats/TP_vins_par_appelations.csv', skip=0, header=TRUE, sep = ';')
vins_appel 

str(vins)
summary(vins)
summary(vins$nombre_declarations);summary(vins$superficie);summary(vins$total)


vinsacp=vins[,-22]


options(max.print = 99999999)
vins
summary(vins)
plot(vins[, 2:5])
boxplot(qte_cognac~region, data=vins)
boxplot(qte_aop_rouge~region, data=vins)
boxplot(qte_aop_blanc~region, data=vins)
boxplot(qte_aop_rose~region, data=vins)
boxplot(qte_vci~region, data=vins)


#ACP sur les données
library(FactoMineR)
res.acp=PCA(vinsacp,scale.unit = TRUE,ncp=3)
summary(res.acp)
plot(res.acp$eig$eigenvalue)
plot.PCA(res.acp,choix="var",1:2,col.hab=c("green","blue","red","yellow4","black","chartreuse","blueviolet","coral","darkgreen","orchid","lightseagreen","red4","saddlebrown","plum","navy","orange","green","midnightblue","black","gray","deeppink4"),lim.cos2.var = 0.7)
plot.PCA(res.acp,choix="ind",1:2,col.hab=c("green","blue","red","yellow4","aquamarine","chartreuse","blueviolet","coral","darkgreen","orchid","lightseagreen","red4","saddlebrown","plum","navy","orange","green","midnightblue","black","gray","deeppink4"))







#Premier modele : superficie en fonction du nombre de déclarations
model1=lm(nombre_declarations~superficie, data=vins)
summary(model1)
anova(model1)
plot(model1)
plot(nombre_declarations~superficie, data=vins)
summary(vins[,2:5])

#Pareil mais sans les valeurs abbérantes 
vins2=vins[-c(30,31,46),]
vins2
model2=lm(nombre_declarations~superficie, data=vins2)
summary(model2)
anova(model2)
plot(model2)
plot(nombre_declarations~superficie, data=vins2)
summary(vins[,2:5])



#Production en fonction du nombre de déclaration
model00=lm(total~1, data=vins)
#Production en fonction du nombre de déclaration et de la superficie
model3=lm(total~nombre_declarations+superficie, data=vins)
summary(model3)
anova(model3)
anova(model00,model3)
plot(model3)
plot(total~nombre_declarations+superficie, data=vins)
summary(vins[,2:5])

#model3bis en prenant sqrt(surface) et sqrt total

#on repere l'amélioration entre ces 2 graphes
plot(total~nombre_declarations+superficie, data=vins)
plot(sqrt(total)~nombre_declarations+sqrt(superficie), data=vins)


model3bis=lm(sqrt(total)~nombre_declarations+sqrt(superficie), data=vins)
summary(model3bis)
anova(model3bis)
anova(model00,model3bis)
plot(model3bis)



#on reprends les groupes
vins_grp=vins[-c(11,16,17,27,29,30,31,46),]

plot(total~nombre_declarations+superficie, data=vins_grp)
plot(sqrt(total)~nombre_declarations+sqrt(superficie), data=vins_grp)


#et du coup le modèle deviens :
model003bis=lm(total~1, data=vins_grp)
model3bis=lm(sqrt(total)~nombre_declarations+sqrt(superficie), data=vins_grp)
summary(model3bis)
anova(model003bis,model3bis)
Anova(model3bis)
plot(model3bis)

#puis on enlève le nbr de déclaration qui n'est finalament pas utile

model003ter=lm(total~1, data=vins_grp)
model3ter=lm(sqrt(total)~sqrt(superficie), data=vins_grp)
summary(model3ter)
anova(model003ter,model3ter)
Anova(model3ter)
plot(model3ter)


model003ter=lm(total~1, data=vins_grp)
model3log=lm(log(total)~(log(superficie+1))+nombre_declarations, data=vins_grp)
summary(model3log)
anova(model003ter,model3log)
anova(model3log)
plot(model3log)



#conclusion : on a donc un modèle d'assez bonne qualitée ( graph d'analye pas mauvais et R² = 0.98)

##################################### INUTILE ###############################

#Production en fonction de la superficie
model4=lm(total~superficie, data=vins)
anova(model00,model3)
summary(model4)
anova(model4)
plot(model4)
plot(total~superficie, data=vins)
summary(vins[,2:5])

#Variance augmente donc on passe au log
vins$total2 = log(vins$total)
model5=lm(total2~superficie, data=vins)
summary(model5)
anova(model5)
plot(model5)
plot(total2~superficie, data=vins)
summary(vins[,2:5])

#Nouveau tableau
#On teste une Ancova à trois facteurs 
vinsnouveau=read.table(file='TP_vinsnouveau.csv', skip=0, header=TRUE, sep = ';')
head(vinsnouveau)
model0=lm(Quantite~1,data=vinsnouveau)

#ANova deux facteurs
model6=lm(Quantite~Norme+Couleur+Norme*Couleur,data=vinsnouveau)
summary(model6)
anova(model6)
anova(model0,model6)

#On enlève l'interraction car ça sert à rien
model7=lm(Quantite~Norme+Couleur,data=vinsnouveau)
summary(model7)
anova(model7)
anova(model0,model7)
model7$fitted.values
model7$coefficients

plot(model7)

#Effet département ? 
head(vinsnouveau)
str(vinsnouveau)
model8=lm(Quantite~Norme+Couleur+Departement+Norme*Couleur+Norme*Departement+Couleur*Departement,data=vinsnouveau)
summary(model8)
anova(model8)
anova(model0,model8)


#On enlève Norme*Couleur car ca sert à rien
model10=lm(Quantite~Norme+Couleur+Departement+Norme*Departement+Couleur*Departement,data=vinsnouveau)
summary(model10)
anova(model10)
plot(model10)
anova(model0,model10)

(model10$fitted.values)
(model10$coefficients)

#Ca diverge
vinsnouveau$Quantite2 = log (vinsnouveau$Quantite) 
model11=lm(Quantite2~Norme+Couleur,data=vinsnouveau)
summary(model11)
anova(model11)
plot(model11)
plot(Quantite2~superficie, data=vins)
#summary(vins[,2:5])

#on teste une ancova sur la quantité totale en fonction de la surface et de l'appelation

################################### FIN INUTILE ###################################


library(car)
model000=lm(Qtité.tot~1,data=vins_appel)

#modele 12 bon mais pas graph bons
model12 = lm(Qtité.tot~Apellation+Surface + Apellation:Surface,data=vins_appel)
summary(model12)
anova(model000,model12)
Anova(model12)
plot(model12)

model12bis = lm(Qtité.tot~Apellation+log(Surface+0.1) + Apellation:log(Surface+0.1),data=vins_appel)
summary(model12bis)
anova(model000,model12bis)
Anova(model12bis)
plot(model12bis)



#regroupement des valeures par classes depuis la table TP_vins :
library(FactoMineR)
TableCN = scale(vinsacp, scale=T, center=T)
vins.dist <- dist(TableCN, method = "euclidean")
vins.cah.ward<-hclust(vins.dist, method = "ward")
par(mfrow = c(1,1))
plot(vins.cah.ward)
res=rect.hclust(vins.cah.ward,k=2)
print(res)
cluster.ward=cutree(vins.cah.ward,k=2)
cluster.ward
plot(res.acp,choix="ind",1:2,col.ind = cluster.ward)


#juste pour vérifier que il reste pas trop de disparités entre les valeurs conservées
vinssimple=vins[-c(11,16,17,27,29,30,31,46),]
vinsacp2 = vinssimple[,-22]
res.acp2=PCA(vinsacp2,scale.unit = TRUE,ncp=3)
TableCN = scale(vinsacp2, scale=T, center=T)
vins.dist <- dist(TableCN, method = "euclidean")
vins.cah.ward<-hclust(vins.dist, method = "ward")
plot(vins.cah.ward)
res=rect.hclust(vins.cah.ward,k=3)
print(res)
cluster.ward=cutree(vins.cah.ward,k=3)
cluster.ward
plot(res.acp2,choix="ind",1:2,col.ind = cluster.ward)




vins_appel_grp=vins_appel[-c(11,16,17,27,29,30,31,46,11+79,16+79,17+79,27+79,29+79,30+79,31+79,46+79,11+158,16+158,17+158,27+158,29+158,30+158,31+158,46+158),]

#modele 12a 
model12a = lm(Qtité.tot~Apellation+Surface + Apellation:Surface  ,data=vins_appel_grp)
summary(model12a)
model0012a=lm(Qtité.tot~1,data=vins_appel_grp)
anova(model0012a,model12a)
Anova(model12a)
plot(model12a)


#modele 12b 
model12b = lm(Qtité.tot~Apellation+log(Surface+1) + Apellation:log(Surface+1),data=vins_appel_grp)
summary(model12b)
anova(model0012a,model12b)
Anova(model12b)
plot(model12b)

vins_appel_grp$Qtité.tot2 = (vins_appel_grp$Qtité.tot)**(2)
model12c = lm(Qtité.tot2~Apellation+Surface + Apellation:Surface,data=vins_appel_grp)
summary(model12c)
anova(model0012a,model12c)
Anova(model12c)
plot(model12c)


#modele 12d avec Quart 
model12d = lm(Qtité.tot~Apellation+Surface + Apellation:Surface +Quart  +Quart:Surface ,data=vins_appel_grp)
summary(model12d)
model0012d=lm(Qtité.tot~1,data=vins_appel_grp)
anova(model0012d,model12d)
Anova(model12d)
plot(model12d)

# Modèle 12dlog TRès Très bôôôô ! ! ! 
model12dlog = lm(log(Qtité.tot+1)~Apellation+Surface + Apellation:Surface +Quart  +Quart:Surface ,data=vins_appel_grp)
summary(model12dlog)
model0012dlog=lm(log(Qtité.tot+1)~1,data=vins_appel_grp)
anova(model0012dlog,model12dlog)
Anova(model12dlog)
par(mfrow = c(2,2))
plot(model12dlog)



model12dlog = lm(log(Qtité.tot+1)~Surface + Apellation:Surface +Quart  +Quart:Surface ,data=vins_appel_grp)
summary(model12dlog)
model0012dlog=lm(log(Qtité.tot+1)~1,data=vins_appel_grp)
anova(model0012dlog,model12dlog)
Anova(model12dlog)
plot(model12dlog)


#on effectue un passage au log aussi pour la surface pour garder l'homogénéité entre surface et production
model12dlog = lm(log(Qtité.tot+1)~Apellation+log(Surface+1) + Apellation:Surface +Quart  +Quart:Surface ,data=vins_appel_grp)
summary(model12dlog)
model0012dlog=lm(log(Qtité.tot+1)~1,data=vins_appel_grp)
anova(model0012dlog,model12dlog)
Anova(model12dlog)
par(mfrow = c(1,1))
plot(model12dlog)








#modele 13 a revoir

model13 = lm(Qtité.blanc~Apellation+Surface + Apellation:Surface ,data=vins_appel)
summary(model13)
model0013=lm(Qtité.blanc~1,data=vins_appel)
anova(model0013,model13)
Anova(model13)

#modele 14 a revoir
model14 = lm(Qtité.rouge~Apellation+Surface ,data=vins_appel)
summary(model14)
model0014=lm(Qtité.rouge~1,data=vins_appel)
anova(model0014,model14)
Anova(model14)

#modele 15 à revoir
model15 = lm(Qtité.rosé~Apellation+Surface + Apellation:Surface,data=vins_appel)
summary(model15)
model0015=lm(Qtité.rosé~1,data=vins_appel)
anova(model0015,model15)
Anova(model15)




# Test d'indépendances : 
superficie = vins[,2]
superficie_aop=vins[,3]
superficie_igp=vins[,5]
superficie_vsig=vins[,6]
aop_bl=vins[,7]
aop_rg=vins[,8]
aop_rs=vins[,9]
igp_bl=vins[,11]
prod_commerciale=vins[,19]
prod_noncommerciale=vins[,20]
prop_noncommerciale = prod_noncommerciale / (prod_commerciale+prod_noncommerciale)
prop_aop = superficie_aop /superficie
prop_igp = superficie_igp/superficie
prop_vsig = superficie_vsig/superficie

test1=cor.test(aop_rg,igp_bl)
test1
test2=cor.test(aop_rg,aop_bl)
test2
test3=cor.test(aop_rg,aop_rs)
test3
test4=cor.test(aop_rs,aop_bl)
test4
test5=cor.test(superficie_igp,superficie_vsig)
test5
test6=cor.test(superficie_aop,superficie_vsig)
test6
test7=cor.test(superficie_igp,superficie_aop)
test7
test8=cor.test(prop_aop,prop_noncommerciale)
test8
test9=cor.test(prop_igp,prop_noncommerciale)
test9
test10=cor.test(prop_vsig,prop_noncommerciale)
test10
