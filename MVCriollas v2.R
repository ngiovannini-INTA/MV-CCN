#Leo el archivo original y le doy formato
R <- read.csv("C:/Dropbox/DATA/CRIOLLO/Multivariado Criollos/R.csv", sep=";")
View(R)

R <- within(R, {
  idr <-as.factor(idr)
  lp <- as.factor(lp)
  down <- as.factor(down)
  cppt <- as.factor(cppt)
  mm <- as.factor(mm)
  pfn <- as.factor(pfn)
  curs <- as.factor(curs)
  curt <- as.factor(curt)
  oret <- as.factor(oret)
  ored <- as.factor(ored)
})

#Para los descriptivos de las variables
library(psych)
describe(R)

#Tablas cruzadas

#2 factores
tapply(R$PT,R$provincia, mean, na.rm=T)
tapply(R$PT,R$provincia, sd, na.rm=T)


#Frecuencias Variables Cuali
edad<-prop.table(table(R$edad,R$provincia),2)
lp<-prop.table(table(R$lp,R$provincia),2)
down<-prop.table(table(R$down,R$provincia),2)
cppt<-prop.table(table(R$cppt,R$provincia),2)
mm<-prop.table(table(R$mm,R$provincia),2)
pfn<-prop.table(table(R$pfn,R$provincia),2)
curt<-prop.table(table(R$curt,R$provincia),2)
oret<-prop.table(table(R$oret,R$provincia),2)
ored<-prop.table(table(R$ored,R$provincia),2)

cuali<-rbind(edad,lp,down,cppt,mm,pfn,curt,oret,ored)

#Para los graficos de barras
barplot(table(R$ored))

#Multivariado
library(FactoMineR)

R.FAMD<-R[, c("CC","LG", "AG", "AP", "LC", "AC", "DL", "PC", "ACR", "PT", "edad", "lp", "down", "cppt", "mm", "pfn", "curt", "oret", "ored", "provincia","idr")]
res<-FAMD(R.FAMD, ncp=5, sup.var=20:21, graph=TRUE)

summary.FAMD(res)

plot.FAMD(res)
plot(res, invisible=(c("quali","quanti")))
plot(res, choix="ind", lab.ind=FALSE, invisible=(c("quali","quanti")),habillage="provincia",new.plot = TRUE)
plot(res, choix="var", axes=c(1,3)) 
?plot
remove(TresD)

#Grafico de las variables
var3D<-as.data.frame(res$var$coord)
#http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization



library(plot3D)
png(file="C:/Dropbox/DATA/CRIOLLO/Multivariado Criollos/vars3D.png", width=800, heigh=600 )
scatter3D(var3D$Dim.1, var3D$Dim.2, var3D$Dim.3, phi = 0, bty = "g",  type = "h", 
          ticktype = "detailed", pch = 19, xlab = "Dim.1", ylab ="Dim.2", 
          zlab = "Dim.3",colvar=NULL) 


text3D(var3D$Dim.1, var3D$Dim.2, var3D$Dim.3,  labels = rownames(var3D), add = TRUE, cex = 1, adj=1.2 )
dev.off()

#Grafico 3D individuos
uno<-as.data.frame(res$ind$coord)
str(uno)
uno$idr2=rownames(uno)

#rownames(R)=R$idr 

dos<-as.data.frame(R[,c(1,2)])
dos$idr2=rownames(dos)


TresD<-merge(uno,dos,by="idr2")


# Grafico que se puede rotar!!!!!
library(rgl)
?plot3d()
plot3d(TresD$Dim.1,TresD$Dim.2,TresD$Dim.3,TresD$idr,
col=rainbow(6)[TresD$provincia],
xlab="Dim.1", ylab="Dim.2", zlab="Dim.3", 
type="s", size=0.5)
legend3d("topright", legend = c("Cordoba","Formosa","La Pampa","La Rioja","Neuquen","San Luis"), pch = 16, col = rainbow(6), cex=1, inset=c(0.02))

library(ggplot2)

#de a pares o mas
ggplot(subset(TresD, provincia==c("neuquen","lapampa","larioja","formosa")), aes(x=Dim.1, y=Dim.2, colour=provincia)) + geom_point()
ggplot(subset(TresD, provincia==c("neuquen","lapampa")), aes(x=Dim.1, y=Dim.3, colour=provincia)) + geom_point()
ggplot(subset(TresD, provincia==c("neuquen","lapampa")), aes(x=Dim.2, y=Dim.3, colour=provincia)) + geom_point()

#matriz individual
ggplot(TresD, aes(x=Dim.1, y=Dim.2, colour=provincia)) + geom_point()+facet_wrap( ~ provincia, nrow=3)

#todo junto
png(file="C:/Dropbox/DATA/CRIOLLO/Multivariado Criollos/Inds2Da.png", width=800, heigh=600 )
ggplot(TresD, aes(x=Dim.1, y=Dim.2, colour=provincia, ellipses=provincia)) + geom_point() + stat_ellipse() + scale_colour_hue("Población", breaks = c("cordoba", "formosa", "lapampa", "larioja", "neuquen", "sanluis"), labels = c("CBA", "FOR", "LPA", "LRI", "NQN", "SLU"))
  #Para hacerlo en B&W reemplazar colour por shape
dev.off()

#Analisis de clusters
res.hcpc = HCPC(res, nb.clust=4, method="ward", metric="euclidean", description = TRUE )
res.hcpc$data.clust 


res.hcpc$call$t$inert.gain
barplot(res.hcpc$call$t$inert.gain)

png(file="C:/Dropbox/DATA/CRIOLLO/Multivariado Criollos/Dendo.png", width=800, heigh=600 )
plot.HCPC(res.hcpc, choice="tree", tree.barplot = F, rect = F)
dev.off()


png(file="C:/Dropbox/DATA/CRIOLLO/Multivariado Criollos/ScatterCluster.png", width=800, heigh=600 )
ggplot(res.hcpc$call$X, aes(x=Dim.1, y=Dim.2, colour=clust, ellipses=clust)) + geom_point() + stat_ellipse() + scale_colour_hue("Cluster")
dev.off()


res.hcpc$call$X

library(gmodels)
CrossTable(res.hcpc$data.clust$clust, res.hcpc$data.clust$provincia, dnn = c("Cluster", "Provincia"), expected = TRUE, format = "SPSS")

#Test de Mantel
res.hcpc$call$t$res$ind$dist
res$ind$dist
