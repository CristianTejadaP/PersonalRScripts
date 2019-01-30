#maps
lga.profile=read.csv("Vic-2013-LGA-Profiles-NoPc.csv")
lga=lga.profile$LGA
peopleno=lga.profile$Population.23
notwell=lga.profile$wellBeing.5*1000
smokers=lga.profile$Health.1*1000
boxplot(smokers,notwell)
snw.box=summary(smokers)
non.smokers.lga=snw.box[1] #create light and heavy smoker
light.smokers.lga=snw.box[2]
medium.smokers.lga=snw.box[5]
heavy.smokers.lga=snw.box[6]

#clasisigfy LGA depending on habits
habits=
  ifelse(smokers<non.smokers.lga,"clear",
         ifelse(smokers<light.smokers.lga,"light",
                ifelse(smokers<medium.smokers.lga,"medium",
                       "heavy")))
habits

snw=data.frame(lga,smokers,notwell)
snw["habits"]=habits
snw["habits"]=factor(habits)
snw=data.frame(lga,smokers,notwell,habits)
levels(snw$habits)
levels(habits)


hist(rnorm(5500,mean=20,sd=5))

####
concha=data.frame(measure=c((rnorm(55,mean=20,sd=1)),
                                (rnorm(48,mean=19,sd=5)),
                                (rnorm(60,mean=18,sd=1))),
                   place=c(rep("UPPER",55),
                           rep("MEDIUM",48),
                           rep("LOWER",60)))
head(concha)
str(concha)
leveneTest(measure~place,data=concha) #more than 0.05 , it say not problem with homogenety,anova good
bartlett.test(measure~place,data=concha)
lm0=lm(measure~place,data=concha)
plot(measure~place,data = concha)
Anova(lm0)
oneway.test(measure ~ place, data = concha)
?oneway.test()

#from excel data
datosconcha=read.csv("Habitat-size.csv",sep=",")
str(datosconcha)
plot(datosconcha)
library(car)
lm1=lm(MEASURE~PLACE,data=datosconcha)
Anova(lm1)
anova(lm1)
summary(aov(lm1))
leveneTest(MEASURE~PLACE,data=datosconcha)

#violated asumption
oneway.test(MEASURE ~ PLACE, data = datosconcha)
pairwise.t.test(datosconcha$MEASURE, datosconcha$PLACE,
                p.adjust.method = "BH", pool.sd = FALSE)

#PERMANOVA

library(vegan)

# two similar populations:
dat1a<-matrix(sample(c(0,1,1,1),200,replace=T),10,20)
dat1b<-matrix(sample(c(0,1,1,1),200,replace=T),10,20)

# generating a third sample from the same population, but with reduced
# number of species occurrences. this set will have higher
# beta-diversity (or "multivariate spread"):
dat2<-matrix(sample(c(0,0,0,1),200,replace=T),10,20)

# distance matrices:
fac<-gl(2,10)
dist11<-vegdist(rbind(dat1a,dat1b))
dist12<-vegdist(rbind(dat1a,dat2)) 
  
  # when computing sets with same beta-dispersion we get a
  # correct adonis result with no sign. group differences
  # in species composition:
anova(betadisper(dist11,fac))
adonis(rbind(dat1a,dat1b)~fac)

# when using sets with different beta-diversity you may
# get false significant adonis results - the location/composition
# is actually the same (!) this result is due to different
# multivariate spread in dat1 and dat2:
anova(betadisper(dist12,fac))
adonis(rbind(dat1a,dat2)~fac)

# see ordination diagram where location (centroids) between dat1 and dat2
# is not shifted more than for dat1a dat1b, still you yield a (false)
# sign. adonis result
# plot:

windows(10,5)

opar<-par()
par(mfrow=c(1,2))
plot(meta11<-metaMDS(dist11,zerodist=ignore),type="n",
     main="same beta-disp\nsame location")
points(meta11,select=which(fac==1),col="red")
points(meta11,select=which(fac==2),col="blue")
ordispider(meta11,group=fac)

plot(meta12<-metaMDS(dist12,zerodist=ignore),type="n",
     main="diff beta-disp\nsame location")
points(meta12,select=which(fac==1),col="red")
points(meta12,select=which(fac==2),col="blue")
ordispider(meta12,group=fac)

par(opar)
Labels: adonis assumptions , adonis() , betadisper() , Multivariate Spread , PERMANOVA , PERMANOVA Assumptions , R , Vegan Package


#second try with MANOVA
library(vegan)
moluscos=read.csv("moluscos.csv",sep=",")
birds<-read.csv('https://raw.githubusercontent.com/collnell/lab-demo/master/bird_by_fg.csv')
trees<-read.csv('https://raw.githubusercontent.com/collnell/lab-demo/master/tree_comp.csv')
head(birds)
head(trees)

bird.matrix<-as.matrix(birds[,3:9])##response variables in a sample x species matrix
trees$B<-as.factor(trees$B)

bird.manova<-manova(bird.matrix~as.factor(B), data=trees) ##manova test
summary(bird.manova) 
#Assumptions of MANOVA
#Normal distribution
#Linearity
#Homogeneity of variances
#Homogeneity of covariances

bird.mat<-sqrt(bird.matrix)
bird.dist<-vegdist(bird.mat, method='bray') 
#Bray-Curtis dissimilarity (abundance weighted)
#Jaccard (presence/absence)
#Gower's (non-continuous variables)
set.seed(36) #reproducible results

bird.div<-adonis2(bird.dist~DIVERSITY, data=birds, permutations = 999, method="bray", strata="PLOT")
bird.div

#######################
library(vegan)
moluscos=read.csv("moluscos.csv",sep=",")
molusco.matrix<-as.matrix(moluscos[,2:45])
moluscos.mat<-sqrt(molusco.matrix)
moluscos.dist<-vegdist(moluscos.mat, method='bray') 
#Bray-Curtis dissimilarity (abundance weighted)
#Jaccard (presence/absence)
#Gower's (non-continuous variables)
set.seed(36) #reproducible results
molusco.div<-adonis2(moluscos.dist~LOCALIDAD, data=moluscos, permutations = 999, method="bray", strata="PLOT")
molusco.div

#perzones in the intertidal
moluscozona=read.csv("moluscos_zonas.csv")
moluscozona.matrix<-as.matrix(moluscozona[,2:45])
moluscozona.mat<-sqrt(moluscozona.matrix)
moluscozona.dist<-vegdist(moluscozona.mat, method='bray') 
#Bray-Curtis dissimilarity (abundance weighted)
#Jaccard (presence/absence)
#Gower's (non-continuous variables)
set.seed(36) #reproducible results
moluscozona.div<-adonis2(moluscozona.dist~ZONAS, data=moluscozona, permutations = 999, method="bray", strata="PLOT")
moluscozona.div

moluscozona.matrix2<-as.matrix(moluscozona[,1:45])
moluscoMDS<-metaMDS(moluscozona.mat, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
moluscoMDS ##metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')

stressplot(moluscoMDS)
plot(moluscoMDS)

ordiplot(moluscoMDS,type="n")
orditorp(moluscoMDS,display="species",col="red",air=0.01)
orditorp(moluscoMDS,display="sites",cex=1.25,air=0.01)
ordiellipse(moluscoMDS,moluscozona$ZONAS,col=1:3,display="sites",cex=1.25,air=0.1,
            draw="polygon",label = T)
points(moluscoMDS, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)
#fin de NMDS

data(dune.env)
data(dune)
attach(dune)
attach(dune.env)
plot(ord, disp="sites", type="n")
ord <- metaMDS(dune)

ordihull(ord, Management, col=1:4, lwd=3)
ordiellipse(ord, Management, col=1:4, kind = "ehull", lwd=3)
ordiellipse(ord, Management, col=1:4, draw="polygon")
ordispider(ord, Management, col=1:4, label = TRUE)
points(ord, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)


