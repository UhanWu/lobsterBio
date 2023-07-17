#data7 <- data6[!duplicated(data6[,-1]),]

# no replicate
data7 <- data6[!is.na(data6$depth),]

namet <- names(data7)
namet <- gsub(" ",".",namet)
namet <- gsub("=",".",namet)
namet <- gsub("_",".",namet)
names(data7) <- namet

namet[grep("americanus",namet)]

data8 <- data7[data7$wtcpue.Homarus.americanus > 0,]
data8 <- data8[,c(2:3534,3535:3541,1)]
data81 <- data8[,2:3534]
data82 <- data81[,c(names(temp1[temp1 >0]))]
data9 <- cbind(data82,data8[,3534:3541])


temp1 <- apply(data8[,c(1:3533)],2,sum)

data9 <- data8[,c(which(temp1 > 476.2),3535:3541 )]
data9[,]

library(rpart)

fitr1 <- rpart(wtcpue.Homarus.americanus ~ .,data=data9[,-c(510,514,516)], control=rpart.control(cp=0.000001))
cp <- fitr1$cptable[which.min(fitr1$cptable[,"xerror"]),"CP"]
fitr2 <- prune(fitr1,cp=cp)
names(fitr2$variable.importance)
data10 <- data9[,c("wtcpue.Homarus.americanus",names(fitr2$variable.importance))]

name9 <- names(data9)
which(name9 == "wtcpue.Homarus.americanus")

fitr3 <- lm(wtcpue.Homarus.americanus ~ .,data=data10)
fitr4 <- step(fitr3)
fitr5 <- step(fitr4,k=log(length(data10[,1])))
library(gam)
data10$region <- factor(data10$region)

fitgam1 <- gam(wtcpue.Homarus.americanus ~ s(year) + s(lat) + s(wtcpue.Dichelopandalus.leptocerus) + 
    s(wtcpue.Asterias.rubens) + s(wtcpue.Crossaster.papposus) + s(wtcpue.Cucumaria.frondosa) + 
    s(wtcpue.Enchelyopus.cimbrius) + s(`wtcpue.Dfo:bryozoans.ectoprocta`) + 
    s(wtcpue.Hippoglossus.hippoglossus) + region + s(wtcpue.Hippoglossoides.platessoides) + 
    s(wtcpue.Arctica.islandica) + s(wtcpue.Hydrozoa) + s(`wtcpue.Dfo:gorgonocephalidae,Asteronychidae.f.(6300)`) + 
    s(wtcpue.Leptasterias) + s(wtcpue.Neptunea.lyrata), data = data10)
summary(fitgam1)


fitgam2 <- gam(wtcpue.Homarus.americanus ~ s(year) + s(lat) + s(wtcpue.Dichelopandalus.leptocerus) + 
    s(wtcpue.Asterias.rubens) + s(wtcpue.Crossaster.papposus) + s(wtcpue.Cucumaria.frondosa) + 
    s(wtcpue.Enchelyopus.cimbrius) + s(`wtcpue.Dfo:bryozoans.ectoprocta`) + 
    s(wtcpue.Hippoglossus.hippoglossus) + region + s(wtcpue.Hippoglossoides.platessoides) + 
    s(wtcpue.Arctica.islandica) + s(wtcpue.Hydrozoa) + `wtcpue.Dfo:gorgonocephalidae,Asteronychidae.f.(6300)` + 
    s(wtcpue.Leptasterias) + s(wtcpue.Neptunea.lyrata), data = data10)
summary(fitgam2)

name10 <- names(data10)
name10 <- gsub(":",".",name10)
name10 <- gsub(",",".",name10)
names(data10) <- name10

fitgam3 <- gam(wtcpue.Homarus.americanus ~ s(year) + s(lat) + s(wtcpue.Dichelopandalus.leptocerus) + 
    s(wtcpue.Asterias.rubens) + s(wtcpue.Crossaster.papposus) + s(wtcpue.Cucumaria.frondosa) + 
    s(wtcpue.Enchelyopus.cimbrius) + s(`wtcpue.Dfo.bryozoans.ectoprocta`) + 
    s(wtcpue.Hippoglossus.hippoglossus) + region + s(wtcpue.Hippoglossoides.platessoides) + 
    s(wtcpue.Arctica.islandica) + wtcpue.Hydrozoa + `wtcpue.Dfo.gorgonocephalidae.Asteronychidae.f.(6300)` + 
    s(wtcpue.Leptasterias) + s(wtcpue.Neptunea.lyrata), data = data10)
summary(fitgam3)


win.graph(12,12)
par(mfrow=c(4,4))
plot(fitgam3)

win.graph(12,12)
par(mfrow=c(4,4))
plot(fitgam3,se=T)
save(data10,fitgam1,fitgam2,fitgam3,file="h1.RDData")
save(fitr2,fitr3,fitr4,fitr5,file="h2.RData")






