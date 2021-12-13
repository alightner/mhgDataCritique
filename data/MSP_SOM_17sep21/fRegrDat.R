#### fRegrDat constructs RegrDat, using ImpCCrepl from !SC_analyz.R
#####     Assumes columns: NGA, PolID, Time; RespVar; Predictors
load("PolsVars.Rdata")
InterpDat <- TableDat ### From Anlz_*
InterpDat <- InterpDat[complete.cases(InterpDat),]

NGAs <- unique(InterpDat$NGA)  
RegrDat <- data.frame()
for(iNGA in 1:length(NGAs)){
  dat <- InterpDat[InterpDat$NGA == NGAs[iNGA],]
  n <- nrow(dat)
  if(n > 1){rdat <- cbind(dat[2:n,1:4],dat[1:(n-1),4:length(dat)])
  RegrDat <- rbind(RegrDat,rdat)
  }
}
colnames(RegrDat)[4] <- paste0(colnames(RegrDat)[4],"(t+1)")
RegrDat$Time <- RegrDat$Time - 100 ### Set RegrDat$Time to t, not t+1
RegrDat <- cbind(RegrDat[,1:5], RegrDat[,5]^2,RegrDat[,6:ncol(RegrDat)])
colnames(RegrDat)[6] <- paste0(colnames(RegrDat)[5],".sq")

##### Calculate Space using estimated dpar (set in Anlz_*)
Space <- RegrDat[,1:3]
Space$Space <- 0

colMat <- colnames(DistMatrix)
rowMat <- rownames(DistMatrix)
for(i in 1:nrow(RegrDat)){
  t1 <- RegrDat$Time[i]
  dat <- RegrDat[RegrDat$Time==t1,c(1:3,5)]
  if(nrow(dat) > 1){
    delta <- vector(length=nrow(dat))
    for(j in 1:nrow(dat)){
      dt <- DistMatrix[colMat==dat$NGA[j],]
      delta[j] <- dt[rowMat==RegrDat$NGA[i]]
    }
    s <- exp(-delta/dpar)*dat[,4]
    s <- s[delta != 0]  ### Exclude i=j
    Space$Space[i] <- mean(s)
  }
}
RegrDat <- cbind(RegrDat,Space$Space)
colnames(RegrDat)[ncol(RegrDat)] <- "Space"

#### Calculate Language = matrix of linguistic distances
Phylogeny <- RegrDat[,1:4]
Phylogeny[,4] <- 0
colnames(Phylogeny) <- c("NGA","PolID","Time","Phylogeny")

for(i in 1:nrow(RegrDat)){
  t1 <- RegrDat$Time[i]
  dat <- RegrDat[RegrDat$Time==t1,c(1:3,5)]
  dat <- dat[dat$NGA != RegrDat$NGA[i],]   ### Exclude i = j
  PolID <- RegrDat$PolID[i]
  PolLang <- polities[polities$PolID==PolID,9:11]
  if(nrow(dat) > 1){
    weight <- vector(length=nrow(dat)) * 0
    for(j in 1:nrow(dat)){
      dt <- dat[j,]
      PolLang2 <- polities[polities$PolID==dt$PolID,9:11]
      if(PolLang[1,3]==PolLang2[1,3]){weight[j] <- 0.25}
      if(PolLang[1,2]==PolLang2[1,2]){weight[j] <- 0.5}
      if(PolLang[1,1]==PolLang2[1,1]){weight[j] <- 1}
    }
    s <- weight*dat[,4]
    Phylogeny$Phylogeny[i] <- mean(s)
  }
}
RegrDat <- cbind(RegrDat,Phylogeny$Phylogeny)
colnames(RegrDat)[ncol(RegrDat)] <- "Phylogeny"
RegrDat$T <- RegrDat$Time

#### Add time lag
Lag2 <- RegrDat[,1:4]
Lag2[,4] <- NA
for(i in 1:nrow(RegrDat)){
  t2 <- RegrDat$Time[i] - 100
  NGA <- RegrDat$NGA[i]
  dat <- RegrDat[RegrDat$Time==t2 & RegrDat$NGA == NGA,]
  if(nrow(dat)>1){print("Error: more than one Lag2")}
  if(nrow(dat)==1){Lag2[i,4] <- dat[1,5]}
}
RegrDat <- cbind(RegrDat,Lag2[,4])
colnames(RegrDat)[ncol(RegrDat)] <- "Lag2"

### Add differenced response variable
# RegrDat$DelResp <- RegrDat[,4] - RegrDat[,5]
NonStRegrDat <- RegrDat
# Standardized coefficients
for(i in 4:(ncol(RegrDat))){
  RegrDat[,i] <- (RegrDat[,i] - mean(RegrDat[,i], na.rm=TRUE))/sd(RegrDat[,i],na.rm=TRUE) }
