#####  Dynamic Regressions with SPC1 as response variable
{                          ### Construct NGARegrDat
  load("MSP_TableData.Rdata")
  AggrDat <- AggrDat[is.na(AggrDat$MSP)==F,]
  AggrDat <- AggrDat[AggrDat$MSP_approv == 1,]  ### Include only approved values **** Option
  #AggrDat$MSP <- AggrDat$minMSP ### Option: use minMSP as the potential predictor; also turn off log-transform
  #AggrDat$MSP <- AggrDat$MSP_this  ### Option
  #AggrDat$MSP <- AggrDat$MSP_after ### Option
  #AggrDat$MSP <- AggrDat$MSP_agen  ### Option
  
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, SPC1, MilTech, Cavalry, Agri, MSP)) 
  TableDat$MSP <- log10(TableDat$MSP)  ### Option: log-transform MSP
  
  dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
  source("fRegrDat.R")  
  NGARegrDat <- RegrDat
  rm( list = setdiff(ls(),c("NGARegrDat") ) )
}
######################################################################
{                         ### Model Selection
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time) )
  summary(fit <- glm(RegrDat))
  
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, Phylogeny, T) ) ## Drop non-sign or weak autocorr terms
  RegrDat <- RegrDat[is.na(RegrDat$Lag2)==FALSE,]  ### Since retaining Lag2 term, drop missing observations for this var
  summary(fit <- lm(RegrDat))
  out <- summary(fit <- glm(RegrDat))
  write.table(out$coefficients, "clipboard", sep="\t")
  
  ####  Exhaustive regressions with these terms
  print(paste("Response variable =",colnames(RegrDat)[1]))
  Predictors <- 3:ncol(RegrDat)
  output <- data.frame()
  for (nPred in 1:length(Predictors)){ print(nPred)
    Preds<- combn(Predictors, nPred)
    for(j in 1:length(Preds[1,])){
      fit <- lm(RegrDat[, c(1:2, Preds[,j])])
      Pval <- summary(fit)$coefficients[,4]
      tval <- summary(fit)$coefficients[,3]
      out <- vector("numeric",length = length(RegrDat))
      out[c(1:2,Preds[,j])] <- tval
      out <- c(out,summary(fit)$r.sq)
      fit <- glm(RegrDat[, c(1:2, Preds[,j])])
      AIC <- summary(fit)$aic
      n <- length(fit$residuals)
      p <- length(fit$coefficients)  
      out <- c(out,AIC,n,p)
      output <- rbind(output,out)
    }
  }
  colnames(output) <- c(colnames(RegrDat),"R-sq","delAIC", "n","p")
  output$delAIC <- output$delAIC - min(output$delAIC)
  write.csv(output, file="output.csv",  row.names=FALSE)
  out <- output[order(output$delAIC),]
  out <- out[out$delAIC < 3,]
  write.table(out, "clipboard", sep="\t", row.names=FALSE)
}

stop()
#####################################################################################
{   ###                       Additional tests 
  RD <- RegrDat
  summary(fit <- lm(RD))
  out <- summary(fit <- glm(RD))
  write.table(out$coefficients, "clipboard", sep="\t")
  
  
  NGARegrDat2 <- NGARegrDat[is.na(NGARegrDat$Lag2) == FALSE,]  ### With Lag2 = NA removed
  ### Check for Space and Phylogeny: NS
  RD1 <- cbind(RD, NGARegrDat2$Space, NGARegrDat2$Phylogeny)
  summary(fit <- lm(RD1))
  
  ### Check for T: NS
  RD1 <- cbind(RD, NGARegrDat2$Time)
  summary(fit <- lm(RD1))
  
  ### Check for NGA fixed effects: doesn't affect the main result
  RD1 <- cbind(RD, NGARegrDat2$NGA)
  summary(fit <- lm(RD1))
  summary(fit <- lm(RD)) ### For comparison, the model without NGA fixed effects
}

### Best regression model
summary(fit <- lm(RD))
out <- summary(fit <- glm(RD))
write.table(out$coefficients, "clipboard", sep="\t")


stop()
################################  End of the sequence for the article
### R2 without autocorrelation terms
RD1 <- subset(RD, select = -c(SPC1, SPC1.sq, Lag2)) ### R2 = 0.769
summary(fit <- lm(RD1))
out <- summary(fit <- glm(RD1))
write.table(out$coefficients, "clipboard", sep="\t")

########################################################################################
{###  Diagnostics: distribution of residuals is overdispersed
  summary(fit <- lm(RD))
  layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page
  plot(fit)
  layout(matrix(1),1,1)
  # Normality of Residuals
  library(MASS)
  sresid <- studres(fit)
  hist(sresid, breaks = seq(-10, 12, by=.25), freq=FALSE, main="Distribution of Studentized Residuals")
  xfit<-seq(-10, 12, by=.25)
  yfit<-dnorm(xfit)
  lines(xfit, yfit) 
  rm(sresid,xfit,yfit)
}#### 

###################################################################
{  ###   Run Bootstrap: the Best Model
  NGA_RD <- subset(NGARegrDat, select = -c(Phylogeny, Space, T))
  NGA_RD <- NGA_RD[complete.cases(NGA_RD),]
  Polities <- unique(NGA_RD$PolID)
  nPol <- length(Polities)
  nRep <- 1000
  
  out <- data.frame()
  for(iRep in 1:nRep){    print(iRep)
    RD <- data.frame()
    for(iPol in 1:nPol){
      iPol <- sample(Polities,1)
      dat <- NGA_RD[NGA_RD$PolID==iPol,]
      RD <- rbind(RD, dat)
    }
    RD <- subset(RD, select = -c(NGA, PolID, Time) )
    fit <- lm(RD)
    out <- rbind(out, fit$coefficients)
  }
  colnames(out) <- colnames(RD)
  write.csv(out, file="BootResult.csv",  row.names=FALSE)
  
  ### out <- read.table('BootResult.csv', sep=",", header=TRUE, stringsAsFactors = 	FALSE)
  BootRes <- data.frame(Predictor = colnames(out), P = NA, Lower=NA, Upper=NA, stringsAsFactors = 	FALSE)
  for(i in 1:ncol(out)){
    hist(out[,i])
    dt <- out[,i]
    dt <- dt[order(dt)]
    BootRes$P[i] <- 2*sum(dt < 0)/length(dt)
    BootRes$Lower[i] <- dt[25]
    BootRes$Upper[i] <- dt[976]
  }
  write.table(BootRes, "clipboard", sep="\t")
}

#########################################################################  END










