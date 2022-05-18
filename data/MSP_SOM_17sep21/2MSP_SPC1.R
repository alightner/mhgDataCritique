#####  Dynamic Regressions with MSP as response variable
{                          ### Construct NGARegrDat
  load("MSP_TableData.Rdata")
  AggrDat <- AggrDat[AggrDat$MSP_approv == 1,]  ### Include only approved values **** Option
  AggrDat$Agri.sq <- AggrDat$Agri^2
  
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, MSP, SPC1, MilTech, Cavalry, Agri, Agri.sq, Pastor, EnvPC1, EnvPC2))
  TableDat$MSP <- log10(TableDat$MSP) 
  
  # for(i in 1:ncol(TableDat)) { print(c(colnames(TableDat)[i], sum(is.na(TableDat[,i])))) }
  dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
  source("fRegrDat.R")  
  NGARegrDat <- RegrDat
  rm( list = setdiff(ls(),c("NGARegrDat") ) )
}

######################################################################
{                         ### Model Selection
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time) )
  summary(fit <- glm(RegrDat))
  
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Phylogeny, Lag2) ) ## Drop non-sign or weak autocorr terms
  summary(fit <- lm(RegrDat))
  
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

#####################################################################################
{   ###                       Additional tests 
  RD <- subset(RegrDat, select = -c(SPC1))   ###  Drop NS terms
  summary(fit <- lm(RD))
  
  ### Check for Space: NS
  RD1 <- cbind(RD, NGARegrDat$Space)
  summary(fit <- lm(RD1))

  ### Check for Phylogeny: negative, NS
  RD1 <- cbind(RD, NGARegrDat$Phylogeny)
  summary(fit <- lm(RD1))
  
  ### Check for T: p = 0.03, EnvPC1 NS
  RD1 <- cbind(RD, NGARegrDat$Time)
  summary(fit <- lm(RD1))
  
  ### Check for Lag2: P = 0.046; doesn't affect the main result
  RD1 <- cbind(RD, NGARegrDat$Lag2)
  summary(fit <- lm(RD1))
  
  ### Check for NGA fixed effects: EnvPC1, EnvPC2 NS (as expected), Pastor NS; MilTech, Cav, Agri unaffected
  RD1 <- cbind(RD, NGARegrDat$NGA)
  summary(fit <- glm(RD1))
  summary(fit <- glm(RD)) 
}

{### Best regression model
  summary(fit <- lm(RD))
  out <- summary(fit <- glm(RD))
  write.table(out$coefficients, "clipboard", sep="\t")
  
  ### R2 without autocorrelation terms
  RD1 <- subset(RD, select = -c(MSP, MSP.sq)) ### R2 = 0.710
  summary(fit <- lm(RD1))
  out <- summary(fit <- glm(RD1))
  write.table(out$coefficients, "clipboard", sep="\t")
}

########################################################################################
{###  Diagnostics
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
}

###################################################################
{  ###   Run Bootstrap: the Best Model
  NGA_RD <- subset(NGARegrDat, select = -c(SPC1, Space, Phylogeny, T, Lag2))  #### Best by AIC
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
  BootRes <- data.frame(Predictor = colnames(out), Lower=NA, Upper=NA, P = NA, stringsAsFactors = 	FALSE)
  for(i in 1:ncol(out)){
    hist(out[,i])
    dt <- out[,i]
    dt <- dt[order(dt)]
    BootRes$P[i] <- sum(dt < 0)/length(dt)
    BootRes$Lower[i] <- dt[25]
    BootRes$Upper[i] <- dt[976]
  }
  BootRes$P[c(3,7)] <- 1 - BootRes$P[c(3,7)] ### Negative MSP.sq and Agri.sq
  write.table(BootRes, "clipboard", sep="\t")
}

#########################################################################  END





