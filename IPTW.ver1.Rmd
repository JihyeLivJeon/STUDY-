
#======================================================#
# Author : JH
# DATE : 2023 - 05 - 19
# SUBJECT : IPTW n Competing risk
# REFERENCE : 1) http://rstudio-pubs-static.s3.amazonaws.com/198545_5fae2128569a4e4a80627bc87c71d1e2.html 
# REFERENCE : 2) https://rpubs.com/sar03/642530
#======================================================#

#Load the library
library(survminer)
library(WeightIt)
library(boot)
library(gridExtra)
library(gbm)            ####arrangement of charts
library(jskm)           ####weighted Kaplan-Meier charts
library(tableone)       ####comparison tables
library(VGAM)           ####Logistic regression model for PS generation
library(TriMatch)
library(dplyr)          ####data cleaning
library(survey)         ####weighting of tables
library(survival)
library(cmprsk)
library(ggplot2)

#============DATA PREPARATION============#

setwd("Z:/JH/SPC/DATA")
DATA.2 <- read.csv("STUDYPOP.FINAL.DATA.csv", header = T)
DATA.2 <- DATA.2[,-1]

DATA.2$PC.YEAR.GRADE <- factor(DATA.2$PC.YEAR.GRADE, levels = c("1","2","3"))  
DATA.2$ADT           <- factor(DATA.2$ADT, levels = c("0","1"))
DATA.2$CCI           <- factor(DATA.2$CCI, levels = c("1","2","3"))
DATA.2$INCOME.GRADE  <- factor(DATA.2$INCOME.GRADE, levels = c("1","2","3"))
DATA.2$cause         <- ifelse(DATA.2$SPC_CODE == "C16", 6, #Rank 1 : Stomach
                               ifelse(DATA.2$SPC_CODE == "C34", 5, #Rank 2 : Lung
                                      ifelse(DATA.2$SPC_CODE == "C18", 4, #Rank 3
                                             ifelse(DATA.2$SPC_CODE == "C22", 3, #Rank 4
                                                    ifelse(DATA.2$SPC_CODE == "C67", 2, #Rank 5
                                                           ifelse(DATA.2$SPC_CODE == "C20", 1,0)))))) #Rank 6, # Other Cancer & Death
## JY: Other cancer -> cause 7
## JY: Death & Censoring -> cause 0

DATA.2$SPC.DATE      <- as.Date(ifelse((DATA.2$cause == 0 & DATA.2$SPC_CODE!=""), DATA.2$INDX_DATE_SPC,
                                       ifelse((DATA.2$cause == 0 & DATA.2$SPC_CODE==""), DATA.2$LAST_DATE, DATA.2$INDX_DATE_SPC)))
DATA.2$first_trt_date<- as.Date(DATA.2$first_trt_date)
DATA.2$SPC.TIME      <- as.numeric((DATA.2$SPC.DATE-DATA.2$first_trt_date)/365.25)    #first_trt_date :  first treatment date (RP or RT)

kableone <- function(x,...){
  capture.output(x<-print(x))
  knitr::kable(x,...)
}  ###makes nice tables with tableone



#============Propensity Score Generation and Weighting============#
## Function to add generalized PS to dataset
AddGPS   <- function(data,
                     formula  = as.formula(formula),
                     psPrefix = "PS_",
                     family   = multinomial(parallel = FALSE)) {
  ## Fit multinomial logistic regression
  resVglm <- vglm(formula = formula,
                  data    = data,
                  family  = family)
  ## Calculate PS
  psData        <- as.data.frame(predict(resVglm, type = "response")).  ## P(Z=1|X)
  names(psData) <- paste0(psPrefix, names(psData))
  ## Add to data
  cbind(data, psData)
}

DATA.2A <- AddGPS(DATA.2, 
                  ## Propensity score model for multinomial regression
                  formula = as.formula('TRT.TYPE ~ AGE.PC + PC.YEAR.GRADE + ADT + CCI + INCOME.GRADE'))


AddIPTW <- function(data, txVar = "Tr", tx = c('RT', 'RP'), psPrefix = "PS_"){
  
  ##Treatment indicator data frame (any number of groups allowed)
  dfAssign <- as.data.frame(lapply(tx, function(tx_k){
    as.numeric(data[txVar] == tx_k)
  }))
  colnames(dfAssign) <- paste0(txVar, tx)
  #Name of PS variables
  psVars <- paste0(psPrefix, tx)
  #Pick denominator (PS for assigned treatment)
  data$PSassign <-rowSums(data[psVars]*dfAssign) ## JY: why rowSums?; weight: for treated:1/ps, for untreated, 1/(1-ps)
  #Calculate the IPTW
  data$iptw <- exp(- log(data$PSassign)) ## JY: why exp(-log())? is there 0? why not just inverse? -> 1/ps
  #Return the whole data
  data
}

DATA.2B <- AddIPTW(data  = DATA.2A,       # DATA SET
                   txVar = 'TRT.TYPE',    # Treatment variable name
                   tx    = c('RT', 'RP')) # Treatment levels

#The stabilised IPTW is calculated by normalising the IPTW to the number of cases of each treatment.
averagePS <- as.data.frame(DATA.2B$iptw)
names(averagePS) <- 'iptw'
for (i in 1:nrow(averagePS)) {
  averagePS$iptw[i] <- mean(DATA.2B$iptw[i])
}

dataPS         <- cbind(DATA.2, averagePS)

stabilisedIPTW <- ifelse(dataPS$TRT.TYPE == 'RP', dataPS$iptw*(plyr::count(dataPS$TRT.TYPE)[1,2]/nrow(dataPS)),
                         ifelse(dataPS$TRT.TYPE == 'RT', dataPS$iptw*(plyr::count(dataPS$TRT.TYPE)[2,2]/nrow(dataPS)), NA))

dataPS$sw      <- stabilisedIPTW

##Check weight distribution
summary(dataPS$iptw) #Naive IPTW
summary(dataPS$sw) #Stablized IPTW



#============Post-weighting balance assessment============#

#Covariate Balance
Unadjusted <- CreateTableOne(data = DATA.2, vars = c('AGE.PC', 'PC.YEAR.GRADE', 'ADT', 'CCI', 'INCOME.GRADE'), strata = 'TRT.TYPE')
iptwsvy    <- svydesign(ids = ~ 1, data = dataPS, weights = ~ sw) #####weighted analysis
iptw       <- svyCreateTableOne(vars = c('AGE.PC', 'PC.YEAR.GRADE', 'ADT', 'CCI', 'INCOME.GRADE'), strata = 'TRT.TYPE', data = iptwsvy)

#smd value
ExtractSmd(Unadjusted)
ExtractSmd(iptw)
#baseline chart
kableone(CreateTableOne(data = DATA.2, vars = c('AGE.PC', 'PC.YEAR.GRADE', 'ADT', 'CCI', 'INCOME.GRADE'), strata = "TRT.TYPE"))
kableone(svyCreateTableOne(vars = c('AGE.PC', 'PC.YEAR.GRADE', 'ADT', 'CCI', 'INCOME.GRADE'), strata = "TRT.TYPE", data = iptwsvy))

#SMD PLOT
dataPlot <- data.frame(variable   = rownames(ExtractSmd(Unadjusted)),
                       Unadjusted = ExtractSmd(Unadjusted),
                       Weighted   = ExtractSmd(iptw))
names(dataPlot)<-c("variable",'Unadjusted','Weighted')

library(reshape2)
dataPlotMelt <- melt(data          = dataPlot,
                     id.vars       = "variable",
                     variable.name = "method",
                     value.name    = "SMD")

varsOrderedBySmd <- rownames(dataPlot)[order(dataPlot[,"Unadjusted"])]

dataPlotMelt$variable <- factor(dataPlotMelt$variable,
                                levels = varsOrderedBySmd)
dataPlotMelt$method   <- factor(dataPlotMelt$method,
                                levels = c("Weighted","Unadjusted"))

library(ggplot2)
smd.plot <- ggplot(data = dataPlotMelt, 
                   mapping = aes(x = variable, y = SMD, group = method, linetype = method, color = method)) +
                   scale_colour_manual(values = c("#374E55FF", "#DF8F44FF")) + 
                   geom_line(size = 1.3) +
                   geom_point(size = 1.8) +
                   geom_hline(yintercept = 0, size = 0.3) +
                   geom_hline(yintercept = 0.1, size = 0.1) +
                   coord_flip() +
                   theme_bw() + theme(legend.key = element_blank())

##SAVE SMD PLOT
setwd("Z:/JH/SPC/Result")
ggsave(plot, 
       file = "051723SMD_JH.pdf",
       width = 6.4,
       height = 5,
       units = "in",
       dpi = 300)



#============Competing Risk============#
fit    <- survfit(Surv(SPC.TIME, cause, type = "mstate") ~ 1, data = dataPS, weights = sw)
fit.RP <- survfit(Surv(SPC.TIME, cause, type = "mstate") ~ 1, data = dataPS, weights = sw, subset = (TRT.TYPE == "RP"))
fit.RT <- survfit(Surv(SPC.TIME, cause, type = "mstate") ~ 1, data = dataPS, weights = sw, subset = (TRT.TYPE == "RT"))

##JY: cumhaz returns cumulative hazard functions, not cumulative incidence function
##RP&RT
df.fit <- data.frame(x   = c(rep(fit$time, 6)),
                     y   = c(fit$cumhaz[,1], fit$cumhaz[,2], fit$cumhaz[,3], fit$cumhaz[,4], fit$cumhaz[,5], fit$cumhaz[,6]),
                     SPC = c(rep("6", nrow(fit$cumhaz)),
                             rep("5", nrow(fit$cumhaz)),
                             rep("4", nrow(fit$cumhaz)),
                             rep("3", nrow(fit$cumhaz)),
                             rep("2", nrow(fit$cumhaz)),
                             rep("1", nrow(fit$cumhaz))))

plot.fit <- ggplot(df.fit, aes(x = x, y = y, color = SPC, linetype = SPC)) +
                  geom_step(position = "stack") +
                  theme_bw() +
                  xlab("Time since Treatment (years)") +
                  ylab("Probability of a SPC") +
                  theme(legend.position = "top") +
                  scale_x_continuous(limits = c(0,18)); plot.fit

#RP
df.fit.RP  <- data.frame(x   = c(rep(fit.RP $time, 6)),
                         y   = c(fit.RP$cumhaz[,1], fit.RP$cumhaz[,2], fit.RP$cumhaz[,3], fit.RP$cumhaz[,4], fit.RP$cumhaz[,5], fit.RP$cumhaz[,6]),
                         SPC = c(rep("6", nrow(fit.RP$cumhaz)),
                                 rep("5", nrow(fit.RP$cumhaz)),
                                 rep("4", nrow(fit.RP$cumhaz)),
                                 rep("3", nrow(fit.RP$cumhaz)),
                                 rep("2", nrow(fit.RP$cumhaz)),
                                 rep("1", nrow(fit.RP$cumhaz))))

plot.fit.RP  <- ggplot(df.fit.RP , aes(x = x, y = y, color = SPC, linetype = SPC)) +
                      geom_step(position = "stack") +
                      theme_bw() +
                      xlab("Time since Treatment (years)") +
                      ylab("Probability of a SPC") +
                      theme(legend.position = "top") +
                      scale_x_continuous(limits = c(0,18)); plot.fit.RP 

#RT
df.fit.RT  <- data.frame(x   = c(rep(fit.RT $time, 6)),
                         y   = c(fit.RT$cumhaz[,1], fit.RT$cumhaz[,2], fit.RT$cumhaz[,3], fit.RT$cumhaz[,4], fit.RT$cumhaz[,5], fit.RT$cumhaz[,6]),
                         SPC = c(rep("6", nrow(fit.RT$cumhaz)),
                                 rep("5", nrow(fit.RT$cumhaz)),
                                 rep("4", nrow(fit.RT$cumhaz)),
                                 rep("3", nrow(fit.RT$cumhaz)),
                                 rep("2", nrow(fit.RT$cumhaz)),
                                 rep("1", nrow(fit.RT$cumhaz))))

plot.fit.RT  <- ggplot(df.fit.RT , aes(x = x, y = y, color = SPC, linetype = SPC)) +
                      geom_step(position = "stack") +
                      theme_bw() +
                      xlab("Time since Treatment (years)") +
                      ylab("Probability of a SPC") +
                      theme(legend.position = "top") +
                      scale_x_continuous(limits = c(0,18)); plot.fit.RT


comp <- grid.arrange(plot.fit, plot.fit.RP, plot.fit.RT, ncol = 3)

setwd("Z:/JH/SPC/Result")
ggsave(comp, 
       file = "051723comp_cif_JH.pdf",
       width = 6.4,
       height = 5,
       units = "in",
       dpi = 300)


## estimate weighted Aalen-Johanson 
## weight: propensity score
## Calculating weights
m.treatment <-  glm(TRT.TYPE ~ AGE.PC + PC.YEAR.GRADE + ADT + CCI + INCOME.GRADE, data=dat, family=binomial(link="logit"))
dat$weight <- ifelse(dat$TRT.TYPE=="RP", 1/predict(m.treatment, type="response"), 1/(1-predict(m.treatment, type="response"))) ## JY: CHECK RP == 1!
## or stablized weight


## compute the ATE at times 
fit.RP <- survfit(Surv(time, fstatus, type = "mstate") ~ 1, data = dt, weights = weight, subset = (TRT.TYPE == "RP"))
data.frame(fit.RP$time, fit.RP$pstate)
fit.RT <- survfit(Surv(time, fstatus, type = "mstate") ~ 1, data = dt, weights = weight, subset = (TRT.TYPE == "RT"))
data.frame(fit.RT$time, fit.RT$pstate)

# fit.RP - fit.RT : bootstrap percentile for risk difference at time 1, 5, 10, 15
