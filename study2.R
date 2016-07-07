
# unemployed
datiu <- read.csv2("282uX2.csv")

# others
datio <- read.csv2("426oX.csv")

# unemployed n = 282
# scales
FSu <- datiu[,1:8]
BAIu <- datiu[,42:62]
BDIu <- datiu[,21:41]
SPANEu <- datiu[,9:20]
SPANEpu<-SPANEu[,c(1,3,5,7,10,12)]
SPANEnu<-SPANEu[,c(2,4,6,8,9,11)]
SPANEbu <- SPANEpu - SPANEnu
# total scores
totFSu <- rowSums(FSu)
totBAIu <- rowSums(BAIu)
totBDIu <- rowSums(BDIu)
totSPu<-rowSums(SPANEpu)
totSNu<-rowSums(SPANEnu)
totSBu <- totSPu - totSNu


# others (control group)  n = 426
# scales
FSo <- datio[,11:18] 
SPANEo <- datio[,19:30]
baio <- datio[,31:51]
bdio <- datio[,52:72]
SPANEpo <-SPANEo[,c(1,3,5,7,10,12)]
SPANEno <-SPANEo[,c(2,4,6,8,9,11)]
SPANEbo <- SPANEpo - SPANEno
# total scores
totFSo <- rowSums(FSo)
totSPo <- rowSums(SPANEpo)
totSNo <- rowSums(SPANEno)
totBDIo <- rowSums(bdio)
totBAIo <- rowSums(baio)

#################################################################################
#############################  STUDY 2  #########################################
#################################################################################


library(psych)

# descriptive statistics
# TABLE 7
describe(totFSu)
describe(totSPu)
describe(totSNu)
describe(totSBu)
describe(FSu)
describe(SPANEpu)
describe(SPANEnu)
describe(SPANEbu)
alpha(FSu)
alpha(SPANEpu)
alpha(SPANEnu)
alpha(SPANEbu)

##############################
######## TABLES 8 - 9 ########
##############################
library(lavaan)
library(semTools)
library(semPlot)



# CFA Flourishing Scale unemployed
modF <- 'f1 =~ fs1 + fs2 + fs3 +fs4 + fs5 + fs6 + fs7 + fs8'
fitF<-cfa(model = modF, orthogonal = FALSE, data = FSu, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
summary(fitF, fit.measures=TRUE)

# CFA Flourishing Scale control group
modFo <- 'f1 =~ fs1 + fs2 + fs3 +fs4 + fs5 + fs6 + fs7 + fs8'
fitFo<-cfa(model = modFo, orthogonal = FALSE, data = FSo, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
summary(fitFo, fit.measures=TRUE)

# Measurement Invariance FS
FsMI <- rbind(FSo,FSu)
gruppo <- c(rep("o",426),rep("u",282))
ddFS <- data.frame(gruppo, FsMI)

measurementInvariance(modF,
                      data=ddFS,
                      group="gruppo",
                      strict = TRUE)


# CFA SPANE unemployed
modS <- 'f1 =~ s1 + s3 + s5 + s7 + s10 + s12
f2 =~ s2 + s4 + s6 + s8 + s9 + s11'
fitS<-cfa(model = modS, orthogonal = FALSE, data = SPANEu, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
summary(fitS, fit.measures=TRUE)
#moreFitIndices(fitS)
#semPaths(fitS, "std",
         curvePivot = TRUE, thresholds = FALSE)

# CFA SPANE control group
modSo <- 'f1 =~ s1 + s3 + s5 + s7 + s10 + s12
f2 =~ s2 + s4 + s6 + s8 + s9 + s11'
fitSo<-cfa(model = modSo, orthogonal = FALSE, data = SPANEo, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
summary(fitSo, fit.measures=TRUE)

# Measurement Invariance SPANE
FsMI <- rbind(SPANEo,SPANEu)
gruppo <- c(rep("o",426),rep("u",282))
ddS <- data.frame(gruppo, FsMI)

measurementInvariance(modS,
                      data = ddS,
                      group = "gruppo",
                      strict = TRUE)


################################
# EVALUATING GROUP DIFFERENCES #
################################

# MANOVA
group <- c(rep("oth",426),rep("unemp",282))
fs <- c(totFSo, totFSu)
sp <- c(totSPo, totSPu)
sn <- c(totSNo, totSNu)

M <- data.frame(group,fs,sp,sn)


wb.manova <- manova(cbind(fs, sp, sn)~ as.factor(group),data = M)
summary(wb.manova)
summary.aov(wb.manova)


####################
# Steiger's z test #
####################

library(cocor)

data <- cbind(totFSu,totSPu,totSNu,totBDIu,totBAIu)
cocor(~totSNu + totBDIu | totSNu + totBAIu, data)
cocor(~totSPu + totBDIu | totSPu + totBAIu, data)
cocor(~totFSu + totBDIu | totFSu + totBAIu, data)


#################
### Cohen's d ###
#################

library(effsize)
# Flourishing Scale: 
condition <- factor(c(rep("unemp",times=282),rep("oth",times=426)))
totF <- c(totFSu,totFSo)
Flourish <- data.frame(condition, totF)
cohen.d(totF, condition)

# SPANE-Positive: 
condition <- factor(c(rep("unemp",times=282),rep("oth",times=426)))
totSP <- c(totSPu,totSPo)
SPANEp <- data.frame(condition,totSP)
cohen.d(totSP, condition)

# SPANE-Negative: 
condition <- factor(c(rep("unemp",times=282),rep("oth",times=426)))
totSN <- c(totSNu,totSNo)
SPANEn <- data.frame(condition,totSN)
cohen.d(totSN, condition)

# BDI
condition <- factor(c(rep("unemp",times=282),rep("oth",times=426)))
totBDI <- c(totBDIu,totBDIo)
BDI <- data.frame(condition,totBDI)
cohen.d(totBDI, condition)

# BAI
condition <- factor(c(rep("unemp",times=282),rep("oth",times=426)))
totBAI <- c(totBAIu,totBAIo)
BAI <- data.frame(condition,totBAI)
cohen.d(totBAI, condition)



####################
##### figure 1 #####
####################

library(overlapping)

dataList <- list(totFSu,totFSo)
overlap(dataList,plot=TRUE)

dataList <- list(totSPu,totSPo)
overlap(dataList,plot=TRUE)

dataList <- list(totSNu,totSNo)
overlap(dataList,plot=TRUE)

dataList <- list(totBDIu,totBDIo)
overlap(dataList,plot=TRUE)

dataList <- list(totBAIu,totBAIo)
overlap(dataList,plot=TRUE)



######################
# TABLE 10           #
# correlation matrix #
######################

tt <- cbind(totFSu,totSPu,totSNu,totBDIu,totBAIu)
round(cor(tt),2)

round(cor(cbind(totFSo,totSPo,totSNo,totBDIo,totBAIo)),2)


