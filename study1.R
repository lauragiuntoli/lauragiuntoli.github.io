
library(car)
library(psych)
library(lavaan)

dati <- read.csv("study1.csv", header = TRUE, sep = ";", na.string = " ")


#################################################################################
#############################  STUDY 1  #########################################
#################################################################################

dati1 <- dati[1:218,]   #online
dati2 <- dati[219:684,] #paper


#online
SPANE_o <- dati1[,28:39]
SPANEp_o<-SPANE_o[,c(1,3,5,7,10,12)]
totSPANEp_o<-rowSums(SPANEp_o)
SPANEn_o<-SPANE_o[,c(2,4,6,8,9,11)]
totSPANEn_o<-rowSums(SPANEn_o)
SPANEb_o <- SPANEp_o - SPANEn_o
totSPANEb_o<-totSPANEp_o - totSPANEn_o
FLO_o <- dati1[,40:47]
totFLO_o<-rowSums(FLO_o)

#paper
SPANE_p <- dati2[,28:39]
SPANEp_p<-SPANE_p[,c(1,3,5,7,10,12)]
totSPANEp_p<-rowSums(SPANEp_p)
SPANEn_p<-SPANE_p[,c(2,4,6,8,9,11)]
totSPANEn_p<-rowSums(SPANEn_p)
SPANEb_p <- SPANEp_p - SPANEn_p
totSPANEb_p<-totSPANEp_p - totSPANEn_p
FLO_p <- dati2[,40:47]
totFLO_p<-rowSums(FLO_p)
############################################

#########
# tab 1 #
#########

#online
describe(totFLO_o)
describe(totSPANEp_o)
describe(totSPANEn_o)
describe(totSPANEb_o)
describe(FLO_o)
describe(SPANEp_o)
describe(SPANEn_o)
describe(SPANEb_o)
alpha(FLO_o)
alpha(SPANEp_o)
alpha(SPANEn_o)
alpha(SPANEb_o)

#paper
describe(totFLO_p)
describe(totSPANEp_p)
describe(totSPANEn_p)
describe(totSPANEb_p)
describe(FLO_p)
describe(SPANEp_p)
describe(SPANEn_p)
describe(SPANEb_p)
alpha(FLO_p)
alpha(SPANEp_p)
alpha(SPANEn_p)
alpha(SPANEb_p)

#---------------------------------------------

#########
# tab 2 #
#########

# CFA 

#  FLOURISHING SCALE
modF <- 'f1 =~ X40 + X41 + X42 +X43 + X44 + X45 + X46 + X47'

# online
fitF<-cfa(model = modF, orthogonal = FALSE, data = FLO_o, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
summary(fitF, fit.measures=TRUE)

# paper
fitF<-cfa(model = modF, orthogonal = FALSE, data = FLO_p, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
summary(fitF, fit.measures=TRUE)


# SPANE

modS <- 'f1 =~ X28 + X30 + X32 +X34 + X37 + X39
f2 =~ X29 + X31 + X33 +X35 + X36 + X38'

# online
fitS<-cfa(model = modS, orthogonal = FALSE, data = SPANE_o, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
summary(fitS, fit.measures=TRUE)


# paper
fitS<-cfa(model = modS, orthogonal = FALSE, data = SPANE_p, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
summary(fitS, fit.measures=TRUE)

#########
# tab 3 #
#########
# Measurement Invariance
library(semTools)

# FS
FsMI <- rbind(FLO_o,FLO_p)
gruppo <- c(rep("o",218),rep("p",466))
ddFS <- data.frame(gruppo, FsMI)

measurementInvariance(modF,
                      data=ddFS,
                      group="gruppo",
                      strict = TRUE)

# SPANE
SMI <- rbind(SPANE_o,SPANE_p)
gruppo <- c(rep("o",218),rep("p",466))
ddS <- data.frame(gruppo, SMI)

measurementInvariance(modS,
                      data=ddS,
                      group="gruppo",
                      strict = TRUE)

#####################################
######### COMBINED SAMPLE ###########
#####################################
sex <- dati[,1]
age <- dati[,2]
gen1 <- dati[,3]  # single item 
SHS <- dati[,4:7]   
PANAS <- dati[,8:27]
SPANE <- dati[,28:39]
FLO <- dati[,40:47]
SWLS <- dati[,48:52]
PWI <- dati[,53:60]
gen2<-dati[,61]  # single item 

#SHS
SHS1<-SHS
SHS1[,4]<-recode(SHS[,4],"1=7;2=6;3=5;4=4;5=3;6=2;7=1")
totSHS1<-rowSums(SHS1)

#PANAS
PANASp <- PANAS[,c(1,3,5,9,10,12,14,16,17,19)]
PANASn <- PANAS[,c(2,4,6,7,8,11,13,15,18,20)]
totPANASp <- rowSums(PANASp)
totPANASn <- rowSums(PANASn)
totPANASb <- totPANASp - totPANASn

#SPANE
SPANEp <- SPANE[,c(1,3,5,7,10,12)]
SPANEn <- SPANE[,c(2,4,6,8,9,11)]

totSPANEp <- rowSums(SPANEp)
totSPANEn <- rowSums(SPANEn)
totSPANEb <- totSPANEp - totSPANEn
totFLO <- rowSums(FLO)
totSWLS <- rowSums(SWLS)
totPWI <- rowSums(PWI)

describe(totFLO)
describe(FLO)
describe(totSPANEp)
describe(SPANEp)
describe(totSPANEn)
describe(SPANEn)
#---------------------------------

#########
# tab 4 #
#########
n <- 684

# FLOURISHING
Rf<-cor(FLO)
ee <- eigen(Rf)
print(ee, 3)
fa(Rf,1,n.obs = n, fm="pa") #.48
alpha(FLO)
# correlazione item-totale (corretta) -> r.drop
# alpha if item deleted: Reliability if an item is dropped -> raw_alpha

# SPANE_p
Rsp<- cor(SPANEp)
ee <- eigen(Rsp)
print(ee, 2)
fa(Rsp,1,n.obs = n, fm="pa") # .61
alpha(SPANEp)
# correlazione item-totale (corretta) -> r.drop
# alpha if item deleted: Reliability if an item is dropped -> raw_alpha

# SPANE_n
Rsn<- cor(SPANEn)
ee <- eigen(Rsn)
print(ee, 2)
fa(Rsn,1,n.obs = n, fm="pa") # .52
alpha(SPANEn)
# correlazione item-totale (corretta) -> r.drop
# alpha if item deleted: Reliability if an item is dropped -> raw_alpha





#########################################################à

tot <- cbind(totSWLS,totSHS1,totPANASp,totPANASn,totPANASb,totPWI,gen2)
wb <- cbind(totFLO,totSPANEp,totSPANEn,totSPANEb)
R<-cor(wb,tot)

#########
# tab 5 #
#########
round(R,2)

#########
# tab 6 #
#########
round(cor(wb),2)


###### dependent correlations ########



library(cocor)


data <- cbind(totSWLS,totFLO,totSPANEp)
cocor(~totSWLS + totFLO | totSWLS + totSPANEp, data)

data <- cbind(totPWI,totFLO,totSPANEp)
cocor(~totPWI + totFLO | totPWI + totSPANEp, data)

data <- cbind(gen2,totFLO,totSPANEp)
cocor(~gen2 + totFLO | gen2 + totSPANEp, data)

data <- cbind(totSHS1,totFLO,totSPANEp)
cocor(~totSHS1 + totFLO | totSHS1 + totSPANEp, data)




