library(lavaan)
library(semTools)
library(car)

#################################################################################
#############################  supplementary analysis  ##########################
#################################################################################

# Study 1

dati <- read.csv("study1.csv", header = TRUE, sep = ";", na.string = " ")

SPANE <- dati[,28:39]
SPANEp <- SPANE[,c(1,3,5,7,10,12)]
SPANEn <- SPANE[,c(2,4,6,8,9,11)]
SPANEn1 <- SPANEn
SPANEn1[,1]<-recode(SPANEn[,1],"1=5;2=4;3=3;4=2;5=1")
SPANEn1[,2]<-recode(SPANEn[,2],"1=5;2=4;3=3;4=2;5=1")
SPANEn1[,3]<-recode(SPANEn[,3],"1=5;2=4;3=3;4=2;5=1")
SPANEn1[,4]<-recode(SPANEn[,4],"1=5;2=4;3=3;4=2;5=1")
SPANEn1[,5]<-recode(SPANEn[,5],"1=5;2=4;3=3;4=2;5=1")
SPANEn1[,6]<-recode(SPANEn[,6],"1=5;2=4;3=3;4=2;5=1")
SPANE_c <- cbind(SPANEp,SPANEn1)
FS_c <- dati[,40:47]
WB <- cbind(FS_c, SPANE_c)


# CFA unico fattore
mod_un <- 'f1 =~ X40 + X41 + X42 +X43 + X44 + X45 + X46 + X47 +
           X28 + X30 + X32 +X34 + X37 + X39 + X29 + X31 + X33 +X35 + X36 + X38'
#fit_un<-cfa(model = mod_un, orthogonal = FALSE, data = WB, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fit_un, fit.measures=TRUE)
#compareFit(fit_sep, fit_un, nested=FALSE)
#anova(fit_sep, fit_un)

# CFA dimensioni separate
mod_sep <- 'f1 =~ X40 + X41 + X42 +X43 + X44 + X45 + X46 + X47
f2 =~ X28 + X30 + X32 +X34 + X37 + X39 + X29 + X31 + X33 +X35 + X36 + X38'
#fit_sep<-cfa(model = mod_sep, orthogonal = FALSE, data = WB, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fit_sep, fit.measures=TRUE)


fit1 <- cfa(mod_un, data = WB, meanstructure = TRUE) 
#summary(fit1, fit.measures=TRUE)
fit2 <- cfa(mod_sep, data = WB, meanstructure = TRUE) 
#summary(fit2, fit.measures=TRUE)

compareFit(fit1, fit2, nested=FALSE)
anova(fit1, fit2)

#################################################################################
rm(list=ls())

# Study 2


# unemployed
datiu <- read.csv2("282uX2.csv")

# others
datio <- read.csv2("426oX.csv")

# unemployed n = 282
FSu <- datiu[,1:8]
SPANEu <- datiu[,9:20]

# others (control group)  n = 426
FSo <- datio[,11:18] 
SPANEo <- datio[,19:30]



# MI dimensione unica  
mod_un <- 'f1 =~ fs1 + fs2 + fs3 + fs4 + fs5 + fs6 + fs7 + fs8 +
           s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12'

WBu <- cbind(FSu, SPANEu)
#fitu_un<-cfa(model = mod_un, orthogonal = FALSE, data = WBu, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fitu_un, fit.measures=TRUE)

WBo <- cbind(FSo, SPANEo)
#fito_un<-cfa(model = mod_un, orthogonal = FALSE, data = WBo, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fito_un, fit.measures=TRUE)



# MI dimensioni separate  
mod_sep <- 'f1 =~ fs1 + fs2 + fs3 + fs4 + fs5 + fs6 + fs7 + fs8
            f2 =~ s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12'



# Measurement Invariance 
wb1MI <- rbind(WBo,WBu)
gruppo <- c(rep("o",426),rep("u",282))
ddwb1 <- data.frame(gruppo, wb1MI)

wb2MI <- rbind(WBo,WBu)
gruppo <- c(rep("o",426),rep("u",282))
ddwb2 <- data.frame(gruppo, wb2MI)



fit1.ef <- cfa(mod_un, data = ddwb1, group = "gruppo", meanstructure = TRUE) # equal form
fit2.ef <- cfa(mod_sep, data = ddwb2, group = "gruppo", meanstructure = TRUE) # equal form

# chi-squared diff tests
compareFit(fit1.ef, fit2.ef, nested=FALSE)

# chi-squared diff tests
anova(fit1.ef, fit2.ef, test = "chisq")



#########################################################################
#########################################################################
# altre prove

#WBu <- cbind(FSu, SPANEu)
#fitu_sep<-cfa(model = mod_sep, orthogonal = FALSE, data = WBu, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fitu_sep, fit.measures=TRUE)

#WBo <- cbind(FSo, SPANEo)
#fito_sep<-cfa(model = mod_sep, orthogonal = FALSE, data = WBo, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fito_sep, fit.measures=TRUE)



# Measurement Invariance 
wb1MI <- rbind(WBo,WBu)
gruppo <- c(rep("o",426),rep("u",282))
ddwb1 <- data.frame(gruppo, wb1MI)

wb2MI <- rbind(WBo,WBu)
gruppo <- c(rep("o",426),rep("u",282))
ddwb2 <- data.frame(gruppo, wb2MI)


#out1 <- measurementInvariance(mod_un, data=ddwb1, group="gruppo", quiet=TRUE)
#out2 <- measurementInvariance(mod_sep, data=ddwb2, group="gruppo", quiet=TRUE)
#compareFit(out1)
#compareFit(out2)



fit1.ef <- cfa(mod_un, data = ddwb1, group = "gruppo", meanstructure = TRUE) # equal form
fit1.efl <- update(fit1.ef, group.equal = c("loadings")) # equal factor laodings
fit1.eii <- update(fit1.efl, group.equal = c("loadings", "intercepts")) # equal indicator intercepts
fit1.eir <- update(fit1.eii, group.equal = c("loadings", "intercepts", "residuals")) # equal indicator error variances
fit1.fv <- update(fit1.eir, group.equal = c("loadings", "intercepts", "residuals", "lv.variances")) # equal factor variances
fit1.fm <- update(fit1.fv, group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "means")) # equal latent means


# chi-squared diff tests
anova(fit1.ef, fit1.efl, fit1.eii, fit1.eir, fit1.fv, fit1.fm, test = "chisq")



fit2.ef <- cfa(mod_sep, data = ddwb2, group = "gruppo", meanstructure = TRUE) # equal form
fit2.efl <- update(fit2.ef, group.equal = c("loadings")) # equal factor laodings
fit2.eii <- update(fit2.efl, group.equal = c("loadings", "intercepts")) # equal indicator intercepts
fit2.eir <- update(fit2.eii, group.equal = c("loadings", "intercepts", "residuals")) # equal indicator error variances
fit2.fv <- update(fit2.eir, group.equal = c("loadings", "intercepts", "residuals", "lv.variances")) # equal factor variances
fit2.fm <- update(fit2.fv, group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "means")) # equal latent means


# chi-squared diff tests
anova(fit2.ef, fit2.efl, fit2.eii, fit2.eir, fit2.fv, fit2.fm, test = "chisq")


compareFit(fit1.ef, fit2.ef, nested=FALSE)

# chi-squared diff tests
anova(fit1.ef, fit2.ef, test = "chisq")




#########################################################
# Senza spane n
#########################################################
rm(list=ls())


# Study 1

dati <- read.csv("study1.csv", header = TRUE, sep = ";", na.string = " ")

SPANE <- dati[,28:39]
SPANEp <- SPANE[,c(1,3,5,7,10,12)]
FS_c <- dati[,40:47]
WB <- cbind(FS_c, SPANEp)


# CFA unico fattore
mod_un <- 'f1 =~ X40 + X41 + X42 +X43 + X44 + X45 + X46 + X47 +
X28 + X30 + X32 +X34 + X37 + X39 '
#fit_un<-cfa(model = mod_un, orthogonal = FALSE, data = WB, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fit_un, fit.measures=TRUE)
#compareFit(fit_sep, fit_un, nested=FALSE)
#anova(fit_sep, fit_un)

# CFA dimensioni separate
mod_sep <- 'f1 =~ X40 + X41 + X42 +X43 + X44 + X45 + X46 + X47
f2 =~ X28 + X30 + X32 +X34 + X37 + X39'
#fit_sep<-cfa(model = mod_sep, orthogonal = FALSE, data = WB, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fit_sep, fit.measures=TRUE)


fit1 <- cfa(mod_un, data = WB, meanstructure = TRUE) 
#summary(fit1, fit.measures=TRUE)
fit2 <- cfa(mod_sep, data = WB, meanstructure = TRUE) 
#summary(fit2, fit.measures=TRUE)

compareFit(fit1, fit2, nested=FALSE)
anova(fit1, fit2)


#################################################################################
rm(list=ls())

# Study 2


# unemployed
datiu <- read.csv2("282uX2.csv")

# others
datio <- read.csv2("426oX.csv")

# unemployed n = 282
FSu <- datiu[,1:8]
SPANEu <- datiu[,9:20]
SPANEup <- SPANEu[,c(1,3,5,7,10,12)]


# others (control group)  n = 426
FSo <- datio[,11:18] 
SPANEo <- datio[,19:30]
SPANEop <- SPANEo[,c(1,3,5,7,10,12)]




# MI dimensione unica  
mod_un <- 'f1 =~ fs1 + fs2 + fs3 + fs4 + fs5 + fs6 + fs7 + fs8 +
s1 + s3 + s5 + s7 + s10 + s12'

WBu <- cbind(FSu, SPANEup)
#fitu_un<-cfa(model = mod_un, orthogonal = FALSE, data = WBu, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fitu_un, fit.measures=TRUE)

WBo <- cbind(FSo, SPANEop)
#fito_un<-cfa(model = mod_un, orthogonal = FALSE, data = WBo, estimator = "MLM",std.lv = TRUE, fixed.x =TRUE, std.ov = TRUE)
#summary(fito_un, fit.measures=TRUE)



# MI dimensioni separate  
mod_sep <- 'f1 =~ fs1 + fs2 + fs3 + fs4 + fs5 + fs6 + fs7 + fs8
f2 =~ s1 + s3 + s5 + s7 + s10 + s12'



# Measurement Invariance 
wb1MI <- rbind(WBo,WBu)
gruppo <- c(rep("o",426),rep("u",282))
ddwb1 <- data.frame(gruppo, wb1MI)

wb2MI <- rbind(WBo,WBu)
gruppo <- c(rep("o",426),rep("u",282))
ddwb2 <- data.frame(gruppo, wb2MI)



fit1.ef <- cfa(mod_un, data = ddwb1, group = "gruppo", meanstructure = TRUE) # equal form
fit2.ef <- cfa(mod_sep, data = ddwb2, group = "gruppo", meanstructure = TRUE) # equal form

# chi-squared diff tests
compareFit(fit1.ef, fit2.ef, nested=FALSE)

# chi-squared diff tests
anova(fit1.ef, fit2.ef, test = "chisq")

