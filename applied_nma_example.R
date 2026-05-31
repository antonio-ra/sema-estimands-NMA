rm(list=ls()) # clean directory

# netmeta package required to perform frequentist meta-analysis
if (!require(netmeta)) install.packages("netmeta") 
library(netmeta) 

# the two treatments being directly compared
treat1 <- c("Semaglutide 2.0 mg", "Semaglutide 1.0 mg", "Dulaglutide 4.5 mg", "Dulaglutide 3.0 mg", "Dulaglutide 4.5 mg")
treat2 <- c("Semaglutide 1.0 mg", "Dulaglutide 1.5 mg", "Dulaglutide 1.5 mg", "Dulaglutide 1.5 mg", "Dulaglutide 3.0 mg") 

# study labels specifying from which study each treatment comparison is extracted from          
studlab <- c("SUSTAIN FORTE", "SUSTAIN 7", "AWARD-11", "AWARD-11", "AWARD-11")

# change from baseline in glycated hemoglobin (HbA1c) 
TE.hba1c.TP <- c(-0.18, -0.34, -0.24, -0.10, -0.13) # treatment policy mean differences
SE.hba1c.TP <- c(0.07, 0.09, 0.06, 0.06, 0.06) # treatment policy standard errors
TE.hba1c.HYPO <- c(-0.23, -0.41, -0.34, -0.17, -0.16) # hypothetical mean differences
SE.hba1c.HYPO <- c(0.06, 0.08, 0.06, 0.06, 0.06) # hypothetical standard errors

# change from baseline in body weight 
TE.bw.TP <- c(-0.77, -3.05, -1.60, -0.90, -0.70) # treatment policy mean differences
SE.bw.TP <- c(0.40, 0.41, 0.28, 0.26, 0.26) # treatment policy standard errors
TE.bw.HYPO <- c(-0.93, -3.55, -1.60, -0.90, -0.70) # hypothetical mean differences
SE.bw.HYPO <- c(0.38, 0.39, 0.26, 0.26, 0.26) # hypothetical standard errors

## Change from baseline in HbA1c (treatment policy meta-analytical estimand)

# input effect sizes, standard errors, contrasted treatments and study labels
studdat.hba1c.TP <- data.frame(TE=TE.hba1c.TP, SE=SE.hba1c.TP, treat1=treat1, treat2=treat2, studlab=studlab)

# fit fixed effects network meta-analysis 
netmeta.hba1c.TP <- netmeta(TE = TE,
                            seTE = SE,
                            treat1 = treat1,
                            treat2 = treat2,
                            studlab = studlab,
                            data = studdat.hba1c.TP,
                            sm = "MD", # mean difference scale
                            fixed = TRUE, # fixed effects analysis
                            random = FALSE,
                            all.treatments = T,
                            tol.multiarm=0.005)

# forest plot displaying the results
png(file="~/TP_hba1c.PNG", width = 1700, height = 2200, res = 300)
forest(netmeta.hba1c.TP,
       sortvar=-TE, studlab=TRUE, 
       reference.group = c("Semaglutide 2.0 mg", "Semaglutide 1.0 mg", 
                           "Dulaglutide 4.5 mg", "Dulaglutide 3.0 mg", "Dulaglutide 1.5 mg"),
       drop.reference = TRUE, xlim = c(-1, 1), baseline.reference=FALSE, 
       xlab="Mean difference in change from baseline in HbA1c percentage points",
       leftlabs=c("Comparison"), smlab="Fixed effects model")
dev.off()

## Change from baseline in HbA1c (hypothetical meta-analytical estimand)

# input effect sizes, standard errors, contrasted treatments and study labels
studdat.hba1c.HYPO <- data.frame(TE=TE.hba1c.HYPO, SE=SE.hba1c.HYPO, treat1=treat1, treat2=treat2, studlab=studlab)

# fit fixed effects network meta-analysis
netmeta.hba1c.HYPO <- netmeta(TE = TE,
                              seTE = SE,
                              treat1 = treat1,
                              treat2 = treat2,
                              studlab = studlab,
                              data = studdat.hba1c.HYPO,
                              sm = "MD", # mean difference scale
                              fixed = TRUE, # fixed effects analysis
                              random = FALSE,
                              all.treatments = T,
                              tol.multiarm=0.005)

# forest plot displaying the results
png(file="~/HYPO_hba1c.PNG", width = 1700, height = 2200, res = 300)
forest(netmeta.hba1c.HYPO,
       sortvar=-TE, studlab=TRUE, 
       reference.group = c("Semaglutide 2.0 mg", "Semaglutide 1.0 mg", 
                           "Dulaglutide 4.5 mg", "Dulaglutide 3.0 mg", "Dulaglutide 1.5 mg"),
       drop.reference = TRUE, xlim = c(-1, 1), baseline.reference=FALSE, 
       xlab="Mean difference in change from baseline in HbA1c percentage points",
       leftlabs=c("Comparison"), smlab="Fixed effects model")
dev.off()

## Change from baseline in body weight (treatment policy meta-analytical estimand)

# input effect sizes, standard errors, contrasted treatments and study labels
studdat.bw.TP <- data.frame(TE=TE.bw.TP, SE=SE.bw.TP, treat1=treat1, treat2=treat2, studlab=studlab)

# fit fixed effects network meta-analysis
netmeta.bw.TP <- netmeta(TE = TE,
                         seTE = SE,
                         treat1 = treat1,
                         treat2 = treat2,
                         studlab = studlab,
                         data = studdat.bw.TP,
                         sm = "MD", # mean difference scale
                         fixed = TRUE, # fixed effects analysis
                         random = FALSE,
                         all.treatments = T)

# forest plot displaying the results
png(file="~/TP_body_weight.PNG", width = 1700, height = 2200, res = 300)
forest(netmeta.bw.TP,
       sortvar=-TE, studlab=TRUE, 
       reference.group = c("Semaglutide 2.0 mg", "Semaglutide 1.0 mg", 
                           "Dulaglutide 4.5 mg", "Dulaglutide 3.0 mg", "Dulaglutide 1.5 mg"),
       drop.reference = TRUE, xlim = c(-6, 6), baseline.reference=FALSE, 
       xlab="Mean difference in change from baseline in body weight (kg)",
       leftlabs=c("Comparison"), smlab="Fixed effects model")
dev.off()

## Change from baseline in body weight (hypothetical meta-analytical estimand)

# input effect sizes, standard errors, contrasted treatments and study labels
studdat.bw.HYPO <- data.frame(TE=TE.bw.HYPO, SE=SE.bw.HYPO, treat1=treat1, treat2=treat2, studlab=studlab)

# fit fixed effects network meta-analysis
netmeta.bw.HYPO <- netmeta(TE = TE,
                           seTE = SE,
                           treat1 = treat1,
                           treat2 = treat2,
                           studlab = studlab,
                           data = studdat.bw.HYPO,
                           sm = "MD", # mean difference scale
                           fixed = TRUE, # fixed effects analysis
                           random = FALSE,
                           all.treatments = T)

# forest plot displaying the results
png(file="~/HYPO_body_weight.PNG", width = 1700, height = 2200, res = 300)
forest(netmeta.bw.HYPO,
       sortvar=-TE, studlab=TRUE, 
       reference.group = c("Semaglutide 2.0 mg", "Semaglutide 1.0 mg", 
                           "Dulaglutide 4.5 mg", "Dulaglutide 3.0 mg", "Dulaglutide 1.5 mg"),
       drop.reference = TRUE, xlim = c(-6, 6), baseline.reference=FALSE, 
       xlab="Mean difference in change from baseline in body weight (kg)",
       leftlabs=c("Comparison"), smlab="Fixed effects model")
dev.off()
