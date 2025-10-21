library(dplyr)
library(glmmTMB)
library(lme4)
library(Matrix)
library(emmeans)
library(car)

dat <- readRDS("sPlot4_sel_var_plot_inv_myc_scaled_SC0625.rds")
dat.sel <- dat[!is.na(dat$biome), ]

###################################################################################
#BIOME MODELS
low.biomes<- c("Mangroves", "Tropical_Coniferous", "Flooded_Grasslands", "Tropical_Deciduous_Broadleaf")
dat.sel <- dat.sel %>% filter (!biome %in% low.biomes)  %>% droplevels()
a <- as.data.frame(count(dat.sel, biome))
a$nbiome <- (a$n)/2

#NM vs All
mod_biome_NMvsALL <- glmmTMB(cbind(NMct, (AMct+EMct+AMNMct))  ~  status  * biome +
                               MAT + MAP + coarsefrag + SOCstock + sand + PH +
                               (1|plot_id), 
                             data = dat.sel, family=binomial(link ="logit"),
                             control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsALL, "mod_biome_NMvsALL.rds")

#NM vs AM
mod_biome_NMvsAM <- glmmTMB(cbind(NMct, AMct)  ~  status  * biome +
                              MAT + MAP + coarsefrag + SOCstock + sand + PH +
                              (1|plot_id), 
                            data = dat.sel, family=binomial(link ="logit"),
                            control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsAM, "mod_biome_NMvsAM.rds")

#NM vs EM
mod_biome_NMvsEM <- glmmTMB(cbind(NMct, EMct)  ~  status  * biome +
                              MAT + MAP + coarsefrag + SOCstock + sand + PH +
                              (1|plot_id), 
                            data = dat.sel, family=binomial(link ="logit"),
                            control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsEM, "mod_biome_NMvsEM.rds")

#NM vs AMNM
mod_biome_NMvsAMNM <- glmmTMB(cbind(NMct, AMNMct)  ~  status  * biome +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsAMNM, "mod_biome_NMvsAMNM.rds")

#################

#AMNM vs NM+AM
mod_biome_AMNMvsAM_NM <- glmmTMB(cbind(AMNMct, (AMct+NMct))  ~  status  * biome +
                                   MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                   (1|plot_id), 
                                 data = dat.sel, family=binomial(link ="logit"),
                                 control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsAM_NM, "mod_biome_AMNMvsAM_NM.rds")

#AMNM vs AM
mod_biome_AMNMvsAM <- glmmTMB(cbind(AMNMct, AMct)  ~  status  * biome +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsAM, "mod_biome_AMNMvsAM.rds")

#AMNM vs EM
mod_biome_AMNMvsEM <- glmmTMB(cbind(AMNMct, EMct)  ~  status  * biome +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsEM, "mod_biome_AMNMvsEM.rds")

#AMNM vs NM
mod_biome_AMNMvsNM <- glmmTMB(cbind(AMNMct, NMct)  ~  status  * biome +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsNM, "mod_biome_AMNMvsNM.rds")


#EMMENAS
means_biome_NMvsALL <- emmeans (mod_biome_NMvsALL, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel)
saveRDS(means_biome_NMvsALL, "means_biome_NMvsALL.rds")

means_biome_NMvsAM <- emmeans (mod_biome_NMvsAM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel)
saveRDS(means_biome_NMvsAM, "means_biome_NMvsAM.rds")

means_biome_NMvsEM <- emmeans (mod_biome_NMvsEM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel)
saveRDS(means_biome_NMvsEM, "means_biome_NMvsEM.rds")

means_biome_NMvsAMNM <- emmeans (mod_biome_NMvsAMNM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel)
saveRDS(means_biome_NMvsAMNM, "means_biome_NMvsAMNM.rds")

means_biome_AMNMvsAM_NM <- emmeans (mod_biome_AMNMvsAM_NM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel)
saveRDS(means_biome_AMNMvsAM_NM, "means_biome_AMNMvsAM_NM.rds")

means_biome_AMNMvsAM <- emmeans (mod_biome_AMNMvsAM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel)
saveRDS(means_biome_AMNMvsAM, "means_biome_AMNMvsAM.rds")

means_biome_AMNMvsNM <- emmeans (mod_biome_AMNMvsNM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel)
saveRDS(means_biome_AMNMvsNM, "means_biome_AMNMvsNM.rds")

means_biome_AMNMvsEM <- emmeans (mod_biome_AMNMvsEM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel)
saveRDS(means_biome_AMNMvsEM, "means_biome_AMNMvsEM.rds")