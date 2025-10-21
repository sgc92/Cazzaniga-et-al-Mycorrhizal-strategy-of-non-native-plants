library(dplyr)
library(glmmTMB)
library(lme4)
library(Matrix)
library(emmeans)
library(car)

dat <- readRDS("sPlot4_sel_var_plot_inv_myc_scaled_SC0625.rds")

#NM vs All
mod_status_NMvsALL <- glmmTMB(cbind(NMct, (AMct+EMct+AMNMct))  ~  status  +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_NMvsALL, "mod_status_NMvsALL.rds")


#NM vs AM
mod_status_NMvsAM <- glmmTMB(cbind(NMct, AMct)  ~  status  +
                               MAT + MAP + coarsefrag + SOCstock + sand + PH +
                               (1|plot_id), 
                             data = dat, family=binomial(link ="logit"),
                             control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_NMvsAM, "mod_status_NMvsAM.rds")

#NM vs EM

mod_status_NMvsEM <- glmmTMB(cbind(NMct, EMct)  ~  status  +
                               MAT + MAP + coarsefrag + SOCstock + sand + PH +
                               (1|plot_id), 
                             data = dat, family=binomial(link ="logit"),
                             control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_NMvsEM, "mod_status_NMvsEM.rds")

#NM vs AMNM
mod_status_NMvsAMNM <- glmmTMB(cbind(NMct, AMNMct)  ~  status  +
                                 MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                 (1|plot_id), 
                               data = dat, family=binomial(link ="logit"),
                               control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_NMvsAMNM, "mod_status_NMvsAMNM.rds")

#################

#AMNM vs NM+AM
mod_status_AMNMvsAM_NM <- glmmTMB(cbind(AMNMct, (AMct+NMct))  ~  status  +
                                    MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                    (1|plot_id), 
                                  data = dat, family=binomial(link ="logit"),
                                  control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_AMNMvsAM_NM, "mod_status_AMNMvsAM_NM.rds")

#AMNM vs AM
mod_status_AMNMvsAM <- glmmTMB(cbind(AMNMct, AMct)  ~  status  +
                                 MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                 (1|plot_id), 
                               data = dat, family=binomial(link ="logit"),
                               control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_AMNMvsAM, "mod_status_AMNMvsAM.rds")

#AMNM vs NM
mod_status_AMNMvsNM <- glmmTMB(cbind(AMNMct, NMct)  ~  status  +
                                 MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                 (1|plot_id), 
                               data = dat, family=binomial(link ="logit"),
                               control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_AMNMvsNM, "mod_status_AMNMvsNM.rds")

#AMNM vs EM
mod_status_AMNMvsEM <- glmmTMB(cbind(AMNMct, EMct)  ~  status  +
                                 MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                 (1|plot_id), 
                               data = dat, family=binomial(link ="logit"),
                               control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_AMNMvsEM, "mod_status_AMNMvsEM.rds")


###Emmeans
means_status_NMvsALL <- emmeans (mod_status_NMvsALL, ~ status, component = "response", cov.reduce = mean, data = dat)
saveRDS(means_status_NMvsALL, "means_status_NMvsALL.rds")

means_status_NMvsAM <- emmeans (mod_status_NMvsAM, ~ status  , component = "response", cov.reduce = mean, data = dat)
saveRDS(means_status_NMvsAM, "means_status_NMvsAM.rds")

means_status_NMvsEM <- emmeans (mod_status_NMvsEM, ~ status  , component = "response", cov.reduce = mean, data = dat)
saveRDS(means_status_NMvsEM, "means_status_NMvsEM.rds")

means_status_NMvsAMNM <- emmeans (mod_status_NMvsAMNM, ~ status  , component = "response", cov.reduce = mean, data = dat)
saveRDS(means_status_NMvsAMNM, "means_status_NMvsAMNM.rds")

means_status_AMNMvsAM_NM <- emmeans (mod_status_AMNMvsAM_NM, ~ status  , component = "response", cov.reduce = mean, data = dat)
saveRDS(means_status_AMNMvsAM_NM, "means_status_AMNMvsAM_NM.rds")

means_status_AMNMvsAM <- emmeans (mod_status_AMNMvsAM, ~ status  , component = "response", cov.reduce = mean, data = dat)
saveRDS(means_status_AMNMvsAM, "means_status_AMNMvsAM.rds")

means_status_AMNMvsNM <- emmeans (mod_status_AMNMvsNM, ~ status  , component = "response", cov.reduce = mean, data = dat)
saveRDS(means_status_AMNMvsNM, "means_status_AMNMvsNM.rds")

means_status_AMNMvsEM <- emmeans (mod_status_AMNMvsEM, ~ status  , component = "response", cov.reduce = mean, data = dat)
saveRDS(means_status_AMNMvsEM, "means_status_AMNMvsEM.rds")
