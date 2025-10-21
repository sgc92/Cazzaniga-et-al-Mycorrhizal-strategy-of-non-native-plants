library(dplyr)
library(glmmTMB)
library(lme4)
library(Matrix)
library(emmeans)
library(car)

dat <- readRDS("sPlot4_sel_var_plot_inv_myc_scaled_SC0625.rds")
low.biomes<- c("Mangroves", "Tropical_Coniferous", "Flooded_Grasslands", "Tropical_Deciduous_Broadleaf", "Tundra", "Boreal")
dat.sel <- dat %>% filter (!biome %in% low.biomes)  %>% droplevels()

#rarefy Temperate_BroadLeaf biome to an average number of plot_id of th other biomes
plot_ids_per_biome <- dat.sel %>%
  distinct(plot_id, .keep_all = TRUE) %>%
  dplyr::select(plot_id, biome)

# Step 2: Get target rarefaction count from Montane_Grasslands
target_n <- plot_ids_per_biome %>%
  filter(biome == "Montane_Grasslands") %>%
  nrow()

# Step 3: Sample plot_ids from each biome
set.seed(123)

selected_plot_ids <- plot_ids_per_biome %>%
  group_by(biome) %>%
  summarise(
    plot_id = list(
      if(n() >= target_n) {
        sample(plot_id, target_n, replace = FALSE)
      } else {
        # optional: skip or keep all if not enough
        plot_id
      }
    ),
    .groups = "drop"
  ) %>%
  tidyr::unnest(plot_id)

# Step 4: Filter full dataset
dat.sel.rar <- dat.sel %>%
  filter(plot_id %in% selected_plot_ids$plot_id)
count(dat.sel.rar, biome)

#NM vs All
mod_global_NMvsALL <- glmmTMB(cbind(NMct, (AMct+EMct+AMNMct))  ~  status  +
                               MAT + MAP + coarsefrag + SOCstock + sand + PH +
                               (1|plot_id), 
                             data = dat.sel.rar, family=binomial(link ="logit"),
                             control = glmmTMBControl(parallel = 16))
saveRDS(mod_global_NMvsALL, "mod_global_NMvsALL_rarall.rds")


#NM vs AM
mod_global_NMvsAM <- glmmTMB(cbind(NMct, AMct)  ~  status  +
                              MAT + MAP + coarsefrag + SOCstock + sand + PH +
                              (1|plot_id), 
                            data = dat.sel.rar, family=binomial(link ="logit"),
                            control = glmmTMBControl(parallel = 16))
saveRDS(mod_global_NMvsAM, "mod_global_NMvsAM_rarall.rds")


#NM vs EM
mod_global_NMvsEM <- glmmTMB(cbind(NMct, EMct)  ~  status  +
                              MAT + MAP + coarsefrag + SOCstock + sand + PH +
                              (1|plot_id), 
                            data = dat.sel.rar, family=binomial(link ="logit"),
                            control = glmmTMBControl(parallel = 16))
saveRDS(mod_global_NMvsEM, "mod_global_NMvsEM_rarall.rds")

#NM vs AMNM
mod_global_NMvsAMNM <- glmmTMB(cbind(NMct, AMNMct)  ~  status  +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel.rar, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_global_NMvsAMNM, "mod_global_NMvsAMNM_rarall.rds")

#################

#AMNM vs NM+AM
mod_global_AMNMvsAM_NM <- glmmTMB(cbind(AMNMct, (AMct+NMct))  ~  status  +
                                   MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                   (1|plot_id), 
                                 data = dat.sel.rar, family=binomial(link ="logit"),
                                 control = glmmTMBControl(parallel = 16))
saveRDS(mod_global_AMNMvsAM_NM, "mod_global_AMNMvsAM_NM_rarall.rds")

#AMNM vs AM
mod_global_AMNMvsAM <- glmmTMB(cbind(AMNMct, AMct)  ~  status  +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel.rar, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_global_AMNMvsAM, "mod_global_AMNMvsAM_rarall.rds")

#AMNM vs EM
mod_global_AMNMvsEM <- glmmTMB(cbind(AMNMct, EMct)  ~  status  +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel.rar, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_global_AMNMvsEM, "mod_global_AMNMvsEM_rarall.rds")

#AMNM vs NM
mod_global_AMNMvsNM <- glmmTMB(cbind(AMNMct, NMct)  ~  status  +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel.rar, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_global_AMNMvsNM, "mod_global_AMNMvsNM_rarall.rds")


#EMMENAS
means_NMvsALL <- emmeans (mod_global_NMvsALL, ~ status , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_NMvsALL, "means_global_NMvsALL_rarall.rds")

means_NMvsAM <- emmeans (mod_global_NMvsAM, ~ status , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_NMvsAM, "means_global_NMvsAM_rarall.rds")

means_NMvsEM <- emmeans (mod_global_NMvsEM, ~ status , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_NMvsEM, "meanss_global_NMvsEM_rarall.rds")

means_NMvsAMNM <- emmeans (mod_global_NMvsAMNM, ~ status , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_NMvsAMNM, "means_global_NMvsAMNM_rarall.rds")

means_AMNMvsAM_NM <- emmeans (mod_global_AMNMvsAM_NM, ~ status , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_AMNMvsAM_NM, "means_global_AMNMvsAM_NM_rarall.rds")

means_AMNMvsAM <- emmeans (mod_global_AMNMvsAM, ~ status , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_AMNMvsAM, "means_global_AMNMvsAM_rarall.rds")

means_AMNMvsNM <- emmeans (mod_global_AMNMvsNM, ~ status , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_AMNMvsNM, "means_global_AMNMvsNM_rarall.rds")

means_AMNMvsEM <- emmeans (mod_global_AMNMvsEM, ~ status , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_AMNMvsEM, "means_global_AMNMvsEM_rarall.rds")

###################################################################################
#BIOME MODELS
dat.sel.rar <- dat.sel.rar[!is.na(dat.sel.rar$biome), ]

#NM vs All
mod_biome_NMvsALL <- glmmTMB(cbind(NMct, (AMct+EMct+AMNMct))  ~  status  * biome +
                               MAT + MAP + coarsefrag + SOCstock + sand + PH +
                               (1|plot_id), 
                             data = dat.sel.rar, family=binomial(link ="logit"),
                             control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsALL, "mod_biome_NMvsALL_rarall.rds")


#NM vs AM
mod_biome_NMvsAM <- glmmTMB(cbind(NMct, AMct)  ~  status  * biome +
                              MAT + MAP + coarsefrag + SOCstock + sand + PH +
                              (1|plot_id), 
                            data = dat.sel.rar, family=binomial(link ="logit"),
                            control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsAM, "mod_biome_NMvsAM_rarall.rds")


#NM vs EM
mod_biome_NMvsEM <- glmmTMB(cbind(NMct, EMct)  ~  status  * biome +
                              MAT + MAP + coarsefrag + SOCstock + sand + PH +
                              (1|plot_id), 
                            data = dat.sel.rar, family=binomial(link ="logit"),
                            control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsEM, "mod_biome_NMvsEM_rarall.rds")

#NM vs AMNM
mod_biome_NMvsAMNM <- glmmTMB(cbind(NMct, AMNMct)  ~  status  * biome +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel.rar, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsAMNM, "mod_biome_NMvsAMNM_rarall.rds")

#################

#AMNM vs NM+AM
mod_biome_AMNMvsAM_NM <- glmmTMB(cbind(AMNMct, (AMct+NMct))  ~  status  * biome +
                                   MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                   (1|plot_id), 
                                 data = dat.sel.rar, family=binomial(link ="logit"),
                                 control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsAM_NM, "mod_biome_AMNMvsAM_NM_rarall.rds")

#AMNM vs AM
mod_biome_AMNMvsAM <- glmmTMB(cbind(AMNMct, AMct)  ~  status  * biome +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel.rar, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsAM, "mod_biome_AMNMvsAM_rarall.rds")

#AMNM vs EM
mod_biome_AMNMvsEM <- glmmTMB(cbind(AMNMct, EMct)  ~  status  * biome +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel.rar, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsEM, "mod_biome_AMNMvsEM_rarall.rds")

#AMNM vs NM
mod_biome_AMNMvsNM <- glmmTMB(cbind(AMNMct, NMct)  ~  status  * biome +
                                MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                (1|plot_id), 
                              data = dat.sel.rar, family=binomial(link ="logit"),
                              control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsNM, "mod_biome_AMNMvsNM_rarall.rds")


#EMMENAS
means_NMvsALL <- emmeans (mod_biome_NMvsALL, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_NMvsALL, "means_biome_NMvsALL_rarall.rds")

means_NMvsAM <- emmeans (mod_biome_NMvsAM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_NMvsAM, "means_biome_NMvsAM_rarall.rds")

means_NMvsEM <- emmeans (mod_biome_NMvsEM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_NMvsEM, "means_biome_NMvsEM_rarall.rds")

means_NMvsAMNM <- emmeans (mod_biome_NMvsAMNM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_NMvsAMNM, "means_biome_NMvsAMNM_rarall.rds")

means_AMNMvsAM_NM <- emmeans (mod_biome_AMNMvsAM_NM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_AMNMvsAM_NM, "means_biome_AMNMvsAM_NM_rarall.rds")

means_AMNMvsAM <- emmeans (mod_biome_AMNMvsAM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_AMNMvsAM, "means_biome_AMNMvsAM_rarall.rds")

means_AMNMvsNM <- emmeans (mod_biome_AMNMvsNM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_AMNMvsNM, "means_biome_AMNMvsNM_rarall.rds")

means_AMNMvsEM <- emmeans (mod_biome_AMNMvsEM, ~ status | biome , component = "response", cov.reduce = mean, data = dat.sel.rar)
saveRDS(means_AMNMvsEM, "means_biome_AMNMvsEM_rarall.rds")