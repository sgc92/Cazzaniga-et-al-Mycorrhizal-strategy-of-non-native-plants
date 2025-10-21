library(dplyr)
library(glmmTMB)
library(lme4)
library(Matrix)
library(emmeans)
library(car)

#dat <- readRDS("C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/data/sPlot4_sel_var_plot_inv_myc_scaled_SC0625_HMI.rds")
dat <- readRDS("sPlot4_sel_var_plot_inv_myc_scaled_SC0625_HMI.rds")
#colSums(is.na(dat))
dat.sel <- dat[!is.na(dat$HMI), ]
count(dat.sel, biome)

###GLOBAL
#Calculate Q25 and Q75 outside of mutate
Q25 <- quantile(dat.sel$HMI, 0.25)
Q75 <- quantile(dat.sel$HMI, 0.75)

#Label data based on quantiles
dat.sel_HMI_global <- dat.sel %>%
  mutate(
    HMI.gl = case_when(
      HMI <= Q25 ~ "low",      # Below 25th percentile
      HMI >= Q75 ~ "high",     # Above 75th percentile
      TRUE ~ "medium"                  # Between 25th and 75th percentiles
    )
  )

#Filter only 'low' and 'high'
dat.sel_HMI_global <- dat.sel_HMI_global %>%
  filter(HMI.gl %in% c("high", "low"))

#model NM vs ALL
mod_status_NMvsALL_HMI <- glmmTMB(cbind(NMct, (AMct+EMct+AMNMct))  ~  status * HMI.gl +
                    MAT + MAP + coarsefrag + SOCstock + sand + PH +
                    (1|plot_id), 
                    data = dat.sel_HMI_global, family=binomial(link ="logit"),
                    control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_NMvsALL_HMI, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/mod_status_NMvsALL_HMI.rds")

mod_status_AMNMvsAM_NM_HMI <- glmmTMB(cbind(AMNMct, (AMct+NMct))  ~  status * HMI.gl +
                                    MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                    (1|plot_id), 
                                  data = dat.sel_HMI_global, family=binomial(link ="logit"),
                                  control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_AMNMvsAM_NM_HMI, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/mod_status_AMNMvsAM_NM_HMI.rds")

########################################################################################################################
## BIOME
dat.sel <- dat.sel[!is.na(dat.sel$biome), ]


#BIOME MODELS
low.biomes<- c("Mangroves", "Tropical_Coniferous", "Flooded_Grasslands", "Tropical_Deciduous_Broadleaf")
dat.sel <- dat.sel %>% filter (!biome %in% low.biomes)  %>% droplevels()
count(dat.sel, biome)
(count(dat.sel, biome)/2)

dat.sel_HMI_biome <- dat.sel %>%
  group_by(biome) %>%
  mutate(
    Q25 = quantile(HMI, 0.25),
    Q75 = quantile(HMI, 0.75)) %>% 
  mutate(
    HMI.bspec = case_when(
      HMI <= Q25 ~ "low",      # Below 25th percentile
      HMI >= Q75 ~ "high",     # Above 75th percentile
      TRUE ~ "medium")) %>%           # Between 25th and 75th percentiles
  ungroup()

dat.sel_HMI_biome <- dat.sel_HMI_biome %>% filter(HMI.bspec %in% c("high", "low"))

count(dat.sel_HMI_biome, biome, Q25, Q75)
count(dat.sel_HMI_biome, biome, HMI.bspec)

mod_biome_NMvsALL_HMI_bspec <- glmmTMB(cbind(NMct, (AMct+EMct+AMNMct))  ~  biome * status * HMI.bspec +
                    MAT + MAP + coarsefrag + SOCstock + sand + PH +
                    (1|plot_id), 
                    data = dat.sel_HMI_biome, family=binomial(link ="logit"),
                    #ziformula = ~1,
                    control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsALL_HMI_bspec, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/mod_biome_status_NMvsALL_HMI.rds")

mod_biome_AMNMvsAM_NM_HMI_bspec <- glmmTMB(cbind(AMNMct, (AMct+NMct))  ~  biome * status * HMI.bspec +
                                 MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                 (1|plot_id), 
                               data = dat.sel_HMI_biome, family=binomial(link ="logit"),
                               #ziformula = ~1,
                               control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsAM_NM_HMI_bspec, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/mod_biome_status_AMNMvsAM_NM_HMI.rds")

##EMMEANS
means_global_NMvsALL_HMI <- emmeans (mod_status_NMvsALL_HMI, pairwise ~ status | HMI.gl , 
                             component = "response", cov.reduce = mean, data = dat.sel_HMI_global)
saveRDS(means_global_NMvsALL_HMI, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/means_global_NMvsALL_HMI.rds")

means_global_AMNMvsAM_NM_HMI <- emmeans (mod_status_AMNMvsAM_NM_HMI, pairwise ~ status | HMI.gl , 
                             component = "response", cov.reduce = mean, data = dat.sel_HMI_global)
saveRDS(means_global_AMNMvsAM_NM_HMI, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/means_global_AMNMvsAM_NM_HMI.rds")

means_biome_NMvsALL_HMI <- emmeans (mod_biome_NMvsALL_HMI_bspec, pairwise ~ status | biome | HMI.bspec , 
                            component = "response", cov.reduce = mean, data = dat.sel_HMI_biome)
saveRDS(means_biome_NMvsALL_HMI, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/means_biome_NMvsALL_HMI.rds")

means_biome_AMNMvsAM_NM_HMI <- emmeans (mod_biome_AMNMvsAM_NM_HMI_bspec, pairwise ~ status | biome | HMI.bspec , 
                                    component = "response", cov.reduce = mean, data = dat.sel_HMI_biome)
saveRDS(means_biome_AMNMvsAM_NM_HMI, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/means_biome_AMNMvsAM_NM_HMI.rds")