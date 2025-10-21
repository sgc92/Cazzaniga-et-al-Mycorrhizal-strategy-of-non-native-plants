library(dplyr)
library(ggplot2)

#check final dataset

sp.myc <- readRDS("data/sPlot4_sp_inv_myc_0625.rds")
plot <- readRDS("C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/data/splot4_plots_clean_SC0625.rds")

#load my final dataset (no rep in plot_id)
dat.sl <- readRDS("C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/data/sPlot4_sel_var_plot_inv_myc_SC0625.rds")

#plot_ids per biome
biome_count <- as.data.frame(count(dat.sl, biome))

#latitude and longitude
summary(dat.sl$avglat)
summary(dat.sl$avglon)

#env variables
summary(dat.sl$MAP)
summary(dat.sl$MAT)
summary(dat.sl$coarsefrag)
summary(dat.sl$SOCstock)
summary(dat.sl$sand)
summary(dat.sl$PH)

#number of plants
sp.myc1 <- sp.myc %>% 
  # make plot numeric to match plot dataset
  mutate(plot_id = as.numeric(plot_id)) %>% 
  # filter to match plots in plot
  filter(plot_id %in% dat.sl$plot_id) 
length(unique(sp.myc1$accepted_bin))

#number of native and non-native
summary(dat.sl$nativect)
summary(dat.sl$invasct)
summary(dat.sl$propinvasive)

#year of sampling
summary(dat.sl$date)


#HMI dates
dat_HMI3 <- readRDS("C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/data/sPlot4_sel_var_plot_inv_myc_scaled_SC0625_HMI.rds")
dat_HMI4 <- dat_HMI3[!is.na(dat_HMI3$HMI), ]
length (unique(dat_HMI4$plot_id))
plot(dat_HMI4$biome, dat_HMI4$HMI)

# Count plots per biome
biome_counts <- dat_HMI4 %>%
  count(biome)

# Bar plot
ggplot(biome_counts, aes(x = reorder(biome, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkolivegreen") +
  labs(
    title = "Number of Plots per Biome",
    x = "Biome",
    y = "Plot Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#SDI dataset
#SDI calcuated between 1999-2019. So retain plots from thiose year
library(lubridate)
dat.sl_SDI <- dat.sl %>% 
  mutate(year = year(as.Date(date))) %>% 
  filter( year>=1999 & year<=2019) #249947


###########################################################################
###############################################################################

#remove biomes
low.biomes<- c("Mangroves", "Tropical_Coniferous", "Flooded_Grasslands", "Tropical_Deciduous_Broadleaf", "Tundra")
dat.sl.sel <- dat.sl %>% filter (!biome %in% low.biomes)  %>% droplevels()

#are plot_ids repeated?
plot_ids_more_than_once <- dat.sl.sel %>%
  count(plot_id) %>%
  filter(n > 1)
#0 plot_id are repeated twice

#percentage of invasive in pots
summary(dat.sl.sel$propinvasive)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.004016 0.041667 0.071429 0.099944 0.125000 0.909091

#number of native and non-native
summary(dat_unique$invasct)
summary(dat_unique$nativect)

#find plot_id with too many native species
dat_binned <- dat_unique %>%
  mutate(native_bin = case_when(
    nativect >= 1   & nativect <= 10  ~ "1–10",
    nativect > 10   & nativect <= 50  ~ "11–50",
    nativect > 50   & nativect <= 100 ~ "51–100",
    nativect > 100  & nativect <= 200 ~ "100-200",
    nativect > 200  & nativect <= 500 ~ "200-500",
    nativect > 500   ~ ">500",
    TRUE                               ~ NA_character_
  ))
native_bin_counts <- dat_binned %>%
  count(native_bin)
native_bin_counts

outlier <- dat_binned %>%  filter (native_bin==">500") #plot_id 177682
outlier_info <- dat.sel %>%  filter (plot_id=="177682") #plot_id 177682
outlier <- dat.sel[dat.sel$plot_id == 177682, ]

outlier_sp <- sp.myc[sp.myc$plot_id == 177682, ]


#count species of trees
filtered_sp.myc <- sp.myc %>%
  filter(plot_id %in% dat_unique$plot_id)
length(unique(filtered_sp.myc$accepted_bin))

#count plots info
filtered_plot <- plot %>%
  filter(plot_id %in% dat_unique$plot_id)
length(unique(filtered_plot$plot_id))
#latitute
summary(filtered_plot$avglat)
#longitude
summary(filtered_plot$avglon)
#area
summary(filtered_plot$area)
#date
summary(filtered_plot$date)
plot_counts_per_date <- filtered_plot %>%
  group_by(date) %>%
  summarise(n_plots = n_distinct(plot_id))
summary(plot_counts_per_date)

#check more the dates

sum(is.na(filtered_plot$year))

# Make sure your date column is in Date format
filtered_plot$date <- as.Date(filtered_plot$date, "%y-%m-%d")

# Ensure date is in Date format and extract year
filtered_plot <- filtered_plot %>%
  mutate(year = as.numeric(format(as.Date(date), "%Y")))

# Define breaks and labels for decade bins
breaks <- seq(1890, 2031, by = 10)
labels <- paste(breaks[-length(breaks)] + 1, breaks[-1], sep = "–")

# Create the bins using cut()
filtered_plot <- filtered_plot %>%
  mutate(decade_bin = cut(year,
                          breaks = breaks,
                          labels = labels,
                          right = TRUE,
                          include.lowest = TRUE))

# Total number of plots (excluding NAs if needed)
total_plots <- nrow(filtered_plot %>% filter(!is.na(decade_bin)))

# Then summarise correctly
decade_summary <- filtered_plot %>%
  filter(!is.na(decade_bin)) %>%
  group_by(decade_bin) %>%
  summarise(
    Count = n(),
    Percentage = round(100 * Count / total_plots, 1),
    .groups = "drop"
  )

print(decade_summary)


##################################################

dat.sl <- readRDS("C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/data/sPlot4_sel_var_plot_inv_myc_SC0625.rds")

#Count biomes
count(dat.sl, biome)

#remove low biomes
low.biomes<- c("Mangroves", "Tropical_Coniferous", "Flooded_Grasslands", "Tropical_Deciduous_Broadleaf", "Tundra")
#low.biomes<- c("Mangroves", "Tropical_Coniferous")
dat.sl.sel <- dat.sl %>% filter (!biome %in% low.biomes)  %>% droplevels() 


