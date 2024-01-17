# this script creates figures for the manuscript

#library(usmap)
library(tidyverse)
library(tidymodels)
library(DALEXtra)
library(ggplot2)
library(sf)
library(colorspace)
library(ggthemes)
library(patchwork)


# load and clean data from all of WI ------------------------
#grass <- read.csv("cropdata/WI_Grass_Soil_full.csv") %>%
grass <- read.csv("../Soil Data/SSURGO data/cropdata/WI_Grass_Soil_full.csv") %>%
  mutate_if(is.character, as.factor) %>%
  filter(cec < 100,
         nonirryield.r < 10) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(-c(yldunits,cokey,comppct.r,compkind,majcompflag,muacres,mukey, musym, compname)) %>%
  rename(yield = nonirryield.r)

## clean county names------------------------------
# county names have to match the names in the WI County Shapefile

levels(grass$county)
length(levels(grass$county))
# simple fixes
grass <- grass %>%
  mutate(county = recode(county,
                         "EauClaire" = "Eau Claire",
                         "FondDuLac" = "Fond du Lac",
                         "GreenLake" = "Green Lake",
                         "StCroix" = "St. Croix",
                         "LaCrosse" = "La Crosse",
                         "CalumetManitowoc" = "Calumet"))

# remove counties listed together as doubles and replace as single counties
doubles <- grass %>%
  filter(county == "KenoshaRacine"
         | county == "MilwaukeeWaukesha")

grass <- anti_join(grass, doubles) %>%
  droplevels()

doubles1 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Kenosha",
                         "MilwaukeeWaukesha" = "Milwaukee"))

doubles2 <- doubles %>%
  mutate(county = recode(county,
                         "KenoshaRacine" = "Racine",
                         "MilwaukeeWaukesha" = "Waukesha"))

grass <- bind_rows(grass, doubles1, doubles2)

levels(grass$county)
length(levels(grass$county))


# observations by county --------------------------------------------------

## fig 1a -------------------

# number of data records from each county
countyObs <- grass %>%
  group_by(county) %>%
  tally()

# load counties
counties <- st_read("data/WI_Counties2010/WI_Counties2010.shp")
counties <- st_read("/Volumes/One Touch/geospatial data carpentry/geospatial capstone/data/WI_Counties2010/WI_Counties2010.shp")
#head(counties)
#str(counties)
counties <- counties %>%
  dplyr::select(c(COUNTY, NAME, geometry))
levels(as.factor(counties$NAME))
levels(as.factor(grass$county))

obs_county <- left_join(counties, countyObs, by = c("NAME" = "county")) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(n_discrete = case_when(between(n, 0, 25) ~ "1-25",
                                between(n, 26, 100) ~ "26-100",
                                between(n, 101, 500) ~ "101-500",
                                between(n, 501,1000) ~ "501-1000",
                                between(n, 1001,1200) ~ "1001-1200"))

summary(obs_county)

png("figures/fig1a.png")
ggplot() +
  geom_sf(data = obs_county, aes(fill = n_discrete), lwd = 0.4, color = "black") +
  coord_sf() +
  theme_tufte()+
  theme(legend.position =  c(0.1,0.21),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        legend.title = element_text(size = 16, family = "Helvetica")) +
  scale_fill_manual(values=c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                             "#68B0AB", "#4A7C59"), 
                    name="Number\nof observations",
                    breaks=c("1-25", "26-100", "101-500", "501-1000","1001-1200"),
                    labels=c("1-25", "26-100", "101-500", "501-1000","1001-1200"))
dev.off()  

## fig 1b average yield by county--------------------------------

grassYld <- grass %>%
  group_by(county) %>%
  summarize(mnYld = mean(yield))

grassYld <- left_join(counties, grassYld, by = c("NAME" = "county"))

summary(grassYld)

grassYld <- grassYld %>%
  mutate(yldCat = case_when(between(mnYld, 1.5, 2.5) ~ "1.5-2.5",
                            between(mnYld, 2.5, 2.81) ~ "2.5-2.8",
                            between(mnYld, 2.8101, 3.132) ~ "2.8-3.1",
                            between(mnYld, 3.133,4.4) ~ "3.1-4.4"))

head(grassYld)

png("figures/avgYld_sf.png")
ggplot() +
  geom_sf(data = grassYld, aes(fill = yldCat), lwd = 0.4, color = "black") +
  coord_sf()+
  theme_tufte()+
  theme(legend.position =  c(0.1,0.21),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        legend.title = element_text(size = 16, family = "Helvetica")) +
  scale_fill_manual(values=c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                             "#68B0AB", "#4A7C59"), 
                    name="Avg. yield\n(tons/acre)",
                    breaks=c("1.5-2.5", "2.5-2.8", "2.8-3.1", "3.1-4.4"),
                    labels=c("1.5-2.5", "2.5-2.8", "2.8-3.1", "3.1-4.4"))

dev.off()  


# fig 2. predicted yield vs ssurgo yield-----------------------------------------

grass <- grass %>%
  drop_na(elev)

modCor <- readRDS("models/pastureCorRanger.rds")

set.seed(123)
split <- initial_split(grass, strata = yield)
train <- training(split)
test <- testing(split)

pred_df <- modCor %>%
  predict(test) %>%
  bind_cols(test) %>%
  filter(yield < 10)

summary(pred_df)

modCor
rmse <- sqrt(0.0178)

png("figures/predVssurgoModCor.png")
ggplot(data = pred_df, aes(x = yield, y = .pred)) +
  geom_jitter() +
  xlim(0.75,6) +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  ylab("Predicted yield") +
  xlab("SSURGO representative yield")
dev.off()

# fig 3. error across state OOB ------------------------------------------------------

# join to county fips
levels(as.factor(pred_df$county))

error <- pred_df %>%
  mutate(err = .pred - yield,
         err2 = err^2) %>%
  group_by(county) %>%
  summarise(count = n(),
            sumSqErr = sum(err2),
            mse = sumSqErr/n(),
            rmse = sqrt(mse)
  )

error <- error %>%
  mutate(rmse_cat = case_when(between(rmse, 0.03, 0.0744) ~ "0.03-0.074",
                              between(rmse, 0.0745, 0.0984) ~ "0.075-0.098",
                              between(rmse, 0.0985, 0.1084) ~ "0.099-0.108",
                              between(rmse, 0.1085, 0.25) ~ "0.109-0.25"))


err_county <- left_join(counties, error, by = c("NAME" = "county"))



png("figures/errPerCountyModCor_cat_sf.png")
ggplot() +
  geom_sf(data = err_county, aes(fill = rmse_cat), lwd = 0.4, color = "black") +
  coord_sf() +
  theme_tufte()+
  theme(legend.position =  c(0.1,0.21),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14, family = "Helvetica"),
        legend.title = element_text(size = 16, family = "Helvetica")) +
  scale_fill_manual(values=c("#FAF3DD", "#C8D5B9", "#8FC0A9",
                             "#68B0AB", "#4A7C59"), 
                    name="RMSE",
                    breaks=c("0.03-0.074", "0.075-0.098", "0.099-0.108", "0.109-0.25"),
                    labels=c("0.03-0.074", "0.075-0.098", "0.099-0.108", "0.109-0.25"))
dev.off()  


# model diagnostics -----------------------------------------------------

trainx <- train %>%
  dplyr::select(-c(yield))

# modCor explainer

cor_explainer <- 
  explain_tidymodels(
    modCor, 
    data = trainx, 
    y = train$yield,
    label = "random forest",
    verbose = FALSE
  )

(eva_rf <- DALEX::model_performance(cor_explainer))
plot(eva_rf, geom = "histogram") 

## fig 4. variable importance ----------------

set.seed(1804)
vip_cor <- model_parts(cor_explainer, loss_function = loss_root_mean_square)
plot(vip_cor)

obj <- list(vip_cor)
metric_name <- attr(obj[[1]], "loss_name")
metric_lab <- paste(metric_name, 
                    "after permutations\n(higher indicates more important)")


full_vip <- vip_cor %>%
  filter(variable != "_baseline_") %>%
  dplyr::select(-c(label))

perm_vals <- full_vip %>% 
  filter(variable == "_full_model_") %>% 
  summarise(dropout_loss = mean(dropout_loss))

p <- full_vip %>%
  filter(variable != "_full_model_") %>% 
  mutate(variable = str_to_sentence(variable),
         variable = fct_reorder(variable, dropout_loss),
         variable = recode(variable,
                           "Cropname" = "Species",
                           "Awc" = "AWC",
                           "Ph" = "pH",
                           "Om" = "OM",
                           "Elev" = "Elevation",
                           "Total.depth" = "Soil depth",
                           "Sandfine" = "Fine sand",
                           "Sandmed" = "Medium sand",
                           "Sandco" = "Coarse sand",
                           "Sandvc" = "Very coarse sand",
                           "Frag3" = "Rock fragments")) %>%
  ggplot(aes(dropout_loss, variable))  +
  geom_boxplot(fill = "#91CBD765") +
  labs(x = "Root mean square error (RMSE) loss", 
       y = NULL,  fill = NULL,  color = NULL)


p <- p + 
  theme_update(plot.tag = element_text(size = 16),
               axis.text = element_text(size = 14),
               axis.title = element_text(size = 14)) 

png("figures/fig4.png")
p
dev.off()

# partial dependance plots (figs 5) ------------------

library(viridisLite)

## species--------------------
pdp_species <- model_profile(
  cor_explainer,
  variables = "cropname",
  N = NULL
)

plot(pdp_species)

group_preds <- pdp_species$cp_profiles %>%
  mutate(cropname = fct_reorder(cropname, `_yhat_`)) %>%
  mutate(cropname = recode(cropname, 
                           "Bluegrass-clover" = "Bluegrass",
                           "Timothy-clover" = "Timothy",
                           "Orchardgrass-clover" = "Orchardgrass"))
group_means <- pdp_species$agr_profiles %>%
  rename(cropname = `_x_`) %>%
  mutate(cropname = fct_reorder(cropname, `_yhat_`)) %>%
  mutate(cropname = recode(cropname, 
                           "Bluegrass-clover" = "Bluegrass",
                           "Timothy-clover" = "Timothy",
                           "Orchardgrass-clover" = "Orchardgrass"))


png("figures/pdp_specieGroupCor.png")
ggplot(group_preds, aes(x = cropname, y = `_yhat_`)) +
  scale_color_brewer()+
  geom_point() +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab(" ") +
  theme(axis.ticks.x = element_blank()) +
  geom_point(data = group_means, aes(x = cropname, y = `_yhat_`), color = "black") +
  geom_line(data = group_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

## awc-------------------
pdp_awc <- model_profile(
  cor_explainer,
  variables = "awc",
  N = NULL
)

plot(pdp_awc)

awc_preds <- pdp_awc$cp_profiles 
awc_means <- pdp_awc$agr_profiles %>%
  rename(awc = `_x_`) 

png("figures/pdp_awcCor.png")
ggplot(awc_preds, aes(x = awc, y = `_yhat_`)) +
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Available water capacity (cm/cm)") +
  geom_line(data = awc_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = 'dashed')
dev.off()

png("figures/pdp_awc_simple.png")
ggplot(data = awc_means, aes(x = awc, y =`_yhat_`)) +
  geom_line() +
  ylab("Mean predicted yield (tons/acre)") +
  xlab("Available water capacity (cm/cm)")
dev.off()


##ksat----------------------
pdp_ksat <- model_profile(
  cor_explainer,
  variables = "ksat",
  N = NULL
)


plot(pdp_ksat)


ksat_preds <- pdp_ksat$cp_profiles 
ksat_means <- pdp_ksat$agr_profiles %>%
  rename(ksat = `_x_`)

png("figures/pdp_ksatCor.png")
ggplot(ksat_preds, aes(x = ksat, y = `_yhat_`)) +
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Ksat (um/s)") +
  geom_line(data = ksat_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

png("figures/pdp_ksat_simple.png")
ggplot(ksat_means, aes(x = ksat, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Ksat (um/s)")
dev.off()


## slope-------------------------
pdp_slope <- model_profile(
  cor_explainer,
  variables = "slope",
  N = NULL
)

plot(pdp_slope)

slope_preds <- pdp_slope$cp_profiles 
slope_means <- pdp_slope$agr_profiles %>%
  rename(slope = `_x_`) 


png("figures/pdp_slopeCor.png")
ggplot(slope_preds, aes(x = slope, y = `_yhat_`)) +
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Slope (%)") +
  geom_line(data = slope_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

png("figures/pdp_slope_simple.png")
ggplot(slope_means, aes(x = slope, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Slope (%)")
dev.off()

##fine sand----------------------
pdp_sandf <- model_profile(
  cor_explainer,
  variables = "sandfine",
  N = NULL
)

plot(pdp_sandf)

sandf_preds <- pdp_sandf$cp_profiles 
sandf_means <- pdp_sandf$agr_profiles %>%
  rename(sandfine = `_x_`) 


png("figures/pdp_fineSandCor.png")
ggplot(sandf_preds, aes(x = sandfine, y = `_yhat_`)) +
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Fine sand (%)") +
  geom_line(data = sandf_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

png("figures/pdp_finesand_simple.png")
ggplot(sandf_means, aes(x = sandfine, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Fine sand (%)")
dev.off()

##clay----------------

pdp_clay <- model_profile(
  cor_explainer,
  variables = "clay",
  N = NULL
)

plot(pdp_clay)

clay_preds <- pdp_clay$cp_profiles 
clay_means <- pdp_clay$agr_profiles %>%
  rename(clay = `_x_`) 


png("figures/pdp_clayCor.png")
ggplot(clay_preds, aes(x = clay, y = `_yhat_`)) +
  geom_point(alpha = 0.5, color = "black",size = 0.1) +
  geom_line(aes(group = `_ids_`), alpha = 0.5, color = "darkgrey",size = 0.1) + 
  ylab("Predicted yield (tons/acre)") +
  xlab("Clay (%)") +
  geom_line(data = clay_means, aes(group = `_ids_`), color = "black", size = 1.5, lty = "dashed")
dev.off()

png("figures/pdp_clay_simple.png")
ggplot(clay_means, aes(x = clay, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Clay (%)")
dev.off()

## ph--------------

pdp_ph <- model_profile(
  cor_explainer,
  variables = "ph",
  N = NULL
)

plot(pdp_ph)

ph_preds <- pdp_ph$cp_profiles 
ph_means <- pdp_ph$agr_profiles %>%
  rename(ph = `_x_`) 


png("figures/pdp_ph_simple.png")
ggplot(ph_means, aes(x = ph, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("pH")
dev.off()

## medium sand----------------

pdp_sandmed <- model_profile(
  cor_explainer,
  variables = "sandmed",
  N = NULL
)

plot(pdp_sandmed)

sandmed_preds <- pdp_sandmed$cp_profiles 
sandmed_means <- pdp_sandmed$agr_profiles %>%
  rename(sandmed = `_x_`) 


png("figures/pdp_sandmed_simple.png")
ggplot(sandmed_means, aes(x = sandmed, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Medium sand (%)")
dev.off()

## cec---------------------

pdp_cec <- model_profile(
  cor_explainer,
  variables = "cec",
  N = NULL
)

plot(pdp_cec)

cec_preds <- pdp_cec$cp_profiles 
cec_means <- pdp_cec$agr_profiles %>%
  rename(cec = `_x_`) 


png("figures/pdp_cec_simple.png")
ggplot(cec_means, aes(x = cec, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Cation exchange capacity (meq/100g)")
dev.off()

## coarse sand-----------------

pdp_sandc <- model_profile(
  cor_explainer,
  variables = "sandco",
  N = NULL
)

plot(pdp_sandc)

sandc_preds <- pdp_sandc$cp_profiles 
sandc_means <- pdp_sandc$agr_profiles %>%
  rename(sandco = `_x_`) 


png("figures/pdp_sandco_simple.png")
ggplot(sandc_means, aes(x = sandco, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Coarse sand (%)")
dev.off()

## om------------------------

pdp_om <- model_profile(
  cor_explainer,
  variables = "om",
  N = NULL
)

plot(pdp_om)

om_preds <- pdp_om$cp_profiles 
om_means <- pdp_om$agr_profiles %>%
  rename(om = `_x_`) 


png("figures/pdp_om_simple.png")
ggplot(om_means, aes(x = om, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Organic matter (%)")
dev.off()

## very coarse sand-----------------------

pdp_sandvc <- model_profile(
  cor_explainer,
  variables = "sandvc",
  N = NULL
)

plot(pdp_sandvc)

sandvc_preds <- pdp_sandvc$cp_profiles 
sandvc_means <- pdp_sandvc$agr_profiles %>%
  rename(sandvc = `_x_`) 


png("figures/pdp_sandvc_simple.png")
ggplot(sandvc_means, aes(x = sandvc, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Very coarse sand (%)")
dev.off()

## elevation--------------------

pdp_elev <- model_profile(
  cor_explainer,
  variables = "elev",
  N = NULL
)

plot(pdp_elev)

elev_preds <- pdp_elev$cp_profiles 
elev_means <- pdp_elev$agr_profiles %>%
  rename(elev = `_x_`) 


png("figures/pdp_elev_simple.png")
ggplot(elev_means, aes(x = elev, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Elevation (m)")
dev.off()

##frag3---------------------

pdp_frag <- model_profile(
  cor_explainer,
  variables = "frag3",
  N = NULL
)

plot(pdp_frag)

frag_preds <- pdp_frag$cp_profiles 
frag_means <- pdp_frag$agr_profiles %>%
  rename(frag = `_x_`) 


png("figures/pdp_frag_simple.png")
ggplot(frag_means, aes(x = frag, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Rock fragments (3-10 in) (%)")
dev.off()

## soil depth----------------------

pdp_depth <- model_profile(
  cor_explainer,
  variables = "total.depth",
  N = NULL
)

plot(pdp_depth)

depth_preds <- pdp_depth$cp_profiles 
depth_means <- pdp_depth$agr_profiles %>%
  rename(depth = `_x_`) 


png("figures/pdp_depth_simple.png")
ggplot(depth_means, aes(x = depth, y = `_yhat_`)) +
  geom_line() +
  ylab("Predicted yield (tons/acre)") +
  xlab("Soil depth (cm)")
dev.off()

# fig 6. yield by species--------------------------

speciesYield <- read_csv("data/speciesYield.csv") 

yield_sum <- speciesYield %>%
  group_by(species, university) %>%
  summarise(mnYld = mean(tons_acre),
            count = n(),
            sd = sd(tons_acre),
            se = sd/sqrt(count)) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(university = recode(university,
                             "yes" = "University trials",
                             "no" = "SSURGO database"))

# reorder species levels
yield_sum$species <- factor(yield_sum$species, levels = c("italian ryegrass", "kentucky bluegrass","meadow fescue",            
                                                          "quackgrass","smooth bromegrass","perennial ryegrass",       
                                                          "timothy","reed canarygrass","meadow bromegrass",        
                                                          "festulolium","orchardgrass","tall fescue"))

# labels abbreviated below
yield_sum_abbr <- yield_sum %>%
  mutate(species = recode(species,
                          "italian ryegrass" = "IR", 
                          "kentucky bluegrass" = "KB",
                          "meadow fescue" = "MF",            
                          "quackgrass" = "QG",
                          "smooth bromegrass" = "SB",
                          "perennial ryegrass" = "PR",       
                          "timothy" = "TM",
                          "reed canarygrass" = "RC",
                          "meadow bromegrass" = "MB",        
                          "festulolium" = "FE",
                          "orchardgrass" = "OG",
                          "tall fescue" = "TF"))

yieldGroups <- yield_sum_abbr %>%
  mutate(grouping = case_when(species == "IR" |
                                species == "KB" |
                                species == "MF" |
                                species == "QG" ~ "Low",
                              species == "SB" |
                                species == "PR" |
                                species == "TM" |
                                species == "RC" ~ "Medium",
                              TRUE ~ "High"))

yieldGroups$grouping <- factor(yieldGroups$grouping, levels = c("High", "Medium", "Low"))

png("figures/yieldgroupsspecies.png")
ggplot(data = yieldGroups, aes(x = species, y = mnYld, color = grouping, shape = university)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mnYld - sd, ymax = mnYld + sd)) +
  ylab("Yield (tons/acre) (± sd)") +
  xlab(" ") +
  labs(color = "Yield \n groups",
       shape = " ") +
  theme(axis.ticks.x = element_blank(),
        text = element_text(size = 16))
dev.off()


# average of each yield group
yieldGroupsUni <- yieldGroups %>%
  filter(university == "University trials") %>%
  group_by(grouping) %>%
  summarise(meanYield = mean(mnYld),
            minYield = min(mnYld),
            maxYield = max(mnYld))

yieldGroupsUni

# fig 7 (from app)---------------------------------

species <- c(
  "Bluegrass-clover",
  "Orchardgrass-clover",
  "Timothy-clover")

# select a single soil type
noCrop <- grass %>%
  select(-cropname) %>%
  filter(county == "Sauk") %>%
  drop_na() %>%
  droplevels()

soil_df <- noCrop[sample(nrow(noCrop),size = 1),]

#add grass_clover species
pred_df <-
  tibble(cropname = factor(species)) %>%
  group_by(cropname) %>%
  do(soil_df)

# run rf prediction
predictions <- predict(modCor, pred_df)
predictions

coef = c(1.2, 1, .95, 0.75, 0.65)
lowYield = round(coef * predictions$.pred[1], 2)
medYield = round(coef * predictions$.pred[3], 2)
highYield = round(coef * predictions$.pred[2], 2)

# combine predictions and rotation length coefficients
prediction_df <-
  tibble(
    Occupancy = rep(c("<1", "1", "3", "7", "Continuous"),3),
    coef = rep(c(1.2, 1, .95, 0.75, 0.65),3)
  ) %>%
  mutate(
    Variety = c(rep("Low",5), rep("Medium", 5), rep("High",5)),
    yield = c(lowYield, medYield, highYield)) 


prediction_df

prediction_df$Variety = factor(prediction_df$Variety, levels = c("Low", "Medium", "High"))

png("figures/websiteGraph.png")
ggplot(prediction_df, aes(x = Occupancy, y = yield, fill = Variety)) +
  geom_col(position = "dodge") +
  scale_fill_manual(breaks = c("Low", "Medium", "High"),
                    labels = c("Low yielding species", "Medium yielding species", "High yielding species"),
                    values = c("#D6EFC7","#96BB7C", "#377338")) +
  ylab("Yield (tons/acre)") +
  geom_abline(slope = 0, intercept = 0, color = "black")+
  theme(legend.title = element_blank(),
        text = element_text(size = 20),
        legend.position = c(0.8, 0.9),
        legend.text = element_text(size = 16),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "lightgrey"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(vjust = 6))
dev.off()

