# this file is used to download and clean the data used in model development

# files downloaded are stored in separate workspace
setwd("/Volumes/GoogleDrive/My Drive/grassland2.0/grazescape/Soil Data/SSURGO data")

#load libraries
library(FedData)
library(dplyr)
library(tidyr)

#counties as listed on web soil survey for WI https://websoilsurvey.nrcs.usda.gov/app/WebSoilSurvey.aspx
counties <- c("Brown", "Crawford", "Kewaunee", "Monroe", "Marathon", "Taylor", "Vernon", "Clark", "Grant", 
              "Shawano", "Lafayette", "Dane",
              "Chippewa", "Dodge", "FondDuLac", "Manitowoc", "Barron", 
              "Adams", "Ashland", "Bayfield", "Buffalo","Burnett", "Columbia", "Door", "Douglas",
              "Dunn", "EauClaire", "Florence", 'Forest', "Green", "GreenLake", "Iowa", "Iron",
              "Jackson", "Jefferson", "Juneau", "LaCrosse", "Langlade","Lincoln", "Marinette",
              "Marquette", "Menominee", "Oconto", "Oneida", "Outagamie", "Ozaukee", "Pepin", "Pierce",
              "Polk", "Portage", "Price", "Richland", "Rock", "Rusk", "StCroix", "Sauk", "Sawyer",
              "Sheboygan", "Trempealeau", "Vilas", "Walworth", "Washburn", "Washington", "Waupaca",
              "Waushara", "Winnebago", "Wood", "CalumetManitowoc", "KenoshaRacine", "MilwaukeeWaukesha")

# downloading functions --------------------------------
#extract chorizon layers
read_horizon <- function(){
  horizon <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_chorizon.csv")
    #print(i)
    #print(filename)
    horizon[[i]] <- read.csv(file = filename, na.strings = c(" ", "", "NA", "NaN"))
  }
  horizon <- do.call(rbind.data.frame, horizon)
}


#extract component layers
read_component <- function(){
  component <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_component.csv")
    component[[i]] <- read.csv(file = filename, na.strings = c(" ", " ", "NA", "NaN"))
  }
  component <- do.call(rbind.data.frame, component)
}


#extract mapunit and attach county name
read_mapunit <- function(){
  mapunit <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_mapunit.csv")
    mapunit[[i]] <- read.csv(file = filename, na.strings = c(" ", "NA", "NaN", ""))
    mapunit[[i]]$county <- counties[i]
  }
  mapunit <- do.call(rbind.data.frame, mapunit)
}


#extract component yields
read_cocropyld <- function(){
  cocropyld <- list()
  for(i in 1:length(counties)){
    filename = paste0("EXTRACTIONS/", counties[i], "/SSURGO/", counties[i], "_SSURGO_cocropyld.csv")
    cocropyld[[i]] <- read.csv(file = filename, na.strings = c("", NA, " ", "NaN"))
  }
  cocropyld <- do.call(rbind.data.frame, cocropyld)
}

# download data-----------------

chorizon <- read_horizon()
component <- read_component()
mapunit <- read_mapunit()
cocropyld <- read_cocropyld()

# clean horizon data to 30 cm depth--------------------------

#deepest horizon bottom of each component
depth <- chorizon %>%
  group_by(cokey) %>%
  summarise(total.depth = max(hzdepb.r))

#filter to remove horizons that start below 30 cm
chorizon <- chorizon %>%
  filter(hzdept.r < 31) %>%
  droplevels()

 colnames(chorizon)

# initial variable selection
chorizon <- chorizon %>%
  dplyr::select(hzdepb.r, hzdept.r, cokey, sandtotal.r, sandvc.r, sandco.r, sandmed.r, sandfine.r,
                silttotal.r, siltco.r, siltfine.r, claytotal.r, partdensity, om.r, ksat.r, gypsum.r, 
                sar.r, ec.r, cec7.r, ph1to1h2o.r, awc.r, fraggt10.r, frag3to10.r)

summary(chorizon)

# weighted means of horizons down to 30 cm
chorizon <- chorizon %>%
  mutate(thick = ifelse(hzdepb.r > 30, 30 - hzdept.r, hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(sand = round(weighted.mean(sandtotal.r, thick, na.rm = TRUE),2),
            sandvc = round(weighted.mean(sandvc.r, thick, na.rm = TRUE),2),
            sandco = round(weighted.mean(sandco.r, thick, na.rm = TRUE), 2),
            sandmed = round(weighted.mean(sandmed.r, thick, na.rm = TRUE), 2),
            sandfine = round(weighted.mean(sandfine.r, thick, na.rm = TRUE), 2),
            silt = round(weighted.mean(silttotal.r, thick, na.rm = TRUE),2),
            siltco = round(weighted.mean(siltco.r, thick, na.rm = TRUE), 2),
            siltfine = round(weighted.mean(siltfine.r, thick, na.rm = TRUE), 2),
            clay = round(weighted.mean(claytotal.r, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om.r, thick, na.rm = TRUE),2),
            partdensity = round(weighted.mean(partdensity, na.rm = TRUE), 2),
            ksat = round(weighted.mean(ksat.r, thick, na.rm = TRUE),2),
            gypsum = round(weighted.mean(gypsum.r, thick, na.rm = TRUE),2),
            sar = round(weighted.mean(sar.r, thick, na.rm = TRUE), 2),
            ec = round(weighted.mean(ec.r, thick, na.rm = TRUE), 2),
            cec = round(weighted.mean(cec7.r, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph1to1h2o.r, thick, na.rm = TRUE),2),
            awc = round(weighted.mean(awc.r, thick, na.rm = TRUE), 2),
            frag3 = round(weighted.mean(frag3to10.r, thick, na.rm = TRUE), 2),
            frag10 = round(weighted.mean(fraggt10.r, thick, na.rm = TRUE), 2)) 
#add deepest soil depth back
chorizon <- left_join(chorizon, depth, by = "cokey")

summary(chorizon)
# gypsum and sar are all 0s and NAs
hist(chorizon$ec) # almost all 0s 
##TODO remove sar, gypsum, ec, siltco and siltfine and partdensity (too many NAs)
hist(chorizon$frag3, breaks = 20)
hist(chorizon$frag10, breaks = 20)

# clean soil series-------------------
names(component)
component <- component %>%
  dplyr::select(c(comppct.r, compname, compkind, majcompflag, slope = slope.r, elev = elev.r, mukey, cokey)) %>%
  filter(compkind == "Series") %>%
  droplevels()

# clean mapunit---------------------
colnames(mapunit)
mapunit <- mapunit %>%
  dplyr::select(c(musym, muacres, mukey, county)) # took out muname

# clean crop yield----------------
colnames(cocropyld)
cocropyld <- cocropyld %>%
  dplyr::select(cropname, yldunits, nonirryield.r, cokey)

# join horizon and soil series and yield ---------------
component_horizon <- left_join(component, chorizon, by = c("cokey"))

full_soil <- left_join(component_horizon, mapunit, by = c("mukey")) %>%
  distinct() 

summary(full_soil)
# full_soil <- full_soil %>%
#   filter(majcompflag == "Yes")

soil_crop <- left_join(cocropyld, full_soil, by = "cokey")
summary(soil_crop)

# select only well represented pasture species------------------

soil_crop_prod <- soil_crop %>%
  drop_na() %>%
  filter(cropname == "Bluegrass-white clover"
         | cropname == "Orchardgrass-alsike"
         | cropname == "Orchardgrass-red clover"
         | cropname == "Timothy-alsike") %>%
  mutate(cropname = recode(cropname, 
                           `Bluegrass-white clover`="Bluegrass-clover",
                           `Orchardgrass-alsike`= "Orchardgrass-clover",
                           `Orchardgrass-red clover` = "Orchardgrass-clover",
                           `Timothy-alsike` = "Timothy-clover")) %>%
  droplevels() %>%
  mutate_if(is.character, as.factor)

summary(soil_crop_prod)

write.csv(soil_crop_prod, file = "cropdata/WI_Grass_Soil_full.csv", row.names = FALSE, quote = FALSE)


