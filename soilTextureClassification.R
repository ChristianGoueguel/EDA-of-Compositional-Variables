#------------------------------------------------------------------------------------------------------
# Soil texture classification based on the USDA classification system.
#------------------------------------------------------------------------------------------------------

# Coarse texture group
sand_class <- 
  soil_data %>%
  filter(soil_data$silt + 1.5 * soil_data$clay < 15)
loamySand_class <- 
  soil_data %>%
  filter(soil_data$silt + 1.5 * soil_data$clay >= 15 & 
           soil_data$silt + 2 * soil_data$clay < 30
         )
sandyLoam_class <- 
  soil_data %>%
  filter(soil_data$clay >= 7 &
           soil_data$clay < 20 & 
           soil_data$sand > 52 & 
           soil_data$silt + 2 * soil_data$clay >= 30 |
           soil_data$clay < 7 &
           soil_data$silt < 50 &
           soil_data$silt + 2 * soil_data$clay >= 30
         )

cat("Sand class: ")
dim(sand_class)

cat("Loamy sand class: ")
dim(loamySand_class)

cat("Sandy loam class: ")
dim(sandyLoam_class)


# Medium texture group
loam_class <- 
  soil_data %>%
  filter(soil_data$clay >= 7 &
           soil_data$clay < 27 & 
           soil_data$silt >= 28 &
           soil_data$silt < 50 &
           soil_data$sand <= 52
         )
siltLoam_class <- 
  soil_data %>%
  filter(soil_data$silt >= 50 &
           soil_data$clay >= 12 &
           soil_data$clay < 27 | 
           soil_data$silt >= 50 & 
           soil_data$silt < 80 & 
           soil_data$clay < 12
         )
silt_class <- 
  soil_data %>%
  filter(soil_data$silt >= 80 & 
           soil_data$clay < 12
         )

cat("Loam class: ")
dim(loam_class)

cat("Silt loam class: ")
dim(siltLoam_class)

cat("Silt class: ")
dim(silt_class)


# Fine texture group
sandyClayLoam_class <- 
  soil_data %>%
  filter(soil_data$clay >= 20 &
           soil_data$clay < 35 &
           soil_data$silt < 28 &
           soil_data$sand > 45
         )
clayLoam.class <- 
  soil_data %>%
  filter(soil_data$clay >= 27 &
           soil_data$clay < 40 &
           soil_data$sand > 20 &
           soil_data$sand <= 45
         )
siltyClayLoam_class <- 
  soil_data %>%
  filter(soil_data$clay >= 27 &
           soil_data$clay < 40 &
           soil_data$sand <= 20
         )
sandyClay_class <- 
  soil_data %>%
  filter(soil_data$clay >= 35 & 
           soil_data$sand > 45
         )
siltyClay_class <- 
  soil_data %>%
  filter(soil_data$clay >= 40 &
           soil_data$silt >= 40
         )
clay_class <- 
  soil_data %>%
  filter(soil_data$clay >= 40 &
           soil_data$sand <= 45 &
           soil_data$silt < 40
         )

cat("Sandy clay loam class: ")
dim(sandyClayLoam_class)

cat("claty loam class: ")
dim(clayLoam_class)

cat("Silty clay loam class: ")
dim(siltyClayLoam_class)

cat("Sandy clay class: ")
dim(sandyClay_class)

cat("Silty clay class: ")
dim(siltyClay_class)

cat("Clay class: ")
dim(clay_class)

# Binding 
Coarse_group <- rbind(sand_class, loamySand_class, sandyLoam_class)
Medium_group <- rbind(loam_class, siltLoam_class, silt_class)
Fine_group <- rbind(sandyClayLoam_class, clayLoam_class, siltyClayLoam_class, sandyClay_class, siltyClay_class, clay_class)

Coarse_group$texture <- replicate(n = nrow(Coarse_group), expr = "coarse")
Medium_group$texture <- replicate(n = nrow(Medium_group), expr = "medium")
Fine_group$texture <- replicate(n = nrow(Fine_group), expr = "fine")

