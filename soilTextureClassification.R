#------------------------------------------------------------------------------------------------------
# Soil texture classification based on the USDA classification system.
#------------------------------------------------------------------------------------------------------
#
# Note: data is a data frame containing the percentage of sand, silt, and clay.
#

# Coarse texture group
sand_class <- 
  data %>%
  filter(data$silt + 1.5 * data$clay < 15)
loamySand_class <- 
  data %>%
  filter(data$silt + 1.5 * data$clay >= 15 & 
           data$silt + 2 * data$clay < 30
         )
sandyLoam_class <- 
  data %>%
  filter(data$clay >= 7 &
           data$clay < 20 & 
           data$sand > 52 & 
           data$silt + 2 * data$clay >= 30 |
           data$clay < 7 &
           data$silt < 50 &
           data$silt + 2 * data$clay >= 30
         )
sand_class$class <- replicate(n = nrow(sand_class), expr = "S")
loamySand_class$class <- replicate(n = nrow(loamySand_class), expr = "LS")
sandyLoam_class$class <- replicate(n = nrow(sandyLoam_class), expr = "SL")


# Medium texture group
loam_class <- 
  data %>%
  filter(data$clay >= 7 &
           data$clay < 27 & 
           data$silt >= 28 &
           data$silt < 50 &
           data$sand <= 52
         )
siltLoam_class <- 
  data %>%
  filter(data$silt >= 50 &
           data$clay >= 12 &
           data$clay < 27 | 
           data$silt >= 50 & 
           data$silt < 80 & 
           data$clay < 12
         )
silt_class <- 
  data %>%
  filter(data$silt >= 80 & 
           data$clay < 12
         )
loam_class$class <- replicate(n = nrow(loam_class), expr = "L")
siltLoam_class$class <- replicate(n = nrow(siltLoam_class), expr = "SIL")
silt_class$class <- replicate(n = nrow(silt_class), expr = "SI")


# Fine texture group
sandyClayLoam_class <- 
  data %>%
  filter(data$clay >= 20 &
           data$clay < 35 &
           data$silt < 28 &
           data$sand > 45
         )
clayLoam_class <- 
  data %>%
  filter(data$clay >= 27 &
           data$clay < 40 &
           data$sand > 20 &
           data$sand <= 45
         )
siltyClayLoam_class <- 
  data %>%
  filter(data$clay >= 27 &
           data$clay < 40 &
           data$sand <= 20
         )
sandyClay_class <- 
  data %>%
  filter(data$clay >= 35 & 
           data$sand > 45
         )
siltyClay_class <- 
  data %>%
  filter(data$clay >= 40 &
           data$silt >= 40
         )
clay_class <- 
  data %>%
  filter(data$clay >= 40 &
           data$sand <= 45 &
           data$silt < 40
         )
sandyClayLoam_class$class <- replicate(n = nrow(sandyClayLoam_class), expr = "SCL")
clayLoam_class$class <- replicate(n = nrow(clayLoam_class), expr = "CL")
siltyClayLoam_class$class <- replicate(n = nrow(siltyClayLoam_class), expr = "SICL")
sandyClay_class$class <- replicate(n = nrow(sandyClay_class), expr = "SC")
siltyClay_class$class <- replicate(n = nrow(siltyClay_class), expr = "SIC")
clay_class$class <- replicate(n = nrow(clay_class), expr = "C")


# Binding 
Coarse_group <- rbind(sand_class, loamySand_class, sandyLoam_class)
Medium_group <- rbind(loam_class, siltLoam_class, silt_class)
Fine_group <- rbind(sandyClayLoam_class, clayLoam_class, siltyClayLoam_class, sandyClay_class, siltyClay_class, clay_class)

Coarse_group$texture <- replicate(n = nrow(Coarse_group), expr = "coarse")
Medium_group$texture <- replicate(n = nrow(Medium_group), expr = "medium")
Fine_group$texture <- replicate(n = nrow(Fine_group), expr = "fine")

