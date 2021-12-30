### Uccelli ###

setwd("/Users/federicotossani/Desktop/R_datacamp/bird_pop_08-12/Art12_MS_2015_csv/")
require(ggplot2)
require(dplyr)      #install.packages('dplyr')

birds <- read.csv("data_birds.csv", stringsAsFactors = FALSE)
birds <- as_tibble(birds)
#str(birds)
birds <- select(birds, "country", "speciesname", "population_maximum_size")
