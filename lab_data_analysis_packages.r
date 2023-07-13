# This is the script in which you can find all the packages used to perform the analysis

      list.of.packages <- c("tidyverse",
                            "gridExtra",
                            "lubridate",
                            "DescTools")

# this part of the code check if every package is installed and load it with require() function, otherwise it will install it before loading it

      {
        new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

        if(length(new.packages)) install.packages(new.packages)

        lapply(list.of.packages, require, character.only = TRUE)
      }
