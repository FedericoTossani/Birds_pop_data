# This is the scrip in which you can find the code to import and select the data I use later

# Prima di tutto imposto la working directory

      setwd("C:/Users/fedet/OneDrive/Documenti/R/lab_data_analysis/data/")

# Ora importo i dataset che mi serviranno nel corso delle analisi

      # dataset con i trend delle popolazioni
      data_raw <- read.csv("Article12_2020_data_birds.csv", stringsAsFactors = F)

      # dataset con i dati tassonimici
      data_taxbirds_raw <- read.csv("Article12_2020_bird_check_list.csv", stringsAsFactors = F)

      # dataset con i dati degli Stati
      data_country <- read.csv("Article12_2020_ref_countries.csv", stringsAsFactors = F)

# seleziono le variabili che mi interessano dal dataset tassonomico e le salvo in un nuovo oggetto

      data_taxbirds <- data_taxbirds_raw%>%
        select("speciescode", "speciesname","taxOrder", "taxFamily", "taxGroup_en", "taxFamily_en")%>%
        distinct(.keep_all = TRUE)


# rimuovo dal dataset le variabili contenenti la sorgente dei dati, per una migliore visualizzazione

      source <- data_raw%>%
        select(matches("_source"))%>%
        colnames()


# creo un vettore variable contenente i nomi delle variabili a cui sono interessato

      variable <- c("country", "season","speciescode", "speciesname", "population_date",
                    "population_size_unit", "population_size_min", "population_size_max",
                    "population_method", "population_trend_period", "population_trend",
                    "population_trend_method", "population_trend_long_period", "population_trend_long",
                    "population_trend_long_method", "use_for_statistics" )

# questa parte di codice mi permette di ottenre il dataset di partenza sul quale sviluppare le successive analisi

      data_eu <- data_raw%>%
        select(-which(names(data_raw) %in% source))%>%  
        filter(use_for_statistics == "Yes")%>%
        select(all_of(variable))%>%
        left_join(data_taxbirds, by = c("speciescode", "speciesname"), keep = F)%>%
        arrange(country, speciescode)

      data_eu <- data_eu%>%
          left_join(data_country, by = c("country" = "Code"))%>%
          rename("country" = "Label",
                 "label" = "country")

      cols.num <- c("population_size_min","population_size_max")
      data_eu[cols.num] <- sapply(data_eu[cols.num],as.numeric)
      sapply(data_eu, class)

      data_eu <- data_eu%>%
        mutate(population_size_mean = rowMeans(data_eu[, c("population_size_min","population_size_max")]))


      data_eu[data_eu == ""] <- NA

# Usa questo pezzo di codice per togliere le varaibili che non sono servite alle analisi così da avere un dataset più pulito
      data_eu <- data_eu%>%
              select(-c("speciescode", "use_for_statistics", "taxGroup_en","taxFamily_en"))

      data_eu <- data_eu%>%
              mutate(population_trend = recode(population_trend, "Unk" = "U", "UNK" = "U"),
                     population_trend_long = recode(population_trend_long, "Unk" = "U", "UNK" = "U"))
