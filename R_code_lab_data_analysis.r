
                          # ---------------------------------- #
                          ## Report Laboratio di Analisi Dati ##
                          # ---------------------------------- #


# Codebook per la scrittura del report finale del corso Laboratorio di Analisi Dati (M-Z)
 
# Prof. Saverio Ranciati
 
# Univesità: Alma Mater Studiorum Università di Bologna.

# Anno: 2022/2023
 
# Dataset: 

# Obiettivo del Progetto: 

# ---------------------------------------------------------------------------------- #

#                                     SOMMARIO

# 1. Pacchetti

# 2. Importazione dati

# 3. Selezione dei dati

# 4. Organizzazione dei dati


# ---------------------------------------------------------------------------------- #

#                                   1. Pacchetti

# Creo una lista che contiene tutti i pacchetti che mi interesanno per le successiva analisi

      list.of.packages <- c("tidyverse",
                            "gridExtra",
                            "stargazer",
                            "lubridate",
                            "ggthemes",
                            "ggpubr",
                            "gganimate",
                            "patchwork",
                            "gifski",
                            "gt",
                            "knitr",
                            "DescTools")

# il seguente comando verifica che i pacchetti siano installati (se necessario li installa) poi li carica

      {
        new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

        if(length(new.packages)) install.packages(new.packages)

        lapply(list.of.packages, require, character.only = TRUE)
      }

# ---------------------------------------------------------------------------------- #

#                                 2. Importazione dati

# Prima di tutto imposto la working directory
      setwd("C:/Users/fedet/OneDrive/Documenti/R/lab_data_analysis/data/")

# Ora importo i dataset che mi serviranno nel corso delle analisi

      data_raw <- read.csv("Article12_2020_data_birds.csv", stringsAsFactors = F)
      data_taxbirds_raw <- read.csv("Article12_2020_bird_check_list.csv", stringsAsFactors = F)
      data_country <- read.csv("Article12_2020_ref_countries.csv", stringsAsFactors = F)

# uso str() per avere un'idea della struttura del data set
      str(data_raw)
      head(data_raw)

      str(data_taxbirds_raw)
      head(data_taxbirds_raw)

# ---------------------------------------------------------------------------------- #

#                                 3. Selezione dei dati

      data_taxbirds <- data_taxbirds_raw%>%
        select("speciescode", "speciesname","taxOrder", "taxFamily", "taxGroup_en", "taxFamily_en")%>%
        distinct(.keep_all = TRUE)


# rimuovo dal data set le variabili contenenti la sorgente dei dati
      source <- data_raw%>%
        select(matches("_source"))%>%
        colnames()

# per creare un sub set seleziono tutte le variabili di cui ho bisogno per le analisi
# prima le reggruppo in un vettore poi con la funzione select() le seleziono dal data set iniziale
# creo il subset del data set iniziale al quale aggiungo le variabili con le informazioni tassonomiche

      variable <- c("country", "season","speciescode", "speciesname", "population_date",
                    "population_size_unit", "population_size_min", "population_size_max",
                    "population_method", "population_trend_period", "population_trend",
                    "population_trend_method", "population_trend_long_period", "population_trend_long",
                    "population_trend_long_method", "use_for_statistics" )

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

      data_eu[data_eu == ""] <- NA

      str(data_eu)

# Let's create a summary data frame with the range of every variables

      data_eu_sum <- data_eu%>%
          mutate_all(as.factor)%>%
          summary()

# 'data.frame':	5207 obs. of  21 variables

data_eu_desc <- Desc(data_eu) 

Desc(data_eu$population_trend, plotit=TRUE)  


# Crea il dataframe con i nomi delle variabili
variabili <- data.frame(NomeVariabile = names(data_eu))

# Ottieni le descrizioni delle variabili utilizzando dplyr
descrizioni <- data.frame(Descrizione = c("Sigla identificativa dal paese", "Stagione di riferimento; B = breeding, P = passage, W = wintering", "Codice identificativo della specie", "Nome scientifico della specie", "Intervallo di tempo a cui fa rifermento la stima della popolazione", "Unità di misura per il conteggio della popolazione. i = individui; p = pairs (coppie); cmales = maschi in canto, bfemales = femmine riproduttive; males = maschi", "Stima minima della popolazione", "Stima massima della popolazione", "Metodo utilizzato per stimare la popolazione. completeSurvey = censimento completo; estimateExpert = stima basata sulla conoscienza degli esperti; estimatePartial = stima basata su una limitata quantità di dati; absentData = dato assente", "Periodo di riferimento per il trend a breve termine", "bla", "bla", "bla", "bla", "bla", "bla", "bla", "bla", "bla", "bla", "bla"))
descrizione <- c("Sigla identificativa dal paese", "Stagione di riferimento; B = breeding, P = passage, W = wintering", "Codice identificativo della specie", "Nome scientifico della specie", "Intervallo di tempo a cui fa rifermento la stima della popolazione", "Unità di misura per il conteggio della popolazione", "Stima minima della popolazione", "Stima massima della popolazione", "Metodo utilizzato per stimare la popolazione", "Periodo di riferimento per il trend a breve termine", "Trend nel breve periodo.", "Metodo di stima utilizzato per la popolazione nel breve periodo", "Periodo di riferimento per il trend a lungo termine", "Trend nel lungo periodo", "Metodo di stima utilizzato per la popolazione nel lungo periodo", "Dato utilizzabile a fine statistici", "Ordine tassonomico", "Famiglia tassonomica", "Gruppo tassonimico", "Famiglia tassonomica in lingua inglese", "Paese di provenienza del dato")

# Combina i dataframe delle variabili e delle descrizioni
tabella <- cbind(variabili, descrizione)

PlotMiss(data_eu, main="Missing european data", clust = TRUE)


# ---------------------------------------------------------------------------------- #

#                                 4. Analisi dei dati

# Stati presi in considerazione
      stati_pres <- data_eu%>%
        distinct(country, .keep_all = F)%>%
        arrange(country, sort = T)

      # L'analisi prenderà in esame i dati provenienti da 28 stati europei, elencati in tabella 1.
      # Per gli stati che comprendono isole oceaniche o territori al di fuori dei confini nazionali 
      # è stata inserita un'ulteriore riga (Spagna, Portogallo, Regno Unito), arrivando così ad un
      # totale di 32.

# Stati con maggior diversità specifica (top 5)
      
      countries <- data_eu%>%
        group_by(country)%>%
        distinct(speciesname, .keep_all = F)%>%
        count(country)%>%
        arrange(desc(n))%>%
        rename("observations" = "n")


      countries <- countries%>%
        mutate(percent= observations/487*100)

      countries_rich <- countries%>%
        head(5)

      country_rich_sp <- data_eu%>%
        filter(country %in% countries_rich$country)%>%
        distinct(speciesname, .keep_all = T)

# In ordine di osservazioni gli stati con il maggior numero di dati sono:
# Bulgaria, Spagna, Francia, Germania, Grecia.
# Questi hanno raccolto i trend di 416 specie diverse.



# Countries with the highest number of wintering, breeding and migrating species

      season_countries <- data_eu%>%
              # group_by(label, season)%>%
                group_by(country, season)%>%
                distinct(speciesname, .keep_all = F)%>%
                count(season)%>%
                arrange(season, desc(n))%>%
                rename("observations" = "n") # %>%
               # filter(season == "B")       # use this filter to select the season you are interested in

      print(season_countries, n = 92)

  # Top 10 countries for every season

        w_10 <- season_countries%>%
                        filter(season == "W")%>%
                        head(10)
        p_10 <- season_countries%>%
                        filter(season == "P")%>%
                        head(10)
        b_10 <- season_countries%>%
                        filter(season == "B")%>%
                        head(10)

# Let's create a barplot to visualize the situation

      ggplot(data=top_season_countries, aes(x=label, y=observations, fill=season)) +
        geom_bar(stat="identity")+
        theme_light()

      # Il grafico mette in evidenza 
      #
      #
      #
      #
      #

# 

# export tables to tex

       tabella %>%
         kable(format = 'latex', booktabs = TRUE) 

















































# Formattazione della tabella degli stati

# constants ----
n = 0
c_col = c("#1e3048", "#274060", "#2f5375", "#4073a0", "#5088b9")
c_col_light_blue = c("#edf2fb", "#e2eafc", "#d7e3fc", "#ccdbfd", "#c1d3fe")
c_container_width = px(800)
c_table_width = px(650)
c_rn = 30
c_save = TRUE
c_format = "pdf"

gt_countries <- countries%>%
  ungroup()%>%
  gt()%>%
  #Add a table title
  #Notice the `md` function allows us to write the title using markdown syntax (which allows HTML)
  tab_header(
    title = md("Stati Europei presi in esame e relativo numero di osservazioni")
    ) %>% 
  #Add a data source footnote
  tab_source_note(
    source_note = "Source: European Environmental Agency - Status and trends of bird populations: datasets from Article 12, Birds Directive 2009/147/EC reporting"
    )%>%
  fmt_number(
    columns = c("Observations"),
    suffixing = F,
    decimals = 0
  ) %>% 
  cols_align(
    align = "center",
    columns = c("Country", "Observations")
  ) %>% 
  # cols_align(
  #   align = "right",
  #   columns = c("")
  # ) #%>%
  # cols_align(
  #   align = "left",
  #   columns = c("")
  #) %>%
  cols_width(
    c("Observations") ~ px(200),
    c("Country") ~ px(300)
    # vars("Born in") ~ px(65),
    # vars("Death Cause", "Release strategy", "Release age (months)") ~ px(200),
  )

gt_countries <- gt_countries%>%
  tab_options(
    table.font.name = "Calibri",
    table.font.color = c_col[1],
    table.border.top.style = "none",
    table.border.bottom.style = "solid",
    table.border.bottom.color = c_col[2],
    table.border.bottom.width = px(3),
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = c_col[2],
    column_labels.border.bottom.width = px(3),
    data_row.padding = px(10)
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        size = px(28),
        weight = "normal",
        align = "left",
        font = "Calibri"
      )
    ),
    locations = list(
      cells_title(groups = "title")
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        size = px(18),
        align = "left"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(
        size = px(17)
      ),
      cell_borders(
        sides = c("bottom", "top"),
        color = c_col[1],
        weight = px(1)
      )
    ),
    locations = list(
      cells_body(gt::everything())
    )
  ) %>% 
  tab_style(
    style = list( 
      cell_text(
        size = px(18),
        color = "#000000",
        weight = "bold",
        font = "Calibri"
      )
    ),
    locations = list(
      cells_column_labels(everything())
    )
  ) %>% 
  # tab_style(
  #   style = list( 
  #     cell_text(
  #       size = px(18),
  #       color = "#000000",
  #       font = "Calibri"
  #     ),
  #     cell_borders(
  #       sides = c("bottom"),
  #       style = "solid",
  #       color = c_col[1],
  #       weight = px(2)
  #     )
  #   ),
  #   locations = list(
  #     cells_row_groups(gt::everything())
  #   )
  # ) %>% 
  tab_style(
    style = list( 
      cell_text(
        size = px(18),
        align = "right",
        weight = "normal",
        color = "#000000",
        font = "Calibri"
      ),
      cell_borders(
        sides = c("bottom", "right"),
        style = "solid",
        color = "white",
        weight = px(1)
      )
    ),
    locations = list(
      cells_stub(gt::everything()),
      cells_stubhead()
    )
  ) #%>% 
  tab_style(
    style = list(
      cell_text(
        font = "Calibri",
        size = px(18), 
        color = "#000000")
    ),
    location = list(
      cells_body(columns = vars(Country))
    )
  ) 

# Stati con minor diversità specifica (top 5)
country_poor <- data_eu%>%
  group_by(country)%>%
  distinct(speciesname, .keep_all = F)%>%
  count(country)%>%
  arrange(n)%>%
  left_join(data_country, by = c("country" = "Code"))%>%
  head(n = 10)

country_poor_sp <- data_eu%>%
  filter(country %in% country_poor$country)%>%
  distinct(speciesname, .keep_all = F)

porto <- c("PT", "PTAC", "PTMA")

portugal <- data_eu%>%
  filter(country %in% porto)

pt_sp <- portugal%>%
  group_by(country)%>%
  distinct(speciesname, .keep_all = F)%>%
  arrange(speciesname)

# i 5 stati che hanno fornito meno dati presentano un totale di 58 specie.

# Stato con più trend positivi


# Stato con più trend negativi


# 


# wintering <- data_eu%>%
#   filter(season == "W")
# 
# breeding <- data_eu%>%
#   filter(season == "B")
# 
# passage <- data_eu%>%
#   filter(season == "P")
# 
# at <- data_eu%>%
#   filter(country == "AT")
# 
# it <-data_eu%>%
#   filter(country == "IT")%>%
#   arrange(speciesname)



# ---------------------------------------------------------------------------------- #


#                                 5. 




