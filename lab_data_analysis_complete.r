
                          # ---------------------------------- #
                          ## Report Laboratio di Analisi Dati ##
                          # ---------------------------------- #


# Prof. Saverio Ranciati
 
# Univesità: Alma Mater Studiorum Università di Bologna.

# Anno: 2022/2023
 
# Dataset: Status and trends of bird populations: datasets from Article 12, Birds Directive 2009/147/EC reporting

# Obiettivo del Progetto: Descrivere il dataset per valutare lo status delle popolazioni di uccelli in europa

# ---------------------------------------------------------------------------------- #

#                                     SOMMARIO

# 1. Pacchetti
# 2. Importazione e selezione dei dati
# 3. Analisi descrittiva
# 4. Analisi di dettaglio


# ---------------------------------------------------------------------------------- #

#                                   1. Pacchetti

    # Here I use the source function to load the script containing the packages needed
      source("https://raw.githubusercontent.com/FedericoTossani/Birds_pop_data/main/lab_data_analysis_packages.r")

# ---------------------------------------------------------------------------------- #

#                                 2. Importazione e selezione dei dati

    # Here I use the source function to load the script containing the data frame's import and variables selection process
      source("https://raw.githubusercontent.com/FedericoTossani/Birds_pop_data/main/lab_data_analysis_import_selection.r")

    # Usa questo pezzo di codice per esportare tabelle in LaTex
    
           d.single_variable_freq %>%
             kable(format = 'latex', booktabs = TRUE)

    # Compongo la tabella che riporta le variabili e la loro descrizione
        # Creo il dataframe con i nomi delle variabili
          variabili <- data.frame(NomeVariabile = names(data_eu))
    
        # Ottieni le descrizioni delle variabili utilizzando dplyr
          descrizione <- data.frame(Descrizione = c("Sigla identificativa dal paese", 
                                                    "Stagione di riferimento; B = breeding, P = passage, W = wintering", 
                                                    "Nome scientifico della specie", 
                                                    "Intervallo di tempo a cui fa rifermento la stima della popolazione", 
                                                    "Unità di misura per il conteggio della popolazione", 
                                                    "Stima minima della popolazione", 
                                                    "Stima massima della popolazione", 
                                                    "Metodo utilizzato per stimare la popolazione", 
                                                    "Periodo di riferimento per il trend a breve termine", 
                                                    "Trend nel breve periodo", 
                                                    "Metodo di stima utilizzato per la popolazione nel breve periodo", 
                                                    "Periodo di riferimento per il trend a lungo termine", 
                                                    "Trend nel lungo periodo", 
                                                    "Metodo di stima utilizzato per la popolazione nel lungo periodo", 
                                                    "Ordine tassonomico", 
                                                    "Famiglia tassonomica", 
                                                    "Paese di provenienza del dato", 
                                                    "Media tra il valore massimo e il minimo della popolazione"))

    # Combina i dataframe delle variabili e delle descrizioni
      tab_variables_desc <- cbind(variabili, descrizione)


# ---------------------------------------------------------------------------------- #

#                                 3. Analisi descrittiva

# Usa questo pezzo di codice per esportare tabelle in LaTex

       d.single_variable_freq %>%
         kable(format = 'latex', booktabs = TRUE, digits = c(0, 0, 3, 3, 3)) 

# Per l'analisi descrittiva del dataset ho utilizzato la funzione Desc() del pacchetto DescTools
# L'output della funzione è una lista in cui ogni elemento è la descrizione di una variabile del dataset inziale

      d.data_eu <- Desc(data_eu)
      d.data_eu

# Andando a modificare la variabile selezionata nella stringa seguente ottengo le statistiche per una singola varaibile
      d.single_variable <- d.data_eu$season
      d.single_variable_freq <- d.single_variable$freq

# Pie chart

ggplot(d.single_variable_freq, aes(x = "", y = perc, fill = level)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  theme_void()+
  labs(fill = "Season")+
  geom_text(aes(label = paste0(round(perc*100, 1),"%")), position = position_stack(vjust = 0.5))

                ggsave("seasonfreq.jpg", plot=last_plot())

# ---------------------------------------------------------------------------------- #

#                                 4. Analisi di dettaglio


# Usa questo pezzo di codice per esportare tabelle in LaTex

       d.single_variable_freq %>%
         kable(format = 'latex', booktabs = TRUE, digits = c(0, 0, 3, 0, 3)) 

      d.b_pop <- Desc(b_pop)
      d.single_variable <- d.data_eu$population_trend
      d.single_variable_freq <- d.single_variable$freq

# 4.1 Species richness nei diversi stati europei

    # Countries with the highest number of wintering, breeding and migrating species
    
          season_countries <- data_eu%>%
                    group_by(country, season)%>%
                    #distinct(speciesname, .keep_all = F)%>%
                    count(season)%>%
                    arrange(season, desc(n))%>%
                    rename("observations" = "n")
    
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
                            mutate(percent = (observations/3617))%>%
                            head(10)


# 4.2 Associazione tra la stagione e il gruppo tassonomico

            orderxseason <- data_eu%>%
                    group_by(taxOrder, season)%>%
                    #filter(taxOrder != "Passeriformes")%>%
                    count()%>%
                    rename("observations" = "n")%>%
                    arrange(desc(observations))
            
                  ggplot(data=orderxseason, aes(x=taxOrder, y=observations, fill=season)) +
                    geom_bar(stat="identity")+
                    geom_text(aes(label = observations), vjust = -1)+
                    facet_wrap(~season)+
                    theme_light()+
                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
                    labs(x = "Taxonomic order", y = "Number of observations")
            
                ggsave("OrderxSeason.jpg", plot=last_plot())

# 4.3 Trend nel breve periodo

    # Breeding population
                b_pop <- data_eu%>%
                      filter(season == "B")

                b_pop_trend <- Desc(b_pop$population_trend)

          # Trend nei paesi
             btb <- ggplot(data=b_pop, aes(x=country, fill=population_trend)) +
                  geom_bar(position = "fill")+
                  theme_light()+
                  labs(x = "Country", fill = "Trend nel breve periodo", title = "Breeding populations")+
                  theme(legend.position = "bottom")+
                  coord_flip()


          # Trend negli ordini tassonomici
               ggplot(data=b_pop, aes(x=taxOrder, fill=population_trend)) +
                  geom_bar(position = "fill")+
                  theme_light()+
                  labs(x = "Taxonomic order")+
                  coord_flip()
          
    # Wintering population
                w_pop <- data_eu%>%
                      filter(season == "W")

                w_pop_trend <- Desc(w_pop$population_trend)

         # Trend nei paesi
            wtb <- ggplot(data=w_pop, aes(x=country, fill=population_trend)) +
                  geom_bar(position = "fill")+
                  theme_light()+
                  labs(x = "", title = "Wintering populations")+
                  theme(legend.position = "none")+
                  coord_flip()

         # Trend negli ordini tassonomici
               ggplot(data=W_pop, aes(x=taxOrder, fill=population_trend)) +
                  geom_bar(position = "fill")+
                  theme_light()+
                  labs(x = "Country")+
                  coord_flip()

          # Combine the legends of the two graphs
          combined_legend <- cowplot::get_legend(btb)
          
          # Remove the legends from the individual graphs
          btb_no_legend <- btb + theme(legend.position = "none")
          wtb_no_legend <- wtb + theme(legend.position = "none")
          
          # Arrange the graphs side by side with the shared legend
          grid.arrange(
            arrangeGrob(btb_no_legend, wtb_no_legend, ncol = 2),
            combined_legend,
            heights = c(1, 0.2)
          )

          ggsave("trend_breve_country.jpg", plot=arrangeGrob(arrangeGrob(btb_no_legend, wtb_no_legend, ncol = 2),combined_legend, heights = c(1, 0.2)), device = "jpeg", dpi = 300)

# 4.4 Trend nel lungo periodo

            # Breeding population

                    b_pop_trend_long <- Desc(b_pop$population_trend_long)

             btl <- ggplot(data=b_pop, aes(x=country, fill=population_trend_long)) +
                  geom_bar(position = "fill")+
                  theme_light()+
                  labs(x = "Country", fill = "Trend nel lungo periodo", title = "Breeding populations")+
                  theme(legend.position = "bottom")+
                  coord_flip()
          
            # Wintering population

                    w_pop_trend_long <- Desc(w_pop$population_trend_long)

            wtl <- ggplot(data=w_pop, aes(x=country, fill=population_trend_long)) +
                  geom_bar(position = "fill")+
                  theme_light()+
                  labs(x = "", title = "Wintering populations")+
                  theme(legend.position = "none")+
                  coord_flip()

          # Combine the legends of the two graphs
          combined_legend <- cowplot::get_legend(btl)
          
          # Remove the legends from the individual graphs
          btl_no_legend <- btl + theme(legend.position = "none")
          wtl_no_legend <- wtl + theme(legend.position = "none")
          
          # Arrange the graphs side by side with the shared legend
          grid.arrange(
            arrangeGrob(btl_no_legend, wtl_no_legend, ncol = 2),
            combined_legend,
            heights = c(1, 0.2)
          )

          ggsave("trend_lungo_country.jpg", plot=arrangeGrob(arrangeGrob(btl_no_legend, wtl_no_legend, ncol = 2),combined_legend, heights = c(1, 0.2)), device = "jpeg", dpi = 300)


# 4.5 Combinazioni di trend

            # Breeding populations
                    ggplot(data=b_pop, aes(x=population_trend, fill=population_trend_long)) +
                  geom_bar(position = "fill")+
                        theme_light()+
                        labs(x = "Population trends in short period", Y = "Count", fill = "Population trend long period")


                ggsave("b_pop_trend_comp.jpg", plot=last_plot())
      

      
            # Wintering populations
                    ggplot(data=w_pop, aes(x=population_trend, fill=population_trend_long)) +
                  geom_bar(position = "fill")+
                        theme_light()+
                        labs(x = "Population trends in the short period", Y = "Count", fill = "Population trend long period")

                ggsave("w_pop_trend_comp.jpg", plot=last_plot())

                    w_pop_trend <- w_pop%>%
                              group_by(population_trend)%>%
                              count(population_trend_long)%>%
                              arrange(desc(n))
