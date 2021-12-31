### Eurpean birds ###

setwd("/Users/federicotossani/Desktop/R_datacamp/bird_pop_08-12/Art12_MS_2015_csv/") #for macOS
require(ggplot2)    #install.packages('ggplot2')
require(dplyr)      #install.packages('dplyr')

birds <- read.csv("data_birds.csv", stringsAsFactors = TRUE)       #stringsAsFactors = TRUE per mantenere invariata la tipologia di dato in una colonna (testo, numero ec)
birds <- as_tibble(birds)                                           # as_tibble() permette una visualizzazione più chiara del dataset
#str(birds)                                                         #str() ci fornisce la struttura del dataset è utile per capire con che variabili abbiamo a che fare

it_birds <- birds%>%
                select(country, speciesname, population_maximum_size)%>%  #select() mi permette di selezionare solo le variabili che mi interessano
                filter(country == "IT")%>%                             #filter() filtra le mie osservazioni in base al valore di una o più variabili
                arrange(population_maximum_size)                    #arrange() ordina una o più variabili in modo crescente (di default) o decrescente (con desc())
                mutate()    #mutate() per aggiungere o modificare le variabili del dataset

it_birds
tail(it_birds)  #tail() per vedere le ultime osservazioni
head(it_birds)  #head() per vedere le prime osservazioni


