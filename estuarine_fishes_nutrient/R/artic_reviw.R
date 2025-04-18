##############################################################
# Script for Literature Analysis and Mapping
# Manuscript: "The potential of estuarine fishes in supplying
# micronutrients to coastal and traditional populations in
# Northeast Brazil"
# Author: Fabricio Albuquerque
# Date: 02/2025
# GitHub Link: https://github.com/Fcl95
# Description: This script analyzes bibliographic records of
# studies focused on estuarine fish and micronutrients in 
# Northeast Brazil. It generates temporal and spatial visualizations.
##############################################################

#============================
# Load required packages
#============================

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(ggthemes)
library(tidyr)
library(janitor)
library(geobr)
library(ggspatial)
library(cowplot)

#============================
# Load and clean data
#============================


savedrecs <- read_excel("dados/savedrecs.xls")

data <- savedrecs |> 
  clean_names()  # Standardize column names

# Print column names for verification
print(names(data))

# Create a unique identifier for each article
data <- data %>% 
  mutate(article_id = row_number())


#============================
# Filter studies from Northeast Brazil
#============================

nordeste_estados <- c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe")

# Filter data only for Northeastern states
data_nordeste <- data %>% 
  filter(state %in% nordeste_estados) %>% 
  count(state, name = "num_estudos")

#============================
# Determine the time window covering 50% of the studies
#============================
df_years <- data %>%
  count(publication_year) %>%
  arrange(publication_year) %>%
  mutate(
    total = sum(n),
    acumulado = cumsum(n),
    perc_acumulado = acumulado / total
  )


# Select the time range representing 25% to 75% of studies
faixa_metade <- df_years %>%
  filter(perc_acumulado >= 0.25 & perc_acumulado <= 0.75)

# # Identify the year range
range(faixa_metade$publication_year)
#result - 2012 2021 - 

#============================
# Plot number of studies by publication year
#============================

ano_inicio <- min(faixa_metade$publication_year)
ano_fim    <- max(faixa_metade$publication_year)


data %>% 
  count(publication_year) %>% 
  ggplot(aes(x = publication_year, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_rect(aes(xmin = ano_inicio - 0.5, xmax = ano_fim + 0.5,
                ymin = 0, ymax = 12),
            fill = "#fc9272", alpha = 0.03, inherit.aes = FALSE)+
  scale_x_continuous(
    breaks = c(1994, 2000, 2005, 2010, 2015, 2020, 2025),
    expand = expansion(0.005,0.2)) +
  scale_y_continuous(
    breaks = seq(0, 12, 2),
    expand = expansion(0,0.1)) +
  theme_minimal() +
  labs(title = "", x = "Year of publication", y = "Number of studies") +
  theme(panel.grid.minor = element_blank())

#============================
# Count most cited species
#============================

data_species <- data %>% 
  select(article_id, species) %>% 
  separate_rows(species, sep = ", ")

# Count frequency of each species
data_species <- data_species %>% 
  count(species) %>% 
  arrange(desc(n))

#============================
# Count number of studies per state
#============================

data_states <- data %>% 
  select(article_id, state) %>% 
  separate_rows(state, sep = "/")


estudos_n <- data_states %>% 
  count(state) #%>% 
  #arrange(desc(n))


# Prepare data frame with state abbreviations and study counts
dados_estudos <- data.frame(
  state = c("AL", "BA",  "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
  num_studies = estudos_n$n
)


#============================
# Create maps: Brazil and Northeast
#============================

# Load Brazilian map and state shapes

brasil <- read_country(year = 2020)
estados <- read_state(year = 2020) %>% 
  filter(abbrev_state %in% dados_estudos$state)

# Compute centroids and coordinates
centroides <- estados %>%
  st_centroid() %>%
  mutate(longitude = st_coordinates(.)[,1],  # Extrair coordenadas X
         latitude = st_coordinates(.)[,2])   # Extrair coordenadas Y

# Merge with study count data
centroides <- centroides %>% left_join(dados_estudos, by = c("abbrev_state" = "state"))


# Mini-map of Brazil with Northeast highlighted
mapa_brasil <- ggplot() +
  geom_sf(data = brasil, fill = "gray90", color = "black") +  # Mapa do Brasil
  geom_sf(data = estados, fill = "red", color = "black") +  # Nordeste destacado
  annotate("text", x = -59, y = -5, 
           label = "Brazil", size = 4, fontface = "bold") +
  theme_linedraw() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Main map of Northeast region
(mapa_nordeste <- ggplot() +
  geom_sf(data = estados, fill = "lightgray", color = "black") +  # Desenha os estados
  geom_point(data = centroides, aes(x = longitude, y = latitude, size = num_studies), 
             color = "red", alpha = 0.6) + 
  geom_text(data = centroides, aes(x = longitude+.5, y = latitude-.5, 
                                   label = abbrev_state), 
            size = 4, fontface = "bold", vjust = -1, color = "black") +
  # Adiciona círculos proporcionais ao número de estudos
  scale_size(range = c(3, 10), name = "No. of Studies") +  # Ajusta tamanho dos círculos
  annotation_scale(location = "bl", width_hint = 0.25) +  # Escala no canto inferior esquerdo
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering()) +  # Seta do Norte
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA))#+
  # labs(title = "Número de Estudos por Estado no Nordeste do Brasil",
  #      subtitle = "Posição baseada nos centróides estaduais",
  #      caption = "Fonte: Base de dados fictícia")
)

# Combine both maps
final_plot <- ggdraw() +
  draw_plot(mapa_nordeste) +
  draw_plot(mapa_brasil, x = 0.59, y = 0.75, width = 0.2, height = 0.2)  # Mini mapa no canto inferior direito

# Display final map
final_plot

