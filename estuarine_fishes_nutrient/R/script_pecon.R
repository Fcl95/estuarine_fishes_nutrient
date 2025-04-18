#-------------------------------------------
# R Script for Data Analysis and visualization
# Manuscript: “The potential of estuarine fishes in supplying micronutrients 
# to coastal and traditional populations in Northeast Brazil”
# This script performs data cleaning, preparation, statistical analysis,
# and visualizations for micronutrient data from fish species 
# and other animal proteins (beef, chicken, pork, sausage).
# The analysis includes:
# 1. Data cleaning and manipulation
# 2. Statistical tests (ANOVA and post hoc tests)
# 3. Visualizations (plots for Calcium, Iron, Zinc, Selenium, Omega-3)
# 4. PCA (Principal Component Analysis)
# Author: Fabricio Albuquerque
# GitHub Link: https://github.com/Fcl95
# Date: 02/2025
#-------------------------------------------

# Load required libraries

library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)
library(cowplot)

#----------------------------------------------------------
# 1. Load and clean fish nutrient dataset
#----------------------------------------------------------

fish_nutri <- read_csv(
  "dados/NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_BRAZIL.csv", 
  col_types = cols(`Omega3 L95` = col_double(), 
                   `Omega3 U95` = col_double())) |> 
  select(-c(2, 4, 5)) |> 
  clean_names() 

#----------------------------------------------------------
# 2. Load and transform USDA calcium, iron, and zinc data
#----------------------------------------------------------

usda_nutri_cfz <- read_xlsx("dados/usda_anml_ca_fe_zn.xlsx") 


nutri_cfz <- usda_nutri_cfz %>%
  group_by(nutrient) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-c(nutrient, id)) |> 
  group_by(nutrient) |> 
  pivot_wider(id_cols = name,
              names_from = nutrient,
              values_from = value) |> 
  rename("scientific_name" = name) |> 
  mutate(catg = c(rep("Beef",32), rep("Chicken",16), 
                  rep("Pork",16), rep("Sausage", 13)))

# Format fish data to match USDA structure
fish_nutri_cfz <- fish_nutri |> 
  select(scientific_name, contains("100g")) |> 
  select(-protein_g_100g) |> 
  select(1,4,2,3,7,5,6) |> 
  mutate(catg = "Fish") |> 
  select(1, 3:5, 8) 

names(fish_nutri_cfz) <- colnames(nutri_cfz)

# Merge fish and USDA data
nutri_spp_cfz <- full_join(fish_nutri_cfz, nutri_cfz) |> 
  clean_names() 


#----------------------------------------------------------
# 3. Load and transform USDA selenium and omega-3 data
#----------------------------------------------------------

usda_nutri_som <- read_xlsx("dados/usda_anml_se_omg.xlsx") 

nutri_som <- usda_nutri_som %>%
  group_by(nutrient) %>%
  mutate(id = row_number()) %>%
  pivot_longer(-c(nutrient, id)) |> 
  group_by(nutrient) |> 
  pivot_wider(id_cols = name,
              names_from = nutrient,
              values_from = value) |> 
  rename("scientific_name" = name) |> 
  mutate(catg = c(rep("Beef",6), rep("Chicken",8), 
                  rep("Pork",5), rep("Sausage", 6)))

# Format fish data for selenium and omega-3
fish_nutri_som <- fish_nutri |> 
  select(scientific_name, contains("100g")) |> 
  select(-protein_g_100g) |> 
  select(1,4,2,3,7,5,6) |> 
  mutate(catg = "Fish") |> 
  select(1, 6, 2, 8) 

names(fish_nutri_som) <- colnames(nutri_som)

# Merge fish and USDA data
nutri_spp_som <- full_join(fish_nutri_som, nutri_som) |> 
  clean_names() 

#----------------------------------------------------------
# 4. Permutational ANOVA tests (PERMANOVA) and post hoc
#----------------------------------------------------------
library(lmPerm)
library(rcompanion)

# --- Omega-3 ---
omg_aovp <- aovp(omega_3 ~ catg, nutri_spp_som)
summary(omg_aovp)

# post hoc test
posthoc_omg_aovp <- pairwisePermutationTest(omega_3 ~ catg, nutri_spp_som)
posthoc_omg_aovp

# --- Selenium ---
nutri_spp_sln  <- nutri_spp_som |> 
  filter(!is.na(selenium))

sln_aovp <- aovp(selenium ~ catg, nutri_spp_sln)
summary(sln_aovp)

# post hoc test
posthoc_sln_aovp <- pairwisePermutationTest(selenium ~ catg, nutri_spp_sln)
posthoc_sln_aovp

#----------calcium, iron, zinc-------------------
#-----Remember: Change nutrient name-----------

clc_aovp <- aovp(zinc ~ catg, nutri_spp_cfz)
summary(clc_aovp)

# post hoc test
posthoc_clc_aovp <- pairwisePermutationTest(zinc ~ catg, nutri_spp_cfz)
posthoc_clc_aovp


#----------------------------------------------------------
# 5. Summary statistics and nutrient plots
#----------------------------------------------------------

nutri_spp_som |> group_by(catg) |> count()

library(Rmisc)

nutri_spp_cfz$catg <-  fct_relevel(nutri_spp_cfz$catg, 
                                   c("Fish"), after = Inf)

# --- Calcium ---
clcm <- summarySE(nutri_spp_cfz, measurevar = "calcium", 
                  groupvars = "catg")

p_clc <- clcm |> 
  ggplot(aes(x = catg, y = calcium, colour = catg)) +
  geom_errorbar(aes(ymin=calcium - ci,
                    ymax=calcium + ci), 
                width=.2, colour="black",size = 1) +
  geom_point(size = 3.5, shape = 15) +
  geom_jitter(data = nutri_spp_cfz, aes(x = catg, y = calcium),
              alpha = 0.5, width = 0.25, size = 2) +
  scale_color_manual(guide = guide_legend(),
                     values = rev(c("#4FAE4A", "gray30",  "gray40", "gray50", "gray60")),
                     drop = F) +
  scale_y_continuous(limits = c(0, 260),
                     breaks = seq(0, 260, 50)) +
  labs(x = "", y = "", title = "Calcium, mg") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14, hjust = 0.5),
        text=element_text(size=16, family="Roboto")) +
  # geom_point(data = nutri_spp, aes(x = catg, y = calcium),
  #            size = 2, alpha = 0.5) +
  coord_flip()

# ---- Iron ---
iron_ntr <- summarySE(nutri_spp_cfz, measurevar = "iron", 
                      groupvars = "catg")


p_irn <- iron_ntr  |> 
  ggplot(aes(x = catg, y = iron, colour = catg)) +
  geom_errorbar(aes(ymin=iron-ci,
                    ymax=iron+ci), 
                width=.2, colour="black",size = 1) +
  geom_point(size = 3.5, shape = 15) +
  geom_jitter(data = nutri_spp_cfz, aes(x = catg, y = iron),
              alpha = 0.5, width = 0.25, size = 2) +
  scale_color_manual(guide = guide_legend(),
                     values = rev(c("#E11B1C", "gray30",  "gray40", "gray50", "gray60")),
                     drop = F) +
  scale_y_continuous(limits = c(0, 5),
                     breaks = seq(0, 5, 1)) +
  labs(x = "", y = "", title = "Iron, mg") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14, hjust = 0.5),
        text=element_text(size=16, family="Roboto")) +
  coord_flip()

# --- Zinc ---
zinc_ntr <- summarySE(nutri_spp_cfz, measurevar = "zinc", 
                      groupvars = "catg")

p_znc <- zinc_ntr  |> 
  ggplot(aes(x = catg, y = zinc, colour = catg)) +
  geom_errorbar(aes(ymin=zinc - ci,
                    ymax=zinc + ci), 
                width=.2, colour="black",size = 1) +
  geom_point(size = 3.5, shape = 15) +
  geom_jitter(data = nutri_spp_cfz, aes(x = catg, y = zinc),
              alpha = 0.5, width = 0.25, size = 2) +
  scale_color_manual(guide = guide_legend(),
                     values = rev(c("#954E9A", "gray30",  "gray40", "gray50", "gray60")),
                     drop = F) +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 2)) +
  labs(x = "", y = "", title = "Zinc, mg") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text=element_text(size=16, family="Roboto")) +
  coord_flip()

# --- Selenium ---
selenium_ntr <- summarySE(nutri_spp_som, measurevar = "selenium", 
                          groupvars = "catg", na.rm = T)

p_sln <- selenium_ntr  |> 
  ggplot(aes(x = catg, y = selenium, colour = catg)) +
  geom_errorbar(aes(ymin=selenium - ci,
                    ymax=selenium + ci), 
                width=.2, colour="black",size = 1) +
  geom_point(size = 3.5, shape = 15) +
  geom_jitter(data = nutri_spp_som, aes(x = catg, y = selenium),
              alpha = 0.5, width = 0.25, size = 2) +
  scale_color_manual(guide = guide_legend(),
                     values = rev(c("#F07C15", "gray30",  "gray40", "gray50", "gray60")),
                     drop = F) +
  labs(x = "", y = "", title = "Selenium, µg") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.y = element_blank(),
        text=element_text(size=16, family="Roboto"),
        axis.ticks.y = element_blank()) +
  coord_flip()

# --- Omega-3 ---
omega_3_ntr <- summarySE(nutri_spp_som, 
                         measurevar = "omega_3",
                         groupvars = "catg")

p_omg <- omega_3_ntr  |> 
  ggplot(aes(x = catg, y = omega_3, colour = catg)) +
  geom_errorbar(aes(ymin= omega_3 - ci,
                    ymax= omega_3 + ci), 
                width=.2, colour="black",size = 1) +
  geom_point(size = 3.5, shape = 15) +
  geom_jitter(data = nutri_spp_som, aes(x = catg, y = omega_3),
              alpha = 0.5, width = 0.25, size = 2) +
  scale_color_manual(guide = guide_legend(),
                     values = rev(c("#367EB8", "gray30",  "gray40", "gray50", "gray60")),
                     drop = F) +
  scale_y_continuous(limits = c(0, 0.4),
                     breaks = seq(0, 1, 0.2)) +
  labs(x = "", y = "", title = "Omega-3, g") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text=element_text(size=16, family="Roboto")) +
  coord_flip()

# --- Combine plots ---
(p_clc | p_omg) / (p_irn | p_znc | p_sln ) + 
  plot_annotation(tag_levels = "A", tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 14))

#----------------------------------------------------------
# 6. Principal Component Analysis (PCA)
#----------------------------------------------------------

# Combine PCA data
som_pca <- nutri_som |> 
  clean_names() |> 
  filter(catg != "Sausage") |> 
  group_by(catg) |> 
  summarise_at(
    c("selenium", "omega_3"), ~ round(mean(.x, na.rm = TRUE), 3))|> 
  mutate(scientific_name = catg)

cfz_pca <- nutri_cfz |> 
  clean_names() |> 
  filter(catg != "Sausage") |> 
  group_by(catg) |> 
  summarise_at(
    vars(calcium:zinc), ~  round(mean(.x, na.rm = TRUE), 3)) |> 
  mutate(scientific_name = catg)

ssp_som <- fish_nutri_som |> 
  clean_names() |> 
  bind_rows(som_pca)

ssp_cfz <- fish_nutri_cfz |> 
  clean_names() |> 
  bind_rows(cfz_pca)

ssp_pca <- left_join(ssp_cfz, ssp_som) |> 
  select(-catg, everything(), catg)

ssp_pca$catg <-  fct_relevel(ssp_pca$catg, "Fish")

levels(ssp_pca$catg)

abbreviated_species <- gsub("^(\\w)\\w+\\s", "\\1. ", ssp_pca$scientific_name)
print(abbreviated_species)
ssp_pca$abbrv_name <- abbreviated_species 

#---------------------------------------------
# 7. PCA analysis
#---------------------------------------------

library(FactoMineR)
library(factoextra)
library(ggpubr)

ssp_pca_var <- ssp_pca[, 2:6]
ssp_pca_var <-as.data.frame(ssp_pca_var)

row.names(ssp_pca_var) <- ssp_pca$abbrv_name 


pca.p <- PCA(X = ssp_pca_var, scale.unit = TRUE, graph = FALSE)

print(row.names(ssp_pca_var))
pca.p$eig


#--- Contribution of nutrients to PCA axis ---

res.desc <- dimdesc(pca.p, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
res.pca.dim.1 <- as.data.frame(res.desc$Dim.1)
res.pca.dim.1$load <- row.names(res.pca.dim.1)

# Description of dimension 2
res.desc$Dim.2
res.pca.dim.2 <- as.data.frame(res.desc$Dim.2)
res.pca.dim.2$load <- row.names(res.pca.dim.2)

res.pca.dim.2 <- res.pca.dim.2 |> 
  mutate(flag = ifelse(quanti.correlation > 0, TRUE, FALSE))
  
#---Species conribuitions to PCA---

fviz_contrib(pca.p, choice = "ind", axes = 2)

# PCA biplot
p1 <- fviz_pca_biplot(X = pca.p,
                      repel = TRUE,
                      #geom.ind = "point", 
                      fill.ind = ssp_pca$catg, 
                      col.ind = "black",
                      alpha.ind = 0.7,
                      pointshape = 21, 
                      pointsize = 6,
                      palette = c("#3182bd", "#a50f15", "#fecc5c", "#fb6a4a"),
                      col.var = "#636363",
                      arrowsize = 0.9,
                      labelsize = 5,
                      invisible = "quali",
                      title = NULL,
                      legend.title = "Animal\nprotein") +
  labs(title = "A) PCA plot axis 1 and 2",
       x = "PC1 (46.4%)", y = "PC2 (27.6%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "None",
        panel.grid.minor.y = element_blank())


#--- PCA axis 1 ---
p2 <- ggplot(res.pca.dim.1, aes(x = quanti.correlation, y =load)) +
  geom_segment(aes(x = 0, y = load, xend = quanti.correlation, yend = load),
               arrow = arrow(length = unit(1.2,"cm")), lwd=6,
               color = "#2b8cbe", show.legend = F) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0, 0.5, 1),
                     expand = c(0, 0.01)) +
  scale_y_discrete(expand = c(0.15, 0)) +
  geom_text(aes(x = quanti.correlation + 0.06,
                y = load,
                label = load)
            ,size = 5) +
  theme_bw(base_size = 12) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(title = "B) Nutrient contribution to axis 1 (46.4%)",
       x = "PCA loading correlation", y = " ")

#--- PCA axis 2 ---
p3 <- ggplot(res.pca.dim.2, aes(x = quanti.correlation, y =load, color = flag)) +
  geom_segment(aes(x = 0, y = load, xend = quanti.correlation, yend = load),
               arrow = arrow(length = unit(1.2,"cm")), lwd=6,
               show.legend = F) +
  scale_colour_manual(values = c("#de2d26", "#2b8cbe"))+
  scale_x_continuous(limits = c(-1, 1),
                     breaks = c(-1, -0.5, 0, 0.5, 1),
                     expand = c(0, 0.01)) +
  scale_y_discrete(expand = c(0.2, 0)) +
  geom_text(aes(x = quanti.correlation + 0.12,
                y = load,
                label = load),
            size = 5, col = "black") +
  theme_bw(base_size = 12) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(title = "C) Nutrient contribution to axis 2 (27.6%)",
       x = "PCA loading correlation", y = " ")
 

# --- Combine plots ---
design <- "AAAA
           AAAA
           AAAA
           AAAA
           BBCC
           BBCC"

p1 + p2+ p3 +  plot_layout(design = design) 
