library("tidyverse")
library("sf")

setwd("~/Documents/R/elections_fr_2017")

# ----- 

# the shapefile of the french communes (in 2012)
communes_2012 <- read_sf("datasets/COMMUNES-LAMB93-2012/COMMUNE.SHP")
str(communes_2012)

# ----- 

# load the dataset of the results in each commune

# read and clean the names of the variables
var_names <- readLines(
  "datasets/Presidentielle_2012_Resultats_Communes_Tour_1.csv",
  n = 1L, encoding = "UTF-8") %>%
  tolower() %>%
  chartr("éèà", "eea", .) %>%
  gsub("%", "per", .) %>%
  gsub("[^a-z0-9;]", "_", .) %>%
  strsplit(";") %>%
  unlist()

# 6 variables are repeated for each candidate (10 times)
rep_var <-  c("sexe", "nom", "prenom", "voix", "per_voix_ins", "per_voix_exp")
rep_var <- lapply(seq_len(10), function(x) paste0(rep_var, "_", x))

var_names <- unlist(c(var_names[1:15], rep_var))

results_2012 <- data.table::fread(
  "datasets/Presidentielle_2012_Resultats_Communes_Tour_1.csv",
  sep = ";", col.names = var_names, skip = 1, encoding = "UTF-8")
str(results_2012)

# -----

# keep only the results of melenchon
melenchon_2012 <- results_2012 %>%
  select(code_du_departement, libelle_du_departement, 
         code_de_la_commune, libelle_de_la_commune,
         per_voix_exp_4) %>%
  rename(melenchon = per_voix_exp_4)

# -----

# join the shapefile of the communes and the results

# create a new variable "insee" which is the concatenation of
# "code_du_departement" and "code_de_la_commune" to join the two datasets
# "code_du_departement" should be coded on two characters and
# "code_de_la_commune" on three
melenchon_2012 <- melenchon_2012 %>%
  mutate(code_de_la_commune = as.character(code_de_la_commune)) %>%
  mutate(code_de_la_commune = case_when(nchar(.$code_de_la_commune) == 1 ~
                                          paste0("00", .$code_de_la_commune),
                                        nchar(.$code_de_la_commune) == 2 ~
                                          paste0("0", .$code_de_la_commune),
                                        TRUE ~ .$code_de_la_commune),
         code_du_departement = case_when(nchar(.$code_du_departement) == 1 ~
                                           paste0("0", .$code_du_departement),
                                         TRUE ~ .$code_du_departement)) %>%
  unite(insee, code_du_departement, code_de_la_commune,
        sep = "", remove = FALSE)

# a special case: cities with districts (Paris, Lyon, Marseille)
districts <- function(city) {
  # -----
  # add one or more rows of data for cities which have districts
  #
  # Args:
  #   city: a city (string)
  #
  # Returns:
  #   a data frame
  # -----
  city_ <- filter(melenchon_2012, libelle_de_la_commune == city)
  arr_ <- filter(communes_2012,
                 grepl(paste0(toupper(city), "-+[0-9]"), NOM_COMM))
  
  melenchon_2012 %>%
    filter(libelle_de_la_commune != city) %>%
    add_row(insee = arr_[["INSEE_COM"]],
            code_du_departement = arr_[["CODE_DEPT"]],
            libelle_du_departement = city_[["libelle_du_departement"]],
            code_de_la_commune = arr_[["CODE_COMM"]],
            libelle_de_la_commune = city_[["libelle_de_la_commune"]],
            melenchon = city_[["melenchon"]])
}

for (city in c("Marseille", "Lyon", "Paris")) {
  melenchon_2012 <- districts(city)
}

# join
melenchon_2012 <- left_join(select(communes_2012, INSEE_COM, geometry),
                            melenchon_2012, by = c("INSEE_COM" = "insee"))

# -----

# create bins and keep only the metropolitan france
melenchon_2012 <- melenchon_2012 %>%
  mutate(melenchon_ = case_when(.$melenchon < 15 ~ 0,
                                .$melenchon >= 15 & .$melenchon < 20 ~ 1,
                                .$melenchon >= 20 & .$melenchon < 25 ~ 2,
                                .$melenchon >= 25 & .$melenchon < 30 ~ 3,
                                .$melenchon >= 30 ~ 4)) %>%
  filter(code_du_departement %in% c(paste0("0", seq_len(9)),
                                    seq(10, 95), "2A", "2B"))

# -----

# map the results in 2012
melenchon <- ggplot(melenchon_2012) +
  geom_sf(aes(fill = melenchon_), color = NA) +
  scale_fill_gradientn(colors = c("#ffede4", "#ffc8b0", "#fe9f7d",
                                  "#dc2a1b", "#a0190d"),
                       labels = c("moins de 15 %", "15 - 20 %", "20 - 25 %",
                                  "25 - 30 %", "30 % et plus")) +
  labs(title = "\nJean-Luc Mélenchon", subtitle = "Le Front de Gauche") +
  theme_void() +
  theme(plot.title = element_text(size = 26, color = "grey40"),
        plot.subtitle = element_text(size = 20, color = "grey40"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "grey40"))
melenchon

# save the plot
ggsave("plots/melenchon_2012.png", melenchon, width = 12, height = 9, dpi = 150)
