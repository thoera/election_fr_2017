library("tidyverse")
library("sf")

setwd("~/Documents/R/elections_fr_2017")

# ----- 

# the shapefile of the french communes (in 2017)
communes_2017 <- read_sf("datasets/COMMUNES-2017/communes-20170112.shp")
str(communes_2017)

# -----

# load the dataset of the results in each commune

# read and clean the names of the variables
var_names <- readLines(
  "datasets/Presidentielle_2017_Resultats_Communes_Tour_1.csv",
  n = 1L, encoding = "UTF-8") %>%
  tolower() %>%
  chartr("éèà", "eea", .) %>%
  gsub("%", "per", .) %>%
  gsub("[^a-z0-9;]", "_", .) %>%
  strsplit(";") %>%
  unlist()

# 7 variables are repeated for each candidate (11 times)
rep_var <-  c("n_panneau", "sexe", "nom", "prenom", "voix",
              "per_voix_ins", "per_voix_exp")
rep_var <- lapply(seq_len(11), function(x) paste0(rep_var, "_", x))

var_names <- unlist(c(var_names[1:18], rep_var))

results_rd_1 <- data.table::fread(
  "datasets/Presidentielle_2017_Resultats_Communes_Tour_1.csv",
  sep = ";", col.names = var_names, skip = 1, encoding = "UTF-8")
str(results_rd_1)

# -----

# reshape the data

# reshape the data to long format
col_names <- paste0("nom_", seq_len(11))
col_per <- paste0("per_voix_exp_", seq_len(11))

results_long_rd_1 <- data.table::melt(results_rd_1,
                                      id.vars = c("code_du_departement",
                                                  "libelle_du_departement",
                                                  "code_de_la_commune",
                                                  "libelle_de_la_commune"),
                                      measure.vars = list(col_names, col_per),
                                      value.name = c("nom", "score"))

# clean the names of the candidates
results_long_rd_1 <- mutate(results_long_rd_1, nom = nom %>%
                              tolower() %>%
                              chartr("éèà", "eea", .) %>%
                              gsub("[ -]", "_", .))

# reshape the data to wide format (with a column of results for each candidate)
results_wide_rd_1 <- results_long_rd_1 %>%
  select(-variable) %>%
  spread(key = "nom", value = "score")

# -----

# join the shapefile of the communes and the results

# create a new variable "insee" which is the concatenation of
# "code_du_departement" and "code_de_la_commune" to join the two datasets
# "code_du_departement" should be coded on two characters and
# "code_de_la_commune" on three
results_wide_rd_1 <- results_wide_rd_1 %>%
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

# join
df_rd_1 <- left_join(select(communes_2017, insee, geometry),
                     results_wide_rd_1, by = c("insee" = "insee"))

# -----

# create different bins for each candidate
df_rd_1 <- df_rd_1 %>%
  mutate(macron_ = case_when(.$macron < 15 ~ 0,
                             .$macron >= 15 & .$macron < 20 ~ 1,
                             .$macron >= 20 & .$macron < 25 ~ 2,
                             .$macron >= 25 & .$macron < 30 ~ 3,
                             .$macron >= 30 ~ 4),
         le_pen_ = case_when(.$le_pen < 15 ~ 0,
                             .$le_pen >= 15 & .$le_pen < 20 ~ 1,
                             .$le_pen >= 20 & .$le_pen < 25 ~ 2,
                             .$le_pen >= 25 & .$le_pen < 30 ~ 3,
                             .$le_pen >= 30 ~ 4),
         fillon_ = case_when(.$fillon < 15 ~ 0,
                             .$fillon >= 15 & .$fillon < 20 ~ 1,
                             .$fillon >= 20 & .$fillon < 25 ~ 2,
                             .$fillon >= 25 & .$fillon < 30 ~ 3,
                             .$fillon >= 30 ~ 4),
         melenchon_ = case_when(.$melenchon < 15 ~ 0,
                                .$melenchon >= 15 & .$melenchon < 20 ~ 1,
                                .$melenchon >= 20 & .$melenchon < 25 ~ 2,
                                .$melenchon >= 25 & .$melenchon < 30 ~ 3,
                                .$melenchon >= 30 ~ 4),
         hamon_ = case_when(.$hamon < 3 ~ 0,
                            .$hamon >= 3 & .$hamon < 5 ~ 1,
                            .$hamon >= 5 & .$hamon < 7 ~ 2,
                            .$hamon >= 7 & .$hamon < 9 ~ 3,
                            .$hamon >= 9 ~ 4),
         dupont_aignan_ = case_when(.$dupont_aignan < 3 ~ 0,
                                    .$dupont_aignan >= 3 &
                                      .$dupont_aignan < 5 ~ 1,
                                    .$dupont_aignan >= 5 &
                                      .$dupont_aignan < 7 ~ 2,
                                    .$dupont_aignan >= 7 &
                                      .$dupont_aignan < 9 ~ 3,
                                    .$dupont_aignan >= 9 ~ 4),
         lassalle_ = case_when(.$lassalle < 1 ~ 0,
                               .$lassalle >= 1 & .$lassalle < 2 ~ 1,
                               .$lassalle >= 2 & .$lassalle < 3 ~ 2,
                               .$lassalle >= 3 & .$lassalle < 4 ~ 3,
                               .$lassalle >= 4 ~ 4),
         poutou_ = case_when(.$poutou < 0.8 ~ 0,
                             .$poutou >= 0.8 & .$poutou < 1.6 ~ 1,
                             .$poutou >= 1.6 & .$poutou < 2.4 ~ 2,
                             .$poutou >= 2.4 & .$poutou < 3.2 ~ 3,
                             .$poutou >= 3.2 ~ 4),
         asselineau_ = case_when(.$asselineau < 0.5 ~ 0,
                                 .$asselineau >= 0.5 & .$asselineau < 1 ~ 1,
                                 .$asselineau >= 1 & .$asselineau < 1.5 ~ 2,
                                 .$asselineau >= 1.5 & .$asselineau < 2 ~ 3,
                                 .$asselineau >= 2 ~ 4),
         arthaud_ = case_when(.$arthaud < 0.4 ~ 0,
                              .$arthaud >= 0.4 & .$arthaud < 0.8 ~ 1,
                              .$arthaud >= 0.8 & .$arthaud < 1.2 ~ 2,
                              .$arthaud >= 1.2 & .$arthaud < 1.6 ~ 3,
                              .$arthaud >= 1.6 ~ 4),
         cheminade_ = case_when(.$cheminade < 0.1 ~ 0,
                                .$cheminade >= 0.1 & .$cheminade < 0.2 ~ 1,
                                .$cheminade >= 0.2 & .$cheminade < 0.3 ~ 2,
                                .$cheminade >= 0.3 & .$cheminade < 0.4 ~ 3,
                                .$cheminade >= 0.4 ~ 4))

# -----

# keep only the metropolitan france
df_metrop_rd_1 <- df_rd_1 %>%
  filter(code_du_departement %in% c(paste0("0", seq_len(9)),
                                    seq(10, 95), "2A", "2B"))

# -----

# map the results

election_map <- function(data, candidate, colors, labels, title, subtitle) {
  # -----
  # create a map with the results of the elections in the french
  # metropolitan commune
  #
  # Args:
  #   data: the shapefile of the communes and the results
  #         in each of them (data frame)
  #   candidate: the candidate (string)
  #   colors: a vector of colors (vector)
  #   labels: a vector of labels (vector)
  #   title: the title of the plot (string)
  #   subtitle: the subtitle of the plot (string)
  #
  # Returns:
  #   a ggplot2 object
  # -----
  data %>%
    ggplot() +
    geom_sf(aes_string(fill = candidate), color = NA) +
    scale_fill_gradientn(colors = colors, labels = labels) +
    labs(title = paste0("\n", title), subtitle = subtitle) +
    theme_void() +
    theme(plot.title = element_text(size = 26, color = "grey40"),
          plot.subtitle = element_text(size = 20, color = "grey40"),
          legend.title = element_blank(),
          legend.text = element_text(size = 12, color = "grey40"))
}

# macron
macron <- election_map(data = df_metrop_rd_1,
                       candidate = "macron_",
                       colors = c("#fff9dc", "#ffebb2", "#ffd75c",
                                  "#fab900", "#f18005"),
                       labels = c("moins de 15 %", "15 - 20 %", "20 - 25 %",
                                  "25 - 30 %", "30 % et plus"),
                       title = "Emmanuel Macron",
                       subtitle = "En Marche !")
macron

# le_pen
le_pen <- election_map(data = df_metrop_rd_1,
                       candidate = "le_pen_",
                       colors = c("#ded6d4", "#bcaea9", "#83726d",
                                  "#524440", "#352f2c"),
                       labels = c("moins de 15 %", "15 - 20 %", "20 - 25 %",
                                  "25 - 30 %", "30 % et plus"),
                       title = "Marine Le Pen",
                       subtitle = "Front National")
le_pen

# fillon
fillon <- election_map(data = df_metrop_rd_1,
                       candidate = "fillon_",
                       colors = c("#e3f2fb", "#bae0f3", "#78bce0",
                                  "#0092ca", "#0474a2"),
                       labels = c("moins de 15 %", "15 - 20 %", "20 - 25 %",
                                  "25 - 30 %", "30 % et plus"),
                       title = "François Fillon",
                       subtitle = "Les Républicains")
fillon

# melenchon
melenchon <- election_map(data = df_metrop_rd_1,
                          candidate = "melenchon_",
                          colors = c("#ffede4", "#ffc8b0", "#fe9f7d",
                                     "#dc2a1b", "#a0190d"),
                          labels = c("moins de 15 %", "15 - 20 %", "20 - 25 %",
                                     "25 - 30 %", "30 % et plus"),
                          title = "Jean-Luc Mélenchon",
                          subtitle = "La France Insoumise")
melenchon

# hamon
hamon <- election_map(data = df_metrop_rd_1,
                      candidate = "hamon_",
                      colors = c("#feedf6", "#fbd3e7", "#f19ec7",
                                 "#e861a5", "#e864a7"),
                      labels = c("moins de 3 %", "3 - 5 %", "5 - 7 %",
                                 "7 - 9 %", "9 % et plus"),
                      title = "Benoît Hamon",
                      subtitle = "Parti Socialiste")
hamon

# dupont_aignan
dupont_aignan <- election_map(data = df_metrop_rd_1,
                              candidate = "dupont_aignan_",
                              colors = c("#e3f2fb", "#bae0f3", "#78bce0",
                                         "#0092ca", "#0474a2"),
                              labels = c("moins de 3 %", "3 - 5 %", "5 - 7 %",
                                         "7 - 9 %", "9 % et plus"),
                              title = "Nicolas Dupont-Aignan",
                              subtitle = "Debout la France")
dupont_aignan

# lassalle
lassalle <- election_map(data = df_metrop_rd_1,
                         candidate = "lassalle_",
                         colors = c("#e3f2fb", "#bae0f3", "#78bce0",
                                    "#0092ca", "#0474a2"),
                         labels = c("moins de 1 %", "1 - 2 %", "2 - 3 %",
                                    "3 - 4 %", "4 % et plus"),
                         title = "Jean Lassalle",
                         subtitle = "Résistons")
lassalle

# poutou
poutou <- election_map(data = df_metrop_rd_1,
                       candidate = "poutou_",
                       colors = c("#ffede4", "#ffc8b0", "#fe9f7d",
                                  "#dc2a1b", "#a0190d"),
                       labels = c("moins de 0.8 %", "0.8 - 1.6 %",
                                  "1.6 - 2.4 %", "2.4 - 3.2 %",
                                  "3.2 % et plus"),
                       title = "Philippe Poutou",
                       subtitle = "Nouveau Parti Anticapitaliste")
poutou

# asselineau
asselineau <- election_map(data = df_metrop_rd_1,
                           candidate = "asselineau_",
                           colors = c("#ded6d4", "#bcaea9", "#83726d",
                                      "#524440", "#352f2c"),
                           labels = c("moins de 0,5 %", "0,5 - 1 %",
                                      "1 - 1,5 %", "1,5 - 2 %", "2 % et plus"),
                           title = "François Asselineau",
                           subtitle = "Union Populaire Républicaine")
asselineau

# arthaud
arthaud <- election_map(data = df_metrop_rd_1,
                        candidate = "arthaud_",
                        colors = c("#ffede4", "#ffc8b0", "#fe9f7d",
                                   "#dc2a1b", "#a0190d"),
                        labels = c("moins de 0,4 %", "0,4 - 0,8 %",
                                   "0,8 - 1,2 %", "1,2 - 1,6 %",
                                   "1,6 % et plus"),
                        title = "Nathalie Arthaud",
                        subtitle = "Lutte Ouvrière")
arthaud

# cheminade
cheminade <- election_map(data = df_metrop_rd_1,
                          candidate = "cheminade_",
                          colors = c("#fff9dc", "#ffebb2", "#ffd75c",
                                     "#fab900", "#f18005"),
                          labels = c("moins de 0,1 %", "0,1 - 0,2 %",
                                     "0,2 - 0,3 %", "0,3 - 0,4 %",
                                     "0,4 % et plus"),
                          title = "Jacques Cheminade",
                          subtitle = "Solidarité et Progrès")
cheminade

# save the plots
candidates_rd_1 <- unique(results_long_rd_1[["nom"]])

lapply(candidates_rd_1, function(x) {
  ggsave(paste0("plots/", x,  ".png"), get(x),
         width = 12, height = 9, dpi = 150)
})
