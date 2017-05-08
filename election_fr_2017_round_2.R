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
  "datasets/Presidentielle_2017_Resultats_Communes_Tour_2.csv",
  n = 1L, encoding = "UTF-8") %>%
  tolower() %>%
  chartr("éèà", "eea", .) %>%
  gsub("%", "per", .) %>%
  gsub("[^a-z0-9;]", "_", .) %>%
  strsplit(";") %>%
  unlist()

# 6 variables in common for the two candidates
rep_var <-  c("sexe", "nom", "prenom", "voix", "per_voix_ins", "per_voix_exp")
rep_var <- lapply(seq_len(2), function(x) paste0(rep_var, "_", x))

var_names <- unlist(c(var_names[1:18], rep_var))

results_rd_2 <- data.table::fread(
  "datasets/Presidentielle_2017_Resultats_Communes_Tour_2.csv",
  sep = ";", col.names = var_names, skip = 1, encoding = "UTF-8")
str(results_rd_2)

# -----

# per_voix_exp_1 are the results for macron and per_voix_exp_2 for le pen
lapply(c("nom_1", "nom_2"), function(x) {
  results_rd_2[, .N, by = .(name = get(x))]
})

# select the columns of interest
results_rd_2 <- results_rd_2 %>%
  select(code_du_departement, libelle_du_departement,
         code_de_la_commune, libelle_de_la_commune,
         per_voix_exp_1, per_voix_exp_2) %>%
  rename(macron = per_voix_exp_1, le_pen = per_voix_exp_2)

# -----

# join the shapefile of the communes and the results

# create a new variable "insee" which is the concatenation of
# "code_du_departement" and "code_de_la_commune" to join the two datasets
# "code_du_departement" should be coded on two characters and
# "code_de_la_commune" on three
results_rd_2 <- results_rd_2 %>%
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
df_rd_2 <- left_join(select(communes_2017, insee, geometry),
                     results_rd_2, by = c("insee" = "insee"))

# -----

# add a flag to know who won in each commune, scale the results of macron 
# and le pen to non-overlapping ranges and create bins 
df_rd_2_ <- df_rd_2 %>%
  mutate(flag = case_when(.$macron > 50 ~ 1,  # win for macron
                          .$macron < 50 ~ 2,  # win for le pen
                          TRUE ~ 0),          # draw
         score = pmax(macron, le_pen) + 100 * flag) %>%
  mutate(score_ = case_when(.$score > 150 & .$score < 155 ~ 1,
                            .$score >= 155 & .$score < 160 ~ 2,
                            .$score >= 160 & .$score < 165 ~ 3,
                            .$score >= 165 & .$score < 170 ~ 4,
                            .$score >= 170 & .$score <= 200 ~ 5,
                            .$score > 250 & .$score < 255 ~ 6,
                            .$score >= 255 & .$score < 260 ~ 7,
                            .$score >= 260 & .$score < 265 ~ 8,
                            .$score >= 265 & .$score < 270 ~ 9,
                            .$score >= 270 ~ 10,
                            TRUE ~ 0))

# -----

# keep only the metropolitan france
df_metrop_rd_2 <- df_rd_2_ %>%
  filter(code_du_departement %in% c(paste0("0", seq_len(9)),
                                    seq(10, 95), "2A", "2B"))

# -----

# map the results

labels <- c("50 - 55 %", "55 - 60 %", "60 - 65 %", "65 - 70 %", "70 % et plus")

# only the communes with 50+ % for macron
macron_rd_2 <- df_metrop_rd_2 %>%
  filter(score_ > 0 & score_ < 6) %>%
  ggplot() +
  geom_sf(aes(fill = score_), color = NA) +
  scale_fill_gradientn(colors = c("#fff9dc", "#ffebb2", "#ffd75c",
                                  "#fab900", "#f18005"),
                       labels = labels) +
  labs(title = "\nEmmanuel Macron", subtitle = "En Marche !") +
  theme_void() +
  theme(plot.title = element_text(size = 26, color = "grey40"),
        plot.subtitle = element_text(size = 20, color = "grey40"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "grey40"))
macron_rd_2

# save the plot
ggsave("plots/macron_rd_2.png", macron_rd_2, width = 12, height = 9, dpi = 150)

# only the communes with 50+ % for le_pen
le_pen_rd_2 <- df_metrop_rd_2 %>%
  filter(score_ > 5) %>%
  ggplot() +
  geom_sf(aes(fill = score_), color = NA) +
  scale_fill_gradientn(colors = c("#ded6d4", "#bcaea9", "#83726d",
                                  "#524440", "#352f2c"),
                       labels = labels) +
  labs(title = "\nMarine Le Pen", subtitle = "Front National") +
  theme_void() +
  theme(plot.title = element_text(size = 26, color = "grey40"),
        plot.subtitle = element_text(size = 20, color = "grey40"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "grey40"))
le_pen_rd_2

# save the plot
ggsave("plots/le_pen_rd_2.png", le_pen_rd_2, width = 12, height = 9, dpi = 150)

colors <- c("white",
            "#fff9dc", "#ffebb2", "#ffd75c", "#fab900", "#f18005",
            "#ded6d4", "#bcaea9", "#83726d", "#524440", "#352f2c")
gradientn <- seq(0, 10)

# all the communes
map_rd_2 <- df_metrop_rd_2 %>%
  ggplot() +
  geom_sf(aes(fill = score_), color = NA) +
  scale_fill_gradientn(colors = colors,
                       labels = labels,
                       values = scales::rescale(gradientn)) +
  labs(title = "\nEmmanuel Macron vs. Marine Le Pen") +
  theme_void() +
  theme(plot.title = element_text(size = 26, color = "grey40"),
        plot.subtitle = element_text(size = 20, color = "grey40"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "grey40"))
map_rd_2

# save the plot
ggsave("plots/macron_le_pen.png", map_rd_2, width = 12, height = 9, dpi = 150)
