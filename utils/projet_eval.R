library(readr)
library(dplyr)
library(gt)
library(gtExtras)
library(cartiflette)
library(stringr)
library(ggplot2)
library(scales)


df <- readr::read_csv(
  'https://www.data.gouv.fr/fr/datasets/r/182268fc-2103-4bcb-a850-6cf90b02a9eb'
)

# 1. Créer les variables:
# - code_insee
# - candidat

df <- df %>% mutate(
  code_commune = paste0(code_departement, code_commune),
  candidat = paste0(paste0(prenom, ' ', nom))
)

# Compléter la phrase suivante grâce à R ?
# (attention aux [votes non exprimés](https://www.conseil-constitutionnel.fr/referendum-traite-constitution-pour-l-europe/bulletins-blancs-et-nuls)
# et aux abstentions)

# En 2022, il y avait XX candidats à l'élection présidentielle

candidats <- df %>%
  filter(!is.na(prenom)) %>%
  summarise(candidats = n_distinct(candidat)) %>%
  as.numeric()


# Calculer les scores nationaux de chaque candidat
# (en voix et en % des scores, en retirant abstentions et votes non exprimés)
# https://www.gouvernement.fr/actualite/election-presidentielle-les-resultats-du-premier-tour


tableau_resultats <- df %>%
  filter(!is.na(prenom)) %>%
  group_by(candidat) %>%
  summarise(votes = sum(voix)) %>%
  mutate(score = votes/sum(votes)) %>%
  arrange(desc(score))


gt(tableau_resultats %>% mutate(color = "", score2 = 100*score)) %>%
  fmt_integer(columns = votes, locale = "fr") %>%
  fmt_percent(columns = score) %>%
  gt_plt_bar_pct(
    column = score2, scaled = TRUE
  ) %>%
  cols_label(
    votes = "Nombre votes (total)",
    score = "Score (% votes exprimés)",
    candidat = "Candidat.e",
    color = "",
    score2 = ""
    ) %>%
  data_color(columns = c('score'), target_columns = color, method = "numeric", palette = "PuOr") %>%
  tab_header(
    title = md("__Elections 🇫🇷__"),
    subtitle = md("Résultats du premier tour (📅 _10 avril 2022_)")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels()
  ) 


# Calculer pour les départements de la petite couronne,
# les scores de E. Macron, M. Le Pen. agréger les votes restants en autre
votes_idf_top3 <- df %>%
  filter(!is.na(prenom)) %>%
  filter(code_departement %in% c(75,92,93,94)) %>%
  mutate(candidat2 = if_else(
    nom %in% c("MACRON", "LE PEN"),
    candidat, "Autres candidats")) %>%
  group_by(code_departement, candidat2) %>%
  summarise(votes = sum(voix)) %>%
  group_by(code_departement) %>% 
  mutate(score = votes/sum(votes))

votes_idf_top3 <- votes_idf_top3 %>%
  group_by(code_departement) %>%
  summarize(list_data = list(score))

gt(votes_idf_top3) %>%
  gt_plt_bar_stack(list_data, width = 65,
                   labels = c("Autres candidats", "Macron", "Le Pen"),
                   palette= c("#bfbfbf", "#ff4343", "#0a1c2b"))


# Calculer le score de chaque candidat par département et rapporter à la moyenne
# nationale. Quels sont les départements où le vote Rassemblement national est
# sur-rerprésenté. Faire de même pour LFI
# 1. Faire un barplot ou lolliplot des 5 principales surreprésentations par candidat 2. Faire une carte

score_departements <- df %>%
  filter(!is.na(prenom)) %>%
  group_by(code_departement, candidat) %>%
  summarise(votes = sum(voix)) %>%
  mutate(score = votes/sum(votes)) 

score_departements <- score_departements %>% left_join(
  tableau_resultats,
  by = "candidat",
  suffix = c("_departement", "_national"))

score_departements <- score_departements %>%
  mutate(surreprésentation = 100*(score_departement/score_national - 1))

score_departements_top <- score_departements %>%
  arrange(candidat, desc(abs(surreprésentation))) %>%
  group_by(candidat) %>%
  filter(row_number() <= 5)


ggplot(score_departements_top) +
  geom_bar(aes(x = surreprésentation, y = factor(code_departement)), stat = "identity") +
  facet_wrap(~candidat, scales = "free")

# carte surreprésentation MLP

departement_borders <- download_vectorfile_url_all(
  crs = 4326,
  values = "metropole",
  borders="DEPARTEMENT",
  vectorfile_format="geojson",
  filter_by="FRANCE_ENTIERE",
  source="EXPRESS-COG-CARTO-TERRITOIRE",
  year=2022)

score_departements_mlp <- score_departements %>%
  filter(candidat == "Marine LE PEN")

departement_borders_mlp <- departement_borders %>%
  left_join(score_departements_mlp, by = c("INSEE_DEP" = "code_departement"))


ggplot(departement_borders_mlp) +
  geom_sf(aes(fill = surreprésentation)) +
  scale_fill_steps(
    n.breaks = 5,
    low = "white", high = "#394679",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  theme_void()  +
  labs(fill = "(% par rapport\nmoyenne nationale)")




score_departements_em <- score_departements %>%
  filter(candidat == "Emmanuel MACRON")

departement_borders_em <- departement_borders %>%
  left_join(score_departements_em, by = c("INSEE_DEP" = "code_departement"))


ggplot(departement_borders_em) +
  geom_sf(aes(fill = surreprésentation)) +
  scale_fill_steps(
    n.breaks = 5,
    low = "white", high = "#f1c237",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  theme_void()  +
  labs(fill = "(% par rapport\nmoyenne nationale)")


score_departements_mlp <- score_departements %>%
  filter(candidat == "Marine LE PEN")

departement_borders_mlp <- departement_borders %>%
  left_join(score_departements_mlp, by = c("INSEE_DEP" = "code_departement"))


ggplot(departement_borders_mlp) +
  geom_sf(aes(fill = surreprésentation)) +
  scale_fill_steps(
    n.breaks = 5,
    low = "white", high = "#394679",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  theme_void()  +
  labs(fill = "(% par rapport\nmoyenne nationale)")


score_departements_jlm <- score_departements %>%
  filter(candidat == "Jean-Luc MÉLENCHON")

departement_borders_jlm <- departement_borders %>%
  left_join(score_departements_jlm, by = c("INSEE_DEP" = "code_departement"))


ggplot(departement_borders_jlm) +
  geom_sf(aes(fill = surreprésentation)) +
  scale_fill_steps(
    n.breaks = 5,
    low = "white", high = "#cb452a",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  theme_void()  +
  labs(fill = "(% par rapport\nmoyenne nationale)")


# Calculer le taux d'absention par commune. Quelles sont les 10 principales
# communes absentitionnistes

df %>%
  mutate(abstention = if_else(nom == "abstentions", "Abstentions", "Exprimés")) %>%
  group_by(code_commune, libelle_commune, abstention) %>%
  summarise(votes = sum(voix)) %>%
  group_by(code_commune, libelle_commune) %>%
  mutate(taux_abstention = votes/sum(votes)) %>%
  filter(abstention == "Abstentions", !str_starts(code_commune, "(fr|98|97)")) %>%
  arrange(desc(taux_abstention))


df %>%
  mutate(abstention = if_else(nom == "abstentions", "Abstentions", "Exprimés")) %>%
  group_by(code_commune, libelle_commune, abstention) %>%
  summarise(votes = sum(voix)) %>%
  group_by(code_commune, libelle_commune) %>%
  mutate(taux_abstention = votes/sum(votes)) %>%
  filter(abstention == "Abstentions", !str_starts(code_commune, "(fr|98|97)"), votes > 2000) %>%
  arrange(desc(taux_abstention))


# Faire une carte du taux d'abstention par commune en corse

corse <- download_vectorfile_url_all(
  crs = 4326,
  values = c("2A","2B"),
  borders="COMMUNE",
  vectorfile_format="geojson",
  filter_by="DEPARTEMENT",
  source="EXPRESS-COG-CARTO-TERRITOIRE",
  year=2022)



abstention_corse <- df %>%
  mutate(abstention = if_else(nom == "abstentions", "Abstentions", "Exprimés")) %>%
  group_by(code_commune, libelle_commune, abstention) %>%
  summarise(votes = sum(voix)) %>%
  group_by(code_commune, libelle_commune) %>%
  mutate(taux_abstention = votes/sum(votes)) %>%
  filter(str_starts(code_commune,'(2A|2B)'), abstention == "Abstentions") %>%
  mutate(code_commune_propre = str_sub(code_commune, 3, -1))


corse_enrichie <- corse %>% inner_join(abstention_corse, by = c("INSEE_COM" = "code_commune"))



ggplot(corse_enrichie) +
  geom_sf(aes(fill = taux_abstention)) +
  scale_fill_fermenter(
    n.breaks = 5,
    palette = "RdPu",
    direction = 1,
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  theme_void() +
  labs(fill = "Taux d'abstention")




mf_map(
  corse_enrichie,
  col = "#CCCCCC",
  border = "white",
  lwd = 0.5
)
mf_map(
  corse_enrichie, var = "taux_abstention",
  type = "prop",
  col = "#c291bc",
  border = "#6b4266",
  lwd = 0.5,
  add = TRUE
)
mf_title("Population du Finistère", bg = "#6b4266")
mf_annotation(
  x = c(132040, 6826675),
  txt = "Finistère (29)",
  pos = "bottomleft",
  col_txt = "#6b4266",
  cex = 1.2,
  font = 2,
  halo = TRUE,
  s = 1.5
)

