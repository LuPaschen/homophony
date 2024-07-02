# Models and figures for Paschen (20xx): Homophones

library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(randomForest)
library(ggplot2)
library(lingtypology)

# Loading workspace
setwd("C:/ling/00_projects/Homophony")
load(".RData")
### homophone_data <- readRDS("01_Preprocessing_Output.rds")



### 7) Random Forest models

# Prepare empty dataframe for results
results_all_purity <- data.frame(lang = character(), morph = character(), factor = character(), contribution = numeric(), r_sq = numeric())

# List of languages
languages <- unique(homophone_data$lang)

# Factors for the Random Forest models
factors <- c("gl", "position_in_ipu", "wd_size", "speech_rate", "speaker", "wd_freq", "segmental_context")
predictor <- "mb_duration"

# Loop to run Random Forests for each homophone set, per language
set.seed(44)
for(l in languages) {

  morphs_raw <- unique(homophone_data$mb_raw[homophone_data$lang == l])
  for(m in morphs_raw) {
    print(paste(l,m)) # debugging
    data_subset <- homophone_data %>% filter(lang == l, mb_raw == m) %>% select(c(all_of(factors)), all_of(predictor))
    if(nrow(data_subset) > 0){
    
      # Run model
      rf <- randomForest(mb_duration ~ ., ntree = 500, mtry = 3, data = data_subset, importance = TRUE)
      
      # Model statistics
      r_sq = max(rf$rsq)
      contributions_purity <- rf$importance[, "IncNodePurity"] / sum(rf$importance[, "IncNodePurity"])
      for (f in factors) {
        results_all_purity <- rbind(results_all_purity, data.frame(lang = l, morph = m, factor = f, contribution = contributions_purity[[f]], r_sq = r_sq))

      }
    }
  }
}

# Rename columns for better readability
results_all_purity <- results_all_purity %>%
  mutate(factor = case_when(
    factor == "gl"  ~ "morph",
    factor == "position_in_ipu" ~ "position",
    factor == "wd_freq" ~ "word_frequency",
    factor == "wd_size" ~ "word_size",
    TRUE ~ factor
  ))

# Remove models below a 25% performance threshold
results_all_purity_unfiltered <- results_all_purity
results_all_purity <- filter(results_all_purity_unfiltered, r_sq > 0.25)



### 8) Figures

# Subsets and re-organized dataframes for sub-studies
results_morph_purity <- results_all_purity %>%
  filter(factor == "morph")
s_data <- results_all_purity %>%
  mutate(s = ifelse(str_detect(morph, "s|z"), "with_alveolar_sibilant", "no_alveolar_sibilant"),
         source = "Alveolar sibilant")  %>%
  filter(factor == "morph") 
remember_homogeneity <- homophone_data %>% select(lang, mb_raw, homogeneity_mt) %>% distinct() %>% rename(morph = mb_raw)
mt_data <- left_join(results_all_purity, remember_homogeneity, by = c("lang", "morph")) %>% 
  filter(factor == "morph") %>% 
  mutate(source = "Homogeneity")
proportion_homo <- mean(mt_data$homogeneity_mt == "Homogeneous")
proportion_s <- mean(s_data$s == "with_alveolar_sibilant")

# Figure: Factor x Contribution (over all languages)
contr_factor <- ggplot(results_all_purity, aes(x = reorder(factor, contribution, FUN = mean), y = contribution)) +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "purple", size = 9) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, color = "purple") +
  #  geom_hline(yintercept = 0.142, linetype = "dashed", color = "black") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(ylim = c(0, 0.4)) +
  labs(x = "Factor", y = "Importance") +
  theme(text = element_text(size = 32))
ggsave(filename = "contr_factor_37_44.png", plot = contr_factor, width = 10, height = 8, units = "in", dpi = 300)

# Figure: Language x Contribution of morph
contrmorph_language <- ggplot(results_morph_purity, aes(x = reorder(lang, contribution, FUN = mean), y = contribution)) +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "purple", size = 9) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, color = "purple") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(ylim = c(0, 0.3)) +
  labs(x = "Language", y = "Importance of morph") +
  theme(text = element_text(size = 32))
ggsave(filename = "contrmorph_language_37_44.png", plot = contrmorph_language, width = 10, height = 12, units = "in", dpi = 300)

# Figure: Crowdedness
homophone_count <- homophone_data %>% select(lang, mb_raw, n_homophones) %>% distinct() %>% rename(morph = mb_raw)
homophone_data_with_counts <- merge(results_morph_purity, homophone_count, by = c("lang", "morph"))
crowdedness_contrmorph <- ggplot(data= homophone_data_with_counts, aes(x = n_homophones, y = contribution)) +
  geom_point() +
  geom_smooth(method = "lm", color = "purple", linewidth = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.175)) +
  labs(x = "Crowdedness", y = "Importance of morph") +
  xlim(2,6) +
  theme(text = element_text(size = 32))
ggsave(filename = "crowdedness_contrmorph_37_44.png", plot = crowdedness_contrmorph, width = 10, height = 10, units = "in", dpi = 300)
r_crowdedness <- cor(homophone_data_with_counts$contribution, homophone_data_with_counts$n_h)
model <- lm(contribution ~ n_homophones, data = homophone_data_with_counts)
p_crowdedness <- summary(model)$coefficients[2, 4]

# Figure: Morphs containing "s" or "z"
contr_fact_s <- ggplot(s_data, aes(x = reorder(factor, contribution, FUN = mean), y = contribution, color = s)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 9) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25) +
  scale_color_manual(values = c("purple", "orange"))  +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(ylim = c(0.05, 0.15)) +
  theme_minimal() +
  labs(x = "Factor", y = "Importance") +
  theme(text = element_text(size = 32))
ggsave(filename = "contr_factor_s_37_44.png", plot = contr_fact_s, width = 12, height = 4, units = "in", dpi = 300)

# Figure: Homogeneity
contr_fact_mt <- ggplot(mt_data, aes(x = reorder(factor, contribution, FUN = mean), y = contribution, color = homogeneity_mt)) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 9) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25) +
  scale_color_manual(values = c("purple", "orange"))  +
  scale_y_continuous(labels = scales::percent) +
  coord_flip(ylim = c(0.05, 0.15)) +
  theme_minimal() +
  labs(x = "Factor", y = "Importance") +
  theme(text = element_text(size = 32))
ggsave(filename = "contr_factor_mt_37_44.png", plot = contr_fact_mt, width = 12, height = 4, units = "in", dpi = 300)



### 9) Descriptive statistics

# Identify the top 20 homophone sets in terms of morph variable importance
top_20 <- results_morph_purity %>%
  arrange(desc(contribution)) %>%
  slice_head(n = 20)

# Table 1: Number of words, homophone sets etc.
n_words_per_lang <- doreco_data_preprocessed %>% group_by(lang) %>% summarize(word_tokens = n_distinct(wd_ID))
n_homophonesets_per_lang <- results_all_purity %>% group_by(lang) %>% summarize(homophone_sets = n_distinct(morph))
n_homophonesets_total <- sum(n_homophonesets_per_lang$homophone_sets)
n_words_total <- sum(n_words_per_lang$word_tokens)

# Crowdedness: Number of homophone sets with n members
crowdedness_table <- table(homophone_data_with_counts$n_homophones)

# For the top 20 homophone sets, calculate mean morph durations
unique_combinations <- top_20 %>%
  select(lang, morph) %>%
  distinct()
mean_durations <- homophone_data %>%
  group_by(lang, mb_raw, gl) %>%
  summarize(
    mean_duration = 1000*mean(mb_duration, na.rm = TRUE),
    mb = first(mb),
    .groups = 'drop') %>%
  rename(morph = mb_raw)
top_20_with_means <- unique_combinations %>%
  left_join(mean_durations, by = c("lang", "morph"))

# Identify smallest/ largest homophone sets
hom_set_counts <- homophone_data %>%
  rename(morph = mb_raw) %>%
  semi_join(results_all_purity, by = c("lang", "morph")) %>%
  group_by(lang, morph) %>%
  reframe(count = n(), n_homophones = n_homophones) %>%
  arrange(count) %>%
  distinct()

smallest_hom_sets <- hom_set_counts %>%
  filter(count == min(count))

largest_hom_sets <- hom_set_counts %>%
  filter(count == max(count))

# Reduplication
reduplication_data_unfiltered <- homophone_data %>% filter(str_detect(mb, "~")) %>% distinct(lang, mb) %>% mutate(morph = str_replace_all(mb, "~", ""))
reduplication_data_filtered <- reduplication_data_unfiltered %>% semi_join(results_all_purity, by = c("lang", "morph"))


### 10) Map

languages_map <- data.frame(lang = c("Arapaho", "Bainounk-Gujaher", "Beja", "Bora", "Cabécar", "Cashinahua", "Daakie", "Ngalkbun", "Dolgan", "Evenki", "Orkon-Fanbak", "Goemai", "Gorowa", "Ho-Chunk", "Jehai", "Jejueo", "Kakabe", "Kamas-Koibal", "Trinitario-Javeriano-Loretano", "Movima", "Nafsan", "Nisvai", "N||ng", "Northern Alta", "Northern Kurdish", "Pnar", "Ruuli", "Sanzhi-Icari", "Savosavo", "Sumi Naga", "Tabasaran", "Teop", "Texistepec Popoluca", "Totoli", "Kipchak Urum", "Vera'a", "Angguruk Yali"),
                            macro_area = c("NAm", "Afr", "Afr", "SAm", "NAm", "SAm", "Pap", "Aus", "Eur", "Eur", "Pap", "Afr", "Afr", "NAm", "Eur", "Eur", "Afr", "Eur", "SAm", "SAm", "Pap", "Pap", "Afr", "Pap", "Eur", "Eur", "Afr", "Eur", "Pap", "Eur", "Eur", "Pap", "NAm", "Pap", "Eur", "Pap", "Pap"))

map.feature(languages = languages_map$lang,
            features= languages_map$macro_area,
            tile = "Esri.WorldGrayCanvas",
            color = c("purple", "blue", "red", "black", "green", "orange"),
            legend = FALSE)



### 11) Housekeeping

# Save workspace
save.image(".RData")

# Generate citations
citation()
packages <- c("dplyr", "tidyverse", "stringr", "readr", "randomForest", "ggplot2", "lingtypology", "purr")
lapply(packages, citation)
