# workflow for constructing/updating the chicago_cooccurrences_full and
# chicago_cooccurrences_summary data sets

library(tidyverse)
library(fqar)

# download raw data

chic_assm <- download_assessment_list(80) 
chic_trans <- download_transect_list(80)

# build co-occurrence data set 

assm_invs <- assessment_list_inventory(chic_assm)
trans_invs <- transect_list_inventory(chic_trans) # different format

chic_db <- download_database(80)
chic_inv <- database_inventory(chic_db)

chic_names <- select(chic_inv,
                     scientific_name,
                     common_name) # for use in trans_adjuster()

trans_adjuster <- function(df){
  df %>% rename(scientific_name = species) %>%
    select(scientific_name:duration)  %>%
    left_join(chic_names, by = "scientific_name")
}

trans_invs_adj <- lapply(trans_invs, trans_adjuster) # inventories should now have common format

all_invs <- c(ass_invs, trans_invs_adj)

chic_co_all <- assessment_cooccurrences(all_invs) 

# uncomment to update and overwrite existing cooccurrence dataframe:
# write_csv(chic_co_all, "chicago_cooccurrences_full.csv")

chic_co_sum_tot <- assessment_cooccurrences_summary(all_invs) # include non-native
chic_co_sum <- chic_co_sum_tot %>% 
  filter(target_species_nativity == "native") # excludes non-native

sum(chic_co_sum$target_species_n) # count total occurrences
sum(chic_co_sum$cospecies_native_n) # count total co-occurrences 

chic_sm <- filter(chic_co_sum, target_species_n >= 3) # remove underrepresented species

# uncomment to update and overwrite existing cooccurrence summary:
# write_csv(chic_sm, "chicago_cooccurrences_summary.csv") 



# Analysis (rough and under construction)

chic_sm <- read_csv("chicago_cooccurrences.csv")

ggplot(chic_sm, aes(y = as.factor(target_species_c), 
                        x = cospecies_native_mean_c)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = .18) +
  theme_minimal() +
  labs(x = "ONMC", y = "Assigned C-value")

cor.test(chic_sm$target_species_c, chic_sm$cospecies_native_mean_c)
model <- lm(target_species_c ~ cospecies_native_mean_c, 
            data = chic_sm)
summary(model)

range(chic_sm$cospecies_native_mean_c)

# estimate target_C = -11.8 + 3.3 * OANC
# note OANC is never below 3 in this set
# slope of 3.3 means the scale for OANC is compressed by a factor
# of about 3.3.

ggplot(chic_sm, aes(y = target_species_c, 
                    x = cospecies_native_mean_c)) +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm") +
  theme_minimal()

# C-values of observed and unobserved species

observed <- chic_inv_native$scientific_name %in% chic_co_sum$target_species
sum(observed) # it checks. 

chic_inv_native <- chic_inv_native %>% 
  mutate(observed = observed) # add a column for whether the species has been observed
view(chic_inv_native)

mean(chic_inv_native$c) # overall mean-c is 6.9 (!)
chic_inv_native %>% 
  group_by(observed) %>% 
  summarize(mean(c)) # higher for taxa not observed in the wild, as we'd expect.

# how about plants observed at least 3 times?

observed3 <- chic_inv_native$scientific_name %in% chic_sm$target_species
sum(observed3)

chic_inv_native2 <- chic_inv_native %>% 
  mutate(observed = observed3) # new column for whether species has been observed at least 3 times

chic_inv_native2 %>% 
  group_by(observed) %>% 
  summarize(mean(c)) # an even larger discrepancy

# data sets to include in paper -------------------------------------------

# a small one for illustration

chic_smaller <- chic_sm %>% 
  select(target_species,
         target_species_c,
         target_species_n, 
         cospecies_native_mean_c,
         cospecies_native_n,
         discrepancy_c)
head(chic_smaller)

overrated <- chic_smaller %>% 
  slice_max(order_by = discrepancy_c, 
            n = 10) %>% 
  arrange(-discrepancy_c)
view(overrated)

underrated <- chic_smaller %>% 
  slice_min(order_by = discrepancy_c, 
            n = 10) %>% 
  arrange(discrepancy_c)
view(underrated)

# profile plots -----------------------------------------------------------

glimpse(chic_sm)

species_profile_plot("Epilobium ciliatum", all_invs, native = TRUE)
species_profile_plot("Solidago canadensis", all_invs, native = TRUE)

