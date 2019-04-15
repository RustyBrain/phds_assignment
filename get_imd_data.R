library(readr)
library(dplyr)

imd_df <- read_csv('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/467774/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv')
lsoa_utla_lookup <- read_csv('https://opendata.arcgis.com/datasets/95ecb220a30e41d5ae759cd1c9aa929f_0.csv')
lsoa_utla_lookup <- lsoa_utla_lookup %>%
  select(LSOA11CD, UTLA18CD)

imd_df <- merge(x = imd_df, y = lsoa_utla_lookup, by.x = 'LSOA code (2011)', by.y = 'LSOA11CD')

pop_weighted_ave_domains <- imd_df %>%
  select("Income Score (rate)", "Employment Score (rate)", "Education, Skills and Training Score", "Health Deprivation and Disability Score",
                 "Crime Score", "Barriers to Housing and Services Score", "Living Environment Score", 
                 "Income Deprivation Affecting Children Index (IDACI) Score (rate)", 
                 "Income Deprivation Affecting Older People (IDAOPI) Score (rate)", "Children and Young People Sub-domain Score",
                 "Adult Skills Sub-domain Score", "Geographical Barriers Sub-domain Score", "Wider Barriers Sub-domain Score", 
                 "Indoors Sub-domain Score", "Outdoors Sub-domain Score", "Total population: mid 2012 (excluding prisoners)")

pop_weighted_ave_domains <- pop_weighted_ave_domains[, 1:15] / pop_weighted_ave_domains[,16]
pop_weighted_ave_domains$UTLA18CD <- imd_df$UTLA18CD

pop_weighted_ave_domains <- pop_weighted_ave_domains %>%
  group_by(UTLA18CD) %>%
  summarise_each(funs(sum))


  
