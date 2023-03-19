library(tidyverse)
library(readxl)

df <- read_excel("metadadosSCS_paper_f.xlsx", sheet="Abril2020_abril2021")
new.genomes <- read_excel("metadadosSCS_paper_f.xlsx", sheet="genomas>75%")
new.genomes <- new.genomes %>% select(id_study_cadde, lineage__autocolor = nextclade_pango_autocolor)
df <- df %>% rename(id_study_cadde = `sequencias>75%`)
df <- df %>% left_join(new.genomes, by = "id_study_cadde")

saveRDS(df, "SCS_data.rds")
