library(tidyverse)

library(ggrepel)
library(gridExtra)
library(geobr)

df <- read.csv("C:/Users/POLI/Downloads/census_tracts2010_sp.csv") %>% filter(code_muni == 3548807)

neighborhoods <- read_neighborhood() %>% filter(code_muni == 3548807)
cts <- read_census_tract(code_tract=3548807, year=2010)
df <- df %>% select(-c("code_state", "code_muni", "code_metro")) %>% mutate(code_tract = as.character(code_tract))
cts <- cts %>% filter(code_tract %in% df$code_tract) %>% left_join(df, by="code_tract")

cts.sum <- cts %>% group_by(name_neighborhood ) %>% summarise(pop_total = sum(pop_total),
                                                             households_total = sum(households_total),
                                                             income_total = sum(income_total),
                                                             pop_branca = sum(pop_branca),
                                                             pop_preta = sum(pop_preta),
                                                             pop_amarela = sum(pop_amarela),
                                                             pop_parda = sum(pop_parda),
                                                             pop_indigena = sum(pop_indigena),
                                                             idade_0a9 = sum(idade_0a9),
                                                             idade_10a14 = sum(idade_10a14),
                                                             idade_15a19 = sum(idade_15a19),
                                                             idade_20a29 = sum(idade_20a29),
                                                             idade_30a39 = sum(idade_30a39),
                                                             idade_40a49 = sum(idade_40a49),
                                                             idade_50a59 = sum(idade_50a59),
                                                             idade_60a69 = sum(idade_60a69),
                                                             idade_70 = sum(idade_70)
)

cts.sum <- cts.sum %>% mutate(income_per_capita = income_total/pop_total,
                              inhabitants_per_household = pop_total/households_total)

cts.sum$name_neighborhood <- factor(cts.sum$name_neighborhood, levels=rev(sort(cts.sum$name_neighborhood)))

scs <- readRDS("SCS_data.rds")
scs$data_coleta <- as.Date(scs$data_coleta)

scs <- scs %>% group_by(bairro) %>% summarise(n = n(), n1 = sum(data_coleta <= as.Date("2020-09-30")), 
                                              n2 = sum((data_coleta >= as.Date("2020-10-01")) & (data_coleta <= as.Date("2020-12-31"))),
                                              n3 = sum(data_coleta >= as.Date("2021-01-01"))) %>% rename(name_neighborhood = bairro)
new.names <- c("Ceramica" = "Cerâmica", "Fundacao" = "Fundação", "Jardim Sao Caetano" = "Jardim São Caetano", "Maua" = "Mauá",
               "Santo Antonio" = "Santo Antônio", "Sao Jose" = "São José", "Olimpico" = "Olímpico", "Nova Gerty" = "Nova Gerti")
I <- scs$name_neighborhood %in% names(new.names)
scs$name_neighborhood[I] <- new.names[scs$name_neighborhood[I]]
cts.sum <- cts.sum %>% left_join(scs)
cts.sum <- cts.sum %>% mutate(cases_per_capita = n/pop_total,
                              cases_per_capita_1 = n1/pop_total,
                              cases_per_capita_2 = n2/pop_total,
                              cases_per_capita_3 = n3/pop_total)

dfplot <- cts.sum %>% as.data.frame() %>% select(name_neighborhood, income_per_capita) %>% gather("key", "value", -c("name_neighborhood"))
ggplot(dfplot, aes(x = value, y = name_neighborhood)) + geom_bar(stat = "identity") + facet_wrap(~ key, scales="free_x")

ggplot(cts.sum, aes(x = income_per_capita, y = name_neighborhood)) + geom_bar(stat = "identity") + theme_bw() +
  labs(x = "Income per Capita (Reais, 2010)", y = "")

g.income <- ggplot(cts.sum, aes(x = income_per_capita, y = 1000*cases_per_capita_3)) + geom_smooth(method='loess', span=1.0) + 
  geom_point() + theme_bw() +
    labs(x = "Income per Capita (Reais, 2010)", y = "Cases per 1,000 inhabitants") + geom_text_repel(aes(label = name_neighborhood)) 

g.age <- ggplot(cts.sum, aes(x = 100*(idade_60a69 + idade_70 + idade_50a59)/pop_total, y = 1000*cases_per_capita_3)) + geom_smooth(method='loess', span=1.0) + 
  geom_point() +  theme_bw() + 
  labs(x = "Proportion of population above 50 (%)", y = "Cases per 1,000 inhabitants") + geom_text_repel(aes(label = name_neighborhood))


g.combined <- grid.arrange(g.income, g.age, ncol=2)
ggsave(g.combined, width=12, height=6, file="figs/cases_bairros.pdf")
ggsave(g.combined, width=12, height=6, file="figs/cases_bairros.png")


dfplot <- cts.sum %>% as.data.frame() %>% select(name_neighborhood, idade_0a9, idade_10a14, idade_15a19, idade_20a29, idade_30a39,
                                                 idade_40a49, idade_50a59, idade_60a69, idade_70) %>% gather("key", "value", -c("name_neighborhood"))
age.map <- c("idade_0a9" = "0-19", "idade_10a14" = "0-19", "idade_15a19" = "0-19", "idade_20a29" = "20-29", "idade_30a39" = "30-39",
             "idade_40a49" = "40-49","idade_50a59" = "50-59","idade_60a69" = "60-69","idade_70" = "70+")

dfplot$age_group <- age.map[dfplot$key]
dfplot <- dfplot %>% group_by(name_neighborhood, age_group) %>% summarise(value = sum(value))
dfplot <- dfplot %>% group_by(name_neighborhood) %>% mutate(value = value/sum(value))
ggplot(dfplot, aes(x = age_group, y = 100*value)) + geom_bar(stat = "identity", position="dodge2") +
  theme_bw() + labs(x = "Proportion (%)", y = "") + theme(legend.position="top") + facet_wrap(~ name_neighborhood, ncol=3)


#g.age <- ggplot(cts.sum, aes(x = inhabitants_per_household, y = cases_per_capita_3)) + geom_point() + theme_bw() +
#  labs(x = "Proportion of population above 50 (%)", y = "Cases per capita") + geom_text_repel(aes(label = name_neighborhood))



ggplot(cts.sum, aes(x = (idade_60a69 + idade_70 + idade_50a59)/pop_total, y = cases_per_capita)) + geom_point() + theme_bw() +
  labs(x = "Income per Capita (Reais, 2010)", y = "") + geom_label(aes(label = name_neighborhood))


ggplot(cts.sum, aes(x = (pop_preta + pop_parda)/pop_total, y = cases_per_capita)) + geom_point() + theme_bw() +
  labs(x = "Income per Capita (Reais, 2010)", y = "") + geom_label(aes(label = name_neighborhood))


dfplot <- cts.sum %>% as.data.frame() %>% select(name_neighborhood, pop_branca, pop_parda, pop_preta, pop_amarela) %>% gather("key", "value", -c("name_neighborhood"))
dfplot <- dfplot %>% group_by(name_neighborhood) %>% mutate(value = value/sum(value))
ggplot(dfplot, aes(x = 100*value, y = name_neighborhood, fill=key)) + geom_bar(stat = "identity", position="dodge2") +
  theme_bw() + labs(x = "Proportion (%)", y = "") + theme(legend.position="top")


