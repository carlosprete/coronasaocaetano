library(tidyverse)
library(data.table)

df <- readRDS("SCS_data.rds")
setDT(df)
df[, date := as.Date(data_coleta)]
df[, month := ifelse(year(date) == 2021, month(date) + 12, month(date))]

df <- df %>% filter(resultado_pcr == "positivo") %>% mutate(sequenced = ifelse(is.na(lineage__autocolor), "no", "yes"),
                                                            qidade = cut(idade, c(12, 19, 39, 59, Inf), include.lowest=TRUE))

counts.sex <- df %>% select(variable=sexo, sequenced) %>% table() %>% data.frame()
counts.age <- df %>% select(variable=qidade, sequenced) %>% table() %>% data.frame()
df$educacao[df$educacao %in% c("Ensino Fundamental (1º a 8º)", "Ensino Fundamental incompleto")] <- "Up to primary education"
counts.education <- df %>% select(variable=educacao, sequenced) %>% filter(variable != "unk") %>% table() %>% data.frame()
counts.occupation <- df %>% select(variable=ocupacao_essencial, sequenced)  %>% filter(variable != "unk") %>% table() %>% data.frame()

counts.alarms <-df %>% select(variable=encaminhamento, sequenced)  %>% 
  filter(!variable %in% c("unk", "selecionado", "orientado", "encaminhamento efetivado")) %>% table() %>% data.frame()

hypothesis.test <- function(x) {chisq.test(cbind(x %>% filter(sequenced == "yes") %>% arrange(variable) %>% .$Freq, 
                                                 x %>% filter(sequenced == "no") %>% arrange(variable) %>% .$Freq))}

result.sex <- hypothesis.test(counts.sex)
result.age <- hypothesis.test(counts.age)
result.education <- hypothesis.test(counts.education)
result.occupation <- hypothesis.test(counts.occupation)
result.alarms <- hypothesis.test(counts.alarms)
