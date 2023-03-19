library(tidyverse)
library(data.table)

#df <- read.csv("microreact_allSCS.csv")
df <- readRDS("SCS_data.rds")
setDT(df)
#df$date <- as.Date(df$date)
df$date <- as.Date(df$data_coleta)

quest0 <- read.csv2("./questionario_pt_BR_/questionario_pt_BR_questionario_inicial-20211031021015.csv")
quest1 <- read.csv2("./questionario_pt_BR_/questionario_pt_BR_Questionário acompanhamento 1-20211031050647.csv")
quest2 <- read.csv2("./questionario_pt_BR_/questionario_pt_BR_Questionário acompanhamento 2-20211031050019.csv")
quest3 <- read.csv2("./questionario_pt_BR_/questionario_pt_BR_Questionário acompanhamento 3-20211031045528.csv")
quest4 <- read.csv2("./questionario_pt_BR_/questionario_pt_BR_Questionário acompanhamento 4-20211031045234.csv")
quest5 <- read.csv2("./questionario_pt_BR_/questionario_pt_BR_Questionário acompanhamento 5-20211031045050.csv")
questf <- read.csv2("./questionario_pt_BR_/questionario_pt_BR_Questionário contato final de acompanhamento-20211031044711.csv")
fields <- c("Id_Paciente", "data_criacao_questionario", "teve_febre", "tosse", "dor_garganta", "congestao_nasal", 
            "coriza", "cefaleia", "fadiga", "astenia", "anorexia", 
            "mialgia", "dor_juntas", "diarreia", "nausea", "vomito", "medico_olfato", "medico_paladar", "medico_falta_ar", 
            "medico_respiracao_rapida", "medico_febre_persisitente", "medico_estado_mental_alt")
quest0 <- quest0 %>% rename(medico_olfato = olfato, medico_paladar = paladar, medico_febre_persisitente = medico_febre_persistente)
quest <- rbind(quest0[, fields],
               quest1[, fields],
               quest2[, fields],
               quest3[, fields],
               quest4[, fields],
               quest5[, fields],
               questf[, fields]) %>%
  filter(Id_Paciente %in% quest0$Id_Paciente)
quest <- quest %>% rename(olfato = medico_olfato, paladar = medico_paladar, medico_febre_persistente = medico_febre_persisitente)

quest <- quest %>% select(-data_criacao_questionario) %>% group_by(Id_Paciente) %>% 
  summarise_all(list(function(x) ifelse(any(x == "Sim"), "Sim", "Não")))
df <- left_join(df %>% select(id_paciente_scs, date, month_year, lineage__autocolor, idade), 
                quest %>% rename(id_paciente_scs = Id_Paciente))
df <- df %>% mutate(phase = case_when(
  (date >= as.Date("2020-04-01")) & (date < as.Date("2020-10-01")) ~ "Phase I",
  (date >= as.Date("2020-10-01")) & (date < as.Date("2021-01-01")) ~ "Phase II",
  (date >= as.Date("2021-01-01")) & (date <= as.Date("2021-04-30")) ~ "Phase III"
))

lineages <- c("P.1" = "P.1", "P.2" = "P.2", "P.1.10" = "P.1","P.1.14" = "P.1", "P.1.15" = "P.1", 
              "B.1.1.28" = "B.1.1.28", "B.1.1.33" = "B.1.1.33", "B.1.1.332" = "B.1.1.33", "P.7" = "Other", "N.9" = "Other")
keys <- c("olfato" = "Asnomia", 
          "paladar" = "Ageusia", 
          "cefaleia" = "Headache", 
          "dor_garganta" = "Sore throat", 
          "coriza" = "Coryza/Stuffy Nose",
          "teve_febre" = "Fever", 
          "medico_febre_persistente" = "Persistent Fever", 
          "tosse" = "Cough",
          "vomito" = "Vomit",
          #          "tosse_catarro" = "Cough with Phlegm",
          "nausea" = "Nausea",
          "mialgia" = "Myalgia",
          "medico_respiracao_rapida" = "Tachypnea",
          "medico_falta_ar" = "Dyspnea",
          "medico_estado_mental_alt" = "Altered Mental Status",
          "fadiga" = "Fatigue",
          #          "encaminhamento_grave" = "Severe Outcome", #Not reliable field
          "dor_juntas" = "Joint Pain",
          "anorexia" = "Anorexia"
)

dfplot <- df[, .(idade, phase, teve_febre, tosse, dor_garganta, congestao_nasal, 
                 coriza, cefaleia, fadiga, astenia, anorexia, 
                 mialgia, dor_juntas, diarreia, nausea, vomito, olfato, paladar, medico_falta_ar, 
                 medico_respiracao_rapida, medico_febre_persistente, medico_estado_mental_alt)]

dfplot[, names(dfplot) := lapply(.SD, function(x) gsub("Sim", 1, x))]
dfplot[, names(dfplot) := lapply(.SD, function(x) gsub("Não", 0, x))]
dfplot$fadiga[dfplot$astenia == "1"] <- "1"
dfplot$coriza[dfplot$congestao_nasal == "1"] <- "1"


dfplot <- dfplot %>% gather("key", "value", -c("phase", "idade")) |> as.data.table()
dfplot$value <- as.numeric(dfplot$value)
dfplot$idade <- as.numeric(dfplot$idade)

dfplot <- dfplot[!is.na(value)]

dfplot <- dfplot %>% filter(key %in% names(keys) )
dfplot$key <- keys[dfplot$key]

ggplot(dfplot %>% filter(value == 1), aes(x = idade, y = phase )) + geom_violin() + geom_boxplot(width=0.25) + facet_wrap(~ key) +
  theme_bw() 

#ggplot(dfplot, aes(x = idade, y = phase, fill=as.character(value))) + geom_violin() + geom_boxplot(width=0.25) + facet_wrap(~ key, ncol=3) +
#  theme_bw() + labs(x = "Age", y = "")


dfplot <- dfplot %>% mutate(faixa_etaria = cut(idade, c(-Inf, 20, 40, 60, 80, Inf), right=F, include.lowest=T, labels=c("0-19", "20-39", "40-59", "60-79", "80+")))
dfplot <- dfplot %>% group_by(faixa_etaria, phase, key) %>% summarise(Np = sum(value), N = n())

dfplot0 <- dfplot

dfplot <- dfplot0 %>% group_by(faixa_etaria, phase, key) %>%
  summarise(q1 = qbeta(0.025, 1+Np, 1+N - Np),
            q2 = qbeta(0.25, 1+Np, 1+N - Np),
            q3 = qbeta(0.5, 1+Np, 1+N - Np),
            q4 = qbeta(0.75, 1+Np, 1+N - Np),
            q5 = qbeta(0.975, 1+Np, 1+N - Np))

library(RColorBrewer)

ggplot(dfplot, aes(x = faixa_etaria, y = 100*q3, fill = phase)) + geom_errorbar(aes(ymin=100*q1, ymax=100*q5), position="dodge2", lwd=0.1) + 
  geom_crossbar(aes(ymin=100*q2, y=100*q3, ymax=100*q4), position="dodge2", lwd=0.1) + facet_wrap(~ key, ncol=3) + theme_bw() + 
  labs(x = "Age group", y = "Probability of symptom (%)") +  scale_fill_manual("", values = brewer.pal(5,"Set2")) + theme(legend.position="top")
ggsave(last_plot(), file="figs/symptoms_per_age_period.pdf", width=10, height=15)


ggplot(dfplot0, aes(x = faixa_etaria, y = Np, fill = phase)) + geom_bar(stat="identity", position="dodge2") + theme_bw() + 
  labs(x = "Age group", y = "Number of symptomatic patients") +  scale_fill_manual("", values = brewer.pal(5,"Set2")) + theme(legend.position="top") +
  facet_wrap( ~ key, ncol = 3, scale = "free_y")
ggsave(last_plot(), file="figs/symptoms_per_age_period_inv.pdf", width=10, height=15)

dfplot0 <- dfplot0 %>% group_by(phase, key) %>% mutate(p = 100*Np/sum(Np))
ggplot(dfplot0, aes(x = faixa_etaria, y = p, fill = phase)) + geom_bar(stat="identity", position="dodge2") + theme_bw() + 
  labs(x = "Age group", y = "Probability of age given symptom (%)") +  scale_fill_manual("", values = brewer.pal(5,"Set2")) + theme(legend.position="top") +
  facet_wrap( ~ key, ncol = 3, scale = "free_y")
ggsave(last_plot(), file="figs/symptoms_per_age_period_inv_prop.pdf", width=10, height=15)


df <- df %>% mutate(faixa_etaria = cut(idade, c(-Inf, 20, 40, 60, 80, Inf), right=F, include.lowest=T, labels=c("0-19", "20-39", "40-59", "60-79", "80+")))
dfplot <- df %>% group_by(faixa_etaria, phase) %>% tally()
dfplot <- dfplot %>% group_by(phase) %>% mutate(p = n/sum(n))
ggplot(dfplot, aes(x = faixa_etaria, y = p, fill = phase)) + geom_bar(stat="identity", position="dodge2") + theme_bw() + 
  labs(x = "Age group", y = "Probability of age (%)") +  scale_fill_manual("", values = brewer.pal(5,"Set2")) + theme(legend.position="top")
