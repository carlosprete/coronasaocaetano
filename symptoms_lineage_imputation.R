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
df <- left_join(df %>% select(id_paciente_scs, date, month_year, lineage__autocolor), 
                quest %>% rename(id_paciente_scs = Id_Paciente))

lineages <- c("P.1" = "P.1", "P.2" = "P.2", "P.1.10" = "P.1","P.1.14" = "P.1", "P.1.15" = "P.1", 
              "B.1.1.28" = "B.1.1.28", "B.1.1.33" = "B.1.1.33", "B.1.1.332" = "B.1.1.33", "P.7" = "Other", "N.9" = "Other")


# --- Imputation --- #
prev <- readRDS("prevalence_lineages.rds")
df.known.lineages <- df[lineage__autocolor != "#N/A"][!is.na(lineage__autocolor)]
df <- df %>% filter((lineage__autocolor == "#N/A") | is.na(lineage__autocolor))
df$lineage__autocolor <- NULL
df <- df %>% left_join(prev %>% select(date, lineage__autocolor, prev))
df <- rbind(df, df.known.lineages %>% mutate(prev = 1.0))

df[, lineage__autocolor := ifelse(lineage__autocolor %in% names(lineages), lineages[lineage__autocolor], "Other")]
df$prev <- as.numeric(df$prev)

dfplot <- df[, .(lineage__autocolor, prev, teve_febre, tosse, dor_garganta, congestao_nasal, 
                 coriza, cefaleia, fadiga, astenia, anorexia, 
                 mialgia, dor_juntas, diarreia, nausea, vomito, olfato, paladar, medico_falta_ar, 
                 medico_respiracao_rapida, medico_febre_persistente, medico_estado_mental_alt)]

dfplot[, names(dfplot) := lapply(.SD, function(x) gsub("Sim", 1, x))]
dfplot[, names(dfplot) := lapply(.SD, function(x) gsub("Não", 0, x))]
dfplot$fadiga[dfplot$astenia == "1"] <- "1"
dfplot$coriza[dfplot$congestao_nasal == "1"] <- "1"


dfplot <- dfplot %>% gather("key", "value", -c("lineage__autocolor", "prev")) |> as.data.table()
dfplot$value <- as.numeric(dfplot$value)
dfplot$prev <- as.numeric(dfplot$prev)

dfplot <- dfplot[!is.na(value)]

#Ns <- 1E4

dfplot <- dfplot[, .(N = sum(prev), Np = sum(value*prev)), by = .(lineage__autocolor, key)]

dfplot <- dfplot[, `:=`(q1 = qbeta(0.025, 1+Np, 1+N - Np),
                        q2 = qbeta(0.25, 1+Np, 1+N - Np),
                        q3 = qbeta(0.5, 1+Np, 1+N - Np),
                        q4 = qbeta(0.75, 1+Np, 1+N - Np),
                        q5 = qbeta(0.975, 1+Np, 1+N - Np))]


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



ggplot(dfplot, aes(x = q3, y = key, fill = lineage__autocolor)) + geom_errorbar(aes(xmin = q1, xmax = q5), position = position_dodge(0.8), width = 0.5) + 
  geom_crossbar(aes(xmin = q2, xmax = q4), position = position_dodge(0.8), width = 0.5) + theme_bw() + labs(x = "Probability (%)", y = "")

ggsave(last_plot(), file = "figs/symptoms_simple.png", width = 15, height = 20)
ggsave(last_plot(), file = "figs/symptoms_simple.pdf", width = 15, height = 20)

dfplot2 <- dfplot[key %in% names(keys)]
dfplot2$key <- keys[dfplot2$key]
library(RColorBrewer)

dfplot2$key <- factor(dfplot2$key, levels = rev(sort(unique(dfplot2$key))))
dfplot2 <- dfplot2 %>% filter(lineage__autocolor != "Other")

ggplot(dfplot2 %>% filter(lineage__autocolor != "Other"), aes(x = 100*q3, y = key, fill = lineage__autocolor)) + 
  geom_errorbar(aes(xmin = 100*q1, xmax = 100*q5), position = position_dodge(0.7), width = 0.5, lwd=0.1) + 
  geom_crossbar(aes(xmin = 100*q2, xmax = 100*q4), position = position_dodge(0.7), width = 0.5, lwd=0.1) + 
  theme_bw() + labs(x = "Probability (%)", y = "") +
  scale_fill_manual("", values = brewer.pal(5,"Set2")) + theme(legend.position = "top")
ggsave(last_plot(), file = "figs/main_symptoms_imputed.png", width = 10, height = 15)
ggsave(last_plot(), file = "figs/main_symptoms_imputed.pdf", width = 10, height = 15, dpi=1200)
