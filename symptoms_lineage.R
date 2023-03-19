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

lineages <- c("P.1" = "P.1", "P.2" = "P.2", "P.1.10" = "P.1","P.1.14" = "P.1", "P.1.15" = "P.1", 
              "B.1.1.28" = "B.1.1.28", "B.1.1.33" = "B.1.1.33", "B.1.1.332" = "B.1.1.33", "P.7" = "Other", "N.9" = "Other")
df <- df[lineage__autocolor != "#N/A"]
df[, lineage__autocolor := ifelse(lineage__autocolor %in% names(lineages), lineages[lineage__autocolor], "Other")]

dfplot <- df[, .(lineage__autocolor, teve_febre, tosse, dor_garganta, congestao_nasal, 
                 coriza, cefaleia, fadiga, astenia, anorexia, 
                 mialgia, dor_juntas, diarreia, nausea, vomito, olfato, paladar, medico_falta_ar, 
                 medico_respiracao_rapida, medico_febre_persistente, medico_estado_mental_alt
)]

dfplot[, names(dfplot) := lapply(.SD, function(x) gsub("Sim", 1, x))]
dfplot[, names(dfplot) := lapply(.SD, function(x) gsub("Não", 0, x))]
dfplot$fadiga[dfplot$astenia == "1"] <- "1"
dfplot$coriza[dfplot$congestao_nasal == "1"] <- "1"


dfplot <- dfplot %>% gather("key", "value", -c("lineage__autocolor")) |> as.data.table()
dfplot$value <- as.numeric(dfplot$value)
dfplot <- dfplot[!is.na(value)]
dfplot <- dfplot[, .(N = .N, Np = sum(value)), by = .(lineage__autocolor, key)]
dfplot <- dfplot[, Ntotal:=sum(Np), by = key]

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

#ggsave(last_plot(), file = "figs/symptoms_simple.png", width = 15, height = 20)
#ggsave(last_plot(), file = "figs/symptoms_simple.pdf", width = 15, height = 20)

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
#ggsave(last_plot(), file = "figs/main_symptoms.png", width = 10, height = 15)
#ggsave(last_plot(), file = "figs/main_symptoms.pdf", width = 10, height = 15, dpi=1200)


# --- p-values ---#
dfplot2$lineage__autocolor[dfplot2$lineage__autocolor == "B.1.1.28"] <- "B.1.1.28 or B.1.1.33"
dfplot2$lineage__autocolor[dfplot2$lineage__autocolor == "B.1.1.33"] <- "B.1.1.28 or B.1.1.33"
dfplot2 <- dfplot2 %>% group_by(lineage__autocolor, key) %>% summarise(N = sum(N), Np = sum(Np))

dfplot.p <- expand.grid(lineage_1 = unique(dfplot2$lineage__autocolor), 
                        lineage_2 = unique(dfplot2$lineage__autocolor), 
                        key = unique(dfplot2$key))
dfplot.p <- dfplot.p %>% left_join(dfplot2 %>% select(lineage_1 = lineage__autocolor, key, N_1=N, Np_1=Np))
dfplot.p <- dfplot.p %>% left_join(dfplot2 %>% select(lineage_2 = lineage__autocolor, key, N_2=N, Np_2=Np))
dfplot.p <- dfplot.p[dfplot.p$lineage_1 != dfplot.p$lineage_2,]
dfplot.p <- dfplot.p %>% mutate(Nn_1 = N_1 - Np_1, Nn_2 = N_2 - Np_2)

#dfplot.p$value <- sapply(1:nrow(dfplot.p), function(i) fisher.test(rbind(dfplot.p[i,c("N_1", "N_2")] %>% as.matrix(),
#                                                                         dfplot.p[i,c("Np_1", "Np_2")] %>% as.matrix()))$p.value)

dfplot.p$value <- sapply(1:nrow(dfplot.p), function(i) fisher.test(rbind(dfplot.p[i,c("Np_1", "Nn_1")] %>% as.matrix(),
                                                                         dfplot.p[i,c("Np_2", "Nn_2")] %>% as.matrix()))$p.value)
random.fisher.test <- function(Nsamples, Np_1, Np_2, Nn_1, Nn_2) {
  p_1 <- rbeta(Nsamples, 1 + Np_1, 1 + Nn_1)
  p_2 <- rbeta(Nsamples, 1 + Np_2, 1 + Nn_2)
  Np_1.random <- rbinom(Nsamples, Np_1+Nn_1, p_1)
  Np_2.random <- rbinom(Nsamples, Np_2+Nn_2, p_2)
  Nn_1.random <- Np_1 + Nn_1 - Np_1.random
  Nn_2.random <- Np_2 + Nn_2 - Np_2.random
  p.value <- sapply(1:Nsamples, function(i) fisher.test(rbind(c(Np_1.random[i], Nn_1.random[i]), 
                                                              c(Np_2.random[i], Nn_2.random[i])))$p.value)
  return(p.value)
}

Ns <- 100

dfplot.p$prob.reject <- sapply(1:nrow(dfplot.p), 
                               function(i) sum(random.fisher.test(Ns, dfplot.p[i, "Np_1"], 
                                                                  dfplot.p[i, "Np_2"], 
                                                                  dfplot.p[i, "Nn_1"], 
                                                                  dfplot.p[i, "Nn_2"] ) < 0.05)/Ns)


Ns <- 1E4

dfplot.p <- dfplot.p %>% filter(((lineage_1 == "P.1") & (lineage_2 == "B.1.1.28 or B.1.1.33")) |
                                  ((lineage_1 == "P.2") & (lineage_2 == "B.1.1.28 or B.1.1.33")))

dfplot.p <- dfplot.p %>% mutate(lineage_comparison = paste0(lineage_1, " vs ", lineage_2))
ggplot(dfplot.p, aes(x = lineage_comparison)) + 
  geom_segment(aes(yend=0,y=value, x=lineage_comparison, xend=lineage_comparison), colour="grey") + geom_point(aes(y=value, colour=(value <= 0.05)), position=position_dodge2(0.5), size=2) +
  facet_wrap(~key, ncol=4) + theme_bw() + geom_hline(yintercept=0.05, linetype ='dashed') + scale_y_log10() + scale_colour_manual(values=c("black", "red")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# --- Benjamini-Hochberg --- #

dfplot.p.bh <- dfplot.p %>% arrange(value)
M <- nrow(dfplot.p.bh)
dfplot.p.bh$rank <- 1:M
Q <- 0.1
dfplot.p.bh <- dfplot.p.bh %>% mutate(critical.value = rank*Q/M)
critical.p <- max(dfplot.p.bh$value[dfplot.p.bh$value < dfplot.p.bh$critical.value])

dfplot.p.bh <- dfplot.p.bh %>% mutate(reject.hypothesis = ifelse(value <= critical.p, "Reject", "Don't reject"))

dfplot.p.bh <- dfplot.p.bh %>% mutate(rejected = case_when(
  value > 0.05 ~ "A",
  (value <= 0.05) & (value > critical.p) ~ "B",
  (value <= critical.p) ~ "C"
))

ggplot(dfplot.p.bh, aes(y = lineage_comparison)) + 
  geom_segment(aes(xend=0,x=value, y=lineage_comparison, yend=lineage_comparison), colour="grey") +
  geom_point(aes(x=value, colour=rejected), position=position_dodge2(0.5), size=2) +
  facet_wrap(~key, ncol=4) + theme_bw() +
  geom_vline(xintercept = critical.p, linetype = 'dashed') + scale_x_log10() + scale_colour_manual(values=c("black", "firebrick3")) +
  theme(legend.position = "none") + labs(x = "p value", y ="") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

lineage_map <- c("P.1" = "Gamma", "P.2" = "Zeta")
dfplot.p.bh$lineage_name = lineage_map[dfplot.p.bh$lineage_1]
ggplot(dfplot.p.bh, aes(y = key)) + 
  geom_segment(aes(xend=0,x=value, y=key, yend=key), colour="grey") +
  geom_point(aes(x=value, colour=rejected), position=position_dodge2(0.5), size=2) +
  facet_wrap(~lineage_name, ncol=4) + theme_bw() +
  geom_vline(xintercept = critical.p, linetype = 'dashed') +   geom_vline(xintercept = 0.05, linetype = 'dashed') + scale_x_log10() + scale_colour_manual(values=c("black", "coral", "firebrick3")) +
  theme(legend.position = "none") + labs(x = "p value", y ="") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))


ggsave(last_plot(), file="figs/p-values-corrected-grouped.pdf", width=10, height=10) 



dfplot.p.new <- dfplot.p
dfplot.p.new$value <- sapply(1:nrow(dfplot.p), 
                             function(i) sum(rbeta(Ns, 1+dfplot.p[i,"Np_1"], 1+dfplot.p[i, "N_1"] - dfplot.p[i, "Np_1"]) >
                                               rbeta(Ns, 1+dfplot.p[i,"Np_2"], 1+dfplot.p[i, "N_2"] - dfplot.p[i, "Np_2"])  )/Ns)

dfplot.p.new$lineage_name = lineage_map[dfplot.p.new$lineage_1]

ggplot(dfplot.p.new, aes(y = key)) + 
  geom_segment(aes(xend=0,x=100*value, y=key, yend=key), colour="grey") +
  geom_point(aes(x = 100*value, y = key, colour=((value >= 1-0.05) | (value <= 0.05)))) + 
  facet_wrap(~lineage_name, ncol=4) + theme_bw() + geom_vline(xintercept=95, linetype ='dashed') + 
  geom_vline(xintercept=5, linetype='dashed') + scale_colour_manual(values=c("black", "coral", "firebrick3")) +
  theme(legend.position = "none") + labs(x = "Probability of symptom being more frequent than for B.1.1.28 and B.1.1.33 (%)", y ="") #+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
ggsave(last_plot(), file="figs/probs-greater-corrected-grouped.pdf", width=10, height=10) 
ggsave(last_plot(), file="figs/probs-greater-corrected-grouped.png", width=10, height=10) 

ggplot(dfplot.p.new, aes(y = lineage_comparison)) + 
  geom_segment(aes(xend=0,x=value, y=lineage_comparison, yend=lineage_comparison), colour="grey") + geom_point(aes(x=value, colour=((value >= 1-0.05/102) | (value <= 0.05/102))), position=position_dodge2(0.5), size=2) +
  facet_wrap(~key, ncol=4) + theme_bw() + geom_vline(xintercept=0.95, linetype ='dashed') + geom_vline(xintercept=0.05/102, linetype='dashed') + scale_colour_manual(values=c("black", "red")) +
  theme(legend.position = "none") + labs(x = "Probability of being more frequent", y ="") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
#ggsave(last_plot(), file="figs/probs_frequent.pdf", width=10, height=10) 


ggplot(dfplot.p, aes(x = lineage_1, colour = lineage_2)) + geom_point(aes(y=value), position=position_dodge2(0.5)) +
  facet_wrap(~key) + theme_bw() + geom_hline(yintercept=0.05) + scale_y_log10()

ggplot(dfplot.p.new, aes(x = lineage_1, colour = lineage_2)) + geom_point(aes(y=value), position=position_dodge2(0.5)) +
  facet_wrap(~key) + theme_bw() + geom_hline(yintercept=0.05/76) + scale_y_log10()

#df <- df %>% mutate(febre_valor = as.numeric(str_replace(febre_mais_alta, ",", ".")))


# --- Odds Ratio --- #


dfplot.rr <- expand.grid(lineage_1 = unique(dfplot2$lineage__autocolor), 
                         lineage_2 = unique(dfplot2$lineage__autocolor), 
                         key = unique(dfplot2$key))
dfplot.rr <- dfplot.rr %>% left_join(dfplot2 %>% select(lineage_1 = lineage__autocolor, key, N_1=N, Np_1=Np))
dfplot.rr <- dfplot.rr %>% left_join(dfplot2 %>% select(lineage_2 = lineage__autocolor, key, N_2=N, Np_2=Np))
dfplot.rr <- dfplot.rr[dfplot.rr$lineage_1 != dfplot.rr$lineage_2,]


Z <- sapply(1:nrow(dfplot.rr), 
            function(i) quantile(rbeta(Ns, 1+dfplot.rr[i,"Np_1"], 1+dfplot.rr[i, "N_1"] - dfplot.rr[i, "Np_1"])/
                                   rbeta(Ns, 1+dfplot.rr[i,"Np_2"], 1+dfplot.rr[i, "N_2"] - dfplot.rr[i, "Np_2"]), 
                                 c(0.025, 0.25, 0.5, 0.75, 0.975)  ))
dfplot.rr$q1 <- Z[1,]
dfplot.rr$q2 <- Z[2,]
dfplot.rr$q3 <- Z[3,]
dfplot.rr$q4 <- Z[4,]
dfplot.rr$q5 <- Z[5,]


ggplot(dfplot.rr, aes(x = q3, y = key, fill = lineage_comparison)) + geom_errorbar(aes(xmin = q1, xmax = q5), position = position_dodge(0.8), width = 0.5) + 
  geom_crossbar(aes(xmin = q2, xmax = q4), position = position_dodge(0.8), width = 0.5) + theme_bw() + labs(x = "Probability (%)", y = "") +
  scale_x_log10()

library(writexl)
library(xlsx)
write.csv(dfplot.rr, file="relative_risks_grouped.csv")
write.csv2(dfplot.rr, file="relative_risks_grouped2.csv")
#write.xlsx(dfplot.rr, "relative_risk.xlsx")

fd <- function(x) format(round(as.numeric(x), 2), nsmall=2)
dfplot.rr <- dfplot.rr %>% filter(lineage_1 %in% c("P.1", "P.2"), lineage_2 == "B.1.1.28 or B.1.1.33") %>% 
  mutate(label = paste0(fd(q3), " (95% CrI ", fd(q1), " - ", fd(q5), ")"))
dfplot.rr.out <- dfplot.rr %>% arrange(lineage_1, desc(key))
write.csv2(dfplot.rr.out, file="relative_risks_grouped2.csv")
