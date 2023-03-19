library(tidyverse)
library(rstan)
library(data.table)
get.week <- function(dt) {
  return(1 + floor(as.numeric(dt - as.Date("2019-12-29"))/7))
}

filename.sivep21 <- "D:/Downloads/INFLUD21-20-06-2022.csv"
filename.sivep20 <- "D:/Downloads/INFLUD20-20-06-2022.csv"

sivep20 <- read.csv2(filename.sivep20) %>% select(DT_SIN_PRI, SEM_PRI, EVOLUCAO, HOSPITAL, ID_MN_RESI, ID_MUNICIP, CLASSI_FIN, SG_UF)
sivep21 <- read.csv2(filename.sivep21) %>% select(DT_SIN_PRI, SEM_PRI, EVOLUCAO, HOSPITAL, ID_MN_RESI, ID_MUNICIP, CLASSI_FIN, SG_UF)
sivep <- rbind(sivep20, sivep21, all = TRUE)
rm(sivep20, sivep21)
setDT(sivep)
sivep <- sivep[CLASSI_FIN %in% c(5), ][ID_MN_RESI == "SAO CAETANO DO SUL"][SG_UF == "SP"][HOSPITAL == 1]
sivep[, DT_SIN_PRI := as.Date(DT_SIN_PRI, format = "%d/%m/%Y")]
sivep[, week := SEM_PRI + 53*(year(DT_SIN_PRI) - 2020)]
sivep <- sivep[week >= 15][week <= 70]

sivep$EVOLUCAO[is.na(sivep$EVOLUCAO)] <- 9

boletim <- read.csv("boletim.csv") %>% select(-X)
setDT(boletim)
boletim <- boletim[week >= 15]

sms <- read.csv2("pt_BR_arquivo_analise-20211031010119.csv")
setDT(sms)
sms[, `:=`(data_coleta = as.Date(data_coleta),  dias_de_sintomas = as.numeric(dias_de_sintomas), cadastro_paciente = as.Date(cadastro_paciente))]
sms[, dias_de_sintomas := ifelse(is.na(dias_de_sintomas), 0, dias_de_sintomas)]
sms <- sms[cadastro_paciente <= as.Date("2021-04-30")]
sms <- sms[, .(n = sum(resultado_PCR == "positivo")), "cadastro_paciente"]
sms <- sms[order(cadastro_paciente)][, n.roll := frollmean(n, 7, align = "center")]
sms[, week := get.week(cadastro_paciente)]
sms <- sms[, .(n = sum(n)), by = .(week)]

boletim <- sms

prev <- readRDS("prevalence_lineages.rds")
prev <- prev %>% group_by(week) %>% summarise(date.min = min(date))
boletim <- boletim %>% left_join(prev)

sivep <- sivep[, .(nhosp = sum((CLASSI_FIN %in% c(4,5))), ndeaths = sum((CLASSI_FIN %in% c(4,5)) & (EVOLUCAO == 2))), by = week][order(week)]
sivep <- data.table(week = 15:70) |> left_join(sivep)# |> as.data.table()
boletim <- boletim %>% left_join(sivep)
boletim <- boletim %>% mutate(phase = case_when(
  (date.min >= as.Date("2020-04-01")) & (date.min < as.Date("2020-10-01")) ~ "Apr 2020 - Sep 2020",
  (date.min >= as.Date("2020-10-01")) & (date.min < as.Date("2021-01-01")) ~ "Oct 2020 - Dec 2020",
  (date.min >= as.Date("2021-01-01")) & (date.min <= as.Date("2021-03-31")) ~ "Jan 2021 - Mar 2021",
  (date.min >= as.Date("2021-04-01"))  ~ "End of Phase III"
))

#z = boletim %>% group_by(phase) %>% summarise(n_sms = sum(n), n_boletim = sum(n_boletim))


boletim <- boletim %>% filter(phase != "End of Phase III")

boletim$phase <- factor(boletim$phase, levels = c("Apr 2020 - Sep 2020", "Oct 2020 - Dec 2020", "Jan 2021 - Mar 2021"))

dfplot <- boletim %>% group_by(phase) %>% summarise(
  n = sum(n), nhosp = sum(nhosp), ndeaths = sum(ndeaths)
)

dfplot.HFR <- dfplot %>% group_by(phase) %>% summarise(q1 = qbeta(0.025, 1+ndeaths, 1 + nhosp - ndeaths),
                                                       q2 = qbeta(0.25, 1+ndeaths, 1 + nhosp - ndeaths),
                                                       q3 = qbeta(0.5, 1+ndeaths, 1 + nhosp - ndeaths),
                                                       q4 = qbeta(0.75, 1+ndeaths, 1 + nhosp - ndeaths),
                                                       q5 = qbeta(0.975, 1+ndeaths, 1 + nhosp - ndeaths),
                                                       type = "In-hospital Fatality Rate (HFR)")

dfplot.CFR <- dfplot %>% group_by(phase) %>% summarise(q1 = qbeta(0.025, 1+ndeaths, 1 + n - ndeaths),
                                                       q2 = qbeta(0.25, 1+ndeaths, 1 + n - ndeaths),
                                                       q3 = qbeta(0.5, 1+ndeaths, 1 + n - ndeaths),
                                                       q4 = qbeta(0.75, 1+ndeaths, 1 + n - ndeaths),
                                                       q5 = qbeta(0.975, 1+ndeaths, 1 + n - ndeaths),
                                                       type = "Case Fatality Rate (CFR)")

dfplot.CHR <- dfplot %>% group_by(phase) %>% summarise(q1 = qbeta(0.025, 1+nhosp, 1 + n - nhosp),
                                                       q2 = qbeta(0.25, 1+nhosp, 1 + n - nhosp),
                                                       q3 = qbeta(0.5, 1+nhosp, 1 + n - nhosp),
                                                       q4 = qbeta(0.75, 1+nhosp, 1 + n - nhosp),
                                                       q5 = qbeta(0.975, 1+nhosp, 1 + n - nhosp),
                                                       type = "Case Hospitalisation Rate (CHR)")

dfplot <- rbind(dfplot.HFR, dfplot.CFR, dfplot.CHR)

library(RColorBrewer)

ggplot(dfplot, aes(x = phase, y = 100*q3, fill = phase)) + facet_wrap(~ type, scale = "free_y") + geom_errorbar(aes(ymin = 100*q1, ymax = 100*q5), lwd=0.1, width=0.5) + 
  geom_crossbar(aes(ymin = 100*q2, ymax = 100*q4, y = 100*q3), lwd=0.1, width=0.5) +
  theme_bw() + theme(legend.position = "top") + labs(x = "", y = "Probability (%)") + 
  scale_fill_manual("", values = brewer.pal(5,"Set2")) + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave(last_plot(), file="figs/CFR_phases.pdf", width=10, height=5)
