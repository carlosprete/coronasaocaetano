library(tidyverse)
library(data.table)
library(zoo)
library(gridExtra)

# --- Flu-Like Syndrome Cases --- #
flulike <- read.csv("flulike.csv")
setDT(flulike)
flulike[, dataInicioSintomas := as.Date(dataInicioSintomas)]
flulike <- flulike[(dataInicioSintomas >= as.Date("2020-04-06")) & (dataInicioSintomas <= as.Date("2021-04-30"))]
flulike <- flulike[municipioNotificacao == "São Caetano do Sul"]
classif.names <- c("Confirmado Clínico-Epidemiológico" = "Confirmed", 
                   "Confirmado Clínico-Imagem" = "Confirmed",
                   "Confirmado Laboratorial" = "Confirmed",
                   "Confirmado por Critério Clínico" = "Confirmed",
                   "Descartado" = "Discarded",
                   "Síndrome Gripal Não Especificada" = "Unknown")
flulike[, classificacaoFinal := ifelse(classificacaoFinal == "", "Unknown", classif.names[classificacaoFinal])]
flulike <- flulike[, .(n = .N, type = "flulike"), by = list(dataInicioSintomas, classificacaoFinal)]

# --- Confirmed Cases: covid.saude.gov.br --- #

#moh <- do.call(rbind, lapply(list.files("./HIST_PAINEL_COVIDBR_27jun2022/"), 
#                             function(x) read.csv2(paste0("./HIST_PAINEL_COVIDBR_27jun2022/", x)) %>% 
#                               filter(municipio == "São Caetano do Sul")))
#write.csv(moh, "moh.csv")

get.week <- function(dt) {
  return(1 + floor(as.numeric(dt - as.Date("2019-12-29"))/7))
}
moh <- read.csv("moh.csv")
setDT(moh)
moh[, data:= as.Date(data)]
moh[, week := get.week(data)]
moh <- moh[week >= 15][week <= 70]
moh <- moh[, .(n = sum(casosNovos)), by = week][order(week)]


# --- SIVEP-Gripe --- #

sivep20 <- read.csv2("D:/Downloads/INFLUD20-20-06-2022.csv") %>% select(DT_SIN_PRI, SEM_PRI, EVOLUCAO, HOSPITAL, ID_MN_RESI, ID_MUNICIP, CLASSI_FIN, SG_UF)
sivep21 <- read.csv2("D:/Downloads/INFLUD21-20-06-2022.csv") %>% select(DT_SIN_PRI, SEM_PRI, EVOLUCAO, HOSPITAL, ID_MN_RESI, ID_MUNICIP, CLASSI_FIN, SG_UF)
sivep <- rbind(sivep20, sivep21, all = TRUE)
rm(sivep20, sivep21)
setDT(sivep)
sivep <- sivep[CLASSI_FIN %in% c(4,5), ][ID_MN_RESI == "SAO CAETANO DO SUL", ][SG_UF == "SP", ][(HOSPITAL == 1) | (EVOLUCAO == 2), ]
sivep[, DT_SIN_PRI := as.Date(DT_SIN_PRI, format = "%d/%m/%Y")]
sivep[, week := SEM_PRI + 53*(year(DT_SIN_PRI) - 2020)]
sivep <- sivep[week >= 15][week <= 70]
sivep$EVOLUCAO[is.na(sivep$EVOLUCAO)] <- 9

deaths <- sivep[, .(n = sum((EVOLUCAO == 2) & (CLASSI_FIN %in% c(4,5)))), by = DT_SIN_PRI][order(DT_SIN_PRI)]
dates <- seq(min(deaths$DT_SIN_PRI), max(deaths$DT_SIN_PRI), by = "day")
deaths <- data.frame(DT_SIN_PRI = dates) |> left_join(deaths) |> as.data.table()
#deaths <- sivep[type %in% c("Deaths (confirmed)", "Deaths (probable)"), .(n = sum(n)), by = DT_SIN_PRI][order(DT_SIN_PRI)]

sivep <- rbind(sivep[, .(n = sum((EVOLUCAO == 2) & CLASSI_FIN == 5), type = "Deaths (confirmed)"), by = week],
               sivep[, .(n = sum((EVOLUCAO == 2) & CLASSI_FIN == 4), type = "Deaths (probable)"), by = week],
               sivep[, .(n = sum(CLASSI_FIN == 5), type = "Hospitalisations (confirmed)"), by = week],
               sivep[, .(n = sum(CLASSI_FIN == 4), type = "Hospitalisations (probable)"), by = week])
sivep$n[is.na(sivep$n)] <- 0



# --- SMS --- #

sms <- read.csv2("pt_BR_arquivo_analise-20211031010119.csv")
setDT(sms)
sms[, `:=`(data_coleta = as.Date(data_coleta),  dias_de_sintomas = as.numeric(dias_de_sintomas), cadastro_paciente = as.Date(cadastro_paciente))]
sms[, dias_de_sintomas := ifelse(is.na(dias_de_sintomas), 0, dias_de_sintomas)]
sms <- sms[cadastro_paciente <= as.Date("2021-04-30")]
sms <- rbind(sms[, .(n = sum(resultado_PCR == "positivo"), type = "sms_positive"), "cadastro_paciente"],
             sms[, .(n = sum(resultado_PCR == "negativo"), type = "sms_negative"), "cadastro_paciente"])#,
#sms[, .(n = sum(resultado_PCR %in% c("", "inconclusivo")), type = "sms_unknown"), "cadastro_paciente"])
sms <- sms[order(cadastro_paciente)][, n.roll := frollmean(n, 14, align = "center")]
sms[, week := get.week(cadastro_paciente)]
sms <- sms[, .(n = sum(n)), by = .(type, week)]

# --- Brasil.io --- #
brasilio <- read.csv("caso_full.csv")
setDT(brasilio)
brasilio <- brasilio[city == "São Caetano do Sul"]
brasilio[, date2 := as.Date(date)]

# --- Municipal report --- #

library(pdftools)
file <- system.file("examples", "20220623_boletim-completo-covid19-pmscs.pdf", package = "tabulizer")
boletim <- pdf_data("20210531-boletim-completo-covid19-pmscs.pdf")[[5]]
setDT(boletim)
boletim <- boletim[y < 165][y > 50][x>10][order(x)]
boletim <- boletim[, .(week = 11:75, n = as.numeric(text))]
boletim <- boletim[week <= 70]
write.csv(boletim, "boletim.csv")
boletim <- read.csv("boletim.csv") %>% select(-X)
setDT(boletim)
boletim <- boletim[week >= 15]
ggplot(boletim, aes(x = week, y = n)) + geom_bar(stat = "identity")


# --- Plots --- #

ggplot(sms, aes(x = cadastro_paciente, y = n, fill = type)) + geom_area(alpha = 0.3) + theme_bw()
ggplot(sivep[type %in% c("Deaths (confirmed)", "Deaths (probable)")], aes(x = DT_SIN_PRI, y  = n, colour = type)) + geom_line(alpha = 0.3) + theme_bw()
ggplot(flulike, aes(x = dataInicioSintomas, y = n, fill = classificacaoFinal)) + geom_area(alpha = 0.3) + theme_bw()
ggplot(moh, aes(x = data, y = n)) + geom_line(alpha = 0.3) + geom_line(aes(y = n.roll)) + theme_bw()

ggplot(sms, aes(x = cadastro_paciente, y = n, colour = type)) +  geom_line(aes(y = n.roll)) + geom_line(alpha = 0.3) + 
  geom_line(data = moh, aes(x = data, y = n.roll), inherit.aes = FALSE, colour = "black") + theme_bw()

ggplot(sms[type == "sms_positive"], aes(x = cadastro_paciente, y = cumsum(n), colour = type)) +  geom_line() + 
  geom_line(data = moh, aes(x = data, y = cumsum(n)), inherit.aes = FALSE, colour = "black") + theme_bw()

#  sivep[, .(n = sum(n)), by = week]

ggplot(sms, aes(x = week, y = n, fill = type)) + geom_bar(stat = "identity")

dfplot <- rbind(sms[type == "sms_positive", .(week, n, type)], 
                moh[, .(week, n, type = "moh")], 
                boletim[, .(week, n, type = "boletim")])
                
dfplot[, date := as.Date("2019-12-29") + 7*(week - 1)]


deaths.week <- sivep[type %in% c("Deaths (confirmed)", "Deaths (probable)"), .(n = sum(n)), by = week][order(week)]
hosp.week <- sivep[type %in% c("Hospitalisations (confirmed)", "Hospitalisations (probable)"), .(n = sum(n)), by = week][order(week)]

deaths.week[, date := as.Date("2019-12-29") + 7*(week - 1)]
hosp.week[, date := as.Date("2019-12-29") + 7*(week - 1)]

library(RColorBrewer)

legend.names <- c("boletim" = "Municipal Bulletin", 
                  "moh" = "Official MoH dataset",
                  "sms_positive" = "Corona São Caetano")
dfplot[, type := legend.names[type]]
ggplot(hosp.week, aes(x = date, y = 5*n)) +   geom_area(alpha = 0.25, fill = "grey") + 
  geom_line(lwd = 2, colour = "grey") + geom_point(size = 5, colour = "grey") + 
  geom_area(data = deaths.week, alpha = 0.25, fill = "grey47") + 
  geom_line(data = deaths.week, lwd = 2, colour = "grey47") + 
  geom_point(data = deaths.week, size = 5, colour = "grey47") + 
 geom_line(data = dfplot, aes(x = date, y = n, colour = type), lwd = 2, inherit.aes = FALSE) + 
   geom_point(data = dfplot, aes(x = date, y = n, colour = type), size = 5, inherit.aes = FALSE) +
  scale_x_continuous(breaks = seq(as.Date("2020-04-01"), as.Date("2021-04-01"), by = "month"),
                     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) + 
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1),
                     legend.position = c(0.2, 0.8), legend.background = element_rect(fill = NA)) + labs(x = "Date", y = "Weekly number of cases") + 
  scale_y_continuous("Weekly number of cases",  sec.axis = sec_axis(~ ./5, name = "Weekly number of hospitalisations or deaths")) +
  scale_colour_manual("", values = brewer.pal(3,"Dark2"))
ggsave(last_plot(), file = "figs/cases_time.png", width = 14, height = 10)
ggsave(last_plot(), file = "figs/cases_time.pdf", width = 14, height = 10)


dfplot.month <- dfplot
dfplot.month[, month := ifelse(year(date) == 2021, month(date)+12, month(date))]
dfplot.month <- dfplot.month[, .(n = sum(n)), by = .(month, type)]

ggplot(dfplot, aes(x = week, y = n, fill = type)) + geom_bar(stat = "identity", position = "dodge2", width = 0.5) 

ggplot(moh, aes(x = week, y = n)) + geom_bar(stat = "identity")
ggplot(sivep, aes(x = week, y = n, fill = type)) + geom_bar(stat = "identity")

# --- Genomics + cases --- #

gen <- read.csv("lineages_week.csv")
setDT(gen)
gen[, date := as.Date(date)]
setnames(gen, old = "N", new = "n")

gen <- gen[, -c("X")]
gen.total <- gen[, .(n = sum(n)), by = date]
gen.total <- rbind( gen.total |> left_join(dfplot[type == "Municipal Bulletin", .(date, ncases = n, type)]),
                    gen.total |> left_join(dfplot[type == "Corona São Caetano", .(date, ncases = n, type)]) 
                  )
gen.total[, prop := n/ncases]

g1 <- ggplot(gen, aes(x = date, y = n, fill = lineage__autocolor)) + geom_bar(stat = "identity", colour = "black") + 
  theme_bw() + scale_x_continuous(breaks = seq(as.Date("2020-04-01"), as.Date("2021-04-01"), by = "month"),
                                  labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1),
                     legend.position = c(0.2, 0.7), legend.background = element_rect(fill = NA)) + labs(x = "Date", y = "Weekly number of cases") + 
  scale_y_continuous("Number of sequences per week") +
  scale_colour_manual("", values = brewer.pal(4,"Set2")[2:3]) + scale_fill_manual("Lineage", values = brewer.pal(5, "Accent"))

g2 <- ggplot(gen, aes(x = date, y = n, fill = lineage__autocolor)) +
  geom_line(data = gen.total, aes(x = date, y = 100*prop, colour = type), lwd = 2, inherit.aes = FALSE) +
  theme_bw() + scale_x_continuous(breaks = seq(as.Date("2020-04-01"), as.Date("2021-04-01"), by = "month"),
                                  labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position = c(0.2, 0.7), legend.background = element_rect(fill = NA)) + labs(x = "Date", y = "Weekly number of cases") + 
  scale_y_continuous("Proportion of sequenced cases (%)") +
  scale_colour_manual("", values = brewer.pal(4,"Set2")[2:3]) + scale_fill_manual("Lineage", values = brewer.pal(5, "Accent"))

g.all <- grid.arrange(g1, g2, nrow=2)
ggsave(g.all, file = "figs/lineages_number.pdf", width = 10, height = 8)
ggsave(g.all, file = "figs/lineages_number.png", width = 10, height = 8)

# --- Cases per variant --- #

lineages <- readRDS("prevalence_lineages.rds")
lineages <- lineages[, .(prev = mean(prev)), by = .(lineage__autocolor, week)]
lineages <- left_join(lineages, boletim) |> as.data.table()
lineages[, date := (as.Date("2019-12-29") + 7*(week - 1))]
lineages <- lineages[order(date, lineage__autocolor)]

lineages[, type := "Weekly cases"]
dfplot <- rbind( lineages[, .(date, lineage__autocolor, n = prev*n, type = "Weekly cases")],
                 lineages[, .(date, n = cumsum(prev*n), type = "Cumulative cases"), by = lineage__autocolor])
ggplot(dfplot, aes(x = date, y = n, fill = lineage__autocolor)) + geom_bar(stat = "identity") + theme_bw() + facet_wrap(~ type, nrow = 2, scale = "free_y") +
  scale_x_continuous(breaks = seq(as.Date("2020-04-01"), as.Date("2021-04-01"), by = "month"),
                     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) + 
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position = c(0.2, 0.8), legend.background = element_rect(fill = NA)) + labs(x = "Date", y = "Cases") + 
  scale_colour_manual("", values = brewer.pal(4,"Set2")[2:3]) + scale_fill_manual("Lineage", values = brewer.pal(5, "Accent"))
ggsave(last_plot(), file = "figs/lineages_cases.pdf", width = 14, height = 7)
ggsave(last_plot(), file = "figs/lineages_cases.png", width = 14, height = 7)


