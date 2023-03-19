library(tidyverse)
library(data.table)
for (f in list.files("./Rt/", pattern="*.R")) {
  source(paste0("Rt/", f))
}


calculate.Rt <- function(Iday, dates, Rgrid){
  Rmin = 0.01; Rmax = 10; eta = 0.1;
  m = length(Rgrid); pR0 = (1/m)*rep(1, m)
  
  nday = length(dates); tday = 1:nday
  
  #wdist = dgamma(tday, shape = 2.3669, scale = 2.7463)
  #wdist = dchisq(tday, 3.03)
  wdist <- dlnorm(tday, meanlog=1.09, sdlog=0.72)
  Lday = rep(0, nday) 
  for(i in 2:nday){
    Lday[i] = sum(Iday[seq(i-1, 1, -1)]*wdist[1:(i-1)])    
  }
  
  eta = 0.1
  
  
  Rfilt = epiFilter(Rgrid, m, eta, pR0, nday, Lday[tday]+1E-12, Iday[tday], 0.025)
  Rsmooth = epiSmoother(Rgrid, m, Rfilt[[4]], Rfilt[[5]], nday, Rfilt[[6]], 0.025)

  return(list(Rsmooth=Rsmooth[[4]]))
}

get.week <- function(dt) {
  return(1 + floor(as.numeric(dt - as.Date("2019-12-29"))/7))
}

prev <- readRDS("prevalence_lineages.rds")

#boletim0 <- read.csv("boletim.csv") %>% select(-X)
#boletim0 <- boletim0 %>% filter(week >= 15)
#boletim <- boletim0 %>% left_join(prev)

flulike <- read.csv("flulike.csv")
setDT(flulike)
flulike[, date := as.Date(dataInicioSintomas)]
flulike <- flulike[(date >= as.Date("2020-04-06")) & (date <= as.Date("2021-04-30"))]
flulike <- flulike[municipioNotificacao == "São Caetano do Sul"]
classif.names <- c("Confirmado Clínico-Epidemiológico" = "Confirmed", 
                   "Confirmado Clínico-Imagem" = "Confirmed",
                   "Confirmado Laboratorial" = "Confirmed",
                   "Confirmado por Critério Clínico" = "Confirmed",
                   "Descartado" = "Discarded",
                   "Síndrome Gripal Não Especificada" = "Unknown")
flulike[, classificacaoFinal := ifelse(classificacaoFinal == "", "Unknown", classif.names[classificacaoFinal])]
flulike <- flulike[, .(n = .N, type = "flulike"), by = list(date, classificacaoFinal)]
flulike <- flulike[classificacaoFinal == "Confirmed"]


sms <- read.csv2("pt_BR_arquivo_analise-20211031010119.csv")
setDT(sms)
sms[, `:=`(data_coleta = as.Date(data_coleta),  dias_de_sintomas = as.numeric(dias_de_sintomas), cadastro_paciente = as.Date(cadastro_paciente))]
sms[, dias_de_sintomas := ifelse(is.na(dias_de_sintomas), 0, dias_de_sintomas)]
sms <- sms[cadastro_paciente <= as.Date("2021-04-30")]
sms[, date := as.Date(fifelse(is.na(data_coleta), cadastro_paciente - dias_de_sintomas, data_coleta - dias_de_sintomas))]

sms <- rbind(sms[, .(n = sum(resultado_PCR == "positivo"), type = "sms_positive"), "date"],
             sms[, .(n = sum(resultado_PCR == "negativo"), type = "sms_negative"), "date"])#,
sms <- sms[type == "sms_positive"]
sms <- sms[order(date)]
#sms[, week := get.week(cadastro_paciente)]
sms <- sms[, .(n = sum(n)), by = .(type, date)]


sms0 <- sms
sms <- sms %>% left_join(prev)
sms <- sms[date <= as.Date("2021-03-31")]
sms0 <- sms0[date <= as.Date("2021-03-31")]


#sms0 <- flulike
#sms <- flulike %>% left_join(prev)


Nsamples <- 1000
Rgrid = seq(0.01, 10.0, length.out = 200)

df.lin <- list()
for(lineage in unique(prev$lineage__autocolor)) {
  df.lin[[lineage]] <- sms %>% filter(lineage__autocolor == lineage) %>% select(date, n, lineage__autocolor, prev) %>% arrange(date)
  t0 <- which(df.lin[[lineage]]$prev > 0)[1]
  tf <- which(df.lin[[lineage]]$prev > 0) %>% tail(n=1)
  df.lin[[lineage]] <- df.lin[[lineage]][t0:tf,]
  Rhat <- matrix(0.0, nrow(df.lin[[lineage]]), length(Rgrid))
  for(i in 1:Nsamples) {
    print(paste0(lineage, ": Sample ", i, "/", Nsamples))
    incidence <- rbinom(rep(1, nrow(df.lin[[lineage]])), df.lin[[lineage]]$n, df.lin[[lineage]]$prev)
    result <- calculate.Rt(incidence, df.lin[[lineage]]$date, Rgrid)
    Rhat <- Rhat + result$Rsmooth/Nsamples
  }
  
  df.lin[[lineage]]$q1 <- sapply(1:nrow(Rhat), function(x) Rgrid[which(cumsum(Rhat[x,]) >= 0.025)[1]])
  df.lin[[lineage]]$q2 <- sapply(1:nrow(Rhat), function(x) Rgrid[which(cumsum(Rhat[x,]) >= 0.5)[1]])
  df.lin[[lineage]]$q3 <- sapply(1:nrow(Rhat), function(x) Rgrid[which(cumsum(Rhat[x,]) >= 0.975)[1]])
}

df.lin[["All lineages"]] <- sms0 %>% mutate(lineage__autocolor = "All lineages", prev=1.0) %>% select(date, n, lineage__autocolor, prev) %>% arrange(date)
result <- calculate.Rt(df.lin[["All lineages"]]$n, df.lin[["All lineages"]]$date, Rgrid)
Rhat <- result$Rsmooth
df.lin[["All lineages"]]$q1 <- sapply(1:nrow(Rhat), function(x) Rgrid[which(cumsum(Rhat[x,]) >= 0.025)[1]])
df.lin[["All lineages"]]$q2 <- sapply(1:nrow(Rhat), function(x) Rgrid[which(cumsum(Rhat[x,]) >= 0.5)[1]])
df.lin[["All lineages"]]$q3 <- sapply(1:nrow(Rhat), function(x) Rgrid[which(cumsum(Rhat[x,]) >= 0.975)[1]])

df <- do.call(rbind, df.lin)
df <- df %>% mutate(scale.factor = case_when(lineage__autocolor %in% c("P.1", "All lineages") ~ 0.05,
                                             lineage__autocolor == "B.1.1.28" ~ 0.1,
                                             lineage__autocolor %in% c("B.1.1.33", "P.2") ~ 1/6), 
                    incidence.plot = prev*n*scale.factor)
#df <- df %>% group_by(lineage__autocolor) %>% mutate(incidence.plot = prev*n/sum(prev*n)) %>% ungroup()

ggplot(df %>% filter(lineage__autocolor != "Other", date >= "2020-04-05", date <= "2021-04-30")) + 
  geom_area(aes(x=date, y = incidence.plot), alpha=0.5, lwd=0.0, color='grey') + geom_line(aes(x = date, y = q2, colour=lineage__autocolor)) + 
  geom_ribbon(aes(x=date, ymin=q1, ymax=q3, fill=lineage__autocolor), alpha = 0.25) +
  theme_bw() + labs(x="Date",y="Effective Reproduction Number (Rt)") +
  scale_x_continuous(breaks = seq(as.Date("2020-04-01"), as.Date("2021-04-01"), by = "month"),
                     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) +
  facet_wrap(~lineage__autocolor, ncol=1) + theme(legend.position = "NA") + scale_y_continuous(sec.axis = sec_axis(~ . , name = "Expected value of daily number of cases"))

ggsave(last_plot(), file="figs/Rt_march.pdf", width=10, height=10)


#plotEpiFilter(Rfilt[[3]][2:nday], Rfilt[[2]][, 2:nday], Ifilt[[1]], Ifilt[[2]],
#              'EpiFilter', Iday[2:nday], "./Rt/", eta)

#plotEpiFilter(Rsmooth[[3]][2:nday], Rsmooth[[2]][, 2:nday], Ismooth[[1]], Ismooth[[2]],
#              'EpiSmooth', Iday[2:nday], "./Rt/", eta)