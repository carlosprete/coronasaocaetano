library(tidyverse)
library(data.table)
library(lubridate)
library(gtools)
library(nnet)
library(zoo)

df <- readRDS("SCS_data.rds")
setDT(df)
df[, date := as.Date(data_coleta)]
df[, month := ifelse(year(date) == 2021, month(date) + 12, month(date))]

lineages <- c("P.1" = "P.1", "P.2" = "P.2", "P.1.10" = "P.1","P.1.14" = "P.1", "P.1.15" = "P.1", 
              "B.1.1.28" = "B.1.1.28", "B.1.1.33" = "B.1.1.33", "B.1.1.332" = "B.1.1.33", "P.7" = "Other", "N.9" = "Other")
df <- df[lineage__autocolor != "#N/A"]
df[, lineage__autocolor := ifelse(lineage__autocolor %in% names(lineages), lineages[lineage__autocolor], "Other")]

df[, day := as.numeric(date - as.Date("2020-04-05"))]
lineage.names <- sort(unique(df$lineage__autocolor))
lineages <- 1:length(lineage.names)
names(lineages) <- lineage.names
df[, lineage.number := lineages[lineage__autocolor]]

df.intro <- df[, .(date = date[which(date == min(date))[1]]), by = lineage__autocolor]
lineage.introduction <- df.intro$date
names(lineage.introduction) <- df.intro$lineage__autocolor

#dates <- seq(as.Date("2020-04-05"), max(df$date), by = "day")
dates <- seq(as.Date("2020-04-05"), as.Date("2021-04-30"), by = "day")
dfplot <- df[, .(N = .N), by = .(lineage__autocolor, date)]

dfplot <- expand.grid(date = dates, lineage__autocolor = unique(df$lineage__autocolor)) |> left_join(dfplot)
dfplot$N[is.na(dfplot$N)] <- 0
setDT(dfplot)
dfplot[, date.introduction := lineage.introduction[lineage__autocolor]]

dfplot.total <- dfplot[, .(N = sum(N)), by = date]
samples.per.day <- sum(dfplot.total$N)/nrow(dfplot.total)
min.samples <- 15*samples.per.day
L.min <- 7


L.total <- rep(NA, nrow(dfplot.total))
for(i in 1:nrow(dfplot.total)) {
  dt <- dfplot.total[i, date]
  N.aux <- rep(NA, 45)
  for(L in 1:45) {
    #N.aux[L] <- dfplot[date >= dt - L][date <= dt + L][dt >= date.introduction]$N |> sum()
    N.aux[L] <- dfplot[date >= dt - L][date <= dt + L]$N |> sum()
  }
  L.total[i] <- max( which(N.aux >= min.samples)[1], L.min)
}
dfplot.total$L <- L.total

dfplot <- dfplot |> left_join(dfplot.total |> setnames(old = "N", new = "N.total"))

dfplot$N.window <- sapply(1:nrow(dfplot), 
                          function(x) dfplot[(date >= dfplot$date[x] - dfplot$L[x]) & (date <= dfplot$date[x] + dfplot$L[x]) & 
                                               (lineage__autocolor == dfplot$lineage__autocolor[x]), N] |> sum())

dfplot[, prev := N.window/sum(N.window), by = date]
dfplot[, N.total.window := sum(N.window), by = date]

get.week <- function(dt) {
  return(1 + floor(as.numeric(dt - as.Date("2019-12-29"))/7))
}

dfplot[, week := get.week(date)]
dfplot.week <- dfplot[, .(N = sum(N)), by = .(week, lineage__autocolor)]
dfplot.week[, date := as.Date("2019-12-29") + 7*(week - 1)]
dfplot.week[, N.total := sum(N), by = week]
dfplot.week[, prev := N/N.total]

#dfplot[, month := month(date)]
#dfplot.month <- dfplot[, .(N = sum(N)), by = .(month, lineage__autocolor)]
#dfplot.week[, date := as.Date("2019-12-29") + 7*(week - 1)]
#dfplot.week[, N.total := sum(N), by = week]
#dfplot.week[, prev := N/N.total]

# --- Plots --- #
library(RColorBrewer)

ggplot(dfplot.week, aes(x = date, y = N, fill = lineage__autocolor)) + geom_bar(stat = "identity", colour = "black") + theme_bw() + 
  scale_x_continuous(breaks = seq(as.Date("2020-04-01"), as.Date("2021-04-01"), by = "month"),
                     labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")) + 
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1),
                     legend.position = c(0.2, 0.8), legend.background = element_rect(fill = NA)) + labs(x = "Date", y = "Weekly number of sequences") + 
  scale_colour_manual("", values = brewer.pal(6,"Accent"))

write.csv(dfplot.week, "lineages_week.csv")


ggplot(dfplot, aes(x = date, y = 100*prev, fill = lineage__autocolor)) + geom_area(alpha = 0.5) + 
  geom_bar(data = dfplot.week, aes(x = date, y = 100*prev, fill = lineage__autocolor), 
           alpha = 0.5, colour = "black", stat = "identity", width = 3, inherit.aes = FALSE, lwd = 0.1) + theme_bw() +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.position = c(0.2, 0.8), legend.background = element_rect(fill = alpha('white', 0.9), colour = "black")) + 
  labs(x = "Date", y = "Lineage prevalence (%)") +
  scale_fill_manual("", values = brewer.pal(6,"Accent"))
ggsave(last_plot(), file = "figs/lineages_prev_estimate.pdf", width = 14, height = 10)
ggsave(last_plot(), file = "figs/lineages_prev_estimate.png", width = 14, height = 10)

saveRDS(dfplot, "prevalence_lineages.rds")


#ggplot(dfplot[lineage__autocolor == "P.1"], aes(x = date, y = N.total.window)) + geom_line() + geom_point(size = 2)

