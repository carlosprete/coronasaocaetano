library(tidyverse)
library(data.table)
library(lubridate)
library(gtools)
library(ggrepel)
library(gridExtra)
library(latex2exp)
get.week <- function(dt) {
  return(1 + floor(as.numeric(dt - as.Date("2019-12-29"))/7))
}

df <- readRDS("SCS_data.rds")
setDT(df)
df[, date := as.Date(data_coleta)]
df[, month := ifelse(year(date) == 2021, month(date) + 12, month(date))]
df[, week := get.week(date)]


dfplot.space <- df %>% group_by(bairro) %>% summarise(npos = n(), nseq = sum(!is.na(lineage__autocolor)))
dfplot.time = df %>% group_by(week) %>% summarise(npos = n(), nseq = sum(!is.na(lineage__autocolor)))

lm.space = lm(npos ~ -1 + nseq, dfplot.space)
lm.time = lm(npos ~ -1 + nseq, dfplot.time)

annotation.time <- paste0("y = ", round(lm.time$coefficients[1], 4), " x\n", "Pearson correlation", " = ", round(cor(dfplot.time$npos, dfplot.time$nseq), 4))
annotation.space <- paste0("y = ", round(lm.space$coefficients[1], 4), " x\n", "Pearson correlation", " = ", round(cor(dfplot.space$npos, dfplot.space$nseq), 4))


dfplot.space <- dfplot.space %>% mutate(npos.pred = nseq*lm.space$coefficients["nseq"])
dfplot.time <- dfplot.time %>% mutate(npos.pred = nseq*lm.time$coefficients["nseq"])


ggplot(dfplot.space, aes(x = nseq, y = npos)) + geom_point() + geom_smooth(method='lm', se=FALSE, color='black') + theme_bw() + 
  labs(x="Number of sequences per neighborhood", y="Number of PCR+ cases per neighborhood")
g.space <- ggplot(dfplot.space, aes(x = nseq, y = npos)) + geom_point(size=3) + geom_line(aes(x = nseq, y = npos.pred), lwd=1.0) + theme_bw() +
  labs(x="Number of sequences per neighborhood", y="Number of PCR+ cases per neighborhood") + geom_text_repel(aes(label=bairro)) +
   annotate('text', label=annotation.space, x=32, y=700, hjust = 0)


ggplot(dfplot.time, aes(x = nseq, y = npos)) + geom_point() + geom_smooth(method='lm', se=FALSE, color='black') + theme_bw() + 
  labs(x="Number of sequences per week", y="Number of PCR+ cases per week")
g.time <- ggplot(dfplot.time, aes(x = nseq, y = npos)) + geom_point(size=3) + geom_line(aes(x = nseq, y = npos.pred), lwd=1.0) + theme_bw() +
  labs(x="Number of sequences per week", y="Number of PCR+ cases per week") + geom_text_repel(aes(label=week)) + 
  annotate('text', label=annotation.time, x=0, y=250, hjust = 0)

g.combined <- grid.arrange(g.space, g.time, ncol=2)
ggsave(g.combined, width=12, height=6, file="figs/correlation.pdf")
ggsave(g.combined, width=12, height=6, file="figs/correlation.png")

