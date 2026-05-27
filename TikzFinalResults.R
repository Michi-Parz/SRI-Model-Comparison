library(tikzDevice)

tikzi <- theme(
  legend.background = element_rect(fill = "transparent"),
  legend.spacing.y = unit(-1.5, "cm"),
  axis.text = element_text(size = 15),
  axis.text.x = element_text(hjust = 0.7),
  axis.title = element_text(size = 15),
  plot.title = element_text(size = 15),
  legend.text = element_text(size = 15),
  legend.title = element_text(size = 15, margin = margin(b = 5)),
  panel.grid.major = element_line(linewidth = 1))


xlag_sri <- "SRIX"
ylag_sri <- "SRIY"

plot_madj <- 4.2

# Modellkurven-------------------------------------------

gg_tikz_save <- ggModelCurves+
  tikzi+
  labs(x = xlag_sri, y = ylag_sri)


gg_tikz_save
tikz("Tikz/Modellkurve.tex", width = plot_madj, height = plot_madj)
gg_tikz_save
dev.off()



# Mahalanobis-------------------------------------------


## Alle Kurven-----------------------------

gg_tikz_save <- ggplot(maha_all_df)+
  aes(x = Method, y = sqrt(sq_maha), fill = Method)+
  geom_boxplot()+
  scale_fill_manual(breaks = c("Bayesian", "Frequentist", "Chi"), values = c(col_vec, "grey"))+
  labs(y = "Mahalanobis distance")+
  theme(legend.position = "none")+
  tikzi



gg_tikz_save
tikz("Tikz/MahlanobisAll.tex", width = plot_madj, height = plot_madj)
gg_tikz_save
dev.off()



## Je Wand-----------------------------

gg_tikz_save <- ggplot(sqmaha_dist_build)+
  aes(x = Building, y = sqrt(SQMahalanobis ), fill = Method)+
  geom_boxplot()+
  scale_fill_manual(values = col_vec)+
  labs(y = "Mahalanobis distance", x = "Wall number")+
  theme(legend.position = "bottom")+
  scale_x_discrete(
                     minor_breaks = 1:27,
                     breaks = seq(3,27,3),
                     labels = seq(3,27,3)
  )+
  tikzi


gg_tikz_save
tikz("Tikz/MahlanobisBuilding.tex", width = plot_madj+3, height = plot_madj)
gg_tikz_save
dev.off()



# Fit Parameter-----------------------------

gg_tikz_save <- ggarrange(gg_ei+tikzi+labs(y = "YYe"),
                          gg_C+tikzi,
                           gg_cL+tikzi+labs(y = "YYc / m/s"),
                          gg_gamma+tikzi+labs(y = "YYg"))


gg_tikz_save
tikz("Tikz/Parameter/FitParameters.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()

## StDev-----------------------------

gg_tikz_save <- ggplot(stdev_df)+
  aes(x = fpHz, group = Method, fill = Method, color = Method)+
  geom_line(aes(y = q50))+
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5), alpha = 1/4)+
  scale_x_continuous(trans = log2_trans(),
                     minor_breaks = c(freq,6300,8000,10000),
                     breaks = c(63,125,250,500,1000,2000,4000,8000),
                     labels = c(63,125,250,500,1000,2000,4000,8000),
  )+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = col_vec)+
  labs(y = "Standard deviation / dB",
       x = xlag_sri)+
  tikzi



gg_tikz_save
tikz("Tikz/Parameter/StDev.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()



## StDev Waterfall-----------------------------

gg_tikz_save <- ggplot(stdev_water_df) +
  aes(x = log2(StDev), y = fpHz, fill = Method)+
  geom_density_ridges2(alpha = 1/3) +
  theme(axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 15),
        panel.grid.major = element_line(linewidth = 1))+
  labs(x = bquote(log[2]~standard~deviation),
       y = xlag_sri)+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = col_vec)+
  tikzi



gg_tikz_save
tikz("Tikz/Parameter/StDevWaterfall.tex", width = plot_madj+2, height = plot_madj+2)
gg_tikz_save
dev.off()

## AR-----------------------------

gg_tikz_save <- ggplot(ar_df)+
  aes(x = fpHz, group = Method, fill = Method, color = Method)+
  geom_line(aes(y = q50))+
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5), alpha = 1/4)+
  scale_x_continuous(trans = log2_trans(),
                     minor_breaks = c(freq,6300,8000,10000),
                     breaks = c(63,125,250,500,1000,2000,4000,8000),
                     labels = c(63,125,250,500,1000,2000,4000,8000),
  )+
  labs(x = xlag_sri,
       y = "AR coefficient")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = col_vec)+
  tikzi



gg_tikz_save
tikz("Tikz/Parameter/ARcoff.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()



## AR Waterfall-----------------------------

gg_tikz_save <- ggplot(ar_water_df) +
  aes(x = AR, y = fpHz, fill = Method)+
  geom_density_ridges2(alpha = 1/3) +
  theme(axis.text.x = element_text(size = 15),
        axis.title = element_text(size = 15),
        panel.grid.major = element_line(linewidth = 1))+
  labs(x = "AR coefficient",
       y = xlag_sri)+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = col_vec)+
  tikzi



gg_tikz_save
tikz("Tikz/Parameter/ARWaterfall.tex", width = plot_madj+2, height = plot_madj+2)
gg_tikz_save
dev.off()



# Correlation-------

## Bayes vs Freq--------------------
corrplot::corrplot(corr_comp, "color", tl.col = "black",
                   title = "TTT",
                   mar = c(0,0,1,0)) #  mit Titel

corrplot::corrplot(corr_comp, "color", tl.col = "black") # Ohne Titel

row.names(corr_comp) <- colnames(corr_comp) <- paste(freq, "Hz")

corrplot::corrplot(corr_comp, "color", tl.col = "black")
tikz("Tikz/Correlation/CorrelationFB.tex", width = 4, height = 4)
corrplot::corrplot(corr_comp, "color", tl.col = "black")

dev.off()


## Bayes range------------------------------
corrplot::corrplot(
  cor_range,
  "color", tl.col = "black",
  title = bquote("Bayesian range: TTT"),
  mar = c(0,0,1,0)
)
tikz("Tikz/Correlation/BayesRange.tex", width = 4, height = 4)
corrplot::corrplot(
  cor_range,
  "color", tl.col = "black",
  title = bquote("Bayesian range: TTT"),
  mar = c(0,0,1,0)
)
dev.off()


## Freq range---------------------------
corrplot::corrplot(
  bs_corr_range,
  "color", tl.col = "black",
  title = bquote("Frequentist range: TTT"),
  mar = c(0,0,1,0)
)

tikz("Tikz/Correlation/FreqRange.tex", width = 4, height = 4)
corrplot::corrplot(
  bs_corr_range,
  "color", tl.col = "black",
  title = bquote("Frequentist range: TTT"),
  mar = c(0,0,1,0)
)

dev.off()

# RW-------------------------------


## Full---------------


gg_tikz_save <- gg_rw + tikzi + labs(y = "YYY", color = "")

gg_tikz_save
tikz("Tikz/RwFull.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()


## Zoom---------------


gg_tikz_save <- gg_rw + tikzi + labs(y = "YYY", color = "")+
  coord_cartesian(ylim = c(0.75, 1))

gg_tikz_save
tikz("Tikz/RwZoom.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()



# Bonus----------

## Residuals vs Fit and Freq-----------


gg_tikz_save <- ggarrange(fitvsres + tikzi,
          freqvsres + labs(x = xlag_sri) + tikzi,
          legend = "bottom", common.legend = T)

gg_tikz_save
tikz("Tikz/ResidualAnalysis.tex", width = plot_madj+3, height = plot_madj)
gg_tikz_save
dev.off()




## Residuals vs Freq Boxplots--------------

theme(
  legend.background = element_rect(fill = "transparent"),
  legend.spacing.y = unit(-1.5, "cm"),
  axis.text = element_text(size = 15),
  axis.text.x = element_text(hjust = 0.7),
  axis.title = element_text(size = 15),
  plot.title = element_text(size = 15),
  legend.text = element_text(size = 15),
  legend.title = element_text(size = 15, margin = margin(b = 5)),
  panel.grid.major = element_line(linewidth = 1))


gg_tikz_save <- ggWR+
  labs(x = xlag_sri)+
  tikzi+
  theme(axis.text.x = element_text(size = 12))


gg_tikz_save
tikz("Tikz/ResidualAnalysis.tex", width = plot_madj+3, height = plot_madj)
gg_tikz_save
dev.off()


## Rw Boxplots-----------


gg_tikz_save <- ggplot(rw_df)+
  aes(x = factor(Thickness), y = Rw, fill = Method)+
  geom_boxplot()+
  scale_fill_manual(values = col_vec)+
  scale_y_continuous(
    minor_breaks = seq(30,80,1),
    breaks = seq(45,65,5),
    labels = seq(45,65,5)
  )+
  scale_x_discrete(
    minor_breaks = factor((18:36)/100),
    breaks = factor(seq(0.18,0.36,0.04)),
    labels = factor(seq(0.18,0.36,0.04)))+
  theme(legend.position = "bottom",
        panel.grid.major = element_line(linewidth  = 1))+
  labs(y = "Weighted sound reduction index Rw / dB",
       x = "Thickness in m")+
  tikzi
  



gg_tikz_save
tikz("Tikz/RwBoxplots.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()




# PPC-----------

## Freq---------


gg_tikz_save <- ppc_freq+
  labs(x = xlag_sri, y = ylag_sri)+
  theme(
    legend.position = c(0.7, 0.2),
    legend.justification = c(0.5, 0.5),
    legend.background = element_rect(fill = "transparent"),
    legend.spacing.y = unit(-0.7, "cm"),
    panel.grid.major = element_line(linewidth = 1))+
  tikzi


gg_tikz_save
tikz("Tikz/PPCfreq.tex", width = plot_madj, height = plot_madj)
gg_tikz_save
dev.off()


## Freq Binom---------


gg_tikz_save <- ppcFreqBinom+
  labs(x = xlag_sri, y = "P(.Binominal - Expected. .= .OOR-Expected.)")+
  tikzi


gg_tikz_save
tikz("Tikz/PPCfreqBinom.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()


## Freq Stdev--------


gg_tikz_save <- ppcFreqSt+
  labs(x = xlag_sri)+
  tikzi


gg_tikz_save
tikz("Tikz/PPCfreqSD.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()



## Both------------



gg_tikz_save1 <- ppc_freq+
  tikzi+ labs(x = xlag_sri, y = ylag_sri)

gg_tikz_save2 <- ppcFreqSt+
  tikzi+ labs(x = xlag_sri)+
  theme(
    axis.text.x = element_text(angle =40, hjust = 1))


ggarrange(gg_tikz_save1, gg_tikz_save2, legend = "bottom", common.legend = T)
tikz("Tikz/PPCfreqBoth.tex", width = plot_madj+3, height = plot_madj+1)
ggarrange(gg_tikz_save1, gg_tikz_save2, legend = "bottom", common.legend = T)
dev.off()






## White---------


gg_tikz_save <- ppcWhite+
  tikzi


gg_tikz_save
tikz("Tikz/PPCwhite.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()


## SMD outlier---------


gg_tikz_save <- mahalanobisOutlier+
  tikzi+
  labs(x = xlag_sri, y = ylag_sri)


gg_tikz_save
tikz("Tikz/PPCsmdCurves.tex", width = plot_madj, height = plot_madj)
gg_tikz_save
dev.off()


## Rw---------


gg_tikz_save <- ppcRW+
  tikzi


gg_tikz_save
tikz("Tikz/PPCrw.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()



# Test---------------


gg_tikz_save <- gg_rw + tikzi + labs(y = "\\SRIxlab")+
  coord_cartesian(ylim = c(0.75, 1))

gg_tikz_save
tikz("Tikz/Test.tex", width = plot_madj+3, height = plot_madj+1)
gg_tikz_save
dev.off()

