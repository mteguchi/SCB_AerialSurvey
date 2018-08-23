#plot_detection_fcn



rm(list=ls())
library(ggplot2)
fig.save <- T

# get the fit for aerial survey:

load('RData/HR_null_1_Strata_out_2017-03-22.RData')

# plot the detection function:
b <- exp(run.hr.null.1$ddf$par['p1'])
s <- exp(run.hr.null.1$ddf$par['p2'])  # p2 is in the log scale (Jeff Laake 3/22/2017)

dhaz <- function(x, b, s){
  dens <- 1 - exp(-(x/s)^(-b))
  return(dens)
}

model <- run.hr.null.1$ddf
df <- data.frame(x = model$data$PerpDist/1000)

bin.width <- 0.015
# observed counts per bin
bins <- seq(from = 0, to = model$meta.data$width, by = bin.width)
obs.counts <- vector(mode = 'numeric', length = (length(bins)-1))
dens.center <- dhaz(bins[2:length(bins)], b, s)
exp.counts <- dim(df)[1] * dens.center/sum(dens.center)  # expected counts in each bin

for (k in 1:length(obs.counts)){
  obs.counts[k] <- length(df$x[df$x>=bins[k] & df$x<bins[k+1]])
}

df.counts <- data.frame(observed = obs.counts,
                        expected = exp.counts,
                        x = bins[2:length(bins)],
                        observed.p = obs.counts/exp.counts[1],
                        expected.p = exp.counts/exp.counts[1],
                        haz.p = dhaz(bins[2:length(bins)], b, s))

x.df <- data.frame(x = seq(from = 0, to = 0.250,
                           by = 0.05))

# ESW computation using the easy way:
# mu = Pa * w
sum.model <- summary(run.hr.null.1)
esw <- sum.model$ds$average.p * sum.model$ddf$meta.data$width
SE.esw <- sum.model$ddf$meta.data$width * sum.model$ds$average.p.se

p1 <- ggplot(data = df.counts) +
  geom_bar(aes(x = x-bin.width, y = observed.p),
           width = bin.width,
           stat = "identity",
           color = 'black',
           fill = 'white') +
  stat_function(data = x.df, fun = dhaz,
                args = list(b = b, s = s),
                lwd = 2,
                col = 'black') +
  #geom_vline(xintercept = esw,
  #           size = 1.2, color = 'black')+
  scale_y_continuous(breaks = c(0.0, 0.25, 0.5, 0.75, 1.0)) +
  scale_x_continuous(breaks = c(0.0, 0.05, 0.10, 0.15, 0.20, 0.25),
                     labels = c('0', '50', '100', '150',
                                '200', '250')) +
  ylab('Detection probability') +
  xlab('Perpendicular distance (m)') +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

dpi <- 300
if (fig.save){
  ggsave(plot = p1,
         dpi = dpi,
         file = paste0('Figures/detection_fcn_', dpi, "dpi_",
                       Sys.Date(), '.png'),
         width = 6.5,
         height = 3.44,
         units = 'in')

}




