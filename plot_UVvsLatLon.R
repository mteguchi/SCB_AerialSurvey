#plot_UVvsLongitude


# tomo eguchi
# 12 December 2016

rm(list=ls())
source('CcSCB_functions.R')
#library(ggplot2)
yrs <- 2009:2015
mos <- 1:12
das <- c(1, 7, 14, 28)
land.color <- '#333333'
alpha.value <- 0.8
wf <- 1000
k <- 1
y <- m <- d <- 1
for (y in 1:length(yrs)){
  for (m in 1:length(mos)){
    for (d in 1:length(das)){
    	date.str <- as.Date(paste0(yrs[y], '-', mos[m], '-', das[d]))
    	print(date.str)
    	sst <- extract.cov.large.bkgd('jplMURSST',
    	                              date.str,
    	                              center.UTM)

    	load(paste0('RData/large_uv_stats/uv_stats2_',
    	            date.str, '.RData'))
    	out.filename <- paste0('Figures/uv_stats_large/uv_stats2_',
    	                       date.str, '.png')
    	uv.stats2 <- na.omit(uv.stats)
    	p1 <- ggplot() +
    	  geom_raster(data = sst,
    	              aes(x =X, y = Y, fill = mean)) +
    	  scale_fill_gradient(limits = c(10, 30),
    	                      low = "blue",
    	                      high = "red") +
    	  geom_polygon(fill = land.color,
    	               data = all.islands.df,
    	               aes(x=newX, y=newY, group = name),
    	               inherit.aes = F) +
    	  stat_contour(data = sst,
    	               aes(x=X, y=Y, z=mean),
    	               breaks = 20,
    	               colour = "gray") +
    	  stat_contour(data = sst,
    	               aes(x=X, y=Y, z=mean),
    	               breaks = 24,
    	               colour = "gold") +
    	  #geom_point(aes(x = center.X, y = u.avg,
    	  #				color = u.SE)) +
    	  geom_segment(data = uv.stats2,
    	               aes(x = center.X, xend = center.X + u.avg * wf,
    	                   y = center.Y, yend = center.Y + v.avg * wf),
    	               arrow = arrow(length = unit(0.2, 'cm'),
    	                             type = 'closed')) +
    	  ylab("y") + xlab("x") +
    	  ggtitle(date.str) +
    	  theme(plot.title = element_text(hjust = 0.5),
    	        legend.title = element_text(size = 10, hjust = 0.5),
    	        legend.text = element_text(size = 8, vjust = 0))

    	ggsave(plot = p1, dpi = 1200,
    	       width = 9.74, height = 6.44,
    	       file = out.filename)
    }
  }
}