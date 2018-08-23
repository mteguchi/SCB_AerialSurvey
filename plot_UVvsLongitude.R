#plot_UVvsLongitude


# tomo eguchi
# 12 December 2016

rm(list=ls())
library(ggplot2)
all.files <- dir('RData/large_uv_stats/', pattern = '.RData')
wf <- 10000
k <- 1
for (k in 1:length(all.files)){
	load(paste0('RData/large_uv_stats/', all.files[k]))
	date.str <- unlist(strsplit(unlist(strsplit(all.files[k], split='_'))[3], split = '.RData'))[1]
	out.filename <- paste0('Figures/uv_stats_large/uv_means_', date.str, '.png')

	p1 <- ggplot(data = uv.stats) + 
			#geom_point(aes(x = center.X, y = u.avg,
			#				color = u.SE)) + 
	 geom_segment(data = uv.stats,
                       aes(x = center.X, xend = center.X + u.avg * wf,
                           y = 0, yend = 0 + v.avg * wf),
                       arrow = arrow(length = unit(0.2, 'cm'),
                                     type = 'closed')) +
	 ylim(c(-450, 450)) + 
          ylab("current (10^-4 m/s)") + xlab("x") + 
          ggtitle(date.str) + 
          theme(plot.title = element_text(hjust = 0.5),
                legend.title = element_text(size = 10, hjust = 0.5),
                legend.text = element_text(size = 8, vjust = 0))

	ggsave(plot = p1, dpi = 1200, width = 9.74, height = 6.44, file = out.filename)

}