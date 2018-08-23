#Cc_Strandings_2017.R

rm(list=ls())

library(ggplot2)
library(ggmap)
library(viridis)
library(cowplot)

source('CcSCB_functions.R')

runDate <- Sys.Date()
save.fig <- F
internet <- T

#infile <- 'data/Stranding_Query_Loggerheads_March2017.txt'
infile <- 'data/CcStrandingQuery_16March2017.csv'
dat0 <- read.table(infile, sep = ",", header = TRUE)
dat1 <- subset(dat0, Alive_Released == 'FALSE' &
                 !is.na(Latitude))

dat1$yr.fac <- as.factor(dat1$Year_Initially_Observed)

# find how many were in N of Pt Conception since 1997
dat1.N <- subset(dat1, Latitude >= 34.45) %>%
  subset(., Year_Initially_Observed > 1996)


# dat1.state <- dat1[, c('State', 'yr.fac',
#                        'Species_Code', 'Latitude', 'Longitude')]
# colnames(dat1.state) <- c('State', 'Year',
#                           'Species_Code', 'Latitude', 'Longitude')
dat0.state <- dat0[dat0$State != '', ]
dat0.state$Year <- as.factor(dat0.state$Year_Initially_Observed)

p1 <- ggplot(data = dat0.state) +
  geom_bar(aes(x = Year, fill = State)) +
  #qplot(yr.fac, data = dat1.fishery, geom = "bar", fill = STATE) +
  scale_y_continuous(breaks = seq(0, 17, 1)) +
  ylab('Counts') + xlab('Year') +
  ggtitle('Stranded loggerhead turtles') +
  theme(axis.text.x = element_text(angle = 90,
                                   size = 15, vjust = 0.5))

dat1.size <- dat1[, c('State', 'yr.fac', 'Species_Code',
                      'Latitude', 'Longitude',
                      'Weight',
                      'Curved_Carapace_Length',
                      'Straight_Carapace_Length')]
#                      'Fishery_Interaction')]
# get rid of one without state info:
#dat1.size <- dat1.size[dat1.size$State != '',]

colnames(dat1.size) <- c('State', 'Year', 'Species_Code',
                         'Latitude', 'Longitude',
                         'Weight',
                         'Curved_Carapace_Length',
                         'Straight_Carapace_Length')

if (internet){
  West.coast <- get_map(location = c(lon = -138.0,
                                     lat = 43.0),
                        zoom = 4,
                        maptype = "satellite",
                        color = 'bw',
                        source = 'google')
  saveRDS(West.coast, file = 'RData/CC_stranding_westcoast.RData')

  So.Cal <- get_map(location = c(lon = -119.0,
                                 lat = 33),
                    zoom = 7,
                    maptype = "satellite",
                    color = 'bw',
                    source = 'google')
  saveRDS(So.Cal, file = 'RData/CC_stranding_SoCal.RData')
} else {
  West.coast <- readRDS(file = 'RData/CC_stranding_westcoast.RData')
  SoCal <- readRDS(file = 'RData/CC_stranding_SoCal.RData')
  print('read from RData')
}

map.west.coast <- ggmap(West.coast)
map.So.Cal <- ggmap(So.Cal)

# coast.line <- getCoastLine('~/R/OceanDepths/coast/coast_Epac.txt',
#                            lon.limits = c(-152, -116),
#                            lat.limits = c(31, 60))
#
# coast.line.df <- do.call(rbind, coast.line)
# colnames(coast.line.df) <- c('X', 'Y', 'idx')
# coast.line.Sp <- latlon2sp(coast.line.df, center.UTM)

p2 <-map.west.coast +
  geom_point(data = dat1.size,
             aes(x = Longitude, y = Latitude,
                 color = Year),
             size = 4) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.5, end = 1.0) +
  xlab("Longitude") +
  ylab("Latitude") +
  # geom_point(data = xy.df,
  #            aes(x = newX, y = newY, color = Year),
  #            shape = 19, size = 2) +
  # scale_color_viridis(discrete = TRUE,
  #                     name = 'Year') +
  # geom_point(data = survey.df,
  #            aes(x = newX, y = newY),
  #            shape = 4) +
  #ggtitle("Loggerhead turtles") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0),
        legend.position = c(0.15, 0.4))


dat.locs.So.Cal <- subset(dat1.size, Latitude < 34.45 & Longitude > -122)
p3 <-   map.So.Cal +
  geom_point(data = dat.locs.So.Cal,
             aes(x = Longitude,
                 y = Latitude,
                 color = Year),
             size = 3) +
  scale_color_viridis(discrete = TRUE, begin = 0.5, end = 1.0) +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle("Loggerhead turtles") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0),
        legend.position = c(0.90, 0.6))


# p2 <- ggplot(data = dat1.size) +
#   geom_histogram(stat = 'bin',
#                  aes(x = Curved_Carapace_Length, fill = Latitude),
#                  binwidth = 5)
#
# +
#   scale_fill_gradient(low = 'blue',
#                       high = 'red')
#
# +
#   ggtitle('Oceanic Nino Index') +
#   annotate('rect', xmin = 2001, xmax = 2002,
#            ymin = -Inf, ymax = Inf, alpha = 0.3) +
#   annotate('rect', xmin = 1997, xmax = 1999,
#            ymin = -Inf, ymax = Inf, alpha = 0.3) +
#   annotate('rect', xmin = 1992, xmax = 1994,
#            ymin = -Inf, ymax = Inf, alpha = 0.3) +
#   theme(plot.title = element_text(hjust = 0.5),
#         axis.text.x = element_text(size = 11, angle = 90),
#         axis.text.y = element_text(size = 12))


dat.Cali <- readr::read_csv('Data/NPCc_Size_Dist_2015BioCon.csv')

dat.size <- na.omit(data.frame(Year = dat1.size$Year,
                               CCL = dat1.size$Curved_Carapace_Length,
                               state = dat1.size$State))
p4 <- ggplot() +
  geom_histogram(data = dat.size,
                 aes(x = CCL),
                 binwidth = 5,
                 color = 'black',
                 fill = 'white') +
  #xlab('CCL (cm)') +
  ylab('Frequency') +
  ggtitle('USA') +
  xlim(10, 100) +
    # scale_x_discrete()
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12))

p5 <- ggplot() +
  geom_histogram(data = dat.Cali,
                 aes(x = CCL),
                 binwidth = 5,
                 color = 'black',
                 fill = 'white') +
  xlab('CCL (cm)') +
  ylab('Frequency') +
  ggtitle('Mexico') +
  xlim(10, 100) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

p4.5 <- plot_grid(p4, p5,
                  align = 'v',
                  ncol = 1,
                  rel_heights = c(1, 1))

dat.Cali$set <- 2
dat.size$set <- 1
dat.size.anova <- rbind(dat.size[, c('CCL', 'set')], dat.Cali)
dat.size.anova$set <- as.factor(dat.size.anova$set)
lm.size <- lm(data = dat.size.anova, CCL ~ set - 1)
anova(lm.size)   # significant difference between them- although unbalanced ANOVA

if (save.fig){
  ggsave(filename = paste0('figures/Cc_strandings_',
                           runDate, '.png'),
         plot = p1,
         width = 8,
         height = 7,
         dpi = 1200)

  ggsave(filename = paste0('figures/Cc_strandings_westcoast_',
                           runDate, '.png'),
         plot = p2,
         width = 9.4,
         height = 8.4,
         dpi = 1200)

  ggsave(filename = paste0('figures/Cc_strandings_SCB_',
                           runDate, '.png'),
         plot = p3,
         width = 9.4,
         height = 8.4,
         dpi = 1200)

  ggsave(filename = paste0('figures/Cc_strandings_USvsMX_',
                           runDate, '.png'),
         plot = p4.5,
         width = 9.4,
         height = 8.4,
         dpi = 1200)


}