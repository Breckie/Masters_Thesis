library(ggplot2)
library(tidyr)
library(dplyr)

kelp <- read.csv('Desktop/benthics_2016.csv')

kelp <- gather(kelp, key = time, value = counts, T0:T8)

colnames(kelp)[4] <- "Number of Individuals"
colnames(kelp)[3] <- "Time"
colnames(kelp)[2] <- "Density"


kelp$size

head(kelp)
  
kelp %>% 
  filter(size == 'large') %>%
  ggplot(data = ., aes(x = Time, y = `Number of Individuals`, colour = Density, group = Density)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.title.x = element_text (size = 35), 
        axis.title.y = element_text (size = 35),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 35)
  ) +
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.25, size = 1.5) + 
  stat_summary(fun.y = mean, geom = 'point', size = 4) + 
  stat_smooth(method = 'lm') + 
  scale_y_continuous(breaks = round(seq(min(0), max(8), by = 2),0))

kelp %>% 
  filter(size == 'medium') %>%
  ggplot(data = ., aes(x = Time, y = `Number of Individuals`, colour = Density, group = Density)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.title.x = element_text (size = 35), 
        axis.title.y = element_text (size = 35),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 35)
        ) +
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.25, size = 1.5) + 
  stat_summary(fun.y = mean, geom = 'point', size = 4) + 
  stat_smooth(method = 'lm')

natural <- read.csv('Desktop/benthics_2016_natural.csv', stringsAsFactors = FALSE)
natural <- natural[-87, ]

natural <- mutate(natural, total = mac + other)
natural$Date <- factor(natural$Date, levels = c('July', 'August', 'September', 'January'))

natural <- gather(natural, key = kelp, value = counts, mac:total)

all_kelp_plot <-   
ggplot() + 
  geom_violin(data = filter(natural, kelp == 'total'), aes(x = Date, y = counts), 
              colour = 'black', fill='grey') + 
theme_classic() +  
theme(axis.text.x = element_text(size = 25, colour = 'black'),
      axis.text.y = element_text(size = 25, colour = 'black'),
      axis.title.x = element_text (size = 25, colour = 'black'), 
      axis.title.y = element_text (size = 25, colour = 'black')
)
all_kelp_plot

all_plus_mac_plot_july <- 
  all_kelp_plot + 
  geom_violin(data = filter(natural, kelp == 'mac', Date == 'July'), aes(x = Date, y = counts), 
            colour = 'black', fill='black')
all_plus_mac_plot_july

all_plus_mac_plot_july_aug <- 
  all_kelp_plot + 
  geom_violin(data = filter(natural, kelp == 'mac', Date %in% c('July', 'August')), aes(x = Date, y = counts), 
              colour = 'black', fill='black')
all_plus_mac_plot_july_aug

all_plus_mac_plot_july_aug_sep <- 
  all_kelp_plot + 
  geom_violin(data = filter(natural, kelp == 'mac', Date != 'January'), aes(x = Date, y = counts), 
              colour = 'black', fill='black')
all_plus_mac_plot_july_aug_sep

all_plus_mac_plot_all_months <- 
  all_kelp_plot + 
  geom_violin(data = filter(natural, kelp == 'mac'), aes(x = Date, y = counts), 
              colour = 'black', fill='black')
all_plus_mac_plot_all_months

head(natural)

january_tags <- 
filter(natural, Date == 'January') %>% 
  select(Tag)


filter(natural, Tag %in% january_tags$Tag) %>% 
  spread(key = Date, value = counts) %>%
  mutate(aug_survival = August / July, 
         sep_survival = September / August, 
         jan_survival = January / September) %>% 
  filter(kelp == 'mac') %>% 
  gather(key = month, value = survival, aug_survival:jan_survival) %>% 
  ggplot(data = ., aes(x = month, y = survival)) + 
    geom_point() + 
    geom_line(aes(group = Tag))
    

colnames(natural)[3] <- "Patch Density (#/m)"

colnames(natural)[3] <- "counts"
head(natural)

kelp <- read.csv('Desktop/benthics_2016.csv')
str(kelp)

survival <- 
kelp %>% 
  mutate(T1p = T1 / T0, 
         T2p = T2 / T1, 
         T3p = T3 / T2, 
         T4p = T4 / T3, 
         T5p = T5 / T4, 
         T6p = T6 / T5, 
         T7p = T7 / T6, 
         T8p = T8 / T7)

survival[is.nan(survival)] <- 0

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

survival[is.nan(survival)] <- 0

survival


