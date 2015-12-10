# So the only times both buoys were in operation together during my study was 
# 12/14/13-02/24/14, below I am testing to see if their data is compareable


MB_comp_data <- read.csv('~/Desktop/Mission Bay Buoy data for COMP.csv', stringsAsFactors = FALSE)

TP_comp_data <- read.csv('~/Desktop/Torrey Pines Buoy data for COMP.csv', stringsAsFactors = FALSE)

daily_temp <- MB_comp_data %>%
  group_by(X.YY, MM, DD) %>%
  summarise(mean_temp = mean(WTMP), max_temp = max(WTMP))

daily_temp

#that gives you mean and max daily temps rather than means and maxes for every 
#30 minutes

condensed_MBt <- unite(daily_temp, Sample_Date, X.YY:DD, sep = "/", remove = TRUE)
condensed_MBt

#This unites the year, month, and day columns into one column calles Sample_Date

condensed_MBt$Sample_Date <-ymd(condensed_MBt$Sample_Date)
condensed_MBt

#This lets R know that the column "Sample_Date" represents times rather than 
# just random characters

daily_TPtemp <- TP_comp_data %>%
  group_by(X.YY, MM, DD) %>%
  summarise(mean_temp = mean(WTMP), max_temp = max(WTMP))

daily_TPtemp

condensed_TPt <- unite(daily_TPtemp, Sample_Date, X.YY:DD, sep = "/", remove = TRUE)
condensed_TPt

condensed_TPt$Sample_Date <-ymd(condensed_TPt$Sample_Date)
condensed_TPt

#Cool, now that your data is squared away, you can begin running stats!

var.test(condensed_MBt$mean_temp, condensed_TPt$mean_temp)

#We obtained p-value greater than 0.05, so we can assume that the two 
# variances are effectively equal.

t.test(condensed_MBt$mean_temp, condensed_TPt$mean_temp)

#We obtained p-value LESS than 0.05, then we can conclude that the averages of 
#two groups are significantly different. DAMN.

#That sucks, so the temps recorded over the same time periods were different so 
#I cannot use the MB Buoy to fill in for the TP Buoy in regrads to temp.

var.test(condensed_MBt$max_temp, condensed_TPt$max_temp)
t.test(condensed_MBt$max_temp, condensed_TPt$max_temp)

#lETS TRY FOR WAVES...

daily_MBwaves <- MB_comp_data %>%
  group_by(X.YY, MM, DD) %>%
  summarise(mean_wave = mean(WVHT), max_wave = max(WVHT))

daily_MBwaves

condensed_MBw <- unite(daily_MBwaves, Sample_Date, X.YY:DD, sep = "/", remove = TRUE)
condensed_MBw

condensed_MBw$Sample_Date <-ymd(condensed_MBw$Sample_Date)
condensed_MBw

----------------------------------

  daily_TPwaves <- TP_comp_data %>%
  group_by(X.YY, MM, DD) %>%
  summarise(mean_wave = mean(WVHT), max_wave = max(WVHT))

daily_TPwaves

condensed_TPw <- unite(daily_TPwaves, Sample_Date, X.YY:DD, sep = "/", remove = TRUE)
condensed_TPw

condensed_TPw$Sample_Date <-ymd(condensed_TPw$Sample_Date)
condensed_TPw
-----------------------------------

  
var.test(condensed_MBw$mean_wave, condensed_TPw$mean_wave)
#We obtained p-value greater than 0.05, so we can assume that the two 
# variances are effectively equal.

t.test(condensed_MBw$mean_wave, condensed_TPw$mean_wave)

#lets see this graphically because I find it hard to believe:

#First for Temps:

MBtemp_mean_plot <- ggplot(data = condensed_MBt, aes(x = Sample_Date, y = mean_temp)) + geom_point() + geom_line() + theme_classic() + ggtitle("Mean Daily Temp at Mission Bay")
MBtemp_mean_plot

TPtemp_mean_plot <- ggplot(data = condensed_TPt, aes(x = Sample_Date, y = mean_temp)) + geom_point() + geom_line() + theme_classic() + ggtitle("Mean Daily Temp at Torrey Pines")
TPtemp_mean_plot

MBtemp_max_plot <- ggplot(data = condensed_MBt, aes(x = Sample_Date, y = max_temp)) + geom_point() + geom_line() + theme_classic() + ggtitle("Max Daily Temp at Mission Bay")
MBtemp_max_plot

TPtemp_max_plot <- ggplot(data = condensed_TPt, aes(x = Sample_Date, y = max_temp)) + geom_point() + geom_line() + theme_classic() + ggtitle("Max Daily Temp at Torrey Pines")
TPtemp_max_plot

#Then for Waves:

MBwave_mean_plot <- ggplot(data = condensed_MBw, aes(x = Sample_Date, y = mean_wave)) + geom_point() + geom_line() + theme_classic() + ggtitle("Mean Daily Wave Height at Mission Bay")
MBwave_mean_plot

TPwave_mean_plot <- ggplot(data = condensed_TPw, aes(x = Sample_Date, y = mean_wave)) + geom_point() + geom_line() + theme_classic() + ggtitle("Mean Daily Wave Height at Torrey Pines")
TPwave_mean_plot

MBwave_max_plot <- ggplot(data = condensed_MBw, aes(x = Sample_Date, y = max_wave)) + geom_point() + geom_line() + theme_classic() + ggtitle("Max Daily Wave Height at Mission Bay")
MBwave_max_plot

TPwave_max_plot <- ggplot(data = condensed_TPw, aes(x = Sample_Date, y = max_wave)) + geom_point() + geom_line() + theme_classic() + ggtitle("Max Daily Wave Height at Torrey Pines")
TPwave_max_plot
