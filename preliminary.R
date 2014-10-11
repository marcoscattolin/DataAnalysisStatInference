library(dplyr)
source("http://bit.ly/dasi_inference")
load(url("http://bit.ly/dasi_gss_data"))

gss <- tbl_df(gss)
fullresp <- nrow(gss)

mosaicplot(gss$year~is.na(gss$degree),shade=T)
mosaicplot(gss$year~is.na(gss$tvhours),shade=T)

gss <- tbl_df(gss)
gss <- select(gss, degree, tvhours) %>% filter(!is.na(degree) & !is.na(tvhours)) %>% mutate(log10tvh = log(tvhours + 1)) %>% group_by(degree)
z <- -qnorm(.025,lower.tail=T)
summary <- summarise(gss,num = n(), 
                     tvh_mean = mean(tvhours),
                     tvh_sd = sd(tvhours),
                     lower_ci = mean(tvhours)-z*(sd(tvhours)/sqrt(n())),
                     upper_ci = mean(tvhours)+z*(sd(tvhours)/sqrt(n())))


library(ggplot2)

limits <- aes(ymax = summary$upper_ci, ymin=summary$lower_ci)
qplot(data=summary,degree,tvh_mean, col = degree, main="Means of tv hours and 95% conf.int.", ylab ="tv hours means") + geom_errorbar(limits, width = 0.7) + theme_bw()
