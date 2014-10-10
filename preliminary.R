library(dplyr)
source("http://bit.ly/dasi_inference")
load(url("http://bit.ly/dasi_gss_data"))

gss <- tbl_df(gss)
gss <- select(gss, degree, tvhours) %>% filter(!is.na(degree) & !is.na(tvhours)) %>% group_by(degree)
a <- summarise(gss,num = n(), mean = mean(tvhours),sd = sd(tvhours))


mod <- lm(gss$tvhours~gss$degree)
aa <- anova(mod)
