library(dplyr)
load(url("http://bit.ly/dasi_gss_data"))


summary(gss)

selected <- gss[,c("degree","tvhours")]
boxplot(selected$tvhours ~ selected$degree)

selected <- group_by(selected,degree)


par(mfrow = c(2,1))
hist(selected$tvhours,main = "Distribution of variable 'tvhours'", xlab="TV hours watched on average day")
barplot(table(selected$degree), main = "Distribution of variable 'degree'")


mean(selected$tvhours,na.rm = T)
sd(selected$tvhours,na.rm = T)

write.csv(selected,"selected.csv")
