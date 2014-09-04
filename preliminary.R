load(url("http://bit.ly/dasi_gss_data"))


summary(gss)

selected <- gss[,c("degree","natenvir")]
mosaicplot(selected$degree ~ selected$natenvir)

Q:  Is there a relationship between the highest school degree achieved
and the unmber of tv hours watched per day?