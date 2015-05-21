# Plot heatmap diagrams for eigen vectors

setwd("/Users/saima/Desktop/curtailment/EigenVectors/")
evs = read.csv("BKS-ev.csv")

v = 4
ev = rbind(c(evs[1:96,v],evs[193:197,v]),
           c(evs[97:192,v],evs[193:197,v]))
heatmap(e1,Rowv=NA, Colv=NA)
image(t(e1))
