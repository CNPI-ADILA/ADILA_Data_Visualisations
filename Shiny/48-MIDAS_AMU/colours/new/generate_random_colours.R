# there are many ways / packages to do this (https://statisticsglobe.com/create-distinct-color-palette-in-r)
# one approach below:

#install.packages("randomcoloR")
library("randomcoloR")
set.seed(1983765)
n <- 421
colours <- distinctColorPalette(n) # last done for the antimicrobial list
pie(rep(1, n),
    col = colours,
    main = "randomcoloR Package")
# export
colours <- as.data.frame(distinctColorPalette(n))
write.csv(colours, "./random_colours.csv", row.names = F)