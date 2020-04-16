regVar <- c("AGL_Gruneisen_parameter", "Debye_temp", "AGL_thermal_conductivity")
#names(test_cset)
str(data[, regVar])
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = data[, regVar], 
            y = data$AGL_thermal_conductivity, 
            plot = "scatter", 
            layout = c(3, 1))

