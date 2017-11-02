Xlabels <- as.Date(paste(seq(1960, 2015, 10), "-01-01", sep = ""))
Xbreaks <- as.numeric(as.Date(paste(seq(1960, 2015, 10), "-01-01", sep = "")))
p <- ggplot()
#p <- p + geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), col = "red", size = 1.5)
p <- p + geom_point(data = dfrA, aes(x = as.numeric(as.Date(date)), y = value, col = source), size = 1, alpha = 0.7)
p <- p + facet_grid(Name ~ ., scales = "free", space = "free")
#p <- p + theme(legend.position='none')
p <- p + guides(fill=guide_legend(title="source"))
#p <- p + ylab("runoff (mm/year)") + xlab("", break = Xbreaks, label = Xlabels)
p <- p + scale_x_continuous(name = "date", breaks = Xbreaks, labels = Xlabels, 
                            limits = c(as.numeric(as.Date("1958-01-01")), as.numeric(as.Date("2015-12-31"))))
p <- p + scale_y_continuous(name = "swe (mm)")
p <- p + theme(axis.text=element_text(size=12), 
               axis.title=element_text(size=14,face="bold"), 
               strip.text.y = element_text(size = 10, face = "bold", colour = "black", angle = 270))

p <- p + scale_colour_manual(values=viridis(4))
#p <- p + xlim(as.Date("1958-01-01"), as.Date("2015-12-31"))

ggsave(plotName, plot = p, width = 24, height = 12.00, units = "in")
