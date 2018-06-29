library(caTools)
library(ggplot2)
library(zoo)
library(data.table)

outliersDetection = function(x, periodsWindow = 4, threshold = 1) {
  df = as.data.table(x)
  names(df)[1] = "y"
  df[, `:=`(n = seq(1:nrow(df)),
            y1 = runmean(y, periodsWindow, endrule = "mean", align = "right"),
            y2 = runmean(y, periodsWindow, endrule = "mean", align = "left"))]
  df[,   yav := (y1 + y2)/2]
  df[,  residuals1 := y - yav]
  df[,  `:=`(outliers1 = residuals1^2/sd(yav)^2,
             yadj = yav)]
  df[outliers1 > 1, yadj := NA]
  df[, yadj := na.approx.default(yadj, na.rm = FALSE)]
  df[, residuals2 := y - yadj]
  df[, outliers2 := residuals2^2/sd(yadj)^2]
  
  print("Outliers:")
  print(df[outliers2 > threshold, .(y, round(outliers2, 2))])
  
  ggplot(df, aes(x = n)) + 
    geom_line(aes(y = y, colour = "original")) + 
    geom_line(aes(y = yav, colour = "averaged")) +
    geom_line(aes(y = yadj, colour = "smoothed")) +
    geom_point(data = df[outliers1 > threshold,], 
               aes(x = n, y = y, fill = "1st type"), colour="royalblue2", size=3) +
    geom_point(data = df[outliers2 > threshold,], 
               aes(x = n, y = y, fill = "2nd type"), colour="red", size=1.5) +
    theme_minimal() + 
    labs(title = "Outliers Detection in Time Series", 
         subtitle = paste0("periods window = ", periodsWindow, ", score > ", threshold),
         x = "Periods", y = "Value") +
    theme(axis.title.y = element_text(angle=0, vjust = 1),
          axis.title.x = element_text(hjust = 1)) + 
#    theme(legend.position=c(1, 1)) +
    scale_color_manual("Series", 
                    breaks = c("original", "averaged", "smoothed"),
                    values = c("royalblue2", "black", "red")) +
    guides(color = guide_legend(override.aes = list(linetype = "solid",
                                                color = c("black", "royalblue2", "red"))),
         fill = guide_legend(override.aes = list(shape = 19, 
                                                color = c("royalblue2", "red")))) +
    scale_fill_discrete("Outliers")
  
}
