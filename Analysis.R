# This code is for visualization and analysis
library(ggplot2)

var_mean = data.frame()

# list of the file names - choose according to the requirement
file_list <- c("variable_wall_0.csv", "variable_wall_0.1.csv", "variable_wall_0.2.csv", 
               "variable_wall_0.3.csv", "variable_wall_0.4.csv", "variable_wall_0.5.csv", 
               "variable_wall_0.6.csv", "variable_wall_0.7.csv", "variable_wall_0.8.csv", 
               "variable_wall_0.9.csv", "variable_wall_1.csv") # variable wall
#file_list <- c("fixed_wall_0.csv", "fixed_wall_0.1.csv", "fixed_wall_0.2.csv",
#               "fixed_wall_0.3.csv", "fixed_wall_0.4.csv", "fixed_wall_0.5.csv",
#               "fixed_wall_0.6.csv", "fixed_wall_0.7.csv", "fixed_wall_0.8.csv") #fixed wall
#file_list <- c("variable_wall_1.csv", "variable_wall_1_2.csv", "variable_wall_1_3.csv", 
#               "variable_wall_0.9.csv") # roughly compare 3 repeats with weight of 1 to the one with weight of 0.9

# calculate the mean for each day with different weights
for (file in file_list) {
  var_data <- read.csv(file)[,2:7]
  for (i in unique(var_data$day)){
    sub = subset(var_data, day == i)
    var_mean = rbind(var_mean, c(i, file, mean(sub$latency)))
  }
}
colnames(var_mean) = c("day","distance cell weight","latency")
var_mean$latency = as.numeric(var_mean$latency)
ggplot(data=var_mean, mapping = aes(x = day, y = latency, group = `distance cell weight`, color = `distance cell weight`))+
  geom_point()+
  geom_line() +
  labs(
    title = "Model performance under different weight of distance cell with variable platform",
    x = "Day",
    y = "Latency"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),  
    panel.grid.minor = element_blank(),  
    legend.position = "bottom", 
    legend.title = element_text(face = "bold"), 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  )


var_all = data.frame()

# combine all the data
j = 0
for (file in file_list) {
  var_data <- read.csv(file)[,2:7]
  for (i in unique(var_data$day)){
    j = j+1
    sub = subset(var_data, day == i)
    for (m in sub$latency){
      var_all = rbind(var_all, c(j, i, file, m))
    }
  }
}
colnames(var_all) = c("each", "day","weight","latency")
var_all$latency =  as.numeric(var_all$latency)
# select data on Day 8
var_sub = subset(var_all, day == 8)
model = aov(latency~weight, var_sub)
summary(model)

tukey <- TukeyHSD(model)
tukey = as.data.frame(tukey$weight)
tukey$pair = rownames(tukey)
sign = subset(tukey, `p adj` < 0.05)
# draw the tukey test results which are significant 
ggplot(sign, aes(colour=cut(`p adj`, c(0, 0.01, 0.05), 
                             label=c("p<0.01","p<0.05")))) +
  theme_bw(base_size = 16)+
  geom_hline(yintercept=0, lty="11", colour="grey30",size = 1) +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size = 1) +
  geom_point(aes(pair, diff),size = 3) +
  labs(colour="", title = "Model performance difference of fixed platform",
         x = "", y = "p adj")+
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 8, color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) 
