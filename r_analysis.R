data = read.csv('github_stats.csv')
options(scipen = 999)

data$languages <- strsplit(as.character(data$languages), '/')


data$tcl.fact <- factor(
                        ifelse(data$total_code_lines < 10000, 1, 
                               ifelse(data$total_code_lines < 100000, 2, 
                                      ifelse(data$total_code_lines < 1000000, 3,
                                             ifelse(data$total_code_lines < 10000000, 4, 5)))), levels = 1:5, labels = c("UNDER 10k","10k-100k","100k-1m","1m-10m","OVER 10m"))
#labels = c("UNDER 10k","10k-100k","100k-1m","1m-10m","OVER 10m")

par(mfrow=c(1,3))

tab <- table(data$tcl.fact)
p <- barplot(tab, col = 3, main = "Total Lines of Code", ylab = "Projects", xlab = "Lines of Code")
text(p,1,labels=tab,cex=.9)

tab3 <- tapply(data$issues.avg_closed_time,data$tcl.fact, mean)
bp = barplot(tab3,
             main = "Average Issue Close Time By Total Code Lines",
             xlab = "Code Lines",
             ylab = "Average Issue Close Time (Days)",
             col = 7)
text(bp,tab3-1.5,labels=format(round(tab3, 0), nsmall = 2),cex=.9)

tab2 <- table(data$issues.avg_closed_time)
p2 <- hist(data[data$issues.avg_closed_time < 2000,4],
           main = "Average Issue Close Time",
           xlab = "Close Time (days)",
           ylab = "Frequency",
           col = '5')

par(mfrow=c(1,1))

plot(data$issues.total_count, data$issues.avg_closed_time, ylim = c(0,300))

tab4 <- tapply(data$issues.total_count, data$issues.avg_closed_time, mean)

range(data$total_code_lines)

var <- data[1,6]

summary(data)

avg_close_over_tcl_table <- tapply(data$issues.avg_closed_time, data$total_code_lines)
plot(data$issues.avg_closed_time, data$total_code_lines)
max(data$total_code_lines)

data$languages
