library(jsonlite)

#data = read.csv('test/github_stats.csv')
#data1 <- read.csv('github_stats1.csv')
#data2 <- read.csv('github_stats2.csv')

data <- fromJSON("github_stats.json", flatten=TRUE)
data_clean <- data
data_clean <- data_clean[ ,!(names(data_clean) %in% cols)]

# data1 <- fromJSON("github_stats1.json", flatten=TRUE)
# data2 <- fromJSON("github_stats2.json", flatten=TRUE)


options(scipen = 999)

nrow(data)


cols <- names(data)
languages <- NULL
for(name in as.character(cols)){
  ifelse( grepl("languages", name),
          ifelse(name %in% languages,
                 NA,
                 languages <- c(languages, name)
          ),
          NA)
}
  
length(languages)

data$tcl.fact <- factor(
                        ifelse(data$openhub.stats.total_code_lines < 10000, 1, 
                               ifelse(data$openhub.stats.total_code_lines < 100000, 2, 
                                      ifelse(data$openhub.stats.total_code_lines < 1000000, 3,
                                             ifelse(data$openhub.stats.total_code_lines < 10000000, 4, 5)))), levels = 1:5, labels = c("UNDER 10k","10k-100k","100k-1m","1m-10m","OVER 10m"))

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

ldata <- data[,languages]
cols <- names(ldata)
for(name in as.character(cols)){
  # print(ldata[name])
  for (i in 1:nrow(data)){
    ldata[i,name] <- ifelse(is.na(ldata[i,name]), 0, ldata[i,name])
    
  }
}
summary(ldata)


rm(l_m_data)
l_m_data <- data[,languages]
cols <- names(l_m_data)
# matchTab <- as.data.frame(c("Count"))
matchTab <- as.data.frame(rep(0,length(cols)), row.names = cols)
for(name in as.character(cols)){
  for (i in c(1:nrow(l_m_data))){
    l_m_data[i,name] <- ifelse(is.na(l_m_data[i,name]), "FALSE", "TRUE")
  }
  l_m_data[name] <- as.factor(l_m_data[,name])
  
  res <- summary(l_m_data[name])
  matchTab[name,1] <- as.integer(substr(res[2],7,9 ))
}
matchTab$count <- as.integer(matchTab[,1])
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################



# 
# summary(data$issues.avg_closed_time)
# plot(data$issues.total_count, data$issues.avg_closed_time, ylim = c(0,300))
# 
# tab4 <- tapply(data$issues.total_count, data$issues.avg_closed_time, mean)
# 
# range(data$total_code_lines)
# 
# var <- data[1,6]
# 
# summary(data)
# 
# avg_close_over_tcl_table <- tapply(data$total_code_lines,data$issues.avg_closed_time)
# hist(avg_close_over_tcl_table )
# tab5 <- tapply(data$issues.avg_closed_time, data$total_code_lines)
# barplot(tab5)
# 
# languages <- c()
# for(v in data$languages){
#   for (lang in v){
#     ifelse(lang %in% languages,print("FOUND"),languages <- c(languages, lang))
#   }
# }
# 
# for (language in languages){
#   data[language] <- rep(0,nrow(data))
# }
# 
# d <- data$languages
# for (i in 1:length(d)){
#   for (lang in languages){
#     print(lang)
#     data[lang] <- ifelse(lang %in% d[i], 1, 0)
#   }
# }
# 
# 
# 
# tab6 <- table(data$tcl.fact,data$bus_factor)
# barplot(tab6, 
#         col = c(2,3,4,5,6))
# legend( x = 10, y = 20, legend=levels(data$tcl.fact), cex=1.2, fill = c(2,3,4,5,6))
# 
# length(levels(data$tcl.fact))
# 
# max(data$total_code_lines)
# 
# data$languages
