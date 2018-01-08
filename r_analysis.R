library(jsonlite)
library(MASS)
# install.packages("ggplot2")
library(plyr)

#data = read.csv('test/github_stats.csv')
#data1 <- read.csv('github_stats1.csv')
#data2 <- read.csv('github_stats2.csv')

data <- fromJSON("final_dataset.json", flatten=TRUE)

# data1 <- fromJSON("github_stats1.json", flatten=TRUE)
# data2 <- fromJSON("github_stats2.json", flatten=TRUE)

options(scipen = 999)

nrow(data)

# Get Languages
##################################################################################################
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

langs_c <- gsub("languages.", "", languages)



# make data frame without languages
##################################################################################################

data_clean <- data
data_clean <- data_clean[ ,!(names(data_clean) %in% languages)]


# Set Total Code Lines factor
##################################################################################################

data$tcl.fact <- factor(
                        ifelse(data$openhub.stats.total_code_lines < 10000, 1, 
                               ifelse(data$openhub.stats.total_code_lines < 100000, 2, 
                                      ifelse(data$openhub.stats.total_code_lines < 1000000, 3,
                                             ifelse(data$openhub.stats.total_code_lines < 10000000, 4, 5)))), levels = 1:5, labels = c("UNDER 10k","10k-100k","100k-1m","1m-10m","OVER 10m"))


tab11 <- table(data$tcl.fact)

p7 <- barplot(tab11,
              main = "Total Code Lines Factor Distribution",
              xlab  = "Total Code Lines Category",
              ylab = "% - of Total Projects",
              ylim = c(0,max(tab11)+5),
              col = 172)
text(p7,tab11+3,labels=paste(tab11, "%", sep=""),cex=1)

# Visualize Total Code Lines by Average Closed Time
##################################################################################################

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

# Make Laguage Matrix and counter
##################################################################################################

par(mfrow=c(1,1))

ldata <- data[,languages]
cols <- names(ldata)
for(name in as.character(cols)){
  # print(ldata[name])
  for (i in 1:nrow(data)){
    ldata[i,name] <- ifelse(is.na(ldata[i,name]), 0, ldata[i,name])
    
  }
}
rm(l_m_data)
l_m_data <- data[,languages]
cols <- names(l_m_data)
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

df1 <- data.frame(
  language = row.names(matchTab),
  count = rep(0)
)
for(lang in df1$language){
  df1[df1$language == lang,"count"] = matchTab[lang,2]
}

ncol(l_m_data)
for (i in 1:nrow(l_m_data)){
  
}


# Make top 10 languages
##################################################################################################

top15bar <- min(tail(sort(df1$count), n=15))
top15 <- df1[df1$count >= top15bar,]

top15$language_c <- gsub("languages.", "", top15$language)

bp <- barplot(top15$count,
              axes = F,
              main = "Top 15 Languages",
              ylim = c(0,100),
              col = "darkgreen")

axis(side = 2, at = seq(0,100, by = 10))
axis(side = 1, at = bp, labels = top15$language_c, las = T)
text(bp,top15$count+3,cex=.9, labels = top15$count)



# Make 15 popular languages
##################################################################################################
pop_langs <- c("languages.JavaScript",
               "languages.Python",
               "languages.Java",
               "languages.C++",
               "languages.C",
               "languages.Ruby",
               "languages.PHP",
               "languages.CSS",
               "languages.C#",
               "languages.Go",
               "languages.TypeScript",
               "languages.Shell",
               "languages.Swift",
               "languages.Objective-C"
               )

# Count pop langs per project
##################################################################################################
data_clean$pop_lang_count <- rep(0)

l_m_data[1,pop_langs]
for (i in 1:100){
  count <- 0
  for(j in pop_langs){
    # print(l_m_data[i,j] == F)
    # print(count)
    if (l_m_data[i,j] == T){
      count <- count + 1
    }
  }
  print(paste(data_clean[i,"name"],count, sep = "-") )
  data_clean[i,"pop_lang_count"] <- count
}

# analyze and plot pop langs
##################################################################################################

summary(data_clean$pop_lang_count)
sd(data_clean$pop_lang_count)


tab14 <- table(data_clean$pop_lang_count)
bp1 <- barplot(tab14,
               main = "Popular Languages Count Distribution",
               xlab = "Number Of Popular Languages Found in Project",
               ylab = "Number of project",
               ylim = c(0,max(tab14) + 5),
               col = "orange")
text(bp1,tab14 + 2, labels = tab14, cex = 1.2)

df2 <- data.frame(
  popl = data_clean[data_clean$PAI.fact != "Not Available","pop_lang_count"],
  BF = data_clean[data_clean$PAI.fact != "Not Available","BF"],
  TCL = data_clean[data_clean$PAI.fact != "Not Available","tcl.fact"],
  PAI = data_clean[data_clean$PAI.fact != "Not Available","PAI.fact"],
  LF = data_clean[data_clean$PAI.fact != "Not Available","license_f"]
)

nrow(df2)
df2$PAI_l <- as.numeric(df2$PAI)
tab15 <- table(df2$popl,df2$PAI_l)
cor.test(df2$popl,as.numeric(df2$PAI), method = "spearman")

plot(df2$PAI_l,df2$popl)
addmargins(tab15)



# Set PAI factor
##################################################################################################

data_clean$openhub.stats.PAI <- factor(ifelse(data_clean$openhub.stats.PAI == 'N/A',"Not Available",data_clean$openhub.stats.PAI))

summary(data_clean$openhub.stats.PAI)
names(data_clean$openhub.stats.PAI)

data_clean$PAI.fact <- factor(
  ifelse(data_clean$openhub.stats.PAI == "Inactive", 1, 
         ifelse(data_clean$openhub.stats.PAI == "Very Low", 2, 
                ifelse(data_clean$openhub.stats.PAI == "Low", 3,
                       ifelse(data_clean$openhub.stats.PAI == "Moderate",4,
                              ifelse(data_clean$openhub.stats.PAI == "High",5,
                                     ifelse(data_clean$openhub.stats.PAI == "Very High",6,7)))))),
  levels = 1:7,
  labels = c("Inactive", "Very Low","Low","Moderate","High","Very High", "Not Available"))

data_clean[,c("PAI.fact","openhub.stats.PAI")]
summary(data_clean$PAI.fact)

# Plot PAI factor
##################################################################################################
tab8 <- table(data_clean$PAI.fact)[1:6]
PAI_P <- format(tab8[1:6]/82*100,digits = 2)
p4 <- barplot(tab8,
              main = "Project Activity Indicator Distribution",
              ylab = "Count",
              xlab = "Activity Indicator",
              ylim = c(0,max(tab8)+10),
              col = "27")
text(p4,tab8-4,labels=tab8,cex=.9)
text(p4,tab8-2,labels=paste(PAI_P, "%", sep="" ) ,cex=1)
table(tab8/82*100)

# Plot ACT 
##################################################################################################
tab9 <- table(data_clean$issues.avg_closed_time)

p5 <- barplot(data_clean[,"issues.avg_closed_time"],
              main = "Project Average Closed Time Distribution",
              ylab = "Count",
              xlab = "Average Close Time",
              ylim = c(0,2200),
              col = "29")
text(p5,tab9,labels=tab9,cex=.9)

range(data_clean$issues.avg_closed_time)

# Plot TCL
##################################################################################################
data_clean$openhub.stats.total_code_lines <- ifelse(data_clean$openhub.stats.total_code_lines < 0 , data_clean$openhub.stats.total_code_lines * -1, data_clean$openhub.stats.total_code_lines)
summary(data_clean[data_clean$openhub.stats.total_code_lines > 1,"openhub.stats.total_code_lines"])
tab10 <- data_clean$openhub.stats.total_code_lines
sd(data_clean$issues.avg_closed_time)
p6 <- barplot(tab10)


df4 <- data.frame(
  TCL = data_clean[data_clean$PAI.fact != "Not Available","tcl.fact"],
  PAI = factor(as.numeric(data_clean[data_clean$PAI.fact != "Not Available","PAI.fact"]),
               levels = 1:6,
               labels = c("Inactive", "Very Low","Low","Moderate","High","Very High"))
)
tab16 <- table(df4$PAI,df4$TCL)
addmargins(tab16)

chisq.test(tab16)

# Set ACT factor
##################################################################################################

data_clean$act.fact <- factor(
  ifelse(data_clean$issues.avg_closed_time < 8, 1, 
         ifelse(data_clean$issues.avg_closed_time < 31, 2, 
                ifelse(data_clean$issues.avg_closed_time < 186, 3,4))), levels = 1:4, labels = c("Under 1 Week","Under 1 Month","Under 6 Monthes","Over 6 Monthes"))


# Plot ACT by PAI
##################################################################################################

par(mfrow=c(1,3))

df <- data.frame(
  PAI = data_clean[!(data_clean$PAI.fact == "Not Available")& !is.na(data_clean$PAI.fact), "PAI.fact"],
  ACT = data_clean[!(data_clean$PAI.fact == "Not Available")& !is.na(data_clean$PAI.fact), "act.fact"])

nrow(df)

t1 <- table(df$PAI)
p1 = barplot(t1,
             main = "Project Activity Indicator Distribution",
             ylab = "Frequency",
             xlab = "PAI",
             col  = "cyan" )
text(p1,1,labels=t1,cex=.9)

tab <- tapply(data_clean$issues.avg_closed_time, data_clean$PAI.fact, mean)
mACT_PAI <- barplot(tab,
                    main = "Average Closed Time mean by Project Activity Indicator",
                    xlab = "Project Activity Indicator",
                    ylab = "Average Closed Time",
                    ylim = c(0,150),
                    col = 22)
text(mACT_PAI, tab-2.5, labels = format(round(tab, 0)), cex=1.1)

data_clean$act_scaled <- scale(data_clean$issues.avg_closed_time)
summary(data_clean$act_scaled)
brks = c(0.0:10.0)
p2 = plot(data_clean[data_clean$issues.avg_closed_time < 300 ,"issues.avg_closed_time"],
          xlim = c(0,100),
          ylime = c(0,300))
axis(1, at = seq(0, 300, by = 15), las=2)

abline(v = mean(data_clean$issues.avg_closed_time))
# 
# t2 <- table(df$ACT)
# p2 = barplot(t2,
#              main = "Average Closed Time Distribution",
#              ylab = "Frequency",
#              xlab = "ACT",
#              col  = "blue" )
# text(p1,1,labels=t2,cex=.9)

table(df$PAI,df$ACT)

par(mfrow=c(1,1))

tab <- table(df$PAI,df$ACT)
p3 <- barplot(tab,
             legend.text = levels(df$PAI),
             main = "Distribution of \"Project Activity Indicator\" over the \"Average Closed Time\"",
             xlab = "Average Closed Time",
             ylab = "Project Activity indicator",
             col = c(10:16))
text(p3,y=1,labels=t2,cex=.9)
  
# Get Licenses
##################################################################################################


for (i in 1:100){
  print(ifelse(length(data_clean$openhub.stats.licenses[[i]]) > 0,data_clean$openhub.stats.licenses[[i]],NA))
  data_clean[i,"license"] = ifelse(length(data_clean$openhub.stats.licenses[[i]]) > 0,data_clean$openhub.stats.licenses[[i]],NA)
}
data_clean$license <- ifelse(grepl(".*GNU.*2.*",data_clean$license, perl=T),
                             "GNU General Public License v2.0 or later",
                             ifelse(grepl(".*GNU.*3.*",data_clean$license, perl=T), "GNU General Public License v3.0 or later", data_clean$license))
data_clean$license <- as.factor(data_clean$license)
tab12 <- table(data_clean$license)
barplot(tab12,
        main = "License Distribution",
        xlab = "License type",
        ylab = "count",
        col = c(1:length(tab12)),
        legend.text = names(tab12)
        )

length(data_clean[!is.na(data_clean$license),"license"])

# Plot Licenses
##################################################################################################

# Pie Chart with Percentages
slices <- tab12 
lbls <- names(tab12)
pct <- paste(round(slices/sum(slices)*100), "%", sep="")
leg <- paste(lbls, "-", pct) # add percents to labels 
p1 <- pie(slices,
          labels = pct,
          col=rainbow(length(lbls)),
          main="License Distribution")
legend("topright",
       legend = leg,
       fill = rainbow(length(lbls)),
       cex=.7)

# Factor Licenses
##################################################################################################
levels(data_clean$license)

restrictive <- c("GNU General Public License v2.0 or later",
                 "GNU General Public License v3.0 or later",
                 "GNU Library or \"Lesser\" GPL (LGPL)",
                 "Creative Commons Attribution 3.0",
                 "Initial Developer's PUBLIC LICENSE Version 1.0",
                 "Artistic License 2.0",
                 "GPL 2",
                 "Vim License"
                 )

permissive <- c("Apache License 2.0",
                "BSD 2-clause \"FreeBSD\" License",
                "BSD 3-clause \"New\" or \"Revised\" License",
                "BSD 4-clause (University of California-Specific)",
                "MirOS Licence",
                "Python Software Foundation License 2.1.1",
                "Commercial License",
                "Eclipse Public License 1.0",
                "ISC License",
                "MIT License"
                )

data_clean$license_f <- as.factor(ifelse(data_clean$license %in% restrictive, 1,
                               ifelse(data_clean$license %in%  permissive, 0, NA)))


# set Bus Factor
##################################################################################################

data_clean$BF <- rep(0)

for(i in 1:100){
  list1 <- data_clean[i,"contributors.list"]
  
  print(length(list1[[1]]))
  
  if(length(list1[[1]]) > 0){  
    
    # print(list1)
    
    sum_list1 <- sum(list1[[1]])
    
    # print(sum_list1)
    
    list_p <- c()
    
    for (j in list1[[1]]){
      p <- as.numeric(format(j/sum_list1*100, nsmall = 2))
      list_p <- c(list_p, p)
    }
    
    sum(list_p)
    list_p_s <- sort(list_p, decreasing = T)
    # print(list_p_s)
    count <- 0
    ind <- 1
  
    while(count < 75){
      count <- count + list_p_s[ind]
      ind <- ind + 1
    }
    # print(paste("BUS FACTOR - ", ind))
    
    data_clean[i,"BF"] <- ind
  } else {
    data_clean[i,"BF"] <- 1
  }
}

# Plot Bus Factor
##################################################################################################
bf <- data_clean$BF

tab13 <- table(bf)
summary(bf)
hist(tab13,
     breaks = 0:40,
     col = 7,
     main = "Bus Factor Distribution",
     xlab = "Bus Factor",
     ylab = "Count")
sd(bf)

cor.test(df2$BF,as.numeric(df2$PAI), method = "spearman")

# chi squered test for license and PAI
##################################################################################################

df3 <- data.frame(
  TCL = data_clean[data_clean$PAI.fact != "Not Available","tcl.fact"],
  PAI = factor(as.numeric(data_clean[data_clean$PAI.fact != "Not Available","PAI.fact"]),
               levels = 1:6,
               labels = c("Inactive", "Very Low","Low","Moderate","High","Very High")),
  LF = factor(data_clean[data_clean$PAI.fact != "Not Available","license_f"], levels = 0:1, labels = c("Permissive", "Restrictive"))
)


tab17 <- table(df3[!is.na(df3$LF),c("PAI", "LF")])
addmargins(tab17)

chisq.test(tab17)

tab18 <- table()
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################
###############################################################################################################################################################################################################


# GENERATE FINAL DATASET
names(data_clean)
DS <- data.frame(
  owner = data$owner,
  name = data$name,
  creation_date = data$created_at,
  issues.total_count = data$issues.total_count,
  issues.avg_closed_time = data$issues.avg_closed_time,
  # total_code_lines = factor(
  #   ifelse(data$openhub.stats.total_code_lines < 10000, 1, 
  #          ifelse(data$openhub.stats.total_code_lines < 100000, 2, 
  #                 ifelse(data$openhub.stats.total_code_lines < 1000000, 3,
  #                        ifelse(data$openhub.stats.total_code_lines < 10000000, 4, 5)))), levels = 1:5, labels = c("UNDER 10k","10k-100k","100k-1m","1m-10m","OVER 10m")),
  total_code_lines = data$openhub.stats.total_code_lines,
  PAI = factor(
    ifelse(data_clean$openhub.stats.PAI == "Inactive", 1, 
           ifelse(data_clean$openhub.stats.PAI == "Very Low", 2, 
                  ifelse(data_clean$openhub.stats.PAI == "Low", 3,
                         ifelse(data_clean$openhub.stats.PAI == "Moderate",4,
                                ifelse(data_clean$openhub.stats.PAI == "High",5,
                                       ifelse(data_clean$openhub.stats.PAI == "Very High",6,7)))))),
    levels = 1:7,
    labels = c("Inactive", "Very Low","Low","Moderate","High","Very High", "Not Available")),
  popular_languages_count = data_clean$pop_lang_count,
  BF = data_clean$BF,
  license = data_clean$license,
  license_f = data_clean$license_f
)

DSM <- cbind(DS, l_m_data)
dim(DSM)
write.csv2(DSM,file = "final_dataset.csv", col.names = T)
