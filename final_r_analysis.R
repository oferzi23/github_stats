myData <- read.csv("final_dataset.csv", sep=';')

names(myData)[23] <- "languages.C++"
names(myData)[24] <- "languages.Objective.C++"
names(myData)[45] <- "languages.C#"

summary(myData$total_code_lines)

myData <- myData[myData$PAI != "Not Available",]
myData$creation_date <- as.Date.character(myData$creation_date)
myData$license_f <- factor(ifelse(is.na(myData$license_f), 0, myData$license_f), levels = 0:1, labels = c("Permissive", "Restrictive"))


#####################################################
# make list of Languages                            #
# ----------------------                            #
#####################################################

cols <- names(myData)
languages <- NULL
for(name in as.character(cols)){
  ifelse( grepl("languages\\.", name, perl = T),
          ifelse(name %in% languages,
                 NA,languages <- c(languages, name)),NA)
}

length(languages)

langs_c <- gsub("languages.", "", languages)


#####################################################
# Factor PAI                                        #
# ----------                                        #
#####################################################

myData$PAI <- factor(
ifelse(myData$PAI == "Inactive", 1, 
         ifelse(myData$PAI == "Very Low", 2, 
                ifelse(myData$PAI == "Low", 3,
                       ifelse(myData$PAI == "Moderate",4,
                              ifelse(myData$PAI == "High",5,
                                     ifelse(myData$PAI == "Very High",6,7)))))),
  levels = 1:7,
  labels = c("Inactive", "Very Low","Low","Moderate","High","Very High", "Not Available"))

#####################################################
# Factor total code lines                           #
# -----------------------                           #
#####################################################

myData$total_code_lines = factor(
    ifelse(myData$total_code_lines < 10000, 1,
           ifelse(myData$total_code_lines < 100000, 2,
                  ifelse(myData$total_code_lines < 1000000, 3,
                         ifelse(myData$total_code_lines < 10000000, 4, 5)))), levels = 1:5, labels = c("UNDER 10k","10k-100k","100k-1m","1m-10m","OVER 10m"))

#####################################################
# Plot PAI                                          #
# --------                                          #
#####################################################

levels(myData$PAI)
tab8 <- table(myData$PAI)[1:6]
PAI_P <- format(tab8[1:6]/82*100,digits = 2)
p4 <- barplot(tab8,
              main = "Project Activity Indicator Distribution",
              ylab = "Count",
              xlab = "Activity Indicator",
              ylim = c(0,max(tab8)+10),
              col = "27")
text(p4,tab8-4,labels=tab8,cex=.9)
text(p4,tab8-2,labels=paste(PAI_P, "%", sep="" ) ,cex=1)

#####################################################
# Plot Average Closed Time                          #
# ------------------------                          #
#####################################################

summary(myData$issues.avg_closed_time)
sd(myData$issues.avg_closed_time)
table(myData$issues.total_count,myData$issues.avg_closed_time)

#####################################################
# Plot Total Code Lines                             #
# ---------------------                             #
#####################################################

summary(myData$total_code_lines)

tab1 <- table(myData$total_code_lines)
p1 <- barplot(tab1,
              main = "Total Code Lines Factor Distribution",
              xlab  = "Total Code Lines Category",
              ylab = "% - of Total Projects",
              ylim = c(0,max(tab1)+5),
              col = 172,
              cex.names = 1.2)
text(p1,tab1+3,labels=paste(round(tab1/sum(tab1)*100), "%", sep=""),cex=1.2)
text(p1,tab1+1,labels=tab1,cex=1.2)

#####################################################
# Plot License                                      #
# ------------                                      #
#####################################################

summary(myData$license)


tab2 <- table(myData$license)
slices <- tab2
lbls <- names(tab2)
pct <- paste(round(slices/sum(slices)*100), "%", sep="")
leg <- paste(lbls, "-", pct) # add percents to labels 
p1 <- pie(slices,
          labels = pct,
          col=rainbow(length(lbls)),
          main="License Distribution")
legend("topright",
       legend = leg,
       fill = rainbow(length(lbls)),
       cex=1)

tab3 <- table(myData$license_f)
slices2 <- tab3
pct2 <- paste(round(slices2/sum(slices2)*100), "%", sep="")
lbls2 <- c("Permissive", "Restrictive")
leg <- paste(lbls2, "-", pct2) # add percents to labels 
p2 <- pie(slices2,
          labels = pct2,
          col=c(4,5),
          main="License Distribution")
legend("center",
       legend = leg,
       fill = c(4,5),
       cex=1.3)

#####################################################
# Plot Bus Factor                                   #
# ---------------                                   #
#####################################################

bf <- myData$BF

tab13 <- table(bf)
summary(bf)
h1 <- hist(tab13,
       breaks = 0:40,
       col = 7,
       main = "Bus Factor Distribution",
       xlab = "Bus Factor",
       ylab = "Count",
       cex = 1.2)

sd(bf)


#####################################################
# Make Languages Matrix                             #
# ---------------------                             #
#####################################################

l_m_data <- myData[,languages]
cols <- names(l_m_data)
matchTab <- data.frame(
  count = rep(0,length(cols)), row.names = cols
)
for(name in as.character(cols)){
  res <- summary(l_m_data[name])
  matchTab[name,"count"] <- ifelse(!is.na(as.integer(substr(res[3],7,9 ))), as.integer(substr(res[3],7,9 )), 0)
}

df1 <- data.frame(
  language = row.names(matchTab),
  count = rep(0)
)
for(lang in df1$language){
  df1[df1$language == lang,"count"] = matchTab[lang,"count"]
}


#####################################################
# Plot Top 15 Languages                             #
# ---------------------                             #
#####################################################

top15bar <- min(tail(sort(df1$count), n=15))
top15 <- df1[df1$count >= top15bar,]

top15$language_c <- gsub("languages.", "", top15$language)

bp <- barplot(top15$count,
              axes = F,
              main = "Top 15 Languages",
              ylim = c(0,100),
              col = "darkgreen")

axis(side = 2, at = seq(0,100, by = 10), cex = 1.2)
axis(side = 1, at = bp, labels = top15$language_c, las = T, cex = 1.2)
text(bp,top15$count-2,cex=1.2, labels = top15$count)
text(bp,top15$count+3,cex=1.2, labels = paste(round(top15$count/nrow(myData)*100), "%", sep = ""))

#####################################################
# Make Popular Languages list                       #
# ---------------------------                       #
#####################################################

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

#####################################################
# Count pop langs per project                       #
# ---------------------------                       #
#####################################################

myData$popular_languages_count <- rep(0)

l_m_data[1,pop_langs]
for (i in 1:100){
  count <- 0
  for(j in pop_langs){
    if (l_m_data[i,j] == T){
      count <- count + 1
    }
  }
  print(paste(myData[i,"name"],count, sep = "-") )
  data_clean[i,"pop_lang_count"] <- count
}

#####################################################
# analyze and plot pop langs                        #
# --------------------------                        #
#####################################################

summary(myData$popular_languages_count)
sd(myData$popular_languages_count)


tab14 <- table(myData$popular_languages_count)
bp1 <- barplot(tab14,
               main = "Popular Languages Count Distribution",
               xlab = "Number Of Popular Languages Found in Project",
               ylab = "Number of project",
               ylim = c(0,max(tab14) + 5),
               col = "orange",
               cex.axis = 1.2,
               cex.names = 1.2)
text(bp1,tab14 + 1, labels = tab14, cex = 1.2)
text(bp1,tab14 + 3, labels = paste(round(tab14/nrow(myData)*100), "%", sep = ""), cex = 1.2)

#####################################################
# correlation test languages and PAI                #
# ----------------------------------                #
#####################################################

cor.test(myData$popular_languages_count, as.integer(myData$PAI), method = "spearman")

#####################################################
# correlation test bus factor and PAI               #
# -----------------------------------               #
#####################################################

cor.test(myData$BF, as.integer(myData$PAI), method = "spearman")

#####################################################
# correlation test total code lines and PAI         #
# -----------------------------------------         #
#####################################################

cor.test(as.integer(myData$total_code_lines), as.integer(myData$PAI), method = "spearman")

#####################################################
# correlation test license factor and PAI           #
# ---------------------------------------           #
#####################################################

cor.test(as.integer(myData$license_f), as.integer(myData$PAI), method = "spearman")
levels(myData$license_f)

#####################################################
# correlation test Avearage Close Time and PAI      #
# --------------------------------------------      #
#####################################################

cor.test(myData$issues.avg_closed_time, as.integer(myData$PAI), method = "spearman")

