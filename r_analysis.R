data = read.csv('github_stats.csv')
data$languages <- strsplit(as.character(data$languages), '/')

data[1,4]

var <- data[1,6]
strsplit(, "/")
