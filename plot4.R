# coursera cours4 sem4 projet
# 4. Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?

getwd()
list.files()

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip?accessType=DOWNLOAD"
destfile <- "./exdata-data-NEI_data.zip"
if(!file.exists("./exdata-data-NEI_data.zip")) {
  download.file(fileUrl,
                destfile,
                mode="wb") # "wb" means "write binary," and is used for binary files
  
}
list.files()
if(!file.exists("./Source_Classification_Code.rds") | !file.exists("./summarySCC_PM25.rds")) {
  unzip(zipfile = "exdata-data-NEI_data.zip") # unpack the files 
}
list.files()

# lecture fichier
##This first line will likely take a few seconds
NEI <- readRDS("summarySCC_PM25.rds")
head(NEI,10)
summary(NEI)
class(NEI)

SCC <- readRDS("Source_Classification_Code.rds")
head(SCC,10)
summary(SCC)

# Identify Coal sources & set up data for ploting

pattern <- '.*Comb.*Coal.*'
matches <- data.frame(unique (grep(pattern, 
                                   SCC$Short.Name, ignore.case = TRUE, value=TRUE)))
colnames(matches) <- c("col1")
xmatches <- as.vector(matches %>% filter(!grepl("Total", col1)))


ma_fonction <- function(x) {
  y <- gsub('/', '',str_extract(x, "/.*/.*/"))
  return(y)
}
SCC_SCC <- SCC[which(SCC$Short.Name %in% xmatches[["col1"]]),]
head(SCC_SCC)
SCC_SCC1 <- SCC_SCC$SCC
SCC_SCC2 <- data.frame(apply(SCC_SCC["Short.Name"],1,ma_fonction))
SCC_SCC3 <- cbind(SCC_SCC1, SCC_SCC2)
colnames(SCC_SCC3) <- c("SCC", "Comb_rel_src")
head(SCC_SCC1)
NEI_coal <- NEI[which(NEI$SCC %in% SCC_SCC1),]
NEI_coal_id <- left_join(NEI_coal,SCC_SCC3,by="SCC")

dev.cur()

# group by coal source + year and calculate total emission
pack_NEI <- NEI_coal_id %>%
  group_by(Comb_rel_src, year) %>%
  summarize(PM2.5_Emissions = sum(Emissions))
head(pack_NEI,16)
str(pack_NEI)

p <- ggplot(pack_NEI, aes(x = year, y = PM2.5_Emissions, color=as.factor(Comb_rel_src))) + 
  geom_point() + geom_smooth(method="lm")
p + ggtitle("Coal CombustionPM2.5 emissions related source") + ylab("PM2.5 Emissions (Tons)") + 
  scale_x_discrete(name ="Year", limits=c(1999,2002,2005,2008))


dev.copy(png,'plot4.png')
dev.off()
