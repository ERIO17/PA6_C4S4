# coursera cours4 sem4 projet
# 3. Of the four types of sources indicated by the type\color{red}{\verb|type|}
# type (point, nonpoint, onroad, nonroad) variable, which of these four 
# sources have seen decreases in emissions from 1999–2008 for Baltimore
# City? Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

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


dev.cur()
# group by rype + year and calculate total emission
pack_NEI <- NEI %>%
  group_by(type, year) %>%
  summarize(PM2.5_Emissions = sum(Emissions))
head(pack_NEI,16)

p <- ggplot(pack_NEI, aes(x = year, y = PM2.5_Emissions, color=as.factor(type))) + 
  geom_point() + geom_smooth(method="lm")
p + ggtitle("PM2.5 emissions by type of source") + ylab("PM2.5 Emissions (Tons)") +
 scale_x_discrete(name ="Year", limits=c(1999,2002,2005,2008))

dev.copy(png,'plot3.png')
dev.off()
