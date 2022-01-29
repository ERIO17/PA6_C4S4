# coursera cours4 sem4 projet
# 1. Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

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
pack_NEI <- NEI %>%
  group_by(year) %>%
  summarize(sump = sum(Emissions))
head(pack_NEI,10)

par(col.axis="blue", font.axis=2)
xyear <- c(1999, 2002, 2005, 2008)
plot(pack_NEI$year, pack_NEI$sump, type="o", pch="X", col="red", xaxt="n", xlab ="year", ylab = "PM2.5 Emissions (Tons)", main ="PM2.5 Emissions decreasing")
text(x=xyear,  par("usr")[3], 
     labels = xyear, pos = 1, xpd = TRUE)

dev.copy(png,'plot1.png')
dev.off()
