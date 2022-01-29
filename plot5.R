# coursera cours4 sem4 projet
# 5. How have emissions from motor vehicle sources changed from 1999–2008 
# in Baltimore City?

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

# Identify Vehicule types & set up data for plotting
pattern <- '.*Highway Veh - .*'
matches <- data.frame(unique(grep(pattern, 
                                  SCC$Short.Name, ignore.case = TRUE, value=TRUE)))
View(xmatches)
summary(matches)
colnames(matches) <- c("col1")
xmatches <- as.vector(matches %>% filter(!grepl("NOT USED", col1)))

class(xmatches)
summary(xmatches)

ma_fonction <- function(x) {
  y <- gsub('-', '',str_extract(x, "-.*-.*-"))
  return(y)
}
SCC_SCC <- SCC[which(SCC$Short.Name %in% xmatches[["col1"]]),]
head(SCC_SCC)
SCC_SCC1 <- SCC_SCC$SCC
SCC_SCC2 <- data.frame(apply(SCC_SCC["Short.Name"],1,ma_fonction))
SCC_SCC3 <- cbind(SCC_SCC1, SCC_SCC2)
colnames(SCC_SCC3) <- c("SCC", "Vehicule")
head(SCC_SCC3)
NEI_motor <- NEI[which(NEI$SCC %in% SCC_SCC1),]
NEI_motor_id <- left_join(NEI_motor,SCC_SCC3,by="SCC")

dev.cur()

# filtering Baltimore data & group by Vehicule + year and calculate total emission
pack_NEI <- NEI_motor_id %>%
  filter(fips == "24510") %>%
  group_by(Vehicule, year) %>%
  summarize(PM2.5_Emissions = sum(Emissions))
head(pack_NEI,10)

p <- ggplot(pack_NEI, aes(x = year, y = PM2.5_Emissions, color=as.factor(Vehicule))) + 
  geom_point() + geom_smooth(method="lm")
p + ggtitle("Motor vehicule PM2.5 Emissions in Baltimore") + ylab("PM2.5 Emissions (Tons)") + 
  scale_x_discrete(name ="Year", limits=c(1999,2002,2005,2008))

dev.copy(png,'plot5.png')
dev.off()
