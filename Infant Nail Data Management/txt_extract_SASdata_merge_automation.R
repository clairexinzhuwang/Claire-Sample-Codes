
#######################################Install Packages############################################

# Package names
packages <- c("readxl","anytime", "tidyverse", "tidyr", "plyr", "dplyr", "data.table", "sas7bdat","ggrepel","grid")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(packages,installed_packages)

source("http://news.mrdwab.com/install_github.R")
install_github("mrdwab/SOfun")
library(SOfun)


##############Read Data from VPN, Extract from txt files and Synthesize RGB Data#################
#library("rstudioapi") 
#getSourceEditorContext()$path 

setwd("/Volumes/Amita_picture_project/picture_files/January 2022/Andriod January 2022")


list_of_files <- list.files(
  path = "/Volumes/Amita_picture_project/picture_files/January 2022/Andriod January 2022", 
  recursive = TRUE,
  pattern = "\\.txt$")
#list_of_files

list <- lapply(list_of_files, function(x) read.delim(x,sep="/", header = F))

#list_of_files[913]
#list_of_files[690]
list_of_files[100]



for (i in 1:length(list)){
  list[[i]] <- list[[i]][,-5]
  colnames(list[[i]]) <- c("R","G","B","ExposureTime","BrightnessValue")
  list[[i]]$Patient <- str_replace_all(gsub("/.*$", "", list_of_files[[i]]), "[^[:alnum:]]", "")
  list[[i]]$Date <- qdapRegex::ex_between(list_of_files[[i]], "_", "_")
  list[[i]]$`Body Region` <- str_sub(gsub("[^a-zA-Z]", "", list_of_files[[i]]), 1, str_length(gsub("[^a-zA-Z]", "", list_of_files[[i]]))-3)
  
}



rgb <- do.call(rbind.data.frame, list)
rgb <- rgb[,c(6:8,1:5)]
rgb$Date <- as.character(rgb$Date)
rgb$Date <- anytime(rgb$Date)

rgb<-rgb[
  with(rgb, order(Patient,Date)),
]

rgb$Patient <- as.numeric(rgb$Patient)

####################################Debug#########################################
#rgb[209:213,3] <- c("LeftFingernail","LeftPalm","RightFingernail","RightPalm","RightToenail")

rgb<-rgb[
  with(rgb, order(Patient,Date)),
]

#list_of_files[835]
#uniq <- unique(rgb$Patient)
#68 unique id

rgb[741,3] <- "RightFingernail"
rgb[742,3] <- "RightPalm"
rgb$`Body Region` <- sub(".*?([A-Z])", "\\1", rgb$`Body Region`)

unique(rgb$`Body Region`)
#####################################Neeta - Wide###########################################

setwd("/Volumes/Amita_picture_project/Infant_cohort_data/2022_01_13")

table002<-read.sas7bdat(file = "dftable_002.sas7bdat", debug = FALSE)
table003<-read.sas7bdat(file = "dftable_003.sas7bdat", debug = FALSE)
table004<-read.sas7bdat(file = "dftable_004.sas7bdat", debug = FALSE)
table005<-read.sas7bdat(file = "dftable_005.sas7bdat", debug = FALSE)
table010<-read.sas7bdat(file = "dftable_010.sas7bdat", debug = FALSE)
table011<-read.sas7bdat(file = "dftable_011.sas7bdat", debug = FALSE)
table017<-read.sas7bdat(file = "dftable_017.sas7bdat", debug = FALSE)
table031<-read.sas7bdat(file = "dftable_031.sas7bdat", debug = FALSE)
table032<-read.sas7bdat(file = "dftable_032.sas7bdat", debug = FALSE)
table100<-read.sas7bdat(file = "dftable_100.sas7bdat", debug = FALSE)
table175<-read.sas7bdat(file = "dftable_175.sas7bdat", debug = FALSE)

setwd("/Volumes/Amita_picture_project/Infant_cohort_data/CodeBook")

dictionary <- read_xlsx("./infant_cohort_crf_variables.xlsx")
dictionary1 <- read_xlsx("./infant_cohort_crf_variables.xlsx",sheet = 2)


table175[,2:13] <- lapply(table175[,2:13], as.Date, origin="1960-01-01")
demog <- table175[,c(16,2,3,5,6,8,9,11,12,20:23,31:34,42:45,53:56)]
pic_date <- demog[,c(1,2,4,6,8)]
date_only <- pic_date[,2:5]
x<-rowSums(is.na(date_only))
pic_date$totalTimepoints<-4-x

add <- table002[,c(9,18:26,3)]
names(add)[8] <- "birthweight"
add$LBWIDOB <- as.Date(add$LBWIDOB,origin="1960-01-01")
demog1 <- left_join(demog,add)



table017$StudyLeftDate017 <- as.Date(table017$StudyLeftDate017,origin="1960-01-01")
left <- table017[,c(7,3,11)]
names(left)<- c("id","StudyLeftDate","Reason")
demog2 <- left_join(demog1,left)

demog3<-demog2%>%
  mutate(dateDiff=StudyLeftDate-LBWIDOB)%>%
  mutate(weekNum=as.integer(dateDiff/7))

demog3$dateDiff<-as.numeric(demog3$dateDiff)
demog3$dayNum=demog3$dateDiff%%7

demog3[,c(41:53)]<-""
names(demog3)[41:53]<-paste("wk",c(1:13),sep = "")

for (i in 1:nrow(demog3)){
  for (j in 41:53){
    ifelse(j<=demog3[i, 39]+40, demog3[i,j] <- 1, demog3[i,j]<-0)
  }
}


demog3$id <- as.character(demog3$id)
wk<-demog3[,41:53]
wk <- wk %>% mutate_if(is.character, as.numeric)
demog3[,41:53]<-wk

#dat <- demog3 %>% group_by(id) %>% filter(n()>1)
x<-demog3%>%pivot_longer(cols=c(2,4,6,8))
x<-x[complete.cases(x$value), ]
#x$value<-as.Date(parse_date_time(x$value, "%m%d%y"))
x<-x[
  with(x, order(id,value)),
]
x <- x %>% dplyr::group_by(id) %>% dplyr::mutate(count = row_number())
x$name<-paste("picturedate",x$count,sep="")
x <- x[,-52]
first<-x[,c(1,50:51)]
y<-first%>%
  group_by(id)%>%
  spread(name, value)


x<-demog3%>%pivot_longer(cols=c(10,14,18,22))
x<-x[complete.cases(x$value), ]
#x<-x[
  #with(x, order(id,value)),
#]
x<-x %>% dplyr::group_by(id) %>% dplyr::mutate(count = row_number())
x$name<-paste("recent_hb",x$count,sep="")
x<-x[,-52]
second<-x[,c(1,50:51)]
z<-second%>%
  group_by(id)%>%
  spread(name, value)


x<-demog3%>%pivot_longer(cols=c(3,5,7,9))
x<-x[complete.cases(x$value), ]
x<-x[
  with(x, order(id,value)),
]
x<-x %>% dplyr::group_by(id) %>% dplyr::mutate(count = row_number())
x$name<-paste("hb_date",x$count,sep="")
x<-x[,-52]
add<-x[,c(1,50:51)]
add1<-add%>%
  group_by(id)%>%
  spread(name, value)



third<-left_join(y,z)
third <- left_join(third,add1)
third$totalTimepoints<-8-rowSums(is.na(third[,2:9]))


LBWIDOB<-unique(demog3[,c(1,35)])
gender<-unique(demog3[,c(1,26)])
reason<-unique(demog3[,c(1,37)])
gestagewk<-unique(demog3[,c(1,29)])
gestageday<-unique(demog3[,c(1,30)])
birthweight<-unique(demog3[,c(1,32)])
StudyLeftDate<-unique(demog3[,c(1,36)])
race <- unique(demog3[,c(1,28)])
hispanic <- unique(demog3[,c(1,27)])


third<-left_join(third,LBWIDOB)
third<-left_join(third,gender)
third<-left_join(third,gestagewk)
third<-left_join(third,gestageday)
third<-left_join(third,birthweight)
third<-left_join(third,StudyLeftDate)
third<-left_join(third,reason)
third<-left_join(third,race)
third<-left_join(third,hispanic)

fourth<-third%>%
  mutate(dateDiff=StudyLeftDate-LBWIDOB)%>%
  mutate(weekNum=as.integer(dateDiff/7))

fourth$dateDiff<-as.numeric(fourth$dateDiff)
fourth$dayNum=fourth$dateDiff%%7

fourth[,c(39:51)]<-""
names(fourth)[39:51]<-paste("wk",c(1:13),sep = "")

fourth<-as.data.frame(fourth)
for (i in 1:nrow(fourth)){
  for (j in 39:51){
    ifelse(j<=fourth[i, 37]+38, fourth[i,j]<-1,fourth[i,j]<-0)
  }
}

names(rgb)[1] <- "id"
rgb$id <- as.character(rgb$id)
merged <- left_join(fourth,rgb)

setwd("/Volumes/Amita_picture_project/Claire")
sub <- rgb[,1:3]
sub1 <- sub%>%
  pivot_wider(names_from = `Body Region`, values_from = Date, values_fn = list(picturedate=list))
sub2 <- col_flatten(sub1, names(which(sapply(sub1, is.list))), drop = TRUE)
sub2[,2:42] <- lapply(sub2[,2:42], anydate)
wide <- left_join(fourth,sub2)
wide <- wide[-62,]
write.csv(wide,"./wide.csv")
#####################################Neeta - Long###########################################
#pic <- fourth[,1:9]
pic <- fourth%>%pivot_longer(cols=c(2:9))
pic <- pic[,c(1,18:45)]
pic <- pic[,-28]

hb <- fourth%>%pivot_longer(cols=c(10:17))
hb <- hb[,c(1,18:45)]
hb <- hb[,-28]

hbd <- fourth%>%pivot_longer(cols=c(18:25))
hbd <- hbd[,c(1,18:45)]
hbd <- hbd[,-28]


names(hb)[28] <- "recent_hb"
names(pic)[28] <- "picturedate"
names(hbd)[28] <- "hb_date"

merged <- cbind(pic,hb[,28],hbd[,28])

merged <- merged[complete.cases(merged$picturedate), ]
merged <- merged[,c(1,28:30,2:27)]

names(rgb)[2]<-"picturedate"

#merged1 <- left_join(merged,rgb)

long <- merge(merged,rgb, all=T)
long<-long[
  with(long, order(id,picturedate)),
]
long <- long[-c(1020:1021),]
#merge <- merged1[-1019,]
write.csv(long,"./long.csv")
x <- unique(rgb$id)
y <- unique(wide$id)
#rob has neeta doesn't
#"1302471" "1306771" "1306991"
setdiff(x,y)
#neeta has rob doesn't
#"1306901" "1306961" "1306981"
setdiff(y,x)




#ignore
install.packages('epiDisplay')
library(epiDisplay)
tab1(merged1$`Body Region`, sort.group = "decreasing", cum.percent = TRUE)

install.packages('gmodels')
library(gmodels)
CrossTable(merged1$`Body Region`, merged1$id, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
table(merged1$`Body Region`)
#merged1[484:488,30] <- c("LeftFingernail","RightFingernail","RightToenail","RightPalm","LeftPalm")




#merged1$`Body Region` <- sub(".*?([A-Z])", "\\1", merged1$`Body Region`)

#grepl("^[:lower:].*$", substr(merged1$`Body Region`, 1, 1))

#for(i in 1:length(merged1$`Body Region`)){
#merged1$`Body Region`[i] <- 
#ifelse(
# grepl("^[:lower:].*$", substr(merged1$`Body Region`[i], 1, 1), fixed = FALSE), 
#merged1$`Body Region`[i] <- substring(merged1$`Body Region`[i], 2),
#merged1$`Body Region`[i] <- merged1$`Body Region`[i]
#)
#}



freq <- aggregate(data = merged1,                # Applying aggregate
                          id ~ `Body Region`,
                          function(x) length(unique(x)))
tab1(merged1$`Body Region`)

freq
names(freq)[2] <- "NumInfantsHavingPics"
uniq <- unique(rgb$id)
ggplot(freq, aes(x=`Body Region`, y=id, fill=id))+
  geom_bar(stat="identity") + ylab("# of Unique Infants Having Pictures")



longitudinal <- rgb %>% group_by(id,`Body Region`) %>% filter(n()>1)
longitudinal<-longitudinal[
  with(longitudinal, order(id,`Body Region`)),
]
x <- unique(longitudinal$id)

longitud1 <- long %>% group_by(id,`Body Region`) %>% filter(n()>1)


#visualization code
merged$week <- (as.integer((merged$picturedate - merged$LBWIDOB)/7)+1)
merged_complete <- merged[complete.cases(merged$LBWIDOB),]

aggregate(data = merged,               
          id ~ week,
          function(x) length(unique(x)))

merged$id <- as.numeric(merged$id)
#paste frequency of #of hemoglobin measurements in total in each week as vector x
x<-paste(c(7,32,28,20,25,22,16,17,16,13,6,13,5),collapse="     ")
x<-paste("# infants: 
     ", x, sep = "")
x
grob <- grobTree(textGrob(x, x=-0.025,  y=0.95, hjust=0,
                          gp=gpar(col="red", fontsize=13, fontface="italic")))


ggplot(merged, aes(x = factor(week), y = recent_hb, color = id, group = factor(id))) + 
  geom_point() + geom_line()+scale_color_gradientn(colours = rainbow(27))+xlab("Week")+
  ylab("HB")+ 
  scale_y_continuous(breaks=seq(8,18,2))+
  theme(legend.position="none")+
  ggtitle("Longitudinal recent HB by Infant")+
  annotation_custom(grob)


######neeta, discreptancy########
#definition of week number is different
#hemoglobin values are automatically sorted 
setwd("/Users/air/Downloads")
neeta<-read.sas7bdat(file = "neeta.sas7bdat", debug = FALSE)
neeta<-neeta[
  with(neeta, order(id,infant_week,recent_hb)),
]
neeta1 <- neeta[,c(1,4,5)]
claire <- merged[,c(1,31,3)]
claire<-claire[
  with(claire, order(id,week,recent_hb)),
]
names(neeta1)[2] <- "week"

library(haven)
library(arsenal)
comparison <- summary(comparedf(neeta,claire))
diff <- comparison$diffs.table
#####end#######

#ignore
merged$id <- as.factor(merged$id)
levels(merged$id)
#individual level rbc trendline over 13 weeks
p <- list()
for(i in 1:length(levels(merged$id))){
  
  p[[i]] <- ggplot(merged[merged$id == levels(merged$id)[i],], aes(x=week, y=recent_hb)) + 
    geom_point() + 
    geom_line()+
    ggtitle(levels(merged$id)[i])+
    xlab("Week")+
    ylab("HB")+ 
    scale_x_continuous(breaks=seq(0,13,1))+
    coord_cartesian(ylim = c(8, 18)) + 
    scale_y_continuous(breaks=seq(8,18,2))+
    theme(legend.position="none")
  
  
}

x<-do.call(grid.arrange,p)
x

merge$week <- as.integer((merge$picturedate - merge$LBWIDOB)/7)

ddply(merged,~week,summarise,mean=mean(recent_hb),sd=sd(recent_hb))
mean(merge$recent_hb,na.rm = T)
sd(merge$recent_hb,na.rm = T)

aggregate(data = merge,               
          id ~ week,
          function(x) length(unique(x)))
