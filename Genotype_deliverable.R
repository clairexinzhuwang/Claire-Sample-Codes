##purpose of code: clean and merge different format of genotypes
##project: HIV Genotype String Extraction for Analysis of Drug Resistance Mutation During Antiretroviral Therapy
##name: Claire Wang
##Date: 01/04/2022

########################Install Packages and Set Working Directory#################################
library(openxlsx)
library(readxl)
library(qdapRegex)
library(stringr)
library(dplyr)
library(stringi)
library(rlist)
library(anytime)
library(expss)
library(zoo)
library(lubridate)
library(janitor)
library(readr)

genotype="/Users/air/OneDrive - Emory University/Genotype_APE_New"
setwd(genotype)

######################Middle Format ############################################
middle11_aug18<-read_excel("./COMPLETE RT_PI_GENO_3FORMATSWITH PIVOT TABLE 81421_CW.xlsx",sheet=3)
#first extract data with complete result
middle11_aug18_1<-middle11_aug18[complete.cases(middle11_aug18$`lab comments results`), ]
#extract hiv subtype by searching the keyword
keyword<-"HIV Subtype:"
keyword1<-"HIV GENOTYPE :"
lookaround <- 1
pattern1 <- paste0(keyword, 
                   "( [[:alnum:]]+){0,",lookaround,"}")
pattern2 <- paste0(keyword, 
                   "([[:alnum:]]+){0,", lookaround, "}")

str<-middle11_aug18_1$`lab comments results`
x<-gsub("[\r\n]", "", str)

r1 <- regexpr(pattern1, x,ignore.case=TRUE)
r2 <- regexpr(pattern2, x,ignore.case=TRUE)

out1 <- rep(NA,length(x))
out2 <- rep(NA,length(x))

out1[r1!=-1] <- regmatches(x, r1)
out2[r2!=-1] <- regmatches(x, r2)
out1
out2
for (i in 1:length(out1)){
  for (j in 1:length(out2)){
    ifelse(nchar(out1[i])<14,out1[i]<-out2[j],out1[i]<-out1[i])
  }
  
}

#extract reverse transcriptase
reverse<-qdapRegex::ex_between(x, "DRUG RESISTANCE MUTATIONS DETECTED:", "Protease Gene:",ignore.case=TRUE)
reverse_novel<-qdapRegex::ex_between(x, "NOVEL MUTATIONS DETECTED:Reverse Transcriptase Gene:", "Protease Gene:",ignore.case=TRUE)
#check if slashes exist
reverse_slash<-str_count(reverse, "/")
which(reverse_slash>0)
#extract protease
protease<-qdapRegex::ex_between(x, "Protease Gene:", "NOVEL MUTATIONS DETECTED",ignore.case=TRUE)
protease
protease_novel<-qdapRegex::ex_between(x, "Protease Gene:", "HIV Subtype",ignore.case=TRUE)
protease_novel<-substring(protease_novel, regexpr("Protease Gene:", protease_novel))
protease_novel[1]
protease_slash<-str_count(protease,"/")
which(protease_slash>0)
protease_novel_slash<-str_count(protease_novel,"/")
which(protease_novel_slash>0)

#concatenate the strings
mid1<-paste("DRUG RESISTANCE MUTATIONS DETECTED:",reverse,sep="\r\n")
mid2<-paste(mid1,"Protease Gene:",sep="\r\n")
mid3<-paste(mid2,protease,sep=" ")
mid4<-paste(mid3,"Reverse Transcriptase Gene 2:",sep="\r\n")
mid5<-paste(mid4,reverse_novel,sep=" ")
mid6<-paste(mid5,"Protease Gene 2:",sep="\r\n")
protease_novel<-protease_novel %>% stringr::str_remove("Protease Gene:")
mid7<-paste(mid6,protease_novel,sep="")
mid8<-paste(mid7,out1,sep="\r\n")
middle11_aug18_1$cleanedResult<-mid8

a<-gsub("[\r\n]", "", middle11_aug18_1$cleanedResult)
RT1<-qdapRegex::ex_between(a, "DRUG RESISTANCE MUTATIONS DETECTED:", "Protease Gene:",ignore.case=TRUE)
RT2<-qdapRegex::ex_between(a, "Reverse Transcriptase Gene 2:", "Protease Gene 2:",ignore.case=TRUE)
RT2<-paste("Reverse Transcriptase Gene 2:",RT2, sep=" ")
RT<-paste(RT1,RT2,sep="\r\n")
middle11_aug18_1$`REVERSE TRANSCRIPTASE`<-RT
middle11_aug18_1$`REVERSE TRANSCRIPTASE`<-ifelse(is.na(middle11_aug18_1$cleanedResult),NA,middle11_aug18_1$`REVERSE TRANSCRIPTASE`)
p1<-qdapRegex::ex_between(a, "Protease Gene:", "Reverse Transcriptase Gene 2:",ignore.case=TRUE)
p1<-paste("Protease Gene:",p1,sep=" ")
p2<-qdapRegex::ex_between(a, "Protease Gene 2:", "HIV Subtype",ignore.case=TRUE)
p2<-paste("Protease Gene 2:",p2,sep=" ")
p<-paste(p1,p2,sep="\r\n")
middle11_aug18_1$PROTEASE<-p
middle11_aug18_1$PROTEASE<-ifelse(is.na(middle11_aug18_1$cleanedResult),NA,middle11_aug18_1$PROTEASE)
sub<-paste("HIV Subtype:",sub(".*HIV Subtype:", "", a,ignore.case=TRUE),sep="")
for(i in 1:length(sub)){
  ifelse(
    grepl("HIV",a[i],fixed = TRUE),
    sub[i]<-sub[i],
    sub[i]<-NA
    
  )
}
middle11_aug18_1$`HIV SUBTYPE`<-sub
middle11_aug18_1$`HIV SUBTYPE`<-ifelse(is.na(middle11_aug18_1$cleanedResult),NA,middle11_aug18_1$`HIV SUBTYPE`)
mid_final<-middle11_aug18_1[,c(1,5,12:14)]
names(mid_final)[1]<-"SID"
names(mid_final)[2]<-"DATE GENOTYPE"
write.csv(middle11_aug18_1,"./mid_cleaned.csv")
write.csv(mid_final,"./mid_final.csv")

#####################Integrase############################
#integrase<-read_excel("./UPDATED COMPLETE INTEGRASE GENO_82421_2015_jul2021.xlsx",sheet=2)

integrase<-read_excel("/Users/air/OneDrive - Emory University/combined integraseaug15_dec21_123021_for CW.xlsx")
t<-integrase$`Component Comments`
t<-gsub("[\r\n]", "", t)
int<-qdapRegex::ex_between(t, "Mutations Detected:", "The method used",ignore.case=TRUE)
s<-unlist(int)
k<-as.list(strsplit(s, ","))
b<-k
for (i in 1:length(k)){
  for(j in 1:length(k[[i]])){
    s<-unlist(gregexpr(pattern ='/',k[[i]][j]))+1
    m<-gsub( "/.*$", "", k[[i]][j])
    b[[i]][[j]]<-split(b[[i]][[j]],(length(m)+1))
  }
}


for (i in 1:length(k)){
  for(j in 1:length(k[[i]])){
    s<-unlist(gregexpr(pattern ='/',k[[i]][j]))+1
    m<-gsub( "/.*$", "", k[[i]][j])
    b[[i]][[j]][[1]] = m
    for(n in 1:length(s)){
      
      b[[i]][[j]][[n+1]]<-paste(substr(k[[i]][j],1,s[1]-3),substr(k[[i]][j],s[n],s[n]),sep="") 
      
    }
  }
}


c<-lapply(b, function(x) do.call(rbind, x))


for (i in 1:length(c)){
  for (j in 1:length(c[[i]])){
    ifelse(
      grepl(substr(c[[i]][j], 1, 1) , substr(c[[i]][j], 2, nchar(c[[i]][j])),fixed = TRUE),
      c[[i]][j]<-"",
      c[[i]][j]<-c[[i]][j]
    )
    
  }
  
}


for (i in 1:length(c)){
  c[[i]]<-unique(unlist(lapply(c[[i]], function(x) x[[1]])))
  
  
}



d<-vapply(c, paste, collapse = ", ", character(1L))

for (i in 1:length(d)){
  ifelse(d[i]=="",
         d[i]<-"NONE",
         d[i]<-d[i])
}


for (i in 1:length(d)){
  ifelse(substr(d[i], 1, 2)==", ",
         d[i]<-substr(d[i],3,nchar(d[i])),
         ifelse(
           substr(d[i], (nchar(d[i])-1),nchar(d[i]))==", ",
           d[i]<-substr(d[i],1,nchar(d[i])-2),
           ifelse(
             grepl(", , , ", d[i], fixed = TRUE),
             d[i]<-gsub(", , , ", ", ", d[i]),
             ifelse(
               grepl(", , ", d[i], fixed = TRUE),
               d[i]<-gsub(", , ", ", ", d[i]),
               
               d[i]<-d[i]))))
}

uniques<-unique(d)
tapply(seq_along(d), d, identity)[uniques]
for (i in 1:length(d)){
  ifelse(d[i]=="NA",
         d[i]<-NA,
         d[i]<-d[i])
}



integrase$cleanedResults<-d
names(integrase)[1]<-"SID"
names(integrase)[14]<-"INTEGRASE"

write.csv(integrase,"./int.csv")

##################Integrase Partition########################

p<-unlist(strsplit(d, ","))
p<-stri_trim(p)
q<-unique(p)
colnames<-q
q
int1<-integrase
int1[,c(11:92)]<-""
colnames(int1)[11:92]<-colnames
colnames(int1)<-as.character(colnames(int1))
int1<-as.data.frame(int1)


for(i in 1:length(c)){
  for(j in 1:length(c[[i]])){
    ifelse(is.na(c[[i]][j]),
           c[[i]][j]<-"Not",
           c[[i]][j]<-c[[i]][j]
    )
  }
}

integrase1<-int1[,-c(1:10)]


d<-integrase$INTEGRASE
for(f in 1:length(integrase1)){
  for(i in 1:length(d)){
    ifelse(
      grepl(q[f],d[i],fixed = TRUE),
      integrase1[i,f]<-1,
      integrase1[i,f]<-0
      
    )
  }
  
  
}
integrase2<-cbind(int1[,1:10],integrase1)
x<-colnames(integrase2)
x
x<-x[11:92]
y<-order(x)
x<-x[order(x)]
y<-y+10
integrase2<-integrase2[,c(1:10,y)]

write.csv(integrase2,"./int_split.csv")


#######################################Integrase Resistance#################################
intResult<-integrase[,c(1,3,9:12,14)]
intResult$ral<-""
intResult$evg<-""
intResult$dgv<-""
intResult$bitk<-""
x<-intResult$`Last Lab Results`
x<-gsub("[\r\n]", "", x)
ral<-qdapRegex::ex_between(x, "RALTEGRAVIR RESISTANCE:", "ELVITEGRAVIR RESISTANCE",ignore.case=TRUE)
#which(is.na(ral))
intResult$ral<-ral
evg<-qdapRegex::ex_between(x, "ELVITEGRAVIR RESISTANCE:", "DOLUTEGRAVIR RESISTANCE",ignore.case=TRUE)
intResult$evg<-evg
dgv<-qdapRegex::ex_between(x, "DOLUTEGRAVIR RESISTANCE:", "VALUE",ignore.case=TRUE)
intResult$dgv<-dgv
bitk<-sub(".*BICTEGRAVIR RESISTANCE:", "", x,ignore.case=TRUE)
for(i in 1:length(bitk)){
  ifelse(
    grepl("BICTEGRAVIR",x[i],fixed = TRUE),
    bitk[i]<-bitk[i],
    bitk[i]<-NA
  )
}
intResult$bitk<-bitk
write.csv(intResult,"./int_resistance.csv")

##########################################old format##########################################
old<-read_xlsx("./COMPLETE RT_PI_GENO_3FORMATSWITH PIVOT TABLE 81421_CW.xlsx",sheet=2)
a<-old$`Comment Results`
a<-gsub("[\r\n]", "", a)
a<-gsub("\\s+"," ",a)
RT1<-qdapRegex::ex_between(a, "MUTATIONS DETECTED", "Protease",ignore.case=TRUE)
for (i in 1:length(RT1)){
  for(j in 1:length(RT1[[i]])){
    RT1[[i]]<-RT1[[i]][1]
  }
}


for (i in 1:length(RT1)){
  RT1[i]<-ifelse(
    grepl(":" , substr(RT1[i], 1, 1),fixed = TRUE),
    RT1[i]<-substr(RT1[i],2,nchar(RT1[i])),
    RT1[i]<-RT1[i]
  )
}


RT2<-qdapRegex::ex_between(a, "MUTATIONS DETECTED", "Protease",ignore.case=TRUE)
for (i in 1:length(RT2)){
  
  RT2[[i]]<-RT2[[i]][2]
}

RT2<-word(RT2,3,sep = "\\:")
RT2[2257]
RT2<-paste("Reverse Transcriptase Gene 2:",RT2,sep="")


RT<-paste(RT1,RT2,sep="\r\n")
RT[112]

for (i in 1:length(RT1)){
  RT[i]<-ifelse(
    grepl("DRUG RESISTANCE" , a[i],fixed = TRUE),
    RT[i]<-RT[i],
    RT[i]<-NA
  )
}


old$`REVERSE TRANSCRIPTASE`<-RT

p1<-qdapRegex::ex_between(a, "Protease Gene:", "NOVEL",ignore.case=TRUE)
p1<-paste("Protease Gene:",p1,sep=" ")
p2<-a
for (i in 1:length(p2)){
  for(j in 1:length(p2[[i]])){
    
    p2[i]<-strsplit(a[i], "Protease Gene:")
    n=length(p2[[i]])
    p2[i]<-p2[[i]][n]
    ifelse(
      grepl("HIV" , p2[i],fixed = TRUE),
      p2[i]<-word(p2[i],1,sep = "\\HIV"),
      p2[i]<-p2[i]
    )
  }
  
}
p2<-stri_trim(p2)
p2<-paste("Protease Gene 2:",p2,sep=" ")

protease<-paste(p1,p2,sep="\r\n")
protease[872]

for (i in 1:length(protease)){
  protease[i]<-ifelse(
    grepl("DRUG RESISTANCE" , a[i],fixed = TRUE),
    protease[i]<-protease[i],
    protease[i]<-NA
  )
}
which(is.na(protease))
old$PROTEASE<-protease



keyword<-"HIV Subtype:"
lookaround <- 1
pattern1 <- paste0(keyword, 
                   "( [[:alnum:]]+){0,",lookaround,"}")
pattern2 <- paste0(keyword, 
                   "([[:alnum:]]+){0,", lookaround, "}")

str<-old$`Comment Results`
x<-gsub("[\r\n]", "", str)

r1 <- regexpr(pattern1, x,ignore.case=TRUE)
r2 <- regexpr(pattern2, x,ignore.case=TRUE)

out1 <- rep(NA,length(x))
out2 <- rep(NA,length(x))

out1[r1!=-1] <- regmatches(x, r1)
out2[r2!=-1] <- regmatches(x, r2)

for (i in 1:length(out1)){
  for (j in 1:length(out2)){
    ifelse(nchar(out1[i])<14,out1[i]<-out2[j],out1[i]<-out1[i])
  }
  
}

uniques<-unique(out1)
tapply(seq_along(out1), out1, identity)[uniques]
which(nchar(out1)>15)
out1[985]<-"HIV Subtype: B"
old$`HIV SUBTYPE`<-out1
names(old)[1]<-"SID"
write.csv(old,"./old_cleaned.csv")
old<-old[,c(1,5,10:12)]
write.csv(old,"./old_final.csv")

#############################New Format#################################
new<-read_xlsx("./COMPLETE RT_PI_GENO_3FORMATSWITH PIVOT TABLE 81421_CW.xlsx",sheet=4)
#count gene near PRB as RT, e.g.#151
str<-new$`Comment Results`
RT<-str_match(str, "(?s)NRTIs(.*?)PIs")[,1]
a<-as.list(strsplit(RT, "\r\n"))


for(i in 1:length(a)){
  for(j in 1:length(a[[i]])){
    ifelse(grepl("YES", a[[i]][j], fixed = TRUE),
           a[[i]][j]<-word(a[[i]][j],2,sep = "\\!YES!"),
           a[[i]][j]<-""
    )
    
  }
}



for(i in 1:length(a)){
  a[[i]]<-unique(unlist(lapply(a[[i]], function(x) x[[1]])))
  
}


for(i in 1:length(a)){
  for(j in 1:length(a[[i]])){
    ifelse(
      a[[i]][j]=="",
      a[[i]]<-list.remove(a[[i]],j),
      a[[i]]<-a[[i]]
    )
    
  }
}


b<-as.character(a)
b<-gsub(","," ",b)
b<-gsub("[^[:alnum:][:blank:]+?&/\\-]", "", b)


for(i in 1:length(b)){
  
  ifelse(
    b[i]=="character0",
    b[i]<-NA,
    b[i]<-b[i]
  )
  
  ifelse(
    grepl("c", substr(b[i], 1, 1), fixed = TRUE),
    b[i]<-b[i]%>%stringr::str_remove("c"),
    b[i]<-b[i]
    
  )
  
}


b<-gsub("\\s+"," ",b)
b<-strsplit(b, " ")
b<-sapply(b, unique)

c<-b

for (i in 1:length(b)){
  for(j in 1:length(b[[i]])){
    m<-gsub( "/.*$", "", b[[i]][j])
    c[[i]][[j]]<-split(c[[i]][[j]],(length(m)+1))
    
  }
}



for (i in 1:length(b)){
  for(j in 1:length(b[[i]])){
    s<-unlist(gregexpr(pattern ='/',b[[i]][j]))+1
    m<-gsub( "/.*$", "", b[[i]][j])
    c[[i]][[j]][[1]] = m
    for(n in 1:length(s)){
      
      c[[i]][[j]][[n+1]]<-paste(substr(b[[i]][j],1,s[1]-3),substr(b[[i]][j],s[n],s[n]),sep="") 
      
    }
  }
}


d<-lapply(c, function(x) do.call(rbind, x))

for (i in 1:length(d)){
  for (j in 1:length(d[[i]])){
    ifelse(
      grepl(substr(d[[i]][j], 1, 1) , substr(d[[i]][j], 2, nchar(d[[i]][j])),fixed = TRUE),
      d[[i]][j]<-"",
      d[[i]][j]<-d[[i]][j]
    )
    
  }
  
}


for (i in 1:length(d)){
  d[[i]]<-unique(unlist(lapply(d[[i]], function(x) x[[1]])))
  
  
}


e<-vapply(d, paste, collapse = ", ", character(1L))


for (i in 1:length(e)){
  ifelse(e[i]=="NA, ",
         e[i]<-"",
         e[i]<-e[i])
}



for (i in 1:length(e)){
  ifelse(substr(e[i], 1, 2)==", ",
         e[i]<-substr(e[i],3,nchar(e[i])),
         ifelse(
           substr(e[i], (nchar(e[i])-1),nchar(e[i]))==", ",
           e[i]<-substr(e[i],1,nchar(e[i])-2),
           ifelse(
             grepl(", , , ", e[i], fixed = TRUE),
             e[i]<-gsub(", , , ", ", ", e[i]),
             ifelse(
               grepl(", , ", e[i], fixed = TRUE),
               e[i]<-gsub(", , ", ", ", e[i]),
               
               e[i]<-e[i]))))
}







f<-as.list(strsplit(RT, "\r\n"))
grep("PRB",f,fixed = TRUE)

for(i in 1:length(f)){
  for(j in 1:length(f[[i]])){
    ifelse(grepl("PRB", f[[i]][j], fixed = TRUE),
           f[[i]][j]<-word(f[[i]][j],2,sep = "\\!PRB!"),
           f[[i]][j]<-""
    )
    
  }
}


for(i in 1:length(f)){
  f[[i]]<-unique(unlist(lapply(f[[i]], function(x) x[[1]])))
  
}


for(i in 1:length(f)){
  for(j in 1:length(f[[i]])){
    ifelse(
      f[[i]][j]=="",
      f[[i]]<-list.remove(f[[i]],j),
      f[[i]]<-f[[i]]
    )
    
  }
}


f<-as.character(f)
f<-gsub(","," ",f)
f<-gsub("[^[:alnum:][:blank:]+?&/\\-]", "", f)


for(i in 1:length(f)){
  ifelse(
    f[i]=="character0",
    f[i]<-NA,
    f[i]<-f[i]
  )
  
  ifelse(
    grepl("c", substr(f[i], 1, 1), fixed = TRUE),
    f[i]<-f[i]%>%stringr::str_remove("c"),
    f[i]<-f[i]
    
  )
  
}


f<-gsub("\\s+"," ",f)
f<-strsplit(f, " ")
f<-sapply(f, unique)

g<-f

for (i in 1:length(f)){
  for(j in 1:length(f[[i]])){
    s<-gsub( "/.*$", "", f[[i]][j])
    g[[i]][[j]]<-split(f[[i]][[j]],(length(s)+1))
    
  }
}



for (i in 1:length(f)){
  for(j in 1:length(f[[i]])){
    z<-unlist(gregexpr(pattern ='/',f[[i]][j]))+1
    l<-gsub( "/.*$", "", f[[i]][j])
    g[[i]][[j]][[1]] = l
    for(n in 1:length(z)){
      
      g[[i]][[j]][[n+1]]<-paste(substr(f[[i]][j],1,z[1]-3),substr(f[[i]][j],z[n],z[n]),sep="") 
      
    }
  }
}


h<-lapply(g, function(x) do.call(rbind, x))
h
for (i in 1:length(h)){
  for (j in 1:length(h[[i]])){
    ifelse(
      grepl(substr(h[[i]][j], 1, 1) , substr(h[[i]][j], 2, nchar(h[[i]][j])),fixed = TRUE),
      h[[i]][j]<-"",
      h[[i]][j]<-h[[i]][j]
    )
    
  }
  
}


for (i in 1:length(h)){
  h[[i]]<-unique(unlist(lapply(h[[i]], function(x) x[[1]])))
  
  
}


h<-vapply(h, paste, collapse = ", ", character(1L))


for (i in 1:length(h)){
  ifelse(h[i]=="NA, ",
         h[i]<-"",
         h[i]<-h[i])
}



for (i in 1:length(h)){
  ifelse(substr(h[i], 1, 2)==", ",
         h[i]<-substr(h[i],3,nchar(h[i])),
         ifelse(
           substr(h[i], (nchar(h[i])-1),nchar(h[i]))==", ",
           h[i]<-substr(h[i],1,nchar(h[i])-2),
           ifelse(
             grepl(", , , ", h[i], fixed = TRUE),
             h[i]<-gsub(", , , ", ", ", h[i]),
             ifelse(
               grepl(", , ", h[i], fixed = TRUE),
               h[i]<-gsub(", , ", ", ", h[i]),
               
               h[i]<-h[i]))))
}

o<-paste(e,h,sep=", ")

for(i in 1:length(i)){
  o[[i]]<-sapply(strsplit(o[[i]], ",", fixed = TRUE), function(x) 
    paste(unique(x), collapse = ","))
  
}

for (i in 1:length(o)){
  ifelse(substr(o[i], 1, 2)==", ",
         o[i]<-substr(o[i],3,nchar(o[i])),
         ifelse(
           substr(o[i], (nchar(o[i])-1),nchar(o[i]))==", ",
           o[i]<-substr(o[i],1,nchar(o[i])-2),
           ifelse(
             grepl(", , , ", o[i], fixed = TRUE),
             o[i]<-gsub(", , , ", ", ", o[i]),
             ifelse(
               grepl(", , ", o[i], fixed = TRUE),
               o[i]<-gsub(", , ", ", ", o[i]),
               
               o[i]<-o[i]))))
}


new$`REVERSE TRANSCRIPTASE`<-o
new$`REVERSE TRANSCRIPTASE`<-paste("Reverse Transcriptase Gene:",new$`REVERSE TRANSCRIPTASE`,sep=" ")






pro<-sub('.*PIs', '', str)
#which(is.na(pro)) #630
a<-as.list(strsplit(pro, "\r\n"))
#grepl("!PRB!",a,fixed = TRUE)


for(i in 1:length(a)){
  for(j in 1:length(a[[i]])){
    ifelse(grepl("YES", a[[i]][j], fixed = TRUE),
           a[[i]][j]<-word(a[[i]][j],2,sep = "\\!YES!"),
           a[[i]][j]<-""
    )
    
  }
}


for(i in 1:length(a)){
  a[[i]]<-unique(unlist(lapply(a[[i]], function(x) x[[1]])))
  
}


for(i in 1:length(a)){
  for(j in 1:length(a[[i]])){
    ifelse(
      a[[i]][j]=="",
      a[[i]]<-list.remove(a[[i]],j),
      a[[i]]<-a[[i]]
    )
    
  }
}


b<-as.character(a)
b<-gsub(","," ",b)
b<-gsub("[^[:alnum:][:blank:]+?&/\\-]", "", b)


for(i in 1:length(b)){
  
  ifelse(
    b[i]=="character0",
    b[i]<-NA,
    b[i]<-b[i]
  )
  
  ifelse(
    grepl("c", substr(b[i], 1, 1), fixed = TRUE),
    b[i]<-b[i]%>%stringr::str_remove("c"),
    b[i]<-b[i]
    
  )
  
}


b<-gsub("\\s+"," ",b)
b<-strsplit(b, " ")
b<-sapply(b, unique)

c<-b

for (i in 1:length(b)){
  for(j in 1:length(b[[i]])){
    m<-gsub( "/.*$", "", b[[i]][j])
    c[[i]][[j]]<-split(c[[i]][[j]],(length(m)+1))
    
  }
}



for (i in 1:length(b)){
  for(j in 1:length(b[[i]])){
    s<-unlist(gregexpr(pattern ='/',b[[i]][j]))+1
    m<-gsub( "/.*$", "", b[[i]][j])
    c[[i]][[j]][[1]] = m
    for(n in 1:length(s)){
      
      c[[i]][[j]][[n+1]]<-paste(substr(b[[i]][j],1,s[1]-3),substr(b[[i]][j],s[n],s[n]),sep="") 
      
    }
  }
}


d<-lapply(c, function(x) do.call(rbind, x))

for (i in 1:length(d)){
  for (j in 1:length(d[[i]])){
    ifelse(
      grepl(substr(d[[i]][j], 1, 1) , substr(d[[i]][j], 2, nchar(d[[i]][j])),fixed = TRUE),
      d[[i]][j]<-"",
      d[[i]][j]<-d[[i]][j]
    )
    
  }
  
}


for (i in 1:length(d)){
  d[[i]]<-unique(unlist(lapply(d[[i]], function(x) x[[1]])))
  
  
}


e<-vapply(d, paste, collapse = ", ", character(1L))


for (i in 1:length(e)){
  ifelse(e[i]=="NA, ",
         e[i]<-"",
         e[i]<-e[i])
}



for (i in 1:length(e)){
  ifelse(substr(e[i], 1, 2)==", ",
         e[i]<-substr(e[i],3,nchar(e[i])),
         ifelse(
           substr(e[i], (nchar(e[i])-1),nchar(e[i]))==", ",
           e[i]<-substr(e[i],1,nchar(e[i])-2),
           ifelse(
             grepl(", , , ", e[i], fixed = TRUE),
             e[i]<-gsub(", , , ", ", ", e[i]),
             ifelse(
               grepl(", , ", e[i], fixed = TRUE),
               e[i]<-gsub(", , ", ", ", e[i]),
               
               e[i]<-e[i]))))
}




f<-as.list(strsplit(pro, "\r\n"))

for(i in 1:length(f)){
  for(j in 1:length(f[[i]])){
    ifelse(grepl("PRB", f[[i]][j], fixed = TRUE),
           f[[i]][j]<-word(f[[i]][j],2,sep = "\\!PRB!"),
           f[[i]][j]<-""
    )
    
  }
}


for(i in 1:length(f)){
  f[[i]]<-unique(unlist(lapply(f[[i]], function(x) x[[1]])))
  
}


for(i in 1:length(f)){
  for(j in 1:length(f[[i]])){
    ifelse(
      f[[i]][j]=="",
      f[[i]]<-list.remove(f[[i]],j),
      f[[i]]<-f[[i]]
    )
    
  }
}


f<-as.character(f)
f<-gsub(","," ",f)
f<-gsub("[^[:alnum:][:blank:]+?&/\\-]", "", f)


for(i in 1:length(f)){
  ifelse(
    f[i]=="character0",
    f[i]<-NA,
    f[i]<-f[i]
  )
  
  ifelse(
    grepl("c", substr(f[i], 1, 1), fixed = TRUE),
    f[i]<-f[i]%>%stringr::str_remove("c"),
    f[i]<-f[i]
    
  )
  
}


f<-gsub("\\s+"," ",f)
f<-strsplit(f, " ")
f<-sapply(f, unique)

g<-f

for (i in 1:length(f)){
  for(j in 1:length(f[[i]])){
    s<-gsub( "/.*$", "", f[[i]][j])
    g[[i]][[j]]<-split(f[[i]][[j]],(length(s)+1))
    
  }
}



for (i in 1:length(f)){
  for(j in 1:length(f[[i]])){
    z<-unlist(gregexpr(pattern ='/',f[[i]][j]))+1
    l<-gsub( "/.*$", "", f[[i]][j])
    g[[i]][[j]][[1]] = l
    for(n in 1:length(z)){
      
      g[[i]][[j]][[n+1]]<-paste(substr(f[[i]][j],1,z[1]-3),substr(f[[i]][j],z[n],z[n]),sep="") 
      
    }
  }
}


h<-lapply(g, function(x) do.call(rbind, x))
h
for (i in 1:length(h)){
  for (j in 1:length(h[[i]])){
    ifelse(
      grepl(substr(h[[i]][j], 1, 1) , substr(h[[i]][j], 2, nchar(h[[i]][j])),fixed = TRUE),
      h[[i]][j]<-"",
      h[[i]][j]<-h[[i]][j]
    )
    
  }
  
}


for (i in 1:length(h)){
  h[[i]]<-unique(unlist(lapply(h[[i]], function(x) x[[1]])))
  
  
}


h<-vapply(h, paste, collapse = ", ", character(1L))


for (i in 1:length(h)){
  ifelse(h[i]=="NA, ",
         h[i]<-"",
         h[i]<-h[i])
}



for (i in 1:length(h)){
  ifelse(substr(h[i], 1, 2)==", ",
         h[i]<-substr(h[i],3,nchar(h[i])),
         ifelse(
           substr(h[i], (nchar(h[i])-1),nchar(h[i]))==", ",
           h[i]<-substr(h[i],1,nchar(h[i])-2),
           ifelse(
             grepl(", , , ", h[i], fixed = TRUE),
             h[i]<-gsub(", , , ", ", ", h[i]),
             ifelse(
               grepl(", , ", h[i], fixed = TRUE),
               h[i]<-gsub(", , ", ", ", h[i]),
               
               h[i]<-h[i]))))
}

o<-paste(e,h,sep=", ")

for(i in 1:length(i)){
  o[[i]]<-sapply(strsplit(o[[i]], ",", fixed = TRUE), function(x) 
    paste(unique(x), collapse = ","))
  
}

for (i in 1:length(o)){
  ifelse(substr(o[i], 1, 2)==", ",
         o[i]<-substr(o[i],3,nchar(o[i])),
         ifelse(
           substr(o[i], (nchar(o[i])-1),nchar(o[i]))==", ",
           o[i]<-substr(o[i],1,nchar(o[i])-2),
           ifelse(
             grepl(", , , ", o[i], fixed = TRUE),
             o[i]<-gsub(", , , ", ", ", o[i]),
             ifelse(
               grepl(", , ", o[i], fixed = TRUE),
               o[i]<-gsub(", , ", ", ", o[i]),
               
               o[i]<-o[i]))))
}

o[650]

new$PROTEASE<-o
new$PROTEASE<-paste("Protease Gene:",new$PROTEASE,sep=" ")


keyword<-"HIV Subtype:"
lookaround <- 1

pattern1 <- paste0(keyword, 
                   "( [[:alnum:]]+){0,", lookaround, "}")
pattern2 <- paste0(keyword, 
                   "([[:alnum:]]+){0,", lookaround, "}")

str<-new$`Comment Results`
x<-gsub("[\r\n]", "", str)


r1 <- regexpr(pattern1, x,ignore.case=TRUE)
r2 <- regexpr(pattern2, x,ignore.case=TRUE)

out1 <- rep(NA,length(x))
out2 <- rep(NA,length(x))

out1[r1!=-1] <- regmatches(x, r1)
out2[r2!=-1] <- regmatches(x, r2)

for(i in 1:length(out1)){
  ifelse(grepl("NOT", out1[i],fixed = TRUE),
         out1[i]<-"NOT DETERMINED",
         out1[i]<-out1[i]
         
  )
}

#out1

new$`HIV SUBTYPE`<-out1
cleaned<-paste("DRUG RESISTANCE MUTATIONS DETECTED:",new$`REVERSE TRANSCRIPTASE`,sep="\r\n")
cleaned<-paste(cleaned,new$PROTEASE,sep="\r\n")
cleaned<-paste(cleaned,new$`HIV SUBTYPE`,sep="\r\n")
new$cleanedResult<-cleaned

new_final<-new[,c(1,5,10:12)]
write.csv(new_final,"./new_final.csv")
write.csv(new,"./new_cleaned.csv")


########################Combine All Formats########################
#int<-integrase[,c(1,5,10)]
int<-integrase[,c(1,2,21)]
names(old)[2]<-"Date"
names(mid_final)[2]<-"Date"
names(new_final)[2]<-"Date"
mid_final$Date<-anydate(mid_final$Date)
mid_new<-rbind(mid_final,new_final)
mid_new<-mid_new[
  with(mid_new, order(SID, Date)),
]

mid_new1<-rbind(old,mid_new)
mid_new1<-mid_new1[
  with(mid_new1, order(SID, Date)),
]

mid_new1<-unique(mid_new1)


RT1<-sub('.*Reverse Transcriptase Gene:', '', mid_new1$`REVERSE TRANSCRIPTASE`)
RT1<-gsub("Reverse Transcriptase Gene 2:.*$", "", RT1)
RT1<-stri_trim(RT1)

all1<-mid_new1
all1$rt_clean<-RT1
rt_clean<-all1[,c(1,6)]
rt_clean1<-rt_clean %>% 
  group_by(SID) %>% 
  mutate(cleaned = paste0(rt_clean, collapse = ", ")) 

rt_clean2<-unique(rt_clean1[,c(1,3)])
e<-rt_clean2$cleaned

for (i in 1:length(e)){
  
  ifelse(
    grepl("NONE", e[i], fixed = TRUE),
    e[i]<-gsub("NONE", "", e[i]),
    ifelse(
      grepl("NA", e[i], fixed = TRUE),
      e[i]<-gsub("NA", "", e[i]),
      
      e[i]<-e[i]))
}
e<-str_replace_all(e, "[^[:alnum:]]", " ")
e1<-str_trim(e)
e1<-gsub("[[:blank:]]+", ", ", e1)

for(i in 1:length(e1)){
  e1[[i]]<-sapply(strsplit(e1[[i]], ", ", fixed = TRUE), function(x) 
    paste(unique(x), collapse = ", "))
  
}

rt_clean2$cleaned<-e1
names(rt_clean2)[2]<-"REVERSE TRANSCRIPTASE"


pro1<-sub('.*Protease Gene:', '', mid_new1$PROTEASE)
pro1<-gsub("Protease Gene 2:.*$", "", pro1)
pro1<-gsub("[\r\n]", "", pro1)
#pro1
all1$pro_clean<-pro1
rt_clean<-all1[,c(1,7)]
rt_clean3<-rt_clean %>% 
  group_by(SID) %>% 
  mutate(cleaned = paste0(pro_clean, collapse = ", ")) 
rt_clean3<-unique(rt_clean3[,c(1,3)])
f<-rt_clean3$cleaned

for (i in 1:length(f)){
  
  ifelse(
    grepl("NONE", f[i], fixed = TRUE),
    f[i]<-gsub("NONE", "", f[i]),
    ifelse(
      grepl("NA", f[i], fixed = TRUE),
      f[i]<-gsub("NA", "", f[i]),
      
      f[i]<-f[i]))
}

f<-str_replace_all(f, "[^[:alnum:]]", " ")
f<-str_trim(f)
f<-gsub("[[:blank:]]+", ", ", f)
f<-str_split(f,", ")
f<-sapply(f, unique)
f<-vapply(f, paste, collapse = ", ", character(1L))
rt_clean3$cleaned<-f
rt_clean2$PROTEASE<-f


hiv<-unique(all1[,c(1,5)])
hiv<-hiv%>%group_by(SID)%>%
  mutate(
    first = dplyr::first(na.omit(`HIV SUBTYPE`)))%>%
  distinct(SID,first)
names(hiv)[2]<-"HIV SUBTYPE"
rt_clean2$`HIV SUBTYPE`<-hiv$`HIV SUBTYPE`

#integrase3<-integrase[,c(1,5,10)]
integrase3<-integrase[,c(1,2,21)]
names(integrase3)[2]<-"Date"
names(integrase3)[3]<-"INTEGRASE"
mid_new2<-bind_rows(mid_new1,integrase3)
q<-mid_new2$INTEGRASE
q<-gsub("[\r\n]", "", q)
for(i in 1:length(q)){
  ifelse(
    grepl("NA",q[i],fix=TRUE),
    q[i]<-str_remove(q[i],"NA"),
    q[i]<-q[i]
  )
  ifelse(
    grepl("NONE",q[i],fix=TRUE),
    q[i]<-gsub("NONE", "", q[i]),
    q[i]<-q[i]
  )
}

q<-ifelse(is.na(q),"",q)
rt_clean4<-cbind(mid_new2$SID,q)
rt_clean4<-as.data.frame(rt_clean4)
names(rt_clean4)[1]<-"SID"
names(rt_clean4)[2]<-"int_clean"


rt_clean4<-rt_clean4 %>% 
  group_by(SID) %>% 
  mutate(cleaned = paste0(int_clean, collapse = ", ")) 

rt_clean4<-unique(rt_clean4[,c(1,3)])
q<-rt_clean4$cleaned

q<-str_replace_all(q, "[^[:alnum:]]", " ")
q<-str_trim(q)
q<-gsub("[[:blank:]]+", ", ", q)
q<-str_split(q,", ")
q<-sapply(q, unique)
q<-vapply(q, paste, collapse = ", ", character(1L))

rt_clean4$cleaned<-q
names(rt_clean4)[2]<-"INTEGRASE"
rt_clean2$SID<-as.character(rt_clean2$SID)
summary<-left_join(rt_clean4,rt_clean2)
#check<-integrase$SID[!integrase$SID%in%summary$SID]
summary$Date<-"summary"
#str(mid_new2$Date)
#str(summary$Date)
mid_new2$Date<-as.character(mid_new2$Date)
combined<-rbind(mid_new2,summary)
combined<-combined[
  with(combined, order(SID)),
]

#combined[12973,3]<-"K103N, Y188C"
combined[12974,3]<-"K103N, Y188C"


write.csv(combined,"./combined_summary.csv")
#################################Occurence Count for RT and PI###########################################
occurence_rt_pi<-mid_new1%>%
  group_by(SID)%>%
  mutate(first_date=first(Date))%>%
  mutate(last_date=last(Date))%>%
  mutate(count=n_distinct(Date))
write.csv(occurence_rt_pi,"./occurence_rt_pi.csv")

occurence_int<-integrase3%>%
  group_by(SID)%>%
  mutate(first_date=first(Date))%>%
  mutate(last_date=last(Date))%>%
  mutate(count=n_distinct(Date))
occurence_int<-occurence_int[
  with(occurence_int, order(SID)),
]

write.csv(occurence_int,"./occurence_int.csv")

########################################Split RT and PI into Columns##########################################

final<-subset(combined,Date=="summary")
rt<-final$`REVERSE TRANSCRIPTASE`
pi<-final$PROTEASE

a<-unlist(strsplit(rt, ", "))
a<-stri_trim(a)
uniques<-unique(a)
uniques
#tapply(seq_along(a), a, identity)[uniques]
#127 and 147
#uniques<-uniques[-c(127,147)]
uniques<-uniques[-6]
colnames<-uniques
final1<-final
final1[,c(7:94)]<-""
colnames(final1)[7:94]<-colnames
colnames(final1)<-as.character(colnames(final1))
final1<-as.data.frame(final1)
final2<-final1[,-c(1:6)]


u<-final$`REVERSE TRANSCRIPTASE`
for(f in 1:length(final2)){
  for(i in 1:length(u)){
    ifelse(
      grepl(uniques[f],u[i],fixed = TRUE),
      final2[i,f]<-1,
      final2[i,f]<-0
      
    )
  }
  
  
}
order1 <- colnames(final2)
number1 <- parse_number(order1)
order1 <- order1[order(number1)]
final2_1 <- final2[order(match(colnames(final2),order1))]



final3<-cbind(final1[,1:6],final2_1)
final3[,95:96]<-""



b<-unlist(strsplit(pi, ", "))
b<-stri_trim(b)
uniq<-unique(b)
uniq
uniq<-uniq[-c(4,39,59)]
colnames<-uniq
final4<-final3
final4[,c(97:152)]<-""
colnames(final4)[97:152]<-colnames
colnames(final4)<-as.character(colnames(final4))
final4<-as.data.frame(final4)
final5<-final4[,-c(1:96)]


v<-final$PROTEASE
for(f in 1:length(final5)){
  for(i in 1:length(v)){
    ifelse(
      grepl(uniq[f],v[i],fixed = TRUE),
      final5[i,f]<-1,
      final5[i,f]<-0
      
    )
  }
  
  
}


order2 <- colnames(final5)
number2 <- parse_number(order2)
order2 <- order2[order(number2)]
final5_1 <- final5[order(match(colnames(final5),order2))]




final6<-cbind(final4[,1:96],final5_1)


#x<-colnames(final6)
#x<-x[7:150]
#y<-order(x)
#x<-x[order(x)]
#y<-y+6
#final7<-final6[,c(1:6,y)]

write.csv(final6,"./rt_pi_split.csv")


#100674082 in new 2021-02-17 and 8858749 in new


int_split$lab.comments.results <- gsub("\\The method.*","",int_split$lab.comments.results)
order3<-colnames(int_split[,c(11:91)])
order3 <- order3[-61]
number3 <- parse_number(order3)
order3 <- order3[order(number3)]
int_split_1<-int_split[,c(11:70,72:91)]
int_split_2 <- int_split_1[order(match(colnames(int_split_1),order3))]
int_split_3 <- cbind(int_split[,1:10],int_split_2)

write.csv(int_split_3,"./int_split.csv")

##################Connecting with Stanford Database#####################
library(tidyverse)
library(rvest)
library(stringr)
library(purrr)
library(gtools)

stanford_pi <- read_html("https://hivdb.stanford.edu/dr-summary/pattern-scores/PI/")
table_pi <- stanford_pi %>%
  html_nodes("table") %>%
  html_table()
table_pi <- as.data.frame(table_pi)
table_pi <- table_pi[,-2]

stanford_NRTI <- read_html("https://hivdb.stanford.edu/dr-summary/pattern-scores/NRTI/")
table_NRTI <- stanford_NRTI %>%
  html_nodes("table") %>%
  html_table()
table_NRTI <- as.data.frame(table_NRTI)
table_NRTI <- table_NRTI[,-2]

stanford_NNRTI <- read_html("https://hivdb.stanford.edu/dr-summary/pattern-scores/NNRTI/")
table_NNRTI <- stanford_NNRTI %>%
  html_nodes("table") %>%
  html_table()
table_NNRTI <- as.data.frame(table_NNRTI)
table_NNRTI <- table_NNRTI[,-2]

#least mutation

stanford_int <- read_html("https://hivdb.stanford.edu/dr-summary/pattern-scores/INSTI/")
table_int_pattern <- stanford_int %>%
  html_nodes("table") %>%
  html_table()
table_int_pattern <- as.data.frame(table_int_pattern)
table_int_pattern <- table_int_pattern[,-2]

stanford_int1 <- read_html("https://hivdb.stanford.edu/dr-summary/mut-scores/INSTI/")
table_int_mutation <- stanford_int1%>%
  html_nodes("table") %>%
  html_table()
table_int_single <- table_int_mutation[1]
table_int_single <- as.data.frame(table_int_single)
names(table_int_single)[1] <- "geneMutation"

#################################Integrase Merge#################################

summary <- subset(combined, Date=="summary")
summary <- summary[,-1]

summary1 <- combined
names(table_int_single)[1] <- "INTEGRASE"

table_int_pattern1 <- table_int_pattern
table_int_pattern1$Pattern <- gsub("\\s", "", table_int_pattern1$Pattern)
table_int_pattern1$Pattern <- gsub("\\+",", ",table_int_pattern1$Pattern)
table_int_pattern1$CAB <- ""
names(table_int_pattern1)[1]<-"INTEGRASE"
int <- rbind(table_int_single,table_int_pattern1)
int <- int[-87,]

sort <- summary$INTEGRASE
sort1 <- NULL
for (i in 1:length(sort)){
  sort[[i]] <- str_split(sort[i], ',')
  sort[[i]] <- unlist(sort[[i]])
  sort[[i]] <- stri_trim(sort[[i]])
  sort1[[i]] <- as.numeric(gsub("[^[:digit:]]", "", sort[[i]]))
  names(sort1[[i]]) <- seq_along(sort1[[i]])
  sort[[i]] <- sort[[i]][as.numeric(names(sort(sort1[[i]])))]
  
}

#sort[43]
sorted <- vapply(sort, paste, collapse = ", ", character(1L))
#sorted[43]
summary$INTEGRASE <- sorted
new <- left_join(summary, int)


######################Protease Merge######################
table_pi1 <- table_pi
table_pi1 <- table_pi1[-41,]
table_pi1$Pattern <- gsub("\\s", "", table_pi1$Pattern)
table_pi1$Pattern <- gsub("\\+",", ",table_pi1$Pattern)
names(table_pi1)[1]<-"PROTEASE"


sort <- summary$PROTEASE
sort1 <- NULL
for (i in 1:length(sort)){
  sort[[i]] <- str_split(sort[i], ',')
  sort[[i]] <- unlist(sort[[i]])
  sort[[i]] <- stri_trim(sort[[i]])
  sort1[[i]] <- as.numeric(gsub("[^[:digit:]]", "", sort[[i]]))
  names(sort1[[i]]) <- seq_along(sort1[[i]])
  sort[[i]] <- sort[[i]][as.numeric(names(sort(sort1[[i]])))]
  
}

#sort[2582]
sorted <- vapply(sort, paste, collapse = ", ", character(1L))
summary$PROTEASE <- sorted
new1 <- left_join(new, table_pi1)

#################################NRTI & NNRTI Merge#################################
table_NRTI1 <- table_NRTI
table_NRTI1 <- table_NRTI1[-41,]
table_NNRTI1 <- table_NNRTI
table_NNRTI1 <- table_NNRTI1[-41,]
rt <- merge(table_NRTI1,table_NNRTI1,by="Pattern", all=T)
names(rt)[1]<-"REVERSE.TRANSCRIPTASE"

sort <- summary$`REVERSE TRANSCRIPTASE`
sort1 <- NULL
for (i in 1:length(sort)){
  sort[[i]] <- str_split(sort[i], ',')
  sort[[i]] <- unlist(sort[[i]])
  sort[[i]] <- stri_trim(sort[[i]])
  sort1[[i]] <- as.numeric(gsub("[^[:digit:]]", "", sort[[i]]))
  names(sort1[[i]]) <- seq_along(sort1[[i]])
  sort[[i]] <- sort[[i]][as.numeric(names(sort(sort1[[i]])))]
  
}

#sort[2582]
sorted <- vapply(sort, paste, collapse = ", ", character(1L))
summary$`REVERSE TRANSCRIPTASE` <- sorted
names(rt)[1]<-"REVERSE TRANSCRIPTASE"
new2 <- left_join(new1, rt)

write.csv(new2,"/Users/air/OneDrive - Emory University/Genotype_APE_New/stanford.csv")



