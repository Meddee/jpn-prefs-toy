


library(tidyr)
library("readr")
library("dplyr")
library("stringr")
library("magrittr")
Sys.setenv(LANG = "en")


#+++++++++++++++++++++++++++++++++++++++++  
#GET DATA FROM JMD INTO R DATA FRAMES
#++++++++++++++++++++++++++++++++++++++++++++


#create all file URLs
path.start= "http://www.ipss.go.jp/p-toukei/JMD/"
prefectures= c(paste0(0,1:9),10:47)
file.paths=paste(path.start,prefectures, "/STATS/Mx_1x1.txt",sep="")

#test file paths
file.paths[1]
read_table(file.paths[1],col_names=TRUE,skip=2)
read_table(file.paths[2],col_names=TRUE,skip=2) 
 
# test one file
test=read_lines("http://www.ipss.go.jp/p-toukei/JMD/33/STATS/Mx_1x1.txt")[1]
dd=unlist(str_split(test,",")[1])[1]
unlist(str_split(dd,"[.]"))[2]

# Get Prefecture name and number
read_lines("http://www.ipss.go.jp/p-toukei/JMD/25/STATS/Mx_1x1.txt")[1]%>% #1st line of file
strsplit(",") %>%
 sapply(`[[`, 1) %>% #extract 1st item in list
str_split("[.]") %>%
  unlist() %>% 
  .[1:2]

#another approach
# a_long_path <- read_lines("http://www.ipss.go.jp/p-toukei/JMD/33/STATS/Mx_1x1.txt")[1]
# j_strsplit <- function(...) unlist(strsplit(...))
# a_long_path %>%
#   j_strsplit(",") %>%
#   j_strsplit("[.]") %>% 
#   .[1:2] 

#read first line from each file
lapply(file.paths, function(x) read_lines(x)[1])

# extract all prefecture number and NAME
pref=lapply(file.paths, function(x) 
  {read_lines(x)[1]%>%
    strsplit(",") %>%
    sapply(`[[`, 1) %>%
    #unlist() %>%
    str_split("[.]") %>%
    unlist() %>%
    .[1:2]})

#extract all prefecture names
pref.name = noquote(sapply(pref, "[[", 2) )

#create empty list to store mx
mx <-vector("list",length(pref.name)); names(mx)<- pref.name
mx
str(mx)

#read death rates directly from JMD website
#test one
read_table("http://www.ipss.go.jp/p-toukei/JMD/01/STATS/Mx_1x1.txt",col_names=TRUE,skip=2)
#OR equivalently
read_table(file.paths[1],col_names=TRUE,skip=2)

#create list of death rates for each prefecture
for (i in 1:length(pref.name)){
 data <- read_table(file.paths[i],col_names=TRUE,skip=2)
 mx[[i]] <- data
}
mx[[1]]

#head(read.table(file.paths[1],header=TRUE,sep="", skip=2),200)

#list of death rates for each gender by Pref.
#only take ages up to 80!
#death rates at high ages are weird
mx.f=lapply(mx,function(x) select(x,Year,Age,Female) %>% filter(Age<=96) );mx.f # NB cutoff Age =96
mx.m=lapply(mx,function(x) select(x,Year,Age,Male) %>% filter(Age<=96) );mx.m

#unlist and store all prefs in one df
mx.f= do.call("rbind", mx.f);mx.f
mx.m= do.call("rbind", mx.m);mx.m


#add column to identify prefectures
#101 ages and 40 years: 81*39= 3159 rows per pref

length(prefectures)
ages=c(0:96); length(ages)
years= 1975:2014;length(years)
length(ages)*length(years)*length(prefectures)==nrow(mx.f)

pref=rep(pref.name, each=length(ages)*length(years))

mx.f2=cbind(mx.f,pref);head(mx.f2) # pref named added
mx.f3 =mx.f2 %>% mutate(sex="F") %>% rename(drate=Female)
mx.f3=tbl_df(mx.f3)
mx.f3$drate=as.numeric(mx.f3$drate)

mx.m2=cbind(mx.m,pref);head(mx.m2) # pref named added
mx.m3 =mx.m2 %>% mutate(sex="M")%>% rename(drate=Male);mx.m3
mx.m3=tbl_df(mx.m3)
mx.m3$drate=as.numeric(mx.m3$drate)

#single df for all dth rates
mx.all= rbind(mx.f3 ,mx.m3);mx.all

                #Check death rates

#If last rate=0 then problem since 1/0=Inf
# 0 mx at age ++>
ratef=mx.f3 %>% filter(drate==0,Age>95) %>%print(n=Inf)
ratem=mx.m3 %>% filter(drate==0,Age>95) %>%print(n=Inf)

ratef$Age;table(ratef$Age)
ratem$Age;table(ratem$Age)
ratem

#read japan death rates
Jap<-read_table(file="http://www.ipss.go.jp/p-toukei/JMD/00/STATS/Mx_1x1.txt",skip = 2)
Jap%>% filter(Age > 94,Age<=97,Year>=1975)%>%print(n=Inf)

#how did Jap mx look at these ages in 1976
Jap%>% filter(Age >= 94,Age<=96,Year==1975) 
#increasein mx from age 95 to 96 for given yrs
#given yrs
sort(unique(ratem$Year))

#mx increases in Jap:
inc75 = as.numeric(Jap%>% filter(Age==96,Year==1975) %>% select(Male) %>% .[1,1] )/ as.numeric(Jap%>% filter(Age==95,Year==1975) %>% select(Male) %>% .[1,1]); inc75

inc76 = as.numeric(Jap%>% filter(Age==96,Year==1976) %>% select(Male) %>% .[1,1] )/ as.numeric(Jap%>% filter(Age==95,Year==1976) %>% select(Male) %>% .[1,1]) ; inc76

inc81 = as.numeric(Jap%>% filter(Age==96,Year==1981) %>% select(Male) %>% .[1,1] )/ as.numeric(Jap%>% filter(Age==95,Year==1981) %>% select(Male) %>% .[1,1]) ; inc81

inc82 = as.numeric(Jap%>% filter(Age==96,Year==1982) %>% select(Male) %>% .[1,1] )/ as.numeric(Jap%>% filter(Age==95,Year==1982) %>% select(Male) %>% .[1,1]) ; inc82

inc83 =  as.numeric(Jap%>% filter(Age==96,Year==1983) %>% select(Male) %>% .[1,1] )/ as.numeric(Jap%>% filter(Age==95,Year==1983) %>% select(Male) %>% .[1,1]) ; inc83


                  #Replace last death rate with somethong else
ratem%>%arrange(Year)

#the foll. prefectures have issues: "Aomori", "Iwate", "Akita","Toyama" ,"Ishikawa" ,"Nara"
mx.m3 %>% filter(Year==1975,Age>=93,pref=="Akita")  # chk rates just before age 96

mx.m3=data.frame(mx.m3)# so we can get row numbers

#1975
newrate1 = mx.m3 %>% filter(Year==1975,Age==95,pref=="Iwate") %>% select(drate) %>% .[1,1] * inc75 
newrate2 = mx.m3 %>% filter(Year==1975,Age==95,pref=="Akita") %>% select(drate) %>% .[1,1] * inc75 
#1976
newrate3 = mx.m3 %>% filter(Year==1976,Age==95,pref=="Aomori") %>% select(drate) %>% .[1,1] * inc76 
newrate4 = mx.m3 %>% filter(Year==1976,Age==95,pref=="Ishikawa") %>% select(drate) %>% .[1,1] * inc76 
#1981
newrate5 = mx.m3 %>% filter(Year==1981,Age==95,pref=="Nara") %>% select(drate) %>% .[1,1] * inc81
# 1982
newrate6 = mx.m3 %>% filter(Year==1982,Age==95,pref=="Akita") %>% select(drate) %>% .[1,1] * inc82
#1983 
newrate7 = mx.m3 %>% filter(Year==1983,Age==95,pref=="Toyama") %>% select(drate) %>% .[1,1] * inc83
newrate8 = mx.m3 %>% filter(Year==1983,Age==95,pref=="Ishikawa") %>% select(drate) %>% .[1,1] * inc83

        ## #Now replace 0 rates at age 96 with estimated death rate

mx.m3=data.frame(mx.m3)# so we can get row numbers of cells to be replaced!!
head(mx.m3)

subset(mx.m3,select=c(Year,Age,pref,drate),subset=(Year==1975 & Age==96 & pref=="Iwate"))
mx.m3[7857,3]<- newrate1 

subset(mx.m3,select=c(Year,Age,pref,drate),subset=(Year==1975 & Age==96 & pref=="Akita"))
mx.m3[15617,3]<- newrate2

subset(mx.m3,select=c(Year,Age,pref,drate),subset=(Year==1976 & Age==96 & pref=="Aomori"))
mx.m3[4074 ,3]<-  newrate3

subset(mx.m3,select=c(Year,Age,pref,drate),subset=(Year==1976 & Age==96 & pref=="Ishikawa"))
mx.m3[62274 ,3]<- newrate4

subset(mx.m3,select=c(Year,Age,pref,drate),subset=(Year==1981 & Age==96 & pref=="Nara"))
mx.m3[109319 ,3]<- newrate5

subset(mx.m3,select=c(Year,Age,pref,drate),subset=(Year==1982 & Age==96 & pref=="Akita"))
mx.m3[16296 ,3]<- newrate6

subset(mx.m3,select=c(Year,Age,pref,drate),subset=(Year==1983 & Age==96 & pref=="Toyama"))
mx.m3[59073 ,3]<- newrate7

subset(mx.m3,select=c(Year,Age,pref,drate),subset=(Year==1983 & Age==96 & pref=="Ishikawa"))
mx.m3[62953 ,3]<- newrate8

                        #################
                        #Life Table Calc:
                        ####################

years= 1975:2014
agemax=96
x <- c(0:agemax)
radix <- 100000

# Read in the period life table function
# ##########NEED TO UPDATE ax as per Preston
life.table <- function(Mx){
  nmax <- 97
  n <- rep(1,nmax)         		#width of the intervals
  ax <- n / 2;		            	# default to .5 of interval
  #ax[1] <- b0 + b1 *Mx[1]    		# from Preston Tbl 3.3
  ax[1] <- .06 ;                #assumen to be .06
  ax[nmax] <- 1/Mx[nmax] 	  	# e_x at open age interval
  qx <- (n*Mx) / (1 + (n-ax)*Mx)
  qx<-ifelse( qx > 1, 1, qx);		# necessary for high nMx
  qx[nmax] <- 1.0
  lx <- radix*c(1,cumprod(1-qx)) ;  		# survivorship lx
  lx <- lx[1:length(Mx)]
  dx <- lx * qx ;
  Lx <- n*lx - ax*dx;       		# equivalent to n*l(x+n) + (n-nax)*ndx
  Lx[nmax] <- lx[nmax]*ax[nmax]
  Tx <- rev(cumsum(rev(Lx)))
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax] , NA);
  lt <- data_frame(Age=x,ax=ax,mx=Mx,qx=qx,lx=lx,dx=dx,Lx=Lx,Tx=Tx,ex=ex)
  return(lt)
}

# find LTs for multiple years FOR all PREFectures
lt.f =mx.f3 %>% group_by(pref,Year) %>% do(life.table(.$drate))
lt.m= mx.m3 %>% group_by(pref,Year) %>% do(life.table(.$drate))

#problem in last age interval if mx=0
lt.f%>% filter(Age==96,mx==0)%>%print(n=Inf)
lt.m%>% filter(Age==96,mx==0)%>%print(n=Inf)

lt.m%>% filter(Year==1978,pref=="Nagano")%>%print(n=Inf)


                                      #test functon
mx.f3 %>% group_by(pref,Year) %>% do(life.table(.$drate)) %>% filter(Year==2010,pref=="Oita")
#males
mx.m3 %>% group_by(pref,Year) %>% do(life.table(.$drate)) %>% filter(Year==2010,pref=="Oita")
#check to confirm values
test=mx.f3 %>% filter(Year==2010,pref=="Oita") %>% .$drate
test
life.table(test)          # Seems to be working fine!!!

                                ##################
                                #Get life table into form suitable for EVT analysis as used in CMD FIle
                                #######################

f.lt <-vector("list",length(prefectures)); names(f.lt)<- pref.name
m.lt <-vector("list",length(prefectures)); names(m.lt)<- pref.name

#FEMALES
  for (i in 1:length(prefectures)){
  pp<- filter(lt.f,pref == pref.name[i])
  f.lt[[i]] <- pp
}
head(f.lt[[1]])

#MALES
for (i in 1:length(prefectures)){
  pp2<- filter(lt.m,pref == pref.name[i])
  m.lt[[i]] <- pp2
}
head(m.lt[[1]])
                                  #########################
                                  #     GET MAXIMA     ####
                                  #########################

#Females
temp.f = lapply(f.lt, function(x) {x$Age <- parse_number(x$Age);x})
f.lt.0  =lapply(temp.f, subset, select=c(Year,ex), Age==0 )
f.lt.65  =lapply(temp.f, subset, select=c(Year,ex),Age==65 )

temp.m = lapply(m.lt, function(x) {x$Age <- parse_number(x$Age);x})
m.lt.0  =lapply(temp.m, subset, select=c(Year,ex), Age==0 )
m.lt.65  =lapply(temp.m, subset, select=c(Year,ex), Age==65 )

#chk dims
lapply(f.lt.0, function(x) dim(x));  lapply(m.lt.0, function(x) dim(x))
names.f = tolower(noquote(paste(pref.name,"f",sep=".")))
names.m = tolower(noquote(paste(pref.name,"m",sep=".")))

#set col names females
for(j in 1:length(names.f)){  colnames(f.lt.0[[j]])<-c("Year", names.f[j])  }
for(j in 1:length(names.f)){  colnames(f.lt.65[[j]])<-c("Year", names.f[j])  }

#chk col names
lapply(f.lt.0, function(x) colnames(x))  ;  lapply(f.lt.65, function(x) colnames(x))

#set col names males
for(j in 1:length(names.m)){   colnames(m.lt.0[[j]])<-c("Year", names.m[j]) }
for(j in 1:length(names.m)){   colnames(m.lt.65[[j]])<-c("Year", names.m[j]) }

#chk col names
lapply(m.lt.0, function(x) colnames(x)); lapply(m.lt.65, function(x) colnames(x))

merge.all <- function(x, y) merge(x, y, all=TRUE, by="Year")

All.f.0 <- Reduce(merge.all, f.lt.0)
All.m.0 <- Reduce(merge.all, m.lt.0)
All.f.65 <- Reduce(merge.all, f.lt.65)
All.m.65 <- Reduce(merge.all, m.lt.65)
dim(All.f.0)

#test = All[,order(names(All))]


