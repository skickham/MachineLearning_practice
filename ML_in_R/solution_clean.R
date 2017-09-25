library(dplyr)
library(stringr)# to count N, L and C
# Data import ------------------------------------------------------------------

before_wl <- read.csv("./data/before_wl.csv")
before_co <- read.csv("./data/before_co.csv")

test_co <- read.csv("./data/test_co.csv")
test_wl <- read.csv("./data/test_wl.csv")

after_co <- read.csv("./data/after_co.csv")
after_wl <- read.csv("./data/after_wl.csv")

# Data Cleaning ----------------------------------------------------------------

clean.co<-function(x,time){
  tab<-x%>%
    group_by(theugid)%>%
    mutate(reg_times = sum(reg_success))%>%
    mutate(Status=ifelse(reg_times>0,"Create","Lost"))
  tab$Phase=time
  col=if(time!="Test"){
    c("thedevice","firstpage","Status" ,"Phase")
  }else{
    c("thedevice","Status","firstpage","Phase","themodule")
  }
  
  tab=tab[,col]
  return(tab)
}

clean.wl<-function(x,time){
  # check the status
  x$number.of.l <- str_count(x$login_or_create, "L")
  x$number.of.c <- str_count(x$login_or_create, "C")
  x$number.of.user <-x$eventcount-str_count(x$isloggedin_r, "by-session")
  x$creat_s<-ifelse(x$number.of.c>0 & x$number.of.user>0,1,0)
  x$creat_f<-ifelse(x$number.of.c>0 & x$number.of.user==0,1,0)
  x$return_s <- ifelse(x$number.of.c==0 & x$number.of.l>0 & x$number.of.user>0,1,0)
  x$return_f <- ifelse(x$number.of.c==0 & x$number.of.l>0 & x$number.of.user==0,1,0)
  # add one column to indicate the status
  tbl<-x%>%
    group_by(date=as.Date(X_time), theugid)%>%
    mutate(cs_times = sum(creat_s),
           cf_times = sum(creat_f),
           rs_times = sum(return_s),
           rf_times = sum(return_f))%>%
    mutate(Status=ifelse(cs_times>0,
                         "Create_S",
                         ifelse(cf_times>0,
                                "Create_F",
                                ifelse(rs_times>0,
                                       "Return_S",
                                       ifelse(rf_times>0,
                                              "Return_F",
                                              "Lost"
                                       )))))
  tbl$Phase=time
  col=if(time!="Test"){
    c("thedevice","firstpage","Status" ,"Phase")
  }else{
    c("thedevice","Status","firstpage","Phase","themodule")
  }
  tbl=tbl[,col] 
  return(tbl)
}

clean.wl.bi<-function(x,time){
  tbl=clean.wl(x,time)
  tbl=tbl%>%
    filter(Status %in% c("Create_S","Create_F"))
  return(tbl)
}

clean.combine<-function(x,time1,y,time2,z,time3,function1){
  DT1=function1(x,time1)
  DT2=function1(y,time2)
  DT3=function1(z,time3)
  Final=as.data.frame(bind_rows(DT1,DT2,DT3))
  Final=as.data.frame(sapply(Final,as.factor)) # sapply won't work????? older version of R
  Final$Status<- relevel(Final$Status, 
                         ref = ifelse(as.character(substitute(function1))=="clean.co",
                                      "Create",
                                      "Create_S"))
  Final$Phase <- relevel(Final$Phase, ref = "Test")
  Final$themodule <- relevel(Final$themodule, ref = "F")
  return(Final)
}

co_final=clean.combine(before_co,"Before",test_co,"Test",after_co,"After",clean.co)
wl_final=clean.combine(before_wl,"Before",test_wl,"Test",after_wl,"After",clean.wl)
wl_final_bi=clean.combine(before_wl,"Before",test_wl,"Test",after_wl,"After",clean.wl.bi)


# Data Analysis

library(ggplot2)

# Basic Visualization

#
ggplot(co_final, aes(x = Phase)) + 
  geom_histogram(stat="count", 
                 aes(fill = Status), 
                 position = 'fill') +
  ggtitle("Checkout: \nStatus By Phase")

#
ggplot(wl_final_bi, aes(x = Phase)) +
  geom_histogram(stat="count", 
                 aes(fill = Status), 
                 position = 'fill') + 
  ggtitle("Wishlist: \n Status by Phase")

#
ggplot(co_final, aes(x = themodule)) +
  geom_histogram(stat="count", 
                 aes(fill = themodule)) +
  ggtitle("Checkout: \n Facebook vs. Google")

#
ggplot(wl_final_bi, aes(x = themodule)) + 
  geom_histogram(stat="count", 
                 aes(fill = themodule)) +
  ggtitle("Wishlist: \n Facebook vs. Google")


# Groupby Summarise to see Numbers


