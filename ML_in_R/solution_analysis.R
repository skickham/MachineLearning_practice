library(dplyr)
library(ggplot2)

### Comparing creation probability after checking out 
co_final %>% head()
co_final %>% 
  mutate(Create=(Status=='Create'), social=(Phase=='Test'))%>% 
  group_by(social) %>% 
  summarise(Ratio = sum(Create/n()))

### Comparing creation probability in editing wish list (Including failure)
wl_final %>% 
  mutate(Create=(Status%in% c('Create_S', 'Create_F')), social=(Phase=='Test'))%>% 
  group_by(social) %>% 
  summarise(Ratio = sum(Create/n()))

### Comparing creation probability in editing wish list (Only success)
wl_final %>% 
  mutate(Create=(Status=='Create_S'), social=(Phase=='Test'))%>%
  group_by(social) %>% 
  summarise(Ratio = sum(Create/n()))

### Exclude the one with account already
wl_final %>% 
  filter(! Status %in% c('Return_S', 'Return_F')) %>%
  mutate(Create=(Status%in% c('Create_S', 'Create_F')), social=(Phase=='Test'))%>%
  group_by(social) %>% 
  summarise(Ratio = sum(Create/n()))


### Further analysis
co_by_date <- function(){
Reduce(rbind,list(
before_co %>% 
  group_by(date=as.Date(X_time), theugid) %>% 
  summarise(create=sum(reg_success)>0) %>% 
  summarise(social=F, ratio = sum(create)/n()),
test_co %>% 
  group_by(date=as.Date(X_time), theugid) %>% 
  summarise(create=sum(reg_success)>0) %>% 
  summarise(social=T, ratio = sum(create)/n()),
after_co %>% 
  group_by(date=as.Date(X_time), theugid) %>% 
  summarise(create=sum(reg_success)>0) %>% 
  summarise(social=F, ratio = sum(create)/n()))
)}


co_by_date() %>% 
  ggplot()+
  geom_bar(aes(x=weekdays(date), 
               y=ratio, 
               fill=social), 
           stat='identity', position='dodge')




### Chisq test
chisq.test(table(co_final$Phase, co_final$Status))

chi_co <- co_final %>% mutate(Test=(Phase=='Test')) %>% 
  (function(x) {chisq.test(table(x$Test, x$Status))})
chi_co$residuals

chi_wl <- wl_final %>% mutate(Test=(Phase=='Test'), Create =(Status %in% c('Create_S', 'Create_F'))) %>% 
  (function(x) {chisq.test(table(x$Test, x$Create))})
chi_wl$residuals

### GLM method:
mod_co = glm(Status ~ Phase, data=co_final, family = binomial())
summary(mod_co)

mod_wl = wl_final %>% 
  mutate(Test=(Phase=='Test'), Create =(Status %in% c('Create_S', 'Create_F'))) %>%
  (function(x) {glm(Create~Test, data=x, family=binomial())})

