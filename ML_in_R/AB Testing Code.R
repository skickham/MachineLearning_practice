#########################
### AB Testing ##########
#########################

library(data.table)

before_co = fread('data/before_co.csv')
after_co = fread('data/after_co.csv')
test_co = fread('data/test_co.csv')

before_wl = fread('data/before_wl.csv')
after_wl = fread('data/after_wl.csv')
test_wl = fread('data/test_wl.csv')

# fix & bind checkouts
after_co$themodule = 0
before_co$themodule = 0

after_co$when = 'after'
before_co$when = 'before'
test_co$when = 'test'

co = rbind(after_co, before_co, test_co)

# fix & bind wishlists
after_wl$themodule = 0
before_wl$themodule = 0

after_wl$when = 'after'
before_wl$when = 'before'
test_wl$when = 'test'

wl = rbind(after_wl, before_wl, test_wl)

# check for missing values
wl[!complete.cases(wl),]
co[!complete.cases(co),]

# if user was logged in at any point, give 1, otherwise 0
wl$isloggedin_r = gsub('user by-session', '0', wl$isloggedin_r)
wl$isloggedin_r = gsub('user', '1', wl$isloggedin_r)
wl$isloggedin_r = gsub(',', ' ', wl$isloggedin_r)

wl$isloggedin_r[grepl('0', wl$isloggedin_r)] = 0
wl$isloggedin_r[grepl('1', wl$isloggedin_r)] = 1


# change login_or_create to reg_success