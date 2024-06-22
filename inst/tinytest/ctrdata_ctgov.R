## RH 2019-09-28

# set server
httr::set_config(httr::timeout(seconds = 60))

#### ctrLoadQueryIntoDb ####

# test
expect_equal(
  suppressMessages(
    ctrLoadQueryIntoDb(
      queryterm = "2010-024264-18",
      register = "CTGOV",
      only.count = TRUE))[["n"]], 1L)



