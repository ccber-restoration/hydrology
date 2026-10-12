# define function for calculating "day of water year"
# (from here: https://stackoverflow.com/questions/55525965/is-there-a-r-function-to-calculate-day-of-water-year)
dowy = function(x, start.month = 10L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}

#test
dowy(as.Date("2025-09-30"))

# define function for calculating day of LOCAL water year (starting in September instead of October)
dolwy = function(x, start.month = 9L){
  start.yr = year(x) - (month(x) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  as.integer(x - start.date + 1L)
}


#test day of water year (dowy) function
dolwy(as.Date("2025-09-30"))