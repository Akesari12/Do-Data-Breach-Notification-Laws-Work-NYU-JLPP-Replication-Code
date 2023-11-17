calc_dollars_saved <- function(reduction) {
  dollars <- reduction*3300*(1300 + (28*200))
  return(dollars)
}

calc_dollars_saved(-1.17)
calc_dollars_saved(-11.250)
calc_dollars_saved(-9.82)
calc_dollars_saved(-3.59)
calc_dollars_saved(-15.71)
calc_dollars_saved(4.10)
calc_dollars_saved(-11.08)
