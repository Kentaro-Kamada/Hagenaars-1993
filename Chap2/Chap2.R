library(tidyverse)
library(kamaken)
set.seed(123)

data <- read_csv('Chap2/tab2.2.csv')


# tab2.3 --------------------------------------------------------------------------------------

lem(
  man = 'man 3',
  dim = 'dim 3 2 2',
  lab = 'lab A S E',
  mod = 'mod ASE', 
  dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
  ite = 'ite 5000', 
  see = 'see 111',
  path = 'lem/tab2.3/'
)


lem_max_loglik('lem/tab2.3/out')


lem_gof('lem/tab2.3/out/see111.out')


