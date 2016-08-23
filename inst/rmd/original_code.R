#' ---
#' title: "Multiple Choice Test Analysis - Follows Jorion (2016) JEE Paper"
#' author: "Alvaro Lopez, Autar Kaw Ph.D."
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:  html_document
#'
#' ---
#' # **Table of Contents:**
#'
#' ## Introduction
#'
#' * Brief description of the purpose of this program.
#'
#' ## Inputs:
#'
#' * Data Input: Students Response vs. Question/Item
#' * Data Input: Answer Key For Each Question
#'
#' ## Outputs:
#'
#' * Data Output: Choice Percentage Per Question
#' * Data Output: Summary of Statistical Variables vs. Items and Respective Average
#' * Data Output: Plots
#' * Data Output: Analysis of Point Biserial
#' * Data Output: Checks/Classification
#' * Data Output: Alpha for Test Subscale
#' * Data Outputs: Diagnostic Classification Modeling (DCM)
#'
#' ***
#'
#' # **Introduction:**
#'
#' The purpose of this generated report is to provide the analytical framework
#' proposed in the paper *An Analytic Framework for Evaluating the Validity of
#' Concept Inventory Claims (Natalie Jorion et al., 2015)* from the University
#' of Chicago, while providing extra statistical routines based on Classical
#' Test Theory. Within the contents of this report, you will find graphical
#' representations such as plots and graphs and tables all intended to validate
#' the 3 claims proposed in Jorion's paper (Natalie Jorion et al., 2015).
#'
#' # **Inputs**
#'
#' Here we will provide the inputs required for the program to run and in turn
#' generate this report. These inputs will be:
#'
#' * a table containing the student's responses per item.
#' * a table containing the answer key per item.
#'
#' **Note:** when inputting the student responses, all rows containing blanks or
#' NaN in your data frame will be discarded and the code will continue its
#' trajectory. For further information on the structure of the data, refer to
#' the document that comes with the folder containing this code.
#'
#'
# ---- Initialization ----
##### START: Installing Packages #####

if (require('calibrate')) {
  (a <-
     'All packages have been succesfully installed, code is loading')
} else {
  install.packages('calibrate')



}

# The package calibrate will help in this code to obtain the section that has to
# do with plots.
# Refer to this website for more information:
# https://cran.r-project.org/web/packages/calibrate/index.html

if (require('lavaan')) {
  (a <-
     'All packages have been succesfully installed, code is loading')
} else {
  install.packages('lavaan')
}

# The Lavaan package will help you to produce a CFA analysis
# Webpage to get the package manual:
# https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
# Webpage for tutorials: http://lavaan.ugent.be/tutorial/cfa.html
# NOTES: Will allow you to both do a CFA analysis and write your own model to
# input to the cfa() function.
######

if (require('ltm')) {
  (a <-
     'All packages have been succesfully installed, code is loading')
} else {
  install.packages('ltm')
}
# Webpage to get the package manual:
# NOTES: The ltm package will help you produce th IRT plots in Jorion's paper
######

if (require('psych')) {
  (a <-
     'All packages have been succesfully installed, code is loading')
} else {
  install.packages('psych')
}
# Webpage to get the package manual: https://cran.r-project.org/web/packages/psych/psych.pdf
# NOTES: The psych package is very important. It will help you produce the EFA
# analysis in Jorion's Paper Also it provides helpful commands such as alpha()
# used to calculate alpha subscales in Jorion's paper
######

if (require('semPlot')) {
  (a <-
     'All packages have been succesfully installed, code is loading')
} else {
  install.packages('semPlot')
}
# Webpage to get the package manual:https://cran.r-project.org/web/packages/semPlot/semPlot.pdf
# NOTES: Will allow you to plot CFA.
######

if (require('GPArotation')) {
  (a <-
     'All packages have been succesfully installed, code is loading')
} else {
  install.packages('GPArotation')
}

if (require('psychometric')) {
  (a <-
     'All packages have been succesfully installed, code is loading')
} else {
  install.packages('psychometric')
}

##### START: Packages to load with its respective usage: #####

### calibrate ###
library(calibrate)
# Webpage to get the package manual:
# https://cran.r-project.org/web/packages/calibrate/calibrate.pdf
# Will help you to make plot of discrimination index vs difficulty index
######

### Lavaan: ###
library("lavaan")
# The Lavaan package will help you to produce a CFA analysis
# Webpage to get the package manual:
# https://cran.r-project.org/web/packages/lavaan/lavaan.pdf
# Webpage for tutorials: http://lavaan.ugent.be/tutorial/cfa.html
# NOTES: Will allow you to both do a CFA analysis and write your own model to
# input to the cfa() function.
######

### ltm: ###
library("ltm")
# Webpage to get the package manual:
# NOTES: The ltm package will help you produce th IRT plots in Jorion's paper
######

### psych: ###
library("psych")
# Webpage to get the package manual:
# https://cran.r-project.org/web/packages/psych/psych.pdf
# NOTES: The psych package is very important. It will help you produce the EFA
# analysis in Jorion's Paper Also it provides helpful commands such as alpha()
# used to calculate alpha subscales in Jorion's paper
######

### GPArotation ###
library("GPArotation")
# Webpage to get the package manual:
# https://cran.r-project.org/web/packages/GPArotation/GPArotation.pdf
# NOTES: Will alow you to perform oblique rotations when using fa() function.
# Recall: fa() belongs to the "psych" Package
######

### Psychometric ###
library(psychometric)
# Webpage to get the package
# manual:https://cran.r-project.org/web/packages/psychometric/psychometric.pdf
# NOTES: May be used to verify alpha calculation within this code.
######

### semPlot ###
library("semPlot")
# Webpage to get the package
# manual:https://cran.r-project.org/web/packages/semPlot/semPlot.pdf
# NOTES: Will allow you to plot CFA.
######

#### DONE: explaining packages ######

# Here I am uploading a data frame called all.data. all.data represents the
# answers of all students where:
##  1=A,  2=B,  3=C,  D=4.
# all.answ: will represent the answer key for the concept inventory tool
#### Note: both files are saved as a CSV file and used in conjunction with the
#### rea.csv() function
all.data <- read.csv(file.choose(), header = T, row.names = 1)
m = nrow(all.data)
n = ncol(all.data)

all.answ <- read.csv(file.choose(), header = T, row.names = 1)
Concepts <- all.answ[, 3]
all.answ <- all.answ[, -3]
total <- length(Concepts)
bad <- sum(is.na(Concepts))
Concepts <- Concepts[-c((total - bad + 1):total)]
col.sum <- sum(Concepts)
columns <- ncol(all.data)
if (col.sum > columns | col.sum < columns) {
  stop('The number of Questions is not equal to the sum of the elements in the Concept vector')
}

if (any(is.na(Concepts))) {
  stop('There is a non-numeric element value in the Concepts vector')
}

titless <- colnames(all.answ)
titless <- titless[2]
title_answ <- all.answ[, 2]
title_answ <- as.character(title_answ)

all.answ <- all.answ[, -2]
# Q will be a vector having elements Q1, Q2, etc.
Q = colnames(all.data)
#'
#'
#' ## Students Response vs. Question/Item.
#'
#' Here we show the responses of the students for the concept inventory tool.
#'
#'
# ---- Table -- All Responses ----
##### Will show: all.data with desired title ######
all.data <- as.data.frame(all.data)
table.all.data <- rbind(title_answ, all.data)
row.names(table.all.data)[1] <- titless

knitr::kable(table.all.data)
#### DONE #########

#'
#'
#' ## Answer Key For Each Question.
#'
#' Here we present the last input required for the code to run. This is the
#' answer key to all questions in the concept inventory tool.
#'
#'
# ---- Table -- Answer Key ----
# all.answ is the table containning the answers keys to all.data
table_keys <-  cbind(all.answ, title_answ)
colnames(table_keys) <- c('Answer Key', 'Title')
knitr::kable(table_keys)
#'

# ---- Main Code Block ----
#########################
############   Main Code    #############
#########################
# The main code starts here. here is where all the data from the inputs is
# mainly manipulated to obtain most of the statistical parameters
item.score <- matrix(0, m, n)
all.answ <- as.matrix(all.answ)
all.data <- as.matrix(all.data)
Q = colnames(all.data)

##### Cleaning Data ######
# Here, I am Eliminationg al NaN values from the original uploaded data
# (all.data)
all.data <- all.data[complete.cases(all.data), ]
m = nrow(all.data)
n = ncol(all.data)
item.score <- matrix(0, m, n)
######## Done cleaning data ######

###### START: item.score calculation ####
##  item.score will be a binary datafram that will result from comparing the
##  answer of the students with the right answer key

item.score <- matrix(0, m, n)
all.answ <- as.matrix(all.answ)
all.data <- as.matrix(all.data)
for (i in 1:m) {
  for (j in 1:n) {
    if (all.data[i, j] == all.answ[j, 1]) {
      item.score[i, j] <- 1

    } else{
      item.score[i, j] <- 0
    }
  }
}
#
######## DONE: item.score calculation ######

Q = colnames(all.data)
m = nrow(all.data)
n = ncol(all.data)

i = 0
j = 0

d.i = item.exam(item.score)
d.i <- d.i[, 4]
d.i.i = item.exam(item.score)

#####    Function  Discrimtoplot  ###
#### We are getting discrimination index with this function ###
# this function will allow you to obtain the discrimination index at the
# selected percentile once the code runs, refer to appendix: B for more
# information on the routine.

ci.score = rowSums(item.score)
item.score1 = item.score[, 1]
percentile <- 27
discrimtoplot <- function(item.score1, ci.score, percentile) {
  num_percentiles <-
    quantile(ci.score, c(percentile / 100, 1 - percentile / 100))
  num_percentiles

  counth = 0
  for (i in 1:m) {
    if (ci.score[i] >= num_percentiles[2]) {
      counth = counth + 1
    }
  }
  countl = 0
  for (i in 1:m) {
    if (ci.score[i] <= num_percentiles[1]) {
      countl = countl + 1
    }
  }
  countl
  num_percentiles[1]
  counthh = 0
  for (i in 1:m) {
    if (ci.score[i] >= num_percentiles[2] | item.score1[i] == 1) {
      counthh = counthh + 1
    }

  }
  countll = 0
  for (i in 1:m) {
    if (ci.score[i] <= num_percentiles[1] | item.score1[i] == 1) {
      countll = countll + 1
    }

  }

  a <- -(counthh - countll) / counth
  return(c(a))
}
# Note: The percentile is selected to be 27 %
##### Done Writting Function ####
#####
Q1_discrim = discrimtoplot(item.score1, ci.score, percentile)

Q.disc <- matrix(0, n)
for (i in 1:n) {
  Q.disc[i] = discrimtoplot(item.score[, i], ci.score, percentile)
}

plot(
  d.i * 100,
  Q.disc,
  xlab = 'Dificulty Index',
  ylab = 'Discrimination Index',
  xlim = c(0, 100),
  ylim = c(0, 1)
)
d.i <- as.data.frame(d.i)

item.var = matrix(0, 1, n)
for (i in 1:n) {
  item.var [i] <- var(item.score[, i])
}
item.var

#####
## Coming up with PBCC values ###
#####
PBCC.Q = matrix(0, 1, n)
for (i in 1:n) {
  PBCC.Q [i] <- cor(item.score[, i], ci.score)
}
PBCC.Q
#####
## DONE: PBCC Values ####
#####
##### Function PBCC#####
# This function is the point biserial coefficient correlation function. This
# function will alow you to calculate the pbcc for further analysis and
# comparison of items For more inofrmation on this function and also on the
# formula that would normally be used if it was to be carried by hand, refer to
# the appendix.
# x <- Ci.score
# y <- item.score1
item.score1
ptbiserial <- function(item.score1 , ci.score) {
  m <- length(ci.score)
  sumx = 0
  sumy = 0
  sumxy = 0
  sumx2 = 0
  sumy2 = 0

  x = matrix(0, m)

  for (i in 1:m) {
    x[i, 1] = ci.score[i] - item.score1[i]
  }
  # x <- Ci.score
  # y <- item.score1


  for (i in 1:m) {
    sumx = sumx + x[i]
    sumy = sumy + item.score1[i]
    sumxy = sumxy + (x[i] * item.score1[i])
    sumx2 = sumx2 + (x[i]) ^ 2
    sumy2 = sumy2 + (item.score1[i]) ^ 2
  }
  num1 = m * sumxy - (sumx * sumy)
  den1 = m * sumx2 - (sumx) ^ 2
  den2 = m * sumy2 - (sumy) ^ 2
  b <- (num1) / ((den1 * den2) ^ (0.5))
  return(c(b))
}
## Done Writting Function  ###

# Modified PT Biserial values per item ###
mod_pbcc = matrix(0, n)
ptbiserial(item.score[, 4], ci.score)
for (i in 1:n) {
  mod_pbcc[i] = ptbiserial(item.score[, i], ci.score)
}
mod_pbcc
### Done: Modified PT Biserial values per item ###

## Item Removed Score:
item.rem.score = matrix(0, m, n)
for (i in 1:m) {
  for (j in 1:n) {
    item.rem.score[i, j] = ci.score[i] - item.score[i, j]
  }
}
item.rem.var = matrix(0, n)
for (i in 1:n) {
  item.rem.var[i] = var(item.rem.score[, i])
}
item.rem.var

### alpha with itm deleted (Very Important) ###

var.sum = rowSums(item.var)
var.sum.Q = rowSums(item.var)
alpha_id = matrix(0, n)
for (i in 1:n) {
  alpha_id[i] = (nrow(all.answ) - 1) / (nrow(all.answ) - 2) * (1 - (var.sum.Q -
                                                                      item.var[i]) / (item.rem.var[i]))
}
alpha_id
#### DONE: alpha with item deleted ###

## Summary:
alpha_id = t(alpha_id)
mod_pbcc = t(mod_pbcc)
d.i <- as.data.frame(d.i)
d.i <- t(d.i)
PBCC.Q
Q.disc <- t(Q.disc)
item.var
summ <- rbind(alpha_id, d.i, Q.disc, item.var, PBCC.Q, mod_pbcc)
class(summ)
summ <- as.data.frame(summ)
row.names(summ) <-
  c(
    'Alpha With Item Deleted',
    'Difficultyn Index',
    'Discrimination Index',
    'Item Variance',
    'PBCC',
    'Modified PBCC'
  )
summ

## Continiues___________

m <- nrow(summ) + 1
n <- ncol(summ)
Check.P = matrix(0, m, n)

summ <- as.matrix(summ)

for (i in 1:n) {
  if (summ[2, i] >= 0.3 & summ[2, i] <= 0.9) {
    Check.P[1, i] <- 1
  } else{
    Check.P[1, i] <- 0
  }
}

for (i in 1:n) {
  if (summ[5, i] >= 0.3) {
    Check.P[2, i] <- 1
  } else{
    Check.P[2, i] <- 0
  }
}

for (i in 1:n) {
  if (summ[5, i] >= 0.2) {
    Check.P[3, i] <- 1
  } else{
    Check.P[3, i] <- 0
  }
}

for (i in 1:n) {
  if (summ[3, i] >= 0.3) {
    Check.P[4, i] <- 1
  } else{
    Check.P[4, i] <- 0
  }
}

for (i in 1:n) {
  if (summ[3, i] >= 0.2) {
    Check.P[5, i] <- 1
  } else{
    Check.P[5, i] <- 0
  }
}


for (i in 1:n) {
  if (summ[6, i] >= 0.2) {
    Check.P[6, i] <- 1
  } else{
    Check.P[6, i] <- 0
  }
}

AVG <- rowMeans(summ[c(2:6), c(1:24)], na.rm = T)
AVG <- as.data.frame(AVG)
AVG <- as.matrix(AVG)
ci.score

#### Alpha without Item Deleted for All Test##

alpha_with_id_alltest <-
  nrow(all.answ) / (nrow(all.answ) - 1) * (1 - var.sum / var(ci.score))

####summ<- cbind.data.frame(summ,AVG)###

for (i in 1:n) {
  if (summ[1, i] <= alpha_with_id_alltest) {
    Check.P[7, i] <- 1
  } else{
    Check.P[7, i] <- 0
  }
}
titles = c(
  'Check Difficulty >30,<90',
  'Chceck PBCC>=.3',
  ' Check PBCC>=.2',
  'Chceck Discrim>=.3',
  'Check Discrim>=.2',
  'Check Modified PBCC >=.2',
  ' Check Alpha With Item Deleted'
)
row.names(Check.P) <- titles


## Continiues
sum1 = 0

## Choice percentage calculation #####

sum1 = 0

key_ <- max(all.data)

cng <- function(all.data, key_) {
  cmg <- matrix(0, nrow = 1, ncol = ncol(all.data))

  for (i in 1:ncol(all.data)) {
    sum1 = 0

    for (j in 1:nrow(all.data)) {
      if (all.data[j, i] == key_) {
        sum1 = 1 + sum1
      } else{
        sum1 = 0 + sum1
      }
      cmg[i] <- sum1
    }
  }
  cmg <- round(cmg * 100 / (nrow(all.data)),  1)
  return(cmg)
}

cng(all.data, key_ = 1)

cmg <- matrix(0, nrow = max(all.data), ncol = ncol(all.data))

for (i in 1:max(all.data)) {
  cmg[i, ] <- cng(all.data, key_ = i)
}

colnames(cmg) <-   Q
row.names(cmg) <- LETTERS[1:max(all.data)]
cmg
### Donce with choices table: choice.per.q ####


#### New Function: keepornot #####
AA <- d.i[1]
BB <- summ[5, 1]
keepornot = function(AA, BB) {
  if (AA >= 0 & AA < 30) {
    if (BB >= .3) {
      g = c('review')
      return(g)
    }
    else if (BB >= .15 & BB < .3) {
      g = c('review/toss')
      return(g)
    } else if (BB >= 0 & BB < .15) {
      g = c('toss')
      return(g)
    }
  }
  else if (AA >= 30 & AA < 50) {
    if (BB >= .3) {
      g = c('keep/toughie')
      return(g)
    } else if (BB >= .15 & BB < .3) {
      g = c('review')
      return(g)
    } else if (BB >= 0 & BB <= .15) {
      g = c('review/toss')
      return(g)
    } else if (BB < 0) {
      g = c('toss')
      return(g)
    }
  }

  else if (AA >= 50 & AA < 80) {
    if (BB >= .3) {
      g = c('keep')
      return(g)
    } else if (BB >= .15 & BB < .3) {
      g = c('keep')
      return(g)
    } else if (BB >= 0 & BB < .15) {
      g = c('keep/review')
      return(g)
    } else if (BB < 0) {
      g = c('review')
      return(g)
    }
  }

  else if (AA >= 80) {
    if (BB >= .3) {
      g = c('keep')
      return(g)

    } else if (BB >= .15 & BB < .3) {
      g = c('keep')
      return(g)
    } else if (BB >= 0 & BB < .15) {
      g = c('keep/review')
      return(g)
    } else if (BB < 0) {
      g = c('review')
      return(g)
    }
  }
  g
  return(g)
}
##### DONE: function #####
s <- keepornot(AA, BB)
s

d.i <- d.i * 100
g = 0
c.ver = matrix(0, n)
for (i in 1:n) {
  c.ver[i] = keepornot(d.i[i], summ[5, i])
}

##Continiue_2/17/16

c.strin <- matrix(0, 1, n, byrow = T)
for (i in 1:n) {
  if (Check.P[1, i] + Check.P[2, i] == 2) {
    c.strin[i] = c('keep')
  } else{
    c.strin[i] = c('toss')
  }
}
c.strin

c.jorion <- matrix(0, 1, n, byrow = T)
for (i in 1:n) {
  if (Check.P[1, i] + Check.P[3, i] == 2) {
    c.jorion[i] = c('keep')
  } else{
    c.jorion[i] = c('toss')
  }
}

c.jorion

c.alpha = matrix(0, 1, n, byrow = T)

for (i in 1:n) {
  if (Check.P[7, i] == 1) {
    c.alpha[i] = c('keep')
  } else{
    c.alpha[i] = c('toss')
  }
}
c.alpha
c.ver <- t(c.ver)
check_class <- rbind(c.alpha, c.jorion, c.ver, c.strin)
colnames(check_class) <- Q
rownames(check_class) <-
  c('Check Alpha',
    'Check Jorion',
    'Check Versatile',
    'Check Stringent')

check_class

#####   START: alpha_subscale ##############

## alpha for subscale ##
# Design of Vector and Important variables
# NOTE: In this part of the code, we use scan() to prompt the user
# to enter the number of questions per items in a vector called:  VECTOR The
# lenght of the vector 'VECTOR' will determine how many concepts the test has.
# The numerical value of each element will determine how many items/Questions
# per concept we will use


#elemento <- readline('Enter number of questions per concept :  ')
#elemento<- as.numeric(elemento)
#largo <- readline('Enter number of concept :  ' )
#largo<- as.numeric(largo)
#vector<- matrix(elemento,nrow = 1,ncol = largo,byrow = T)

# largo <- readline('Enter number of concepts :  ' )
# largo<- as.numeric(largo)
# vector <- matrix(0,nrow = 1,ncol = largo)
# for (i in 1:largo){
#   #vector[i]<- readline('Enter number of questions per concept :  ')
#   vector[i]<- readline(paste('Enter number of questions per concept ',i,":  "))
#   vector <- as.numeric(vector)
# }

vector <- Concepts
vector <- as.matrix(vector)
item.score <- as.matrix(item.score)
max.n <- max(vector)
vector <- as.matrix(vector)
concept.n <- length(vector)
# Design matrix with answers

#alpha.subscale <- matrix(0,concept.n,1,byrow = T)
#alpha.subscale<- as.data.frame(alpha.subscale)
#cuenta=0
#for (  i in 1:concept.n  )   {
#   alpha.subscale [i,1]=as.numeric(    cronbach.alpha( item.score[,  c((1+cuenta):(i*max.n) )   ]    )$alpha  )
#  cuenta=cuenta+3
# }

lame = 0
lola = 0
alpha.subscale <- matrix(0, concept.n, 1, byrow = T)
alpha.subscale <- as.data.frame(alpha.subscale)
i = 0
for (i in 1:concept.n) {
  alpha.subscale[i, 1] <-
    cronbach.alpha(item.score[, as.numeric(1 + lame):as.numeric(lola + vector[i])])$alpha
  lame = vector[i] + lola
  lola = vector[i, ] + lola
}

alpha.subscale <- signif(alpha.subscale, digits = 2)
colnames(alpha.subscale) <- 'Alpha Per Concept/Group'
concepts <- LETTERS[1:concept.n]
vector <- as.data.frame(vector)
sub.scale <- cbind(concepts, alpha.subscale, vector)

colnames(sub.scale) <-
  c('Concepts',
    'Alpha Per Concept/Group',
    'Number of Items per Concept')


####### DONE: Alpha subScale #########

####### Tetrachoric Work ###############
# Tetrachoric correlation is used when the data of interest is binary; that is
# Dichotomous. Furthermore, the function used is only available with the psych
# package.
# tet.cor: will be a list containing the tetrachoric correlation matrix as
# element 1 in the list
# Q: is a vector containing the name that each question will have

colnames(item.score) <- Q
tet.cor <- tetrachoric(item.score)
Q_dim <- length(Q)
Q_decreasing <- matrix(0, Q_dim, 1)
for (i in 1:Q_dim) {
  uuv <- Q_dim - i + 1
  Q_decreasing[i, 1] <- Q[uuv]
}

Q_decreasing

###### done with function ##########

#### NEW Function: mirror.matrix ###
# it is a function designed to allocate the tetrachoric correlation matrix in a
# presentable manner.
tetra.corr <- tet.cor[[1]]
TTA <- tetra.corr
n <- nrow(TTA)
B <- matrix(0, n, n)

mirror.matrix <- function(TTA, B) {
  for (i in 1:n) {
    for (j in 1:n) {
      uuv = n - i + 1
      B[uuv, j] <- TTA[i, j]
    }
  }
  return(B)
}

##### DONE : mirror.matrix ######
# tetra.corr: is the matrix to be used to outpput the tetrachoric correlation
# plot shown in the report.
# mirror.matrix: is a function designed to give the desired format to the
# tetrachorica correlation table before it is plotted

tetra.corr <- mirror.matrix(tetra.corr, B)
row.names(tetra.corr) <- Q_decreasing
colnames(tetra.corr) <- Q
tetra.corr <- as.data.frame(tetra.corr)

png('tetraplot.png')
cor.plot(tetra.corr, numbers = TRUE, main = "Question Tetrachoric Correlation")
dev.off()
#'
#'
#' ***
#'
#' # **Outputs:**
#'
#' ## Choice Percentage Per Question:
#'
#' **Background:**
#'
#' The information in the table presented below, shows the percentage of
#' students who picked a certain choice for an specific item.
#'
#'
# ---- Table - Choice Percentage per Question ----
cmg <- round(x = cmg, digits = 0)
cmg <- as.data.frame(cmg)
table.cmg.all <- rbind(title_answ, cmg)
row.names(table.cmg.all)[1] <- titless

knitr::kable(table.cmg.all)
#'
#'
#' ## Summary of Statistical Variables vs. Items and Respective Average
#'
#' **Background:**
#'
#' Here we will be providing the results of many imperative statistical
#' parameters designed to describe the concept inventory. The statistical
#' parameters calculated in the table below show:
#'
#' * Alpha with item deleted
#' * Difficulty Index
#' * Discrimination  Index
#' * Item Variance
#' * Point Biserial Correlation Coefficient; and
#' * Modified Point Biserial Correlation Coefficient
#'
#' Please, feel free to refer to Appendix B to see the relations and formulas
#' employed to calculate the above statistical parameters.
#'
#' *Alpha with Item Deleted*, will provide a coefficient of internal
#' reliability, indicating how closely related the set of items are as a group
#' if you should eliminate the item of interest. ("Welcome to the Institute for
#' Digital Research and Education." SPSS FAQ: What Does Cronbach's Alpha Mean?
#' Idre, 02 May 2015. Web. 04 Apr. 2016).
#'
#' *Difficulty Index*, this measure prompts educators to calculate the
#' proportion of students who answered the test item accurately. By looking at
#' each alternative (for multiple choice), we can also find out if there are
#' answer choices that should be replaced.("Classroom Assessment | Basic
#' Concepts." Classroom Assessment | Basic Concepts. Classroom Assessment, n.d.
#' Web. 03 Feb. 2016.)
#'
#' *Discrimination Index*, refers to how well an assessment differentiates
#' between high and low scorers. In other words, you should be able to expect
#' that the high-performing students would select the correct answer for each
#' question more often than the low-performing students.("Classroom Assessment |
#' Basic Concepts." Classroom Assessment | Basic Concepts. Classroom Assessment,
#' n.d. Web. 03 Feb. 2016)
#'
#' *Item Variance*, it is the average of the squared difference from the mean. Measures how far a set of quantities are spread out.
#'
#' *The Point Biserial Correlation Coefficient*, here symbolized as PBCC,
#' belongs to the case where one variable is dichotomous and the other is
#' non-dichotomous. By convention, the dichotomous variable is treated as the X
#' variable, its two possible values being coded as X=0 and X=1; and the
#' non-dichotomous variable is treated as the Y variable.(Point Biserial
#' Correlation Coefficient." Point Biserial Correlation Coefficient. Richard
#' Lowry, n.d. Web. 03 Jan. 2016. )
#'
#' *Modified Point Biserial Correlation Coefficient* it is a modification of the
#' point biserial correlation, intended to calculate a correlation when X is a
#' dichotomous variable and Y isn't. Further specifications on the routine is
#' give in Appendix: B.
#'
#'
# ---- Tables - `table.summ.all` and `AVG` ----
summ = as.data.frame(summ)
summ <- round(summ, 2)
colnames(summ) <- Q
summ <- as.data.frame(summ)
table.summ.all <- rbind(title_answ, summ)
row.names(table.summ.all)[1] <- titless
knitr::kable(table.summ.all)

AVG = as.data.frame(AVG)
AVG <- round(AVG, 2)

knitr::kable(AVG)
#'
#'
#' ## Plots
#'
#'
# ---- Plot - difficulty -----
# Here we will be plotting the difficulty index values agains the discrimination
# index The next plot will be comparing the point biserical correlation
# coefficient againts the discrimination index
# d.i: is the difficulty index vector
# disc: is the discrimination index vector
# PBCC.Q: is the point biseria correlation vector
plot(
  d.i,
  Q.disc,
  xlab = 'Dificulty Index',
  ylab = 'Discrimination Index',
  xlim = c(0, 100),
  ylim = c(0, 1),
  main = ' Discrimination Index vs Difficulty Index'
)
plot(
  d.i,
  PBCC.Q,
  xlab = 'Difficulty Index',
  ylab = 'Point Biserial Correlation Coefficient',
  main = 'PBCC vs. Difficulty Index',
  xlim = c(0, 100),
  ylim = c(0, 1)
)
#'
#'
#' ## Plots With Lables.
#'
#' Plots that are shown in this section are the ones already presented above.
#' The only differences are that the following plots contain labels to
#' facilitate identification of statistical parameters.
#'
#'
# ---- Plots (repeated with labels) ----
# Here I plot the graphs concerning to classical test theory (CTT) including
# labels to identify desirable and non-desirable items:
#
# Discrimination index vs Difficulty index

plot(
  d.i,
  Q.disc,
  xlab = 'Dificulty Index',
  ylab = 'Discrimination Index',
  xlim = c(0, 100),
  ylim = c(0, 1),
  main = ' Discrimination Index vs Difficulty Index'
)

textxy(d.i,
       Q.disc,
       labs = Q,
       cex = 0.6,
       offset = 1)

abline(v = 30)
abline(v = 90)
abline(h = .21)
#'
#'
#'
# ---- Plot - CTT ----
# Here I plot the graphs concerning to classical test theory (CTT) including
# labels to identify desirable and non-desirable items:
#
# Point Biserial Correlation Coefficient vs Difficulty index

plot(
  d.i,
  PBCC.Q,
  xlab = 'Difficulty Index',
  ylab = 'Point Biserial Correlation Coefficient',
  main = 'PBCC vs. Difficulty Index',
  xlim = c(0, 100),
  ylim = c(0, 1)
)
textxy(d.i,
       PBCC.Q,
       labs = Q,
       cex = 0.6,
       offset = 1)

abline(v = 30)
abline(v = 90)
abline(h = .21)
#'
#'
#' ## Alpha For The Test
#'
#' **Background:**
#'
#' the parameter cronbach alpha is a measure of internal consistency.In other
#' words, how closely related a set of items are as a group. It is considered to
#' be a measure of scale reliability. Technically speaking, Cronbach's alpha is
#' not a statistical routine or test; instead, it is a coefficient of
#' reliability (or consistency)("Welcome to the Institute for Digital Research
#' and Education." SPSS FAQ: What Does Cronbach's Alpha Mean? Idre, 02 May 2015.
#' Web. 04 Apr. 2016).
#'
#'
# ---- Table - cronbach alpha - not used ----
alpha_with_id_alltest <- round(alpha_with_id_alltest, 2)

#knitr::kable(alpha_with_id_alltest)
#'
#' So, the Cronbach alpha reliability coefficient
#' resulted to be: $\alpha$= `r alpha_with_id_alltest`
#'
#' ## Suggestions Based on Criteria
#'
#' ### Analysis
#'
#'
# ---- Table - check class table - table.check.all ----
#### Showing check class table #############
# This table will show checks on the items and suggest to keep it or throw it
# way.

Check.P = as.data.frame(Check.P)
colnames(Check.P) <- Q

Check.P <- as.data.frame(Check.P)
table.check.all <- rbind(title_answ, Check.P)
row.names(table.check.all)[1] <- titless
knitr::kable(table.check.all)

### Done doing the check ##########
#'
#'
#' ### Check/Classification
#'
#'
# ---- Code Formatted - Check/classification ----
check_class <- as.matrix(check_class, stringsAsFactors = FALSE)

title_answ <- as.matrix(title_answ, stringsAsFactors = FALSE)

table.class.all <- rbind(t(unname(title_answ)), unname(check_class))
colnames(table.class.all) <- Q
rownames(table.class.all) <-
  c('Titles',
    'Check Alpha',
    'Check Jorion',
    'Check Versatile',
    'Check Stringent')

table.class.all
#knitr::kable(table.class.all)
#'
#'
#' ## Item Response Theory
#'
#'
# ---- Code Formatted - IRT ----
item.score <- as.data.frame(item.score)
# 1 PL Fit
fit1 <-
  rasch(item.score, constraint = cbind(length(item.score) + 1, 1))
summary(fit1)

# 2 PL Fit
fit2 <- ltm(item.score ~ z1)
summary(fit2)

# 3 PL Fit
fit3 <- tpm(item.score, type = "latent.trait", max.guessing = 1)
#'
#'
#'
# ---- some kind of strange ----
#### You will have to fix this ###
onevstwo <- anova(fit1, fit2)
e <- onevstwo[[3]]
twovsthree <- anova(fit2, fit3)
ee <- twovsthree [[3]]
onevsthree <- anova(fit1, fit3)
eee <- onevsthree[[3]]
eeee <- c(e, ee, eee)
eeeee <- min(eeee)
if (eeeee == eeee[1]) {
  eeeeee = fit1

} else if (eeeee == eeee[2]) {
  eeeeee = fit2
} else if (eeeee == eeee[3]) {
  eeeeee = fit3
}
if (eeeee == eeee[1]) {
  rrr = 'we used fit1'

} else if (eeeee == eeee[2]) {
  rrr = 'we used fit2'
} else if (eeeee == eeee[3]) {
  rrr = 'we used fit 3'
}
rrr
#'
#'
#' **Background:**
#'
#' IRT stands for item response theory, and they are models that have been
#' largely used with large-scale assessment, such as those done in education and
#' for professional licensure. The tradition is to respresent these models with
#' the classical graphical representation of the item characteristic curves
#' (ICC) shown below (Beaujean, A. Alexander. Latent Variable Modeling Using R:
#' A Step by Step Guide. Hove: Routledge, 2014):
#'
#'
# ---- Plot - IRT Curves ----
######## IRT curve Plots ##########
plot(
  eeeeee,
  legend = TRUE,
  cx = "bottomright",
  lwd = 3,
  cex.main = 1.3,
  cex.lab = 1.4,
  cex = 0.5
)
#'
#'
#' For better appreciation and further decision making regarding keeping or
#' discarding a certain item, this report presents, below, the IRT curves per
#' item. In that way, the user may identify which questions requires further
#' analysis or may be discarded.
#'
#'
# ---- Plots - Multiple IRT plots ----
n <- length(Q)

for (i in 1:n) {
  plot(
    eeeeee,
    legend = TRUE,
    item = i,
    cx = "topright",
    lwd = 1,
    cex.main = 1.5,
    cex.lab = 1.3,
    cex = 0.7
  )
}
#'
#'
#' ## Alpha For Test Subscale
#'
#' **Background:**
#'
#' Subscale reliabilities (Cronbach's alphas) were calculated for each of the
#' developers concepts.Because alpha is influenced by test length (i.e. number
#' of observations), lower values of subscale alphas usually yield from low
#' numbers of items per subscale.( Natalie Jorion D.Gane, Brian, Katie James,
#' Lianne Schroeder, Louis V DiBello, and James Pellegrino. "An Analytic
#' Framework for Evaluating the Validity of Concept Inventory Claims."
#' ResearchGate. University of Illinois AtChicago, 1 Oct. 2015. Web. 02 Jan.
#' 2016.)
#'
#'
# ---- Table - Alpha Test Subscale ----
knitr::kable(sub.scale)
#'
#'
#' ## Tetrachoric Correlation Table
#'
#' **Background:**
#'
#' Tetrachoric correlation approximates what the Pearson correlation would be
#' between two dichotomous variables if they were recorded on their true
#' continuous scale.(Beaujean, A. Alexander. Latent Variable Modeling Using R: A
#' Step by Step Guide. Hove: Routledge, 2014.)
#'
#'
# ---- Table - Tetrachoric Correlation ----
tetra.corr <- round(tetra.corr, 2)
tet.cor[[1]] <- round(tet.cor[[1]], 1)


knitr::kable(tet.cor[[1]])
#'
#'
#' ![Tetrachoric Correlation, Heatmap](tetraplot.png)
#'
#' ## Exploratory Factor Analysis
#'
#' **Background:**
#'
#' The goal of exploratory analysis is to explore all possible latent variable
#' patterns within the data set or, in this case, the concept inventory (Finch,
#' W. Holmes, and Brian F. French. Latent Variable Modeling with R. New York:
#' Routledge, 2015.).
#'
#' ### Parallel Analysis For Optimal Number of Factors
#'
#' Parallel Analysis is a Monte Carlo simulation method that helps researchers
#' in obtaining the most desirable number of factors to retain in Principal
#' Component and Exploratory Factor Analysis. This method provides a superior
#' choice to other methods and routines that are usually employed for that
#' purpose, such as the Scree test or the Kaiser's eigenvalue-greater-than-one
#' rule. Furthermore, Parallel Analysis is not well known among
#' researchers/educators, mainly because it is not included as an analysis
#' method in the most popular statistical softwares out there. (Ledesma, Rub?n
#' Daniel, and Pedro Valero-Mora. Determining the Number of Factors to Retain in
#' EFA: An Easy-to- Use Computer Program for Carrying out Parallel Analysis
#' (n.d.): n. pag. <Http://pareonline.net/>. Practical Assessment, Research &
#' Evaluation, Feb. 2007. Web. Feb. 2016.)
#'
#'
horn <-
  fa.parallel(x = item.score,
              n.obs = nrow(item.score),
              fm = 'ml')
horn
num_factoors <-
  horn$nfact # Recall: this is the appropiate number of factors.
#'
#'
#' ### EFA: Loadings and Inter-Factor Correlation
#'
#'
# ---- Code Formatted - EFA Loadings and Inter-Factor Correlation ----
fa.2 <-
  fa(item.score,
     nfactors = horn$nfact,
     rotate = 'bentlerQ',
     fm = 'wls')
print.psych(fa.2, digits = 2, cut = .39)

### Best Results#
# print.psych(fa(item.score[,-26],nfactors = horn$nfact,rotate = 'bentlerQ',fm='ml'),digits = 2,cut = .39)
#'
#'
#' ## Confirmatory Factor Analysys
#'
#' **Background:**
#'
#' when strong
#' theory does exist regarding the nature of the latent structure of the data,
#' and there is exploratory work suggesting the nature of this structure, then
#' CFA may be most appropriate. In this context, the researcher has both a
#' conceptual foundation regarding the latent variables and their relationships
#' with the observed indicators that is grounded in the literature, and results
#' from empirical work suggesting a limited number of possible factor
#' structures. The goal in this case is not to explore all possible latent
#' variable patterns, as with EFA, but rather to assess which of the
#' hypothesized or specified models is most likely given the data at
#' hand.(Finch, W. Holmes, and Brian F. French. Latent Variable Modeling with R.
#' New York: Routledge, 2015.)
#'
#' ### Independence Model
#'
#' Code to be completed
#' Please refer to the following web page for further assistance:
#' <http://lavaan.ugent.be/tutorial/cfa.html> Also, for plotting the line paths
#' schematic, you may need to refer to the following website on the package
#' semPLOT: <https://cran.r-project.org/web/packages/semPlot/semPlot.pdf>
#'
#'
# ---- Incomplete - Independence Model ----
# Here we will calculate and define the first model employed in Jorion's paper:
# the Independence model. Such model is to be orthogonal For this part of the
# code, we will have to use the lavaan package since it facilitates the
# calculation of the CFA model Please, for further asisstance refer to the
# following web page, URL below:
# http://lavaan.ugent.be/tutorial/cfa.html





# HS.model.full <- '
# A =~ Q1 + Q2 + Q3
# B =~ Q4 + Q5 + Q6
# C =~ Q7 + Q8 + Q9
# D =~ Q10 + Q11 + Q12
# E =~ Q13 + Q14 + Q15
# F =~ Q16 + Q17 + Q18
# G =~ Q19 + Q20 + Q21
# H =~ Q22 + Q23 + Q24
# I =~ Q25 + Q26 + Q27'
#
# nigi<- tetrachoric(item.score)
#
# tetra_table<-  nigi$rho
# item.score <- as.data.frame(item.score)
# stan<- SD(item.score)
# cov.tet<- cor2cov(R = tetra_table,sds =stan)
# colnames(cov.tet) <- Q
# rownames(cov.tet) <- Q
# fit <- cfa(HS.model.full,sample.cov =cov.tet , orthogonal = TRUE, sample.nobs = m, std.lv = TRUE)
# summary(fit, fit.measures = TRUE)
# semPaths(fit, layout = 'tree', sizeMan = 3, sizeLat = 4, fixedStyle = 5)
#'
#'
#' ### Higher Order Model
#'
#' Code to be completed
#' Please refer to the following web page for further asisstance:
#' <http://lavaan.ugent.be/tutorial/cfa.html> Also, for plotting the line paths
#' schematic, you may need to refer to the following website on the package
#' semPLOT:
#' <https://cran.r-project.org/web/packages/semPlot/semPlot.pdf>
#'
#'
# ---- Incomplete - Higher Order Model ----
# Here we will calculate and define the 2nd model employed in Jorion's paper:
# the Higher Order Model. Such model is not fully orthogonal Instead, it shares
# correlations and variances with an extra factor called: Statics For this part
# of the code, we will have to use the lavaan package since it facilitates the
# calculation of the CFA model Please, for further asisstance refer to the
# following web page, URL below:
# http://lavaan.ugent.be/tutorial/cfa.html

# HS.model.full2 <- '
# A =~ Q1 + Q2 + Q3
# B =~ Q4 + Q5 + Q6
# C =~ Q7 + Q8 + Q9
# D =~ Q10 + Q11 + Q12
# E =~ Q13 + Q14 + Q15
# F =~ Q16 + Q17 + Q18
# G =~ Q19 + Q20 + Q21
# H =~ Q22 + Q23 + Q24
# I =~ Q25 + Q26 + Q27
# ZZ =~ A+B+C+D+E+F+G+H+I
#
# A ~~ 0*B
# A ~~ 0*C
# A ~~ 0*D
# A ~~ 0*E
# A ~~ 0*F
# A ~~ 0*G
# A ~~ 0*H
# A ~~ 0*I
#
#
#
# B ~~ 0*C
# B ~~ 0*D
# B ~~ 0*E
# B ~~ 0*F
# B ~~ 0*G
# B ~~ 0*H
# B ~~ 0*I
#
#
#
#
# C ~~ 0*D
# C ~~ 0*E
# C ~~ 0*F
# C ~~ 0*G
# C ~~ 0*H
# C ~~ 0*I
#
#
#
# D ~~ 0*E
# D ~~ 0*F
# D ~~ 0*G
# D ~~ 0*H
# D ~~ 0*I
#
#
#
# E ~~ 0*F
# E ~~ 0*G
# E ~~ 0*H
# E ~~ 0*I
#
#
#
# F ~~ 0*G
# F ~~ 0*H
# F ~~ 0*I
#
#
#
# G ~~ 0*H
# G ~~ 0*I
#
#
# H ~~ 0*I'
#
#
#
# fit2 <-cfa(HS.model.full2,sample.cov =cov.tet ,sample.nobs = m, std.lv = T)
# summary(fit2, fit.measures = TRUE)
#
# semPaths(object = fit2, layout = 'tree', edge.label.cex = 0.2, sizeLat = 4, subRes =2, fixedStyle = 5, freeStyle = 1, thresholdSize = 0.9, sizeInt = 2, sizeMan = 3, nCharNodes = 2, nCharEdges = 3)

#'
#'
#'
#' ## Diagnostic Classification Modeling (DCM)
#'
#' **Background:**
#'
#' Provides an evaluation of whether reliable reporting of student profiles of
#' individual concept proficiencies is possible (Natalie Jorion D.Gane, Brian,
#' Katie James, Lianne Schroeder, Louis V DiBello, and James Pellegrino.
#' (Natalie Jorion et al., 2015)
#'
#' In the tables below, note that the upper row of each group, identified by the
#' choice of interest, is the percentage of students who picked that choice who
#' lie in the upper selected percentile. Conversely, the lower row per each
#' choice group will be the percentage of people who picked that choice that lie
#' in the lower selected percentile.
#'
#' ### Percentage of the upper and lower 50 % that picked each choice with respect to the total
#'
#'
# ---- Multiple - Diagnostic Classification Modeling ----
r.w <- item.score
a = all.data[, 1]
b <- rowSums(r.w)
c <- .5
d <- 1
m <- nrow(all.data)
n <- ncol(all.data)

##### NEW Function: Discrimchoicehigh ########
#
# Will tell you the percentage of people who got a certain question right that
# belongs to the top percentile.
# You get to choose the top or low percentile
#
#
Discrimchoicehigh <- function(a, b, c, d) {
  R <- nrow(r.w)
  f <- c / 100
  inp <- quantile(b, 1 - c)
  inp1 <- quantile(b, c)
  chc <- 0



  for (i in 1:R)    {
    if (b[i] >= inp & a[i] == d)  {
      chc <- chc + 1
    }
  }
  result <- round(chc / R * 100, digits = 1)
  return(result)

}

### DONE: Discrimchoicehigh ##########

Discrimchoicehigh(a, b, c, d)
up <- matrix(0,
             nrow = max(all.data),
             ncol = n,
             byrow = T)
for (j in 1:max(all.data)) {
  for (i in 1:n) {
    up[j, i] <- Discrimchoicehigh(all.data[, i], rowSums(r.w), 0.5, j)
  }
}

##### Using Discrimchoicehigh #########

Discrimchoicelow <- function (a, b, c, d) {
  R <- nrow(r.w)
  f <- c / 100
  inp <- quantile(b, 1 - c)
  inp1 <- quantile(b, c)
  clc <- 0

  for (i in 1:R)    {
    if (b[i] < inp1 & a[i] == d)  {
      clc <- clc + 1
    }
  }
  result <- round(clc / R * 100, digits = 1)
  return(result)
}

Discrimchoicelow(a, b, c, d)
low <- matrix(0,
              nrow = max(all.data),
              ncol = n,
              byrow = T)
for (j in 1:max(all.data)) {
  for (i in 1:n) {
    low[j, i] <- Discrimchoicelow(all.data[, i], rowSums(r.w), 0.5, j)
  }
}
Q <- colnames(all.data)
choices <-
  c(
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z'
  )
choice_n <- max(all.data)
choices <- choices[c(1:choice_n)]

rownames(up) <- choices
rownames(low) <- choices

colnames(up) <- Q
colnames(low) <- Q


letters_names <- matrix(0, choice_n, 2)

final_dcm_5 <-
  matrix(0, nrow = 2 * max(all.data), ncol = ncol(all.data))

finalll <- matrix(0, 1, ncol = ncol(all.data))
for (i in 1:choice_n) {
  final_dcm_5 <- rbind(up[i, ], low[i, ])
  finalll <- rbind(finalll, final_dcm_5)

}

for (i in 1:choice_n) {
  for (j in 1:2) {
    letters_names [i, j] <- LETTERS[i]
  }
}
names_dcm <- sort(c(letters_names))

finalll <- finalll[-1, ]

row.names(finalll) <- names_dcm

#'
#'
#'
# ---- Table - finalll?? ----
knitr::kable(finalll)
#'
#'
#' ### Percentage of the upper and lower (1/3)=33.3 % that picked each choice with respect to the total
#'
#'
# ---- Prep - Percentage upper and lower that chose each choice ----
##### Creating matrix low.3 ########
#
# This matrix will enable
#

low.3 <- matrix(0,
                nrow = max(all.data),
                ncol = n,
                byrow = T)
for (j in 1:max(all.data)) {
  for (i in 1:n) {
    low.3[j, i] <- Discrimchoicelow(all.data[, i], rowSums(r.w), 1 / 3, j)
  }
}

up.3 <- matrix(0,
               nrow = max(all.data),
               ncol = n,
               byrow = T)
for (j in 1:max(all.data)) {
  for (i in 1:n) {
    up.3[j, i] <- Discrimchoicehigh(all.data[, i], rowSums(r.w), 1 / 3, j)
  }
}

rownames(up.3) <- choices
rownames(low.3) <- choices

(colnames(up.3) <- Q)
colnames(low.3) <- Q

final_dcm_3 <-
  matrix(0, nrow = 2 * max(all.data), ncol = ncol(all.data))

finalll.3 <- matrix(0, 1, ncol = ncol(all.data))
for (i in 1:choice_n) {
  final_dcm_3 <- rbind(up.3[i, ], low.3[i, ])
  finalll.3 <- rbind(finalll.3, final_dcm_3)
}

for (i in 1:choice_n) {
  for (j in 1:2) {
    letters_names [i, j] <- LETTERS[i]
  }
}
names_dcm <- sort(c(letters_names))

finalll.3 <- finalll.3[-1, ]

row.names(finalll.3) <- names_dcm
#'
#'
#'
#'
# ---- Table - Percentage upper and lower that chose each choice ----
knitr::kable(finalll.3)
#'
#'
#' ***
#'
#' # **Appendix**
#'
#' # Appendix A: My Functions
#'
#' ## discrimtoplot
#'
#' Computes the discrimination index in a concept inventory
#'
#' #### Inputs
#'
#' item.score1,ci.score,percentile
#'
#' * item.score1 - Vector variable with binary elements.
#' * ci.score - The score of each student (how many right answers)
#' * percentile - percentile to use to allocate the data
#'
#' #### Outputs
#'
#' * Q1_discrim: discrimination index for the vector variable you just used in itemscore1; that is, disrimination index of the question/item.
#'
#' #### Calling the function
#'
#' Q1_discrim=discrimtoplot(item.score1,ci.score,percentile)
#'
#' ## mirror.matrix
#'
#' Outputs a rearranged covariance or correlation matrix so that item numbers
#' are in ascending order from down to up and left to right.
#'
#' #### Inputs
#'
#' * TTA - output correlation matrix from tetrachoric() function
#' * B - n by n matrix (where n is the number of columns and rows TTA posesses) filled with 0s
#'
#' #### Outputs
#'
#' * B: New tetrachoric correlation ordered in ascending manner starting from the leftmost item  and down in the bottom item.
#'
#' #### Calling the function
#'
#' B<- mirror.matrix(TTA,B)
#'
#' ## keepornot
#'
#' Compares difficulty index to PBCC and based on a criteria it suggests wether
#' item should be kept or not
#'
#' #### Inputs
#'
#' * AA - difficulty index value of a question/item
#' * BB - PBCC value of a question/item
#'
#' #### Outputs
#'
#' * s: A string specifiying to keep or get rid off a question.
#'
#' #### Calling the function
#'
#' s<- keepornot(AA,BB)
#'
#' ## ptbiserial
#'
#' It is a correlation coefficient used when one varibale is dichotomous. This
#' function will output this correlation coefficient for each item/question.
#'
#' #### Inputs:
#'
#' * item.score1: Vector column containing all the responses of all students.
#' * ci.score: Vector containing the total scores of each student for the concept inventory.
#'
#' #### Outputs:
#'
#' * Point biserial correlation for an item
#'
#' #### Calling the function
#'
#' PT<- ptbiserial(item.score1,ci.score)
#'
#' ## Discrimchoicehigh
#'
#' Will tell you the percentage of people who got a certain item right that
#' belongs to the top, selected, percentile. You get to choose the  percentile.
#'
#' #### Inputs:
#'
#' * a: Answer choice per in item 1. Needs to be a column vector.
#' * b: Total scores of all students in concept inventory
#' * c: Percentile to be picked
#' * d: Choice picked to calculate this percentage
#'
#' #### Outputs:
#'
#' * Percentage of the number of students in the top n percentile that picked a choice d (input)
#'
#' #### Calling the function
#'
#' up.50th<- Discrimchoicehigh(a, b, c, d)
#'
#' ## Discrimchoicelow
#'
#' Will tell you the percentage of people who got a certain item right that
#' belongs to the bottom selected, percentile. You get to choose the percentile.
#'
#' #### Inputs:
#'
#' * a: Answer choice per in item 1. Needs to be a column vector.
#' * b: Total scores of all students in concept inventory
#' * c: Percentile to be picked
#' * d: Choice picked to calculate this percentage
#'
#' #### Outputs:
#'
#' * Percentage of the number of students in the bottom n percentile that picked a choice d (input)
#'
#' #### Calling the function
#'
#' low.50th<- Discrimchoicelow(a, b, c, d)
#'
#' ***
#'
#' # Appendix B: My relations and formulas
#'
#' Here it is shown the required formulas of interest employed for the
#' calculation of the statistical parameters and rutines displayed in this
#' report.
#'
#' > Cronbach Alpha
#'
#' $$ \alpha = \frac{K}{K-1}*\left ( 1-\frac{\sum_{i=1}^{K} \sigma _{Y}^{2}}{\sigma _{X }^{2}} \right ) $$
#'
#' * $\alpha$ <- Alpha, the realiability coefficient
#' * K <- The total number of items/questions that concept inventory has
#' * $\sigma _{Y}^{2}$ <- The variance of each question
#' * $\sigma _{X}^{2}$ <-Variance of the test scores
#'
#' > Difficulty Index
#'
#' $$ Difficulty.Index = \frac{X_{ia}}{N} $$
#'
#' * $Difficulty.Index$ <- Yields a number indicating how difficult can a an
#' item be considered. A low numbe would describe a challenging question while a
#' high number, clse to 1, would describe an easy question.
#' * $X_{ia}$ <- The number of students that goat a ceratain question right,
#' where, i, represents the question number and a the choice that was
#' correspondingly right for that question number.
#' * $N$ <- The number of total students who took the concept inventory tool.
#'
#'
#' > Discrimination Index
#'
#' $$ Discrimination.Index=\left (    \frac{h}{H}-\frac{l}{L}\right ) $$
#'
#' * $Discrimination.Index$ <- If positive, the closet to 1 means the more
#' likely that high performing students were more likely to get this item
#' correct. Conversely, if negative and closest to -1, the more likely low
#' performing students will get the question right.
#' * $h$ <-  the number of students who got the right answer that lie above the
#' selected percentile.
#' * $H$ <- the number of students that lie above the selected persentile.
#' * $l$ <- the number of students who got the right answer that lie below the selected percentile.
#' * $L$ <- the number of students that lie below the selected persentile.
#'
#'
#' > Modified Point Biserial Correlation Coefficient
#'
#' $$ Modified PBCC=\frac{N\cdot \sum _{i=1}^{N}x_{i}y_{i}-\sum _{i=1}^{N}x_{i}\cdot \sum _{i=1}^{N}y_{i}}{\sqrt{\left ( N\cdot \sum _{i=1}^{N}x_{i}^{2}-\left(\sum _{i=1}^{N}x_{i} \right )^{2}\right )\left (   N\cdot \sum _{i=1}^{N}y_{i}^{2}-\left(\sum _{i=1}^{N}y_{i} \right )^{2}\right )}} $$
#'
#' * PBCC <- Gives the correlation between a dichotomous variable and a non dichotomous variable.
#' * $x$ <- Item Scores.
#' * $y$ <- Test Scores.
#'
#'
#' > Variance
#'
#' $$ \sigma ^{2}=\frac{\sum (X-\mu )^{2}}{N} $$
#'
#' * $\sigma^{2}$ <- The actual variance calculated for a particular item/question.
#' * $X$ <- Item selected sequentially
#' * $\mu$ <- The mean of a set of student's responses per question
#' * N <- Total number of students
#'
#' ***
#'
#' # **References**
#'
#' Beaujean, A. Alexander. Latent Variable Modeling Using R: A Step by Step
#' Guide. Hove: Routledge, 2014. Print.
#'
#' Bernaards, Coen, and Robert Jennrich. "CRAN - Package GPArotation." CRAN -
#' Package GPArotation. CRAN, 19 Feb. 2015. Web. Feb.-Mar. 2016.
#' <https://cran.r-project.org/web/packages/GPArotation/>.
#'
#' "Classroom Assessment | Basic Concepts." Classroom Assessment | Basic
#' Concepts. Calssroom Asessment, n.d. Web. 03 Feb. 2016.
#' <http://fcit.usf.edu/assessment/selected/responsec.html>.
#'
#'  Natalie Jorion D.Gane, Brian, Katie James, Lianne Schroeder, Louis V
#'  DiBello, and James Pellegrino. "An Analytic Framework for Evaluating the
#'  Validity of Concept Inventory Claims." ResearchGate. University of Illinois
#'  AtChicago, 1 Oct. 2015. Web. 02 Jan. 2016.
#'  <https://www.researchgate.net/publication/283818695_An_Analytic_Framework_for_Evaluating_the_Validity_of_Concept_Inventory_Claims>.
#'
#' Epskamp, Sacha. "CRAN - Package SemPlot." CRAN - Package SemPlot. CRAN, 15
#' Aug. 2015. Web. 10 Apr. 2016.
#' <https://cran.r-project.org/web/packages/semPlot/>.
#'
#' Finch, W. Holmes, and Brian F. French. Latent Variable Modeling with R. New
#' York: Routledge, 2015. Print.
#'
#' Fletcher, Thomas D. "CRAN - Package Psychometric." CRAN - Package
#' Psychometric. CRAN, 08 July 2010. Web. 05 Apr. 2016.
#' <https://cran.r-project.org/web/packages/psychometric>.
#'
#' Graffelman, Jan. "Contributed Packages." CRAN -. CRAN, 19 Feb. 2015. Web. 20
#' Apr. 2016. <https://cran.r-project.org/web/packages/>.
#'
#' Ledesma, Rub?n Daniel, and Pedro Valero-Mora. Determining the Number of
#' Factors to Retain in EFA: An Easy-to- Use Computer Program for Carrying out
#' Parallel Analysis (n.d.): n. pag. <Http://pareonline.net/>. Practical
#' Assessment, Research & Evaluation, Feb. 2007. Web. Feb. 2016.
#'
#'  "Point Biserial Correlation Coefficient." Point Biserial Correlation
#'  Coefficient. Richard Lowry, n.d. Web. 03 Jan. 2016.
#'  <http://vassarstats.net/pbcorr.html>.
#'
#'
#' Revelle, William. "CRAN - Package Psych." CRAN - Package Psych. CRAN, 30 Aug.
#' 2015. Web. Mar.-Apr. 2016. <https://cran.r-project.org/web/packages/psych/>.
#'
#' Rizopoulos, Dimitris. "CRAN - Package Ltm." CRAN - Package Ltm. CRAN, 20 Feb.
#' 2015. Web. 02 Mar. 2016. <https://cran.r-project.org/web/packages/ltm/>.
#'
#'
#' Rosseel, Yves, Daniel Oberski, Jarret Byrnes, Leonard Vanbrabant, Victoria
#' Savalei, Ed Merkle, Michael Hallquist, Mijke Rhemtulla, Myrsini Katsikatsou,
#' and Mariska Barendse. "Contributed Packages." CRAN -. CRAN, 07 Nov. 2015.
#' Web. 20 Apr. 2016. <https://cran.r-project.org/web/packages/>.
#'
#'
#' Starkweather, Jon. "UNT | University of North Texas." UNT | University of
#' North Texas. Benchmarks RSS Matters, n.d. Web. 20 Sept. 2014.
#' <http://www.unt.edu/>.
#'
#'
#' "Welcome to the Institute for Digital Research and Education." SPSS FAQ: What
#' Does Cronbach's Alpha Mean? Idre, 02 May 2015. Web. 04 Apr. 2016.
#' <http://www.ats.ucla.edu/stat/spss/faq/alpha.html>.
#'
#' ***
