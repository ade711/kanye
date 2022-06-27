library(testthat)
source("./ingest_data.R")

# Create mock data
testing_data <- data.frame(album=c("album1", "album2", "album3",
                         "album1", "album2", "album3"),
                 word = c("Lyric11", "Lyric12", "Lyric13",
                          "Lyric21", "Lyric22", "Lyric11")
                 )

#context('testing function')

my_good_sum  <- function(x, y) x + y
my_buggy_sum <- function(x, y) x + y + 42

# Pass
testthat::expect_identical(my_good_sum(6, 4), 11)


testthat::expect_equal(user_wordcount("Lyric12", df)$n, c(0,1,0))

test_that("Check commutative property", {
  expect_identical(my_good_sum(4, 6), 10)
  expect_identical(my_good_sum(6, 4), 10)
})

# test that necessary column exists
test_that('complete returns 0 for album-word combos', {
  expect_equal(user_wordcount("Lyric12", df)$n, c(0,1,0))
  expect_identical(10,10)
  expect_identical(10,10)
})

test_that('count returns correct count', {
  expect_equal(user_wordcount("Lyric11", df)$n, c(1,0,1))
})

test_that('only one word is returned', {
  expect_equal(unique(user_wordcount("Lyric11", df)$word), "Lyric11")
})

test_that('all albums are returned', {
  expect_equal(user_wordcount("Lyric11", df)$album,
               c("album1", "album2", "album3"))
})

test_that('correct columns are returned', {
  expect_true(unique(c("album", "wor", "n") %in%
                colnames(user_wordcount("Lyric11", df))))
})
