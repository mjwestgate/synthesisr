# to match the fuzzy wuzzy test set, needs to be out of 100 as a similarity score

test_that("fuzz_m_ratio() works", {
  ratio <-
    100 - round(fuzz_m_ratio("this is a test", "this is a test!"), 2) * 100
  expect_equal(ratio, 97)
})

test_that("fuzz_partial_ratio() works", {
  partial_ratio <-
    100 - (round(fuzz_partial_ratio("this is a test", "this is a test!"), 2) *
             100)
  expect_equal(partial_ratio, 100)
})

test_that("fuzz_m_ratio() works", {
  ratio2 <-
    100 - (round(fuzz_m_ratio(
      "fuzzy wuzzy was a bear", "wuzzy fuzzy was a bear"
    ), 2) * 100)
  expect_equal(ratio2, 91)
})

test_that("fuzz_token_sort_ratio() works", {
  sort_ratio <-
    100 - (round(
      fuzz_token_sort_ratio("fuzzy wuzzy was a bear", "wuzzy fuzzy was a bear"),
      2
    ) * 100)
  expect_equal(sort_ratio, 100)
})

test_that("fuzz_token_sort_ratio() works twice", {
  sort_ratio2 <-
    100 - (round(
      fuzz_token_sort_ratio("fuzzy was a bear", "fuzzy fuzzy was a bear"),
      2
    ) * 100)
  expect_equal(sort_ratio2, 84)
})

# I have not been able to sort out why the R implementation does not match python
# It seems fuzzywuzzy matches by tokens, but m_dist is not doing this
# So actually, token_sort_ratio is the same as token_set_ratio when sorted, but without removing unique

test_that("fuzz_token_set_ratio() works", {
  set_ratio <-
    100 - (round(
      fuzz_token_set_ratio("fuzzy was a bear", "fuzzy fuzzy was a bear"),
      2
    ) * 100)
  expect_equal(set_ratio, 100)
})
