library(DynaRankR)
library(testthat)

context('Ranking function examples')

########################Elo
test_that('Informed elo works', {
  ##Informed elo
  female.ranks <- informed_elo(contestants = C.crocuta.female$contestants, convention = 'mri',
                               initial.ranks = C.crocuta.female$initial.ranks,
                               interactions = C.crocuta.female$interactions)
  
  expect_is(female.ranks, 'data.frame')
})

test_that('Standard elo works', {
  ##Standard elo
  female.ranks <- informed_elo(contestants = C.crocuta.female$contestants, convention = 'none',
                               interactions = C.crocuta.female$interactions)
  
  expect_is(female.ranks, 'data.frame')
})

test_that('Standard elo works with no convention', {
  ##Standard elo
  female.ranks <- informed_elo(contestants = C.crocuta.female$contestants[,c('id', 'period')], convention = 'none',
                               interactions = C.crocuta.female$interactions)
  
  expect_is(female.ranks, 'data.frame')
})

########################DS
test_that('Informed DS works', {
  ##Informed ds
  female.ranks <- informed_ds(contestants = C.crocuta.female$contestants, convention = 'mri',
                              initial.ranks = C.crocuta.female$initial.ranks,
                              interactions = C.crocuta.female$interactions)
  
  expect_is(female.ranks, 'data.frame')
})

test_that('Standard DS works', {
  ##Standard ds
  female.ranks <- informed_ds(contestants = C.crocuta.female$contestants, convention = 'none',
                              interactions = C.crocuta.female$interactions)
  
  expect_is(female.ranks, 'data.frame')
})

test_that('Standard DS works with no convention', {
  ##Standard ds
  female.ranks <- informed_ds(contestants = C.crocuta.female$contestants[,c('id', 'period')], convention = 'none',
                              interactions = C.crocuta.female$interactions)
  
  expect_is(female.ranks, 'data.frame')
})

########################Informed MatReorder
test_that('informed_matreorder mri', {
  female.ranks <- informed_matreorder(contestants = C.crocuta.female$contestants, convention = 'mri',
                                      n = 5, shuffles = 10, require.corroboration = TRUE,
                                      initial.ranks = C.crocuta.female$initial.ranks,
                                      interactions = C.crocuta.female$interactions)
  expect_is(female.ranks, 'data.frame')
})

test_that('informed_matreorder mri', {
  male.ranks <- informed_matreorder(contestants = C.crocuta.male$contestants, convention = 'tenure',
                                    n = 5, shuffles = 10, require.corroboration = TRUE,
                                    initial.ranks = C.crocuta.male$initial.ranks,
                                    interactions = C.crocuta.male$interactions)
  expect_is(male.ranks, 'data.frame')
})
