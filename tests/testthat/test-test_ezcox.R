library(survival)

# Build unvariable models
t1 <- ezcox(lung, covariates = c("age", "sex", "ph.ecog"))

# Build multi-variable models
# Control variable 'age'
t2 <- ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age")

test_that("Return model resuts works", {
  expect_equal(nrow(t1), 3)
  expect_equal(nrow(t2), 4)
})

t3 <- ezcox(lung,
  covariates = c("age", "sex", "ph.ecog"),
  return_models = TRUE
)
t4 <- ezcox(lung,
  covariates = c("sex", "ph.ecog"), controls = "age",
  return_models = TRUE
)

test_that("Return ezcox object works", {
  expect_s3_class(t3, "ezcox")
  expect_s3_class(t4, "ezcox")
})
