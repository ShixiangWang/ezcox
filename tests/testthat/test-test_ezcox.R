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

t5 <- ezcox(lung,
  covariates = c("sex", "ph.ecog"), controls = "age",
  return_models = TRUE,
  verbose = FALSE
)

t6 <- ezcox_parallel(lung,
  covariates = c("sex", "ph.ecog"),
  controls = "age"
)

test_that("Return ezcox object works", {
  expect_s3_class(t3, "ezcox")
  expect_s3_class(t4, "ezcox")
  expect_s3_class(t5, "ezcox")
  expect_s3_class(t6, "ezcox")
})


# Filter ezcox
zz <- ezcox(lung, covariates = c("sex", "age"), controls = "ph.ecog")
zz
filter_ezcox(zz, c("0", "2"))
filter_ezcox(zz, c("0", "2"), type = "contrast")
filter_ezcox(zz, c("0", "2"), type = "ref")


# Get models
zz <- ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age", return_models = TRUE)
mds <- get_models(zz)
str(mds, max.level = 1)
