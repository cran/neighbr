context("knn function")

test_that("knn with jaccard object contains expected items - 1", {
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(456)
  n_dat_rows <- 10
  feat1 <- sample(x=c(0,1),size=n_dat_rows,replace=TRUE)
  feat2 <- sample(x=c(0,1),size=n_dat_rows,replace=TRUE)
  feat3 <- sample(x=c(0,1),size=n_dat_rows,replace=TRUE)
  cat_tar <- sample(x=c("red","green","orange"),size=n_dat_rows,replace=TRUE)
  cont_tar <- runif(n_dat_rows)
  ID <- c(1:n_dat_rows)
  
  dat <- data.frame(feat1,feat2,feat3,cat_tar,cont_tar,ID)
  
  train_set <- dat[1:(n_dat_rows-2),]
  test_set <- dat[n_dat_rows,c("feat1","feat2","feat3")]
  
  fit <- knn(train_set=train_set,test_set=test_set,
             k=2,
             categorical_target="cat_tar",
             continuous_target="cont_tar",
             comparison_measure="jaccard",
             return_ranked_neighbors=2,
             id="ID")
  
  expect_equal(dim(summary(fit))[1],16) #expect fit to have 16 items
  expect_equal(deparse(fit$call),c("knn(train_set = train_set, test_set = test_set, k = 2, categorical_target = \"cat_tar\", ",
                                   "    continuous_target = \"cont_tar\", comparison_measure = \"jaccard\", ",
                                   "    return_ranked_neighbors = 2, id = \"ID\")"))
  expect_equal(fit$k,2)
  expect_equal(fit$categorical_target,"cat_tar")
  expect_equal(fit$continuous_target,"cont_tar")
  expect_equal(fit$comparison_measure,"jaccard")
  expect_equal(fit$categorical_scoring_method,"majority_vote")
  expect_equal(fit$continuous_scoring_method,"average")
  expect_equal(fit$return_ranked_neighbors,2)
  expect_equal(fit$id,"ID")
  expect_equal(fit$features,c("feat1","feat2","feat3"))
  expect_equal(fit$function_name,"mixed")
  expect_equal(fit$categorical_levels,c("green","orange","red"))
  expect_equal(fit$num_train_rows,8)
  expect_equal(fit$num_test_rows,1)
  expect_equal(fit$train_set$feat1,c(0,0,1,1,1,0,0,0))
  expect_equal(fit$train_set$feat2,c(0,0,1,1,1,1,1,0))
  expect_equal(fit$train_set$feat3,c(0,1,1,0,1,1,1,1))
  expect_equal(fit$test_set_scores$categorical_target,"orange")
  expect_equal(fit$test_set_scores$continuous_target,0.23333005653694272041)
  expect_equal(fit$test_set_scores$neighbor1,2)
  expect_equal(fit$test_set_scores$neighbor2,3)
})

test_that("knn with jaccard object contains expected items - 2", {
  # skip("skipping test")
  set.seed(123)
  n_dat_rows <- 10
  feat1 <- sample(x=c(0,1),size=n_dat_rows,replace=TRUE)
  feat2 <- sample(x=c(0,1),size=n_dat_rows,replace=TRUE)
  feat3 <- sample(x=c(0,1),size=n_dat_rows,replace=TRUE)
  cat_tar <- sample(x=c("red","green","blue"),size=n_dat_rows,replace=TRUE)
  cont_tar <- runif(n_dat_rows)
  ID <- c(1:n_dat_rows)

  # dat <- data.frame(feat1,feat2,feat3,ID)
  dat <- data.frame(feat1,feat2,feat3,cat_tar,cont_tar,ID)

  train_set <- dat[1:(n_dat_rows-2),]
  test_set <- dat[n_dat_rows,c("feat1","feat2","feat3")]

  fit <- knn(train_set=train_set,test_set=test_set,
             k=3,
             categorical_target="cat_tar",
             continuous_target="cont_tar",
             comparison_measure="jaccard",
             return_ranked_neighbors=3,
             id="ID")


  expect_equal(names(fit$test_set_scores),c("categorical_target","continuous_target","neighbor1","neighbor2","neighbor3"))

  expect_equal(fit$test_set_scores$categorical_target,"blue")
  expect_equal(fit$test_set_scores$continuous_target,0.231776804042359202418)
  expect_equal(fit$test_set_scores$neighbor1,1)
  expect_equal(fit$test_set_scores$neighbor2,3)
  expect_equal(fit$test_set_scores$neighbor3,6)
})

test_that("knn euclidean clustering object contains expected items", {
  set.seed(334455)
  n_dat_rows <- 27
  feat1 <- runif(n_dat_rows)
  feat2 <- runif(n_dat_rows)
  the_id_variable <- c(1:n_dat_rows)
  
  dat <- data.frame(feat1,feat2,the_id_variable)
  
  train_set <- dat[1:(n_dat_rows-10),]
  test_set <- dat[(n_dat_rows-5):n_dat_rows,c("feat1","feat2")]
  
  fit <- knn(train_set=train_set,test_set=test_set,
             k=5,
             comparison_measure="euclidean",
             return_ranked_neighbors=3,
             id="the_id_variable")
  
  expect_equal(dim(summary(fit))[1],16) #expect fit to have 16 items
  expect_equal(deparse(fit$call),c("knn(train_set = train_set, test_set = test_set, k = 5, comparison_measure = \"euclidean\", ",
                                   "    return_ranked_neighbors = 3, id = \"the_id_variable\")"))
  expect_equal(fit$k,5)
  expect_equal(fit$categorical_target,NULL)
  expect_equal(fit$continuous_target,NULL)
  expect_equal(fit$comparison_measure,"euclidean")
  expect_equal(fit$categorical_scoring_method,"majority_vote")
  expect_equal(fit$continuous_scoring_method,"average")
  expect_equal(fit$return_ranked_neighbors,3)
  expect_equal(fit$id,"the_id_variable")
  expect_equal(fit$features,c("feat1","feat2"))
  expect_equal(fit$function_name,"clustering")
  expect_equal(fit$categorical_levels,NULL)
  expect_equal(fit$num_train_rows,17)
  expect_equal(fit$num_test_rows,6)
  expect_equal(dim(fit$test_set_scores),c(6,3))
  expect_equal(fit$test_set_scores$neighbor1,c(15,15,3,13,13,6))
  expect_equal(fit$test_set_scores$neighbor2,c(4,4,10,15,1,9))
  expect_equal(fit$test_set_scores$neighbor3,c(11,11,16,2,7,12))
})

