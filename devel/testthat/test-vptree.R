require(testthat)
require(stringi)
context("test-vptree.R")

test_that("vptree_euclidian_build", {
  
   space <- vector("list", 21*21) 
   index <- 1
   for(x in seq(0,100,5))
      for(y in seq(0,100,5))
      {
         space[[index]] <- c(x,y)
         index <- index + 1
      }
   space <- sample(x = space, size = length(space), replace = FALSE)
   #plot(unlist(lapply(space, function(p){p[1]})), unlist(lapply(space, function(p){p[2]})))

   euclidianDistance <- function(x,y)
   {
      sqrt((x[1]-y[1])^2+(x[2]-y[2])^2)
   }
   
   vpt <- vptree_create(euclidianDistance)
   vptree_build(vpt, space)
   ret <- vptree_searchKNN(vpt, c(45,45), 13)
   retWithoutItself <- vptree_searchKNN(vpt, c(45,45), 13, FALSE)
   
   #points(unlist(lapply(ret[[1]], function(p){p[1]})), unlist(lapply(ret[[1]], function(p){p[2]})), col='red')
   for(vec in list(c(45,45), c(40,45), c(45,40), c(45,50), c(50,45), c(50,50), c(40,40), c(40,50), c(50,40), c(55,45) , c(45,35) , c(45,55), c(35,45)))
   {
      found = FALSE
      for(vec2 in ret[[1]])
      {
         if(identical(vec2, vec))
         {
            found = TRUE
            break
         }
      }
      expect_true(found)
   }
   expect_equal(ret[[2]], list(0, 5, 5, 5, 5, 7.071068, 7.071068, 7.071068, 7.071068, 10,10,10, 10), tolerance = 1e-7)
   
   for(vec in list(c(40,45), c(45,40), c(45,50), c(50,45), c(50,50), c(40,40), c(40,50), c(50,40), c(55,45) , c(45,35) , c(45,55), c(35,45)))
   {
      found = FALSE
      for(vec2 in retWithoutItself[[1]])
      {
         if(identical(vec2, vec))
         {
            found = TRUE
            break
         }
      }
      expect_true(found)
   }
   expect_equal(retWithoutItself[[2]], list( 5, 5, 5, 5, 7.071068, 7.071068, 7.071068, 7.071068, 10,10,10, 10, 11.18034), tolerance = 1e-7)
   
   ret <- vptree_searchRadius(vpt, c(45,45), 10)
   
   for(vec in list(c(45,45), c(40,45), c(45,40), c(45,50), c(50,45), c(50,50), c(40,40), c(40,50), c(50,40), c(55,45) , c(45,35) , c(45,55), c(35,45)))
   {
      found = FALSE
      for(vec2 in ret[[1]])
      {
         if(identical(vec2, vec))
         {
            found = TRUE
            break
         }
      }
      expect_true(found)
   }
   expect_equal(ret[[2]], list(0, 5, 5, 5, 5, 7.071068, 7.071068, 7.071068, 7.071068, 10,10,10, 10), tolerance = 1e-7)
   
   retWithoutItself <- vptree_searchRadius(vpt, c(45,45), 10, FALSE)
   for(vec in list(c(40,45), c(45,40), c(45,50), c(50,45), c(50,50), c(40,40), c(40,50), c(50,40), c(55,45) , c(45,35) , c(45,55), c(35,45)))
   {
      found = FALSE
      for(vec2 in retWithoutItself[[1]])
      {
         if(identical(vec2, vec))
         {
            found = TRUE
            break
         }
      }
      expect_true(found)
   }
   expect_equal(retWithoutItself[[2]], list(5, 5, 5, 5, 7.071068, 7.071068, 7.071068, 7.071068, 10,10,10, 10), tolerance = 1e-7)
})

test_that("vptree_euclidian_insert", {
   
   space <- vector("list", 21*21) 
   index <- 1
   for(x in seq(0,100,5))
      for(y in seq(0,100,5))
      {
         space[[index]] <- c(x,y)
         index <- index + 1
      }
   space <- sample(x = space, size = length(space), replace = FALSE)
   #plot(unlist(lapply(space, function(p){p[1]})), unlist(lapply(space, function(p){p[2]})))
   
   euclidianDistance <- function(x,y)
   {
      sqrt((x[1]-y[1])^2+(x[2]-y[2])^2)
   }
   
   vpt <- vptree_create(euclidianDistance)
   vptree_build(vpt, space[1:100])
   for(i in space[101:length(space)])
      vptree_insert(vpt, i)
   ret <- vptree_searchKNN(vpt, c(45,45), 13)
   
   #points(unlist(lapply(ret[[1]], function(p){p[1]})), unlist(lapply(ret[[1]], function(p){p[2]})), col='red')
   for(vec in list(c(45,45), c(40,45), c(45,40), c(45,50), c(50,45), c(50,50), c(40,40), c(40,50), c(50,40), c(55,45) , c(45,35) , c(45,55), c(35,45)))
   {
      found = FALSE
      for(vec2 in ret[[1]])
      {
         if(identical(vec2, vec))
         {
            found = TRUE
            break
         }
      }
      expect_true(found)
   }
   expect_equal(ret[[2]], list(0, 5, 5, 5, 5, 7.071068, 7.071068, 7.071068, 7.071068, 10,10,10, 10), tolerance = 1e-7)
   
   ret <- vptree_searchRadius(vpt, c(45,45), 10)
   
   for(vec in list(c(45,45), c(40,45), c(45,40), c(45,50), c(50,45), c(50,50), c(40,40), c(40,50), c(50,40), c(55,45) , c(45,35) , c(45,55), c(35,45)))
   {
      found = FALSE
      for(vec2 in ret[[1]])
      {
         if(identical(vec2, vec))
         {
            found = TRUE
            break
         }
      }
      expect_true(found)
   }
   expect_equal(ret[[2]], list(0, 5, 5, 5, 5, 7.071068, 7.071068, 7.071068, 7.071068, 10,10,10, 10), tolerance = 1e-7)
})


test_that("vptree_adist_insert", {
   set.seed(1234)
   words <- stri_extract_all_words(stri_paste(stri_rand_lipsum(nparagraphs = 5), collapse=" "))[[1]]
   space <- vector("list", length(words))
   index <- 1
   for(word in words)
   {
      space[[index]] <- word
      index <- index + 1
   }
   
   myadist <- function(w1,w2)
   {
      adist(w1, w2)[1,1] 
   }
   
   vpt <- vptree_create(myadist)
   vptree_build(vpt, space[1:100])
   for(i in space[101:length(space)])
      vptree_insert(vpt, i)
   ret <- vptree_searchKNN(vpt, "sem", 13)
   
   #points(unlist(lapply(ret[[1]], function(p){p[1]})), unlist(lapply(ret[[1]], function(p){p[2]})), col='red')
   for(vec in c("sed", "sem"))
   {
      found = FALSE
      for(vec2 in ret[[1]])
      {
         if(identical(vec2, vec))
         {
            found = TRUE
            break
         }
      }
      expect_true(found)
   }
   expect_equal(ret[[2]], list(0, 0, 0, 1, 1, 1, 1, 1, 1, 1,1,1, 1), tolerance = 1e-7)
   
   ret <- vptree_searchRadius(vpt, "sed", 1)
   
   for(vec in list("sed", "Sed", "sem"))
   {
      found = FALSE
      for(vec2 in ret[[1]])
      {
         if(identical(vec2, vec))
         {
            found = TRUE
            break
         }
      }
      expect_true(found)
   }
   expect_equal(unlist(ret[[2]]), unlist(list(rep(0,11), rep(1, 5) )), tolerance = 1e-7)
})

test_that("vptree_euclidian_known", {
   
   space <- vector("list", 21*21) 
   index <- 1
   for(x in seq(0,100,5))
      for(y in seq(0,100,5))
      {
         space[[index]] <- c(x,y)
         index <- index + 1
      }
   space <- sample(x = space, size = length(space), replace = FALSE)
   #plot(unlist(lapply(space, function(p){p[1]})), unlist(lapply(space, function(p){p[2]})))
   
   euclidianDistance <- function(x,y)
   {
      sqrt((x[1]-y[1])^2+(x[2]-y[2])^2)
   }
   
   vpt <- vptree_create(euclidianDistance)
   vptree_build(vpt, space)
   
   index <- 4
   ret <- vptree_searchKNN(vpt, space[[index]], 13)
   retKnown <- vptree_searchKNNKnown(vpt, space[[index]], 13)
   retKnownIndex <- vptree_searchKNNKnownIndex(vpt, index, 13) #remember
   
   expect_equal(ret, retKnown)
   expect_equal(retKnown, retKnownIndex)
   
   ret <- vptree_searchKNN(vpt, space[[index]], 13, FALSE)
   retKnown <- vptree_searchKNNKnown(vpt, space[[index]], 13, FALSE)
   retKnownIndex <- vptree_searchKNNKnownIndex(vpt, index, 13, FALSE) #remember
   
   expect_equal(ret, retKnown)
   expect_equal(retKnown, retKnownIndex)
   
   ret <- vptree_searchRadius(vpt, space[[index]], 10)
   retKnown <- vptree_searchRadiusKnown(vpt, space[[index]], 10)
   retKnownIndex <- vptree_searchRadiusKnownIndex(vpt, index, 10) #remember
   
   expect_equal(ret, retKnown)
   expect_equal(retKnown, retKnownIndex)
   
   ret <- vptree_searchRadius(vpt, space[[index]], 10, FALSE)
   retKnown <- vptree_searchRadiusKnown(vpt, space[[index]], 10, FALSE)
   retKnownIndex <- vptree_searchRadiusKnownIndex(vpt, index, 10, FALSE) #remember
   
   expect_equal(ret, retKnown)
   expect_equal(retKnown, retKnownIndex)
})


test_that("vptree_euclidian_testSaveLoad", {
   set.seed(1234)
   space <- vector("list", 21*21) 
   index <- 1
   for(x in seq(0,100,5))
      for(y in seq(0,100,5))
      {
         space[[index]] <- c(x,y)
         index <- index + 1
      }
   space <- sample(x = space, size = length(space), replace = FALSE)
   #plot(unlist(lapply(space, function(p){p[1]})), unlist(lapply(space, function(p){p[2]})))
   
   euclidianDistance <- function(x,y)
   {
      sqrt((x[1]-y[1])^2+(x[2]-y[2])^2)
   }
   
   vpt <- vptree_create(euclidianDistance)
   vptree_build(vpt, space)
   
   index <- 31
   retKNN <- vptree_searchKNN(vpt, space[[index]], 13)
   retKnownKNN <- vptree_searchKNNKnown(vpt, space[[index]], 13)
   retKnownIndexKNN <- vptree_searchKNNKnownIndex(vpt, index, 13) #remember
   
   
   retRadius <- vptree_searchRadius(vpt, space[[index]], 10)
   retKnownRadius <- vptree_searchRadiusKnown(vpt, space[[index]], 10)
   retKnownIndexRadius <- vptree_searchRadiusKnownIndex(vpt, index, 10) #remember
   
   vptree_save(vpt, "test") # devel/testthat/test
   vptRead <- vptree_load("test")   # devel/testthat/test 
   
   retKNNRead <- vptree_searchKNN(vptRead, space[[index]], 13)
   retKnownKNNRead <- vptree_searchKNNKnown(vptRead, space[[index]], 13)
   retKnownIndexKNNRead <- vptree_searchKNNKnownIndex(vptRead, index, 13) #remember
   
   expect_equal(retKNN, retKNNRead)
   expect_equal(retKnownKNN, retKnownKNNRead)
   expect_equal(retKnownIndexKNN, retKnownIndexKNNRead)
   
   retRadiusRead <- vptree_searchRadius(vptRead, space[[index]], 10)
   retKnownRadiusRead <- vptree_searchRadiusKnown(vptRead, space[[index]], 10)
   retKnownIndexRadiusRead <- vptree_searchRadiusKnownIndex(vptRead, index, 10) #remember
   
   expect_equal(retRadius, retRadiusRead)
   expect_equal(retKnownRadius, retKnownRadiusRead)
   expect_equal(retKnownIndexRadius, retKnownIndexRadiusRead)
})


test_that("vptree_euclidian_hashtable", {
   set.seed(1234)
   space <- vector("list", 21*21) 
   index <- 1
   for(x in seq(0,100,5))
      for(y in seq(0,100,5))
      {
         space[[index]] <- c(x,y)
         index <- index + 1
      }
   space <- sample(x = space, size = length(space), replace = FALSE)
   #plot(unlist(lapply(space, function(p){p[1]})), unlist(lapply(space, function(p){p[2]})))
   
   euclidianDistance <- function(x,y)
   {
      sqrt((x[1]-y[1])^2+(x[2]-y[2])^2)
   }
   
   vptRead <- vptree_load("test")   #devel/testthat/test
   index <- 31
   
   res <- microbenchmark(retKnownIndexKNNRead <- vptree_searchKNNKnownIndex(vptRead, index, 13), unit = "us", times = 1L)
   expect_true(res$time < 1000000)
})


test_that("vptree_euclidian_outofbounds", {
   set.seed(1234)
   space <- vector("list", 21*21) 
   index <- 1
   for(x in seq(0,100,5))
      for(y in seq(0,100,5))
      {
         space[[index]] <- c(x,y)
         index <- index + 1
      }
   space <- sample(x = space, size = length(space), replace = FALSE)
   #plot(unlist(lapply(space, function(p){p[1]})), unlist(lapply(space, function(p){p[2]})))
   
   euclidianDistance <- function(x,y)
   {
      sqrt((x[1]-y[1])^2+(x[2]-y[2])^2)
   }
   
   vpt <- vptree_create(euclidianDistance)
   vptree_build(vpt, space)
   
   expect_error(vptree_searchKNNKnownIndex(vpt, length(space)+1, 13))
   expect_error(vptree_searchKNNKnownIndex(vpt, 0, 13))
   expect_error(vptree_searchRadiusKnownIndex(vpt, length(space)+1, 13))
   expect_error(vptree_searchRadiusKnownIndex(vpt, 0, 13))
   
})