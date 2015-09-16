require(testthat)
context("test-vector.R")

test_that("vector_push_back_and_as.list.stack", {
   mv = vector_create()
   vector_push_back(mv, 1)
   vector_push_back(mv, 2)
   vector_push_back(mv, 3)
   vector_push_back(mv, "maciek")
   vector_push_back(mv, matrix(c(1,2,3,4),nrow = 2))
   vector_push_back(mv, list(1:5, 6:10))
   
   mylist = vector("list", 6)
   mylist[[1]]=1
   mylist[[2]]=2
   mylist[[3]]=3
   mylist[[4]]="maciek"
   mylist[[5]]=matrix(c(1,2,3,4),nrow = 2)
   mylist[[6]]=list(1:5, 6:10)
   
   mve = vector_create()
   myliste = vector("list", 0)
   
   expect_identical(as.list.Vector(mv), mylist)
   expect_identical(as.list.Vector(mve), myliste)
})

test_that("vector_pop_and_back", {
   mv = vector_create()
   vector_push_back(mv, 1)
   vector_push_back(mv, 2)
   vector_push_back(mv, 3)
   vector_push_back(mv, "maciek")
   vector_push_back(mv, matrix(c(1,2,3,4),nrow = 2))
   vector_push_back(mv, list(1:5, 6:10))
   
   mylist = vector("list", 6)
   mylist[[1]]=1
   mylist[[2]]=2
   mylist[[3]]=3
   mylist[[4]]="maciek"
   mylist[[5]]=matrix(c(1,2,3,4),nrow = 2)
   mylist[[6]]=list(1:5, 6:10)
   
   mve = vector_create()
   myliste = vector("list", 0)
   
   expect_error(vector_pop_back(mve))
   expect_identical(vector_back(mv), list(1:5, 6:10))
   vector_pop_back(mv)
   expect_identical(as.list.Vector(mv), mylist[1:5])
})

test_that("vector_front", {
   mv = vector_create()
   vector_push_back(mv, 1)
   expect_identical(vector_front(mv), 1)
   vector_push_back(mv, 2)
   expect_identical(vector_front(mv), 1)
   vector_push_back(mv, 3)
   expect_identical(vector_front(mv), 1)
   vector_push_back(mv, "maciek")
   expect_identical(vector_front(mv), 1)
   vector_push_back(mv, matrix(c(1,2,3,4),nrow = 2))
   expect_identical(vector_front(mv), 1)
   vector_push_back(mv, list(1:5, 6:10))

   mve = vector_create()
   expect_error(vector_front(mve))
})


test_that("vector_size", {
   mv = vector_create()
   expect_equivalent(vector_size(mv), 0)
   vector_push_back(mv, 1)
   expect_equivalent(vector_size(mv), 1)
   vector_push_back(mv, 2)
   expect_equivalent(vector_size(mv), 2)
   vector_pop_back(mv)
   expect_equivalent(vector_size(mv), 1)
   vector_pop_back(mv)
   expect_equivalent(vector_size(mv), 0)   
})

test_that("vector_at", {
   mv = vector_create()
   vector_push_back(mv, 1)
   vector_push_back(mv, 2)
   vector_push_back(mv, 3)
   vector_push_back(mv, "maciek")
   vector_push_back(mv, matrix(c(1,2,3,4),nrow = 2))
   vector_push_back(mv, list(1:5, 6:10))
   
   expect_error(vetor_at(mv,7))
   expect_error(vetor_at(mv,0))
   expect_error(vetor_at(mv,-1))
   expect_error(vetor_at(mv,-20))
   
   expect_identical(vector_at(mv,1),1)
   expect_identical(vector_at(mv,2),2)
   expect_identical(vector_at(mv,3),3)
   expect_identical(vector_at(mv,4),"maciek")
   expect_identical(vector_at(mv,5),matrix(c(1,2,3,4),nrow = 2))
   expect_identical(vector_at(mv,6),list(1:5, 6:10))
   
   vector_set_at(mv,1,"delta")
   expect_identical(vector_at(mv,1),"delta")
   vector_set_at(mv,3,123)
   expect_identical(vector_at(mv,3),123)
   vector_set_at(mv,4,321)
   expect_identical(vector_at(mv,4),321)
   
})

test_that("vector_empty", {
   mv = vector_create()
   vector_push_back(mv, 1)
   vector_push_back(mv, 2)
   vector_push_back(mv, 3)
   vector_push_back(mv, "maciek")
   vector_push_back(mv, matrix(c(1,2,3,4),nrow = 2))
   vector_push_back(mv, list(1:5, 6:10))
   
   mve = vector_create()
   
   expect_identical(vector_empty(mve), TRUE)
   vector_pop_back(mv)
   expect_identical(vector_empty(mv), FALSE)
   vector_pop_back(mv)
   expect_identical(vector_empty(mv), FALSE)
   vector_pop_back(mv)
   expect_identical(vector_empty(mv), FALSE)
   vector_pop_back(mv)
   expect_identical(vector_empty(mv), FALSE)
   vector_pop_back(mv)
   expect_identical(vector_empty(mv), FALSE)
   vector_pop_back(mv)
   expect_identical(vector_empty(mv), TRUE)
})


test_that("stack_format", {
   mv = vector_create()
   vector_push_back(mv, 1)
   vector_push_back(mv, 2)
   vector_push_back(mv, 3)
   vector_push_back(mv, "maciek")
   vector_push_back(mv, matrix(c(1,2,3,4),nrow = 2))
   vector_push_back(mv, list(1:5, 6:10))
   
   mve = vector_create()
   
   expect_identical(format.Vector(mv, digits = 5, nsmall = 3), 
                    rev(c("1, 2, 3, 4, 5, 6, 7, 8, 9, 10", "1.000, 2.000, 3.000, 4.000", "maciek", "3.000", "2.000", "1.000")))
})

test_that("vector_class_checking", {
   
   mq <- queue_create()
   expect_error(vector_push_back(mq,1))
   expect_error(vector_empty(mq))
   expect_error(vector_pop(mq))
   expect_error(as.list.Vector(mq))
})
