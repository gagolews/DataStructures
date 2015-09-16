require(testthat)
context("test-stack.R")

test_that("stack_push_and_as.list.stack", {
   ms = stack_create()
   stack_push(ms, 1)
   stack_push(ms, 2)
   stack_push(ms, 3)
   stack_push(ms, "maciek")
   stack_push(ms, matrix(c(1,2,3,4),nrow = 2))
   stack_push(ms, list(1:5, 6:10))
   
   mylist = vector("list", 6)
   mylist[[1]]=1
   mylist[[2]]=2
   mylist[[3]]=3
   mylist[[4]]="maciek"
   mylist[[5]]=matrix(c(1,2,3,4),nrow = 2)
   mylist[[6]]=list(1:5, 6:10)
   
   mse = stack_create()
   myliste = vector("list", 0)
   
   expect_identical(as.list.Stack(ms), rev(mylist))
   expect_identical(as.list.Stack(mse), rev(myliste))
})

test_that("stack_pop", {
   ms = stack_create()
   stack_push(ms, 1)
   stack_push(ms, 2)
   stack_push(ms, 3)
   stack_push(ms, "maciek")
   stack_push(ms, matrix(c(1,2,3,4),nrow = 2))
   stack_push(ms, list(1:5, 6:10))
   
   mylist = vector("list", 6)
   mylist[[1]]=1
   mylist[[2]]=2
   mylist[[3]]=3
   mylist[[4]]="maciek"
   mylist[[5]]=matrix(c(1,2,3,4),nrow = 2)
   mylist[[6]]=list(1:5, 6:10)
   
   mse = stack_create()
   myliste = vector("list", 0)
   
   expect_error(stack_pop(mse))
   expect_identical(stack_pop(ms), list(1:5, 6:10))
   expect_identical(as.list.Stack(ms), rev(mylist)[2:6])
})


test_that("stack_empty", {
   ms = stack_create()
   stack_push(ms, 1)
   stack_push(ms, 2)
   stack_push(ms, 3)
   stack_push(ms, "maciek")
   stack_push(ms, matrix(c(1,2,3,4),nrow = 2))
   stack_push(ms, list(1:5, 6:10))
   
   mse = stack_create()
   
   expect_identical(stack_empty(mse), TRUE)
   stack_pop(ms)
   expect_identical(stack_empty(ms), FALSE)
   stack_pop(ms)
   expect_identical(stack_empty(ms), FALSE)
   stack_pop(ms)
   expect_identical(stack_empty(ms), FALSE)
   stack_pop(ms)
   expect_identical(stack_empty(ms), FALSE)
   stack_pop(ms)
   expect_identical(stack_empty(ms), FALSE)
   stack_pop(ms)
   expect_identical(stack_empty(ms), TRUE)
})


test_that("stack_format", {
   ms = stack_create()
   stack_push(ms, 1)
   stack_push(ms, 2)
   stack_push(ms, 3)
   stack_push(ms, "maciek")
   stack_push(ms, matrix(c(1,2,3,4),nrow = 2))
   stack_push(ms, list(1:5, 6:10))
   
   mse = stack_create()
   
   expect_identical(format.Stack(ms, digits = 5, nsmall = 3), 
                    c("1, 2, 3, 4, 5, 6, 7, 8, 9, 10", "1.000, 2.000, 3.000, 4.000", "maciek", "3.000", "2.000", "1.000"))
})

test_that("stack_class_checking", {
   
   mq <- queue_create()
   expect_error(stack_push(mq,1))
   expect_error(stack_empty(mq))
   expect_error(stack_pop(mq))
   expect_error(as.list.Stack(mq))
})
