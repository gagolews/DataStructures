require(testthat)
context("test-queue.R")

test_that("queue_push_and_as.list.queue", {
   mq = queue_create()
   queue_push(mq, 1)
   queue_push(mq, 2)
   queue_push(mq, 3)
   queue_push(mq, "maciek")
   queue_push(mq, matrix(c(1,2,3,4),nrow = 2))
   queue_push(mq, list(1:5, 6:10))
   
   mylist = vector("list", 6)
   mylist[[1]]=1
   mylist[[2]]=2
   mylist[[3]]=3
   mylist[[4]]="maciek"
   mylist[[5]]=matrix(c(1,2,3,4),nrow = 2)
   mylist[[6]]=list(1:5, 6:10)
   
   mqe = queue_create()
   myliste = vector("list", 0)
   
   expect_identical(as.list.Queue(mq), mylist)
   expect_identical(as.list.Queue(mqe), myliste)
   })

test_that("queue_pop", {
   mq = queue_create()
   queue_push(mq, 1)
   queue_push(mq, 2)
   queue_push(mq, 3)
   queue_push(mq, "maciek")
   queue_push(mq, matrix(c(1,2,3,4),nrow = 2))
   queue_push(mq, list(1:5, 6:10))
   
   mylist = vector("list", 6)
   mylist[[1]]=1
   mylist[[2]]=2
   mylist[[3]]=3
   mylist[[4]]="maciek"
   mylist[[5]]=matrix(c(1,2,3,4),nrow = 2)
   mylist[[6]]=list(1:5, 6:10)
   
   mqe = queue_create()
   myliste = vector("list", 0)
   
   expect_error(queue_pop(mqe))
   expect_identical(queue_pop(mq), 1)
   expect_identical(as.list.Queue(mq), mylist[2:6])
})


test_that("queue_empty", {
   mq = queue_create()
   queue_push(mq, 1)
   queue_push(mq, 2)
   queue_push(mq, 3)
   queue_push(mq, "maciek")
   queue_push(mq, matrix(c(1,2,3,4),nrow = 2))
   queue_push(mq, list(1:5, 6:10))
   
   mqe = queue_create()
   
   expect_identical(queue_empty(mqe), TRUE)
   queue_pop(mq)
   expect_identical(queue_empty(mq), FALSE)
   queue_pop(mq)
   expect_identical(queue_empty(mq), FALSE)
   queue_pop(mq)
   expect_identical(queue_empty(mq), FALSE)
   queue_pop(mq)
   expect_identical(queue_empty(mq), FALSE)
   queue_pop(mq)
   expect_identical(queue_empty(mq), FALSE)
   queue_pop(mq)
   expect_identical(queue_empty(mq), TRUE)
})

test_that("queue_format", {
   mq = queue_create()
   queue_push(mq, 1)
   queue_push(mq, 2)
   queue_push(mq, 3)
   queue_push(mq, "maciek")
   queue_push(mq, matrix(c(1,2,3,4),nrow = 2))
   queue_push(mq, list(1:5, 6:10))
   
   mqe = stack_create()
   
   expect_identical(format.Queue(mq, digits = 5, nsmall = 3), 
                    rev(c("1, 2, 3, 4, 5, 6, 7, 8, 9, 10", "1.000, 2.000, 3.000, 4.000", "maciek", "3.000", "2.000", "1.000")))
})

test_that("queue_class_checking", {
  
   ms <- stack_create()
   expect_error(queue_push(ms,1))
   expect_error(queue_empty(ms))
   expect_error(queue_pop(ms))
   expect_error(as.list.Queue(ms))
})

