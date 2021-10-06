#https://adv-r.hadley.nz/s3.html
library(sloop)
library(pryr)

typeof(NULL)
typeof(1L)
typeof(1i)
typeof(list(x=1:10))

df <- data.frame(x=runif(5))
typeof(df)

sloop::s3_class(df)
attr(df, "class")

fac <- factor(1:5)
typeof(fac)
sloop::s3_class(fac)
attr(fac, "class")


print(df)
print(unclass(df))


print.data.frame(df)
print.default(df)

print
print.data.frame
print.default

# also 
sloop::s3_get_method(print.data.frame)


#################



foo <- structure(
    list(foo=1:3, bar=c("a", "b")),
    class="FooType")

foo <- list(foo=1:3, bar=c("a", "b"))
attr(foo, "class") <- "FooType"

rm(print.FooType)

print(foo)


print.FooType <- function(foo) {
    cat("FooType:\n", foo$foo, "\n(bar is secret)\n")
}

print(foo)

df_bad <- df
class(df_bad) <- "FooType"
print(df_bad)

foo_bad <- foo
class(foo_bad) <- "data.frame"
print(foo_bad)

library(Matrix)
s3_class(Matrix(c(1,1)))

sloop::s3_dispatch(print(foo))
sloop::s3_dispatch(print(df))
sloop::s3_dispatch(print(structure(list(foo=1:3), class="BarType")))

print.data.frame <- function(x) {
    cat("THIS IS A DATAFRAME", "\n")
}

print(df)
where("print.data.frame")

rm(print.data.frame)
where("print.data.frame")
print(df)

rm(print.data.frame, envir=baseenv())


#################

new_FooType <- function(foo_content, bar_content) {
    structure(list(foo=foo_content, bar=bar_content), class="FooType") 
}

validate_FooType <- function(foo) {
    stopifnot(class(foo) == "FooType")
    stopifnot(setequal(names(foo), c("foo", "bar"))) 
    return(invisible(foo))
}

FooType <- function(foo_content) {
    validate_FooType(new_FooType(
        foo_content=foo_content, 
        bar_content="bar bar bar"))
}


foo <- FooType(runif(4))
print(foo)


as.data.frame.FooType <- function(foo) {
    data.frame(row=1:length(foo$foo), foo=foo$foo)
}

as.data.frame(foo)


#################################


display <- function(x) {
    UseMethod("display")
}

display.default <- function(x) {
    cat("I don't know how to display that.\n")
    cat("Or do I?\n")
    print(x)
}

display.FooType <- function(foo) {
    cat("Ok, here is bar:\n", foo$bar, "\n")
}

display(foo)

display(df)


#################################


baz <- structure(list(foo=runif(4), bar="secret", baz="clap your hands"),
                 class=c("BazType", "FooType"))

print(baz)

display.BazType <- function(baz) {
    cat("Ok, here is bar AND baz:\n", baz$bar, baz$baz, "\n")
}

sloop::s3_dispatch(display(baz))

sloop::s3_dispatch(print(baz))

