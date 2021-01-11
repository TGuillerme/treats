## Test
test_that("make.events works", {

    condition.test.ok <- time.condition(5)
    condition.test.wrong <- "bob"
    modification.test.ok <- update.traits(process = OU.process)
    modification.test.wrong <- "bub"

    error <- capture_error(make.events(target = "target"))
    expect_equal(error[[1]], "target argument must be one of the following: taxa, bd.params, traits, modifiers, founding.")

    error <- capture_error(make.events(target = "taxa", condition = condition.test.wrong))
    expect_equal(error[[1]], "function for condition is not a function.")

    error <- capture_error(make.events(target = "bd.params", condition = condition.test.ok, modification = modification.test.wrong))
    expect_equal(error[[1]], "function for modification is not a function.")

    error <- capture_error(make.events(target = "traits", condition = condition.test.ok, modification = modification.test.ok, additional.args = "bib"))
    expect_equal(error[[1]], "additional.args must be of class list.")

    event <- make.events(target = "traits", condition = condition.test.ok, modification = modification.test.ok)
    print <- capture_output(print(event))
    expect_equal(print, " ---- dads events object ---- \nEvent targeting \"traits\" to be triggered 1 time.\nThe condition function is: condition.test.ok\nThe modification function is: modification.test.ok\n")

})

# test_that("More that one events works", {

#     ## Sanitizing
#     #expect_error(make.events())


# })
