context('Testing Divvy')

test_that(
  desc = 'testing Divvy using num.groups',
  code = {
    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 10,
                                    which.group = 1,
                                    num.groups = 1),
                     expected = 1:10)

    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 10,
                                    which.group = 1,
                                    num.groups = 2),
                     expected = 1:5)

    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 10,
                                    which.group = 2,
                                    num.groups = 2),
                     expected = 6:10)

    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 11,
                                    which.group = 1,
                                    num.groups = 2),
                     expected = 1:6)

    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 11,
                                    which.group = 2,
                                    num.groups = 2),
                     expected = 7:11)
  }
)

test_that(
  desc = 'testing Divvy using group.size',
  code = {
    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 10,
                                    which.group = 1,
                                    group.size = 10),
                     expected = 1:10)

    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 10,
                                    which.group = 1,
                                    group.size = 5),
                     expected = 1:5)

    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 10,
                                    which.group = 2,
                                    group.size = 5),
                     expected = 6:10)

    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 11,
                                    which.group = 1,
                                    group.size = 6),
                     expected = 1:6)

    expect_identical(object = Divvy(first.idx = 1,
                                    last.idx = 11,
                                    which.group = 2,
                                    group.size = 6),
                     expected = 7:11)

  }
)

test_that(
  desc = 'testing Divvy without group.num',
  code = {
    Divvy(first.idx = 7,
          last.idx = 22,
          num.groups = 3)
  }
)
