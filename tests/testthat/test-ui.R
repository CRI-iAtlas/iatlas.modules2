test_that("ui_items", {
  expect_type(titleBox("title"), "list")
  expect_type(subTitleBox("title"), "list")
  expect_type(sectionBox(title = "title"), "list")
  expect_type(optionsBox(), "list")
  expect_type(plotBox(), "list")
  expect_type(tableBox(), "list")
  expect_type(textBox(), "list")
  expect_type(messageBox(), "list")
  expect_type(
    imgLinkBox(
      linkId = "", title  = "", imgSrc  = "", boxText  = "", linkText = ""
    ),
    "list"
  )
})
