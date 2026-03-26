test_that("mod_DataInput_ui returns a shiny tag", {
  ui <- mod_DataInput_ui("test")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("mod_DataInput_server returns list with measures and ctas_results", {
  shiny::testServer(mod_DataInput_server, {
    returned <- session$getReturned()
    expect_type(returned, "list")
    expect_true("measures" %in% names(returned))
    expect_true("ctas_results" %in% names(returned))

    expect_null(returned$measures())
    expect_null(returned$ctas_results())
  })
})

test_that("mod_DataInput_server loads ctas sample data on button click", {
  shiny::testServer(mod_DataInput_server, {
    session$setInputs(dataset_choice = "ctas", load_sample = 1)

    returned <- session$getReturned()
    m <- returned$measures()
    res <- returned$ctas_results()

    expect_s3_class(m, "data.frame")
    expect_true(nrow(m) > 0)
    expect_true("parameter_id" %in% names(m))

    expect_type(res, "list")
    expect_true("site_scores" %in% names(res))
    expect_true("timeseries" %in% names(res))
  })
})

test_that("mod_DataInput_server loads SDTM sample data on button click", {
  shiny::testServer(mod_DataInput_server, {
    session$setInputs(dataset_choice = "sdtm", load_sample = 1)

    returned <- session$getReturned()
    m <- returned$measures()
    res <- returned$ctas_results()

    expect_s3_class(m, "data.frame")
    expect_true(nrow(m) > 0)

    cats <- unique(m$parameter_category_3)
    expect_true("categorical" %in% cats || "bar" %in% cats || "range_normalized" %in% cats)
  })
})

test_that("mod_DataInput_server renders status after loading", {
  shiny::testServer(mod_DataInput_server, {
    session$setInputs(dataset_choice = "ctas", load_sample = 1)

    status <- output$status
    expect_true(inherits(status, "list") || is.character(status$html))
  })
})
