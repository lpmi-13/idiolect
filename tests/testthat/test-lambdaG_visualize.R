test_that("lambdaG visualize works", {

  q.data <- quanteda::corpus_trim(enron.sample[1], "sentences", max_ntoken = 10) |>
    quanteda::tokens("sentence")
  k.data <- enron.sample[2:5] |> quanteda::tokens("sentence")
  ref.data <- enron.sample[6:ndoc(enron.sample)] |> quanteda::tokens("sentence")

  set.seed(2)

  lambdaG_visualize(q.data, k.data, ref.data, r = 2, print = "") |>
    expect_snapshot()

  lambdaG_visualize(q.data, k.data, ref.data, r = 2, print = "", scale = "relative") |>
    expect_snapshot()

  lambdaG_visualize(q.data, k.data, ref.data, r = 2, print = tempfile()) |>
    expect_no_error()

  lambdaG_visualize(q.data, k.data, ref.data, r = 2, print = tempfile(), negative = TRUE) |>
    expect_no_error()

  # output = "table" renders a colour-coded HTML table; the data frame
  # returned in $table is the same one the heatmap path produces, only the
  # $colourcoded_text representation differs.
  set.seed(2)
  res.table <- lambdaG_visualize(q.data, k.data, ref.data, r = 2, output = "table")

  res.table |> expect_snapshot()

  set.seed(2)
  res.heatmap <- lambdaG_visualize(q.data, k.data, ref.data, r = 2)

  expect_equal(res.table$table, res.heatmap$table)
  expect_match(res.table$colourcoded_text, "^<table")
  expect_match(res.table$colourcoded_text, "<thead>")
  expect_match(res.table$colourcoded_text, "<td>J</td>", fixed = TRUE)

  # output = "table" also writes to a file when `print` is set.
  out.file <- tempfile(fileext = ".html")
  lambdaG_visualize(q.data, k.data, ref.data, r = 2, output = "table", print = out.file) |>
    expect_no_error()
  expect_true(file.exists(out.file))

})
