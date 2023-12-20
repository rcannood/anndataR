skip_if_not_installed("hdf5r")

test_that("writing H5AD from SingleCellExperiment works", {
  skip_if_not_installed("SingleCellExperiment")

  file <- withr::local_file(tempfile(fileext = ".h5ad"))

  sce <- generate_dataset(format = "SingleCellExperiment")
  write_h5ad(sce, file)
  expect_true(file.exists(file))
})

test_that("writing H5AD from Seurat works", {
  skip_if_not_installed("SeuratObject")
  skip("while Seurat converter is failing")

  file <- withr::local_file(tempfile(fileext = ".h5ad"))

  seurat <- generate_dataset(format = "Seurat")
  write_h5ad(seurat, file)
  expect_true(file.exists(file))
})

test_that("writing gzip compressed files works", {
  dummy <- generate_dataset(100, 200)
  non_random_X <- matrix(5, 100, 200) # nolint

  adata <- AnnData(
    X = non_random_X,
    obs = dummy$obs,
    var = dummy$var
  )

  h5ad_file_none <- tempfile(pattern = "hdf5_write_none_", fileext = ".h5ad")
  h5ad_file_default <- tempfile(pattern = "hdf5_write_default_", fileext = ".h5ad")
  h5ad_file_max <- tempfile(pattern = "hdf5_write_gzip_", fileext = ".h5ad")

  write_h5ad(adata, h5ad_file_none, gzip_level = 0)
  write_h5ad(adata, h5ad_file_default)
  write_h5ad(adata, h5ad_file_max, gzip_level = 9)

  expect_true(file.info(h5ad_file_none)$size > file.info(h5ad_file_default)$size)
  expect_true(file.info(h5ad_file_default)$size > file.info(h5ad_file_max)$size)
})
