rm(list = ls())
library(ghtravis)
library(neuroc.deps)
library(neurocInstall)
library(dplyr)
library(lubridate)
library(httr)
library(desc)

out_folder = "release_2017_1"

table_path = neuroc_table_path(dev = FALSE)
tab = neuro_package_table(
  path = table_path,
  long = FALSE)
repos = paste0("neuroconductor",
               "/", tab$repo)
repos = unique(repos)

islug = 1
for (islug in seq_along(repos)) {
  slug = repos[islug]
  print(slug)
  bin_tab = binary_release_table(slug)
  date_names = c("asset_updated_at",
                 "asset_created_at", "created_at",
                 "published_at")
  date_names = intersect(date_names,
                         colnames(bin_tab))
  make_ct = function(x) {
    if (is.POSIXlt(x)) {
      x = as.POSIXct(x)
    }
    x
  }
  for (icol in date_names) {
    bin_tab[, icol] = make_ct(bin_tab[, icol])
  }

  # keep only stable
  bin_tab = bin_tab %>%
    filter(grepl("s", tag_name)) %>%
    filter(grepl("(t.*gz|zip)$",
                 asset_name)) %>%
    filter(!grepl("linux.*.tar.gz$",
                  asset_name))
  bin_tab = bin_tab %>%
    mutate(
      type = if_else(
        grepl(".tar.gz$", asset_name),
        "src",
        if_else(
          grepl(".tgz$", asset_name),
          "bin/macosx",
          if_else(
            grepl(".zip$", asset_name),
            "bin/windows", NA_character_)
        )
      )
    )
  bin_tab = bin_tab %>%
    group_by(type) %>%
    slice(1) %>%
    ungroup()
  bin_tab = bin_tab %>%
    select(asset_name,
           asset_browser_download_url,
           type) %>%
    mutate(outfile =
             file.path(
               out_folder,
               type, "contrib", asset_name))
  # antsr workaround
  bin_tab = bin_tab %>%
    mutate(
      outfile = sub(
        "(.*_.*)_(.*_nctag).tgz",
        "\\1.tgz", outfile)
    )

  irow = 1
  for (irow in seq(nrow(bin_tab))) {
    outfile = bin_tab$outfile[irow]
    if (!file.exists(outfile)) {
      url = bin_tab$asset_browser_download_url[irow]
      res = GET(url,
                write_disk(path = outfile))
      stop_for_status(res)
    }
  }

}

src_folder = file.path(out_folder, "src", "contrib")
tools::write_PACKAGES(dir = src_folder, type = "source",
                      verbose = TRUE)

win_folder = file.path(out_folder, "bin", "windows", "contrib")
tools::write_PACKAGES(dir = win_folder, type = "win.binary",
                      verbose = TRUE)

osx_version = "el-capitan"
osx_folder = file.path(out_folder, "bin", "macosx",
                       "contrib")
tools::write_PACKAGES(dir = win_folder, type = "mac.binary",
                      verbose = TRUE)


# }
