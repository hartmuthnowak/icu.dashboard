date <- ymd_hms("2022-06-06 11:22:33")

c(date, date + 1:2 * hours(1))

bin <- "day"
factor <- 24
start <- ymd_hms("2022-06-06 11:22:33")
end <- ymd_hms("2022-06-08 17:22:33")

bins_skipped <- fn_raw_insertTimesplit_endsOnOtherBin(start, end, bin)

new_start <- c(start, floor_date(start, unit = bin, week_start = 1) + 1:bins_skipped * hours(factor))
new_end <- c(floor_date(start, unit = bin, week_start = 1) + 1:bins_skipped * hours(factor) - seconds(1), end)
-(1 * 1000):-(1 * 1000 + bins_skipped)

sapply(c(1, 2, 3), function(x) 1:x)
rep(1, times = 5)

t <- c(1, 2, 3)
y <- c(4, 5, 6)

sapply(t, function(x) x * y)

testdata <- fn_raw_insertTimesplit(rawdata, bin = "stundenweise")
