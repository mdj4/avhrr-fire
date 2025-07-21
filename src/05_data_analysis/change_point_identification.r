
# Identification of change points in AVHRR (ANF+seasonal), VPD, and FWSL 
# timeseries, by different regional groupings.

# examples: https://www.marinedatascience.co/blog/2019/09/28/comparison-of-change-point-detection-methods/


rm(list = ls())
library(dplyr)
library(scales)
library(gridExtra)
library(dtplyr)
library(data.table)
library(tibble)
library(stringr)
library(lubridate)
library(reshape2)
library(segmented)  # REQUIRES V1.3-3 ###
library(trend)


# =============================
# AVHRR ANF and FWSL (GFED REGIONS)
# =============================
dir_base = "/path/to/working/directory"
dir_base = "C:/Users/mdejong/OneDrive - NRCan RNCan/projects/avhrr/publication_data_285k/"
fp = paste0(dir_base, 'avhrr_and_fire_weather_fulljoin_1986-2016.csv')
fp_out = paste0(dir_base, 'change_points/')

df = as_tibble(as.data.frame(fread(file=fp)))
df = df %>% dplyr::select(gfed, gfed_name, year, ncount, fdi_n_mean)



# plot change points for AVHRR counts
fp = paste0(fp_out, 'avhrr_anf_breakpoints_gfed.png')
png(filename = fp, res = 300, width = 8, height = 12, units = 'in')
par(mfrow=c(5, 3), mar=c(2,2,1,1))
assign("last.warning", NULL, envir = baseenv())

for (name in unique(df$gfed_name)) {
  sub = df %>% filter(gfed_name==name)
  out.lm = lm(ncount ~ year, data=sub)
  o = segmented(out.lm, seg.Z=~year, psi=2000)
  
  #tmp outputs
  df_tmp = data.frame(matrix(nrow = 0, ncol= 5), stringsAsFactors = FALSE)
  colnames(df_tmp) = c("name","breakpnt", "ci_95_low", "ci_95_hi", "ci_width")
  
  if (!is.null(o$psi)) {
    
    bp = round(o$psi[2])
    ci_lo = round(confint(o)[2])
    ci_hi = round(confint(o)[3])
    ci_width = round(ci_hi - ci_lo)
    
    plot(ncount ~ year, data=sub, type='b', pch=20, main=name)
    plot(o, add=TRUE,link=FALSE,lwd=2, lty=c(3,3))
    lines(o, col=2, pch=19, bottom=T, lwd=2)
    points(o, col=4, link=FALSE)
    text(2000, max(sub[!is.na(sub$ncount),'ncount'])*0.95, paste('CP:', bp), col='red')
    text(2000, max(sub[!is.na(sub$ncount),'ncount'])*0.85, paste0('95% CI (range, n. years): ',ci_lo, '-',ci_hi,' n=',ci_width), col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = bp
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = ci_lo
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = ci_hi
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = ci_width
    
  }else{
    plot(ncount ~ year, data=sub, type='b', pch=20, main=name)
    text(2000, max(sub[!is.na(sub$ncount),'ncount'])*0.95, 'NO CHANGE POINT',col='red')
  
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = NA    
    
  }
  
  # final output
  if (exists("df_results") == TRUE){
    df_results = bind_rows(df_results, df_tmp)
  } else {df_results = df_tmp}
  
}
dev.off()

# create analysis specific df for combining with other df at end
df_gfed_anf = df_results
df_gfed_anf$experiment = 'gfed_anf'
rm(df_results)


# plot change points for fire weather counts 
fp = paste0(fp_out, 'fwsl_breakpoints_gfed.png')
png(filename = fp, res = 300, width = 8, height = 12, units = 'in')
par(mfrow=c(5, 3),mar=c(2,2,1,1))
assign("last.warning", NULL, envir = baseenv())

for (name in unique(df$gfed_name)) {
  sub = df %>% filter(gfed_name==name)
  out.lm = lm(fdi_n_mean ~ year, data=sub)
  o = segmented(out.lm, seg.Z =~year, psi=2000)
  
  #tmp outputs
  df_tmp = data.frame(matrix(nrow = 0, ncol= 5), stringsAsFactors = FALSE)
  colnames(df_tmp) = c("name","breakpnt", "ci_95_low", "ci_95_hi", "ci_width")
  
  if (!is.null(o$psi)) {
    
    bp = round(o$psi[2])
    ci_lo = round(confint(o)[2])
    ci_hi = round(confint(o)[3])
    ci_width = round(ci_hi - ci_lo)
    
    plot(fdi_n_mean ~ year, data=sub, type='b', pch=20, main=name)
    plot(o, add=TRUE,link=FALSE,lwd=2, lty=c(3,3))
    lines(o, col=2, pch=19, bottom=T, lwd=2)
    points(o, col=4, link=FALSE)
    text(2000, max(sub[!is.na(sub$fdi_n_mean),'fdi_n_mean'])*0.95, paste('CP:', bp), col='red')
    text(2000, max(sub[!is.na(sub$fdi_n_mean),'fdi_n_mean'])*0.85, paste0('95% CI (range, n. years): ',ci_lo, '-',ci_hi,' n=',ci_width), col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = bp
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = ci_lo
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = ci_hi
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = ci_width
    
  }else{
    plot(fdi_n_mean ~ year, data=sub, type='b', pch=20, main=name)
    text(2000, max(sub[!is.na(sub$fdi_n_mean),'fdi_n_mean'])*0.95, 'NO CHANGE POINT',col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = NA    
    
  }
  
  # final output
  if (exists("df_results") == TRUE){
    df_results = bind_rows(df_results, df_tmp)
  } else {df_results = df_tmp}
  
}
dev.off()

# create analysis specific df for combining with other df at end
df_gfed_fwsl = df_results
df_gfed_fwsl$experiment = 'gfed_fwsl'
rm(df_results)


# =============================
# AVHRR ANF and FWSL (BIOMES)
# =============================
fp = paste0(dir_base, 'avhrr_and_fire_weather_fulljoin_1986-2016_TropTempBoreal.csv')
fp_out = paste0(dir_base, 'change_points/')

df = as_tibble(as.data.frame(fread(fp)))
df = df %>% dplyr::select(ttb, ttb_name, year, ncount, fdi_n_mean)


# plot AVHRR change points
fp = paste0(fp_out, 'avhrr_anf_breakpoints_temptropboreal.png')

png(filename =  fp, res = 300, width = 8, height = 8, units = 'in')
par(mfrow=c(2, 2),mar=c(2,2,1,1))
assign("last.warning", NULL, envir = baseenv())

for (name in unique(df$ttb_name)) {
  
  sub = df %>% filter(ttb_name==name)
  out.lm = lm(ncount ~ year, data=sub)
  o = segmented(out.lm, seg.Z =~year, psi=2000)
  
  #tmp outputs
  df_tmp = data.frame(matrix(nrow = 0, ncol= 5), stringsAsFactors = FALSE)
  colnames(df_tmp) = c("name","breakpnt", "ci_95_low", "ci_95_hi", "ci_width")
  
  if (!is.null(o$psi)) {
    
    bp = round(o$psi[2])
    ci_lo = round(confint(o)[2])
    ci_hi = round(confint(o)[3])
    ci_width = round(ci_hi - ci_lo)
    
    plot(ncount ~ year, data=sub, type='b', pch=20, main=name)
    plot(o, add=TRUE,link=FALSE,lwd=2, lty=c(3,3))
    lines(o, col=2, pch=19, bottom=T, lwd=2)
    points(o, col=4, link=FALSE)
    text(2000, max(sub[!is.na(sub$ncount),'ncount'])*0.95, paste('CP:', bp), col='red')
    text(2000, max(sub[!is.na(sub$ncount),'ncount'])*0.85, paste0('95% CI (range, n. years): ',ci_lo, '-',ci_hi,' n=',ci_width), col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = bp
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = ci_lo
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = ci_hi
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = ci_width
    
  }else{
    plot(ncount ~ year, data=sub, type='b', pch=20, main=name)
    text(2000, max(sub[!is.na(sub$ncount),'ncount'])*0.95, 'NO CHANGE POINT',col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = NA    
    
  }
  
  # final output
  if (exists("df_results") == TRUE){
    df_results = bind_rows(df_results, df_tmp)
  } else {df_results = df_tmp}
  
}
dev.off()

# create analysis specific df for combining with other df at end
df_ttb_anf = df_results
df_ttb_anf$experiment = 'ttb_anf'
rm(df_results)


# plot FWI change points 
fp = paste0(fp_out, 'fwsl_breakpoints_temptropboreal.png')
png(filename =  fp, res = 300, width = 8, height = 8, units = 'in')
par(mfrow=c(2, 2),mar=c(2,2,1,1))
assign("last.warning", NULL, envir = baseenv())

for (name in unique(df$ttb_name)) {
  sub = df %>% filter(ttb_name==name)
  out.lm = lm(fdi_n_mean ~ year, data=sub)
  o = segmented(out.lm, seg.Z = ~year, psi=2000)
  
  
  #tmp outputs
  df_tmp = data.frame(matrix(nrow = 0, ncol= 5), stringsAsFactors = FALSE)
  colnames(df_tmp) = c("name","breakpnt", "ci_95_low", "ci_95_hi", "ci_width")
  
  if (!is.null(o$psi)) {
    
    bp = round(o$psi[2])
    ci_lo = round(confint(o)[2])
    ci_hi = round(confint(o)[3])
    ci_width = round(ci_hi - ci_lo)
    
    plot(fdi_n_mean ~ year, data=sub, type='b', pch=20, main=name)
    plot(o, add=TRUE,link=FALSE,lwd=2, lty=c(3,3))
    lines(o, col=2, pch=19, bottom=T, lwd=2)
    points(o, col=4, link=FALSE)
    text(2000, max(sub[!is.na(sub$fdi_n_mean),'fdi_n_mean'])*0.95, paste('CP:', bp), col='red')
    text(2000, max(sub[!is.na(sub$fdi_n_mean),'fdi_n_mean'])*0.85, paste0('95% CI (range, n. years): ',ci_lo, '-',ci_hi,' n=',ci_width), col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = bp
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = ci_lo
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = ci_hi
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = ci_width
    
  }else{
    plot(fdi_n_mean ~ year, data=sub, type='b', pch=20, main=name)
    text(2000, max(sub[!is.na(sub$fdi_n_mean),'fdi_n_mean'])*0.95, 'NO CHANGE POINT',col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = NA    
    
  }
  
  # final output
  if (exists("df_results") == TRUE){
    df_results = bind_rows(df_results, df_tmp)
  } else {df_results = df_tmp}
  
}
dev.off()

# create analysis specific df for combining with other df at end
df_ttb_fwsl = df_results
df_ttb_fwsl$experiment = 'ttb_fwsl'
rm(df_results)





######################################
## VPD
######################################

# =============================
# VPD (GFED REGIONS)
# =============================
fp = paste0(dir_base, 'avhrr_and_vpd_fulljoin_1986-2016.csv')
fp_out = paste0(dir_base, 'change_points/')

df = as_tibble(as.data.frame(fread(file=fp)))
df = df %>% dplyr::select(gfed, gfed_name, year, ncount, n_mean)


# plot change points for VPD
fp = paste0(fp_out, 'vpd_breakpoints_gfed.png')
png(filename = fp, res = 300, width = 8, height = 12, units = 'in')
par(mfrow=c(5, 3),mar=c(2,2,1,1))
assign("last.warning", NULL, envir = baseenv())

for (name in unique(df$gfed_name)) {
  sub = df %>% filter(gfed_name==name)
  out.lm = lm(n_mean ~ year, data=sub)
  o = segmented(out.lm, seg.Z =~year, psi=2000)
  
  #tmp outputs
  df_tmp = data.frame(matrix(nrow = 0, ncol= 5), stringsAsFactors = FALSE)
  colnames(df_tmp) = c("name","breakpnt", "ci_95_low", "ci_95_hi", "ci_width")
  
  if (!is.null(o$psi)) {
    
    bp = round(o$psi[2])
    ci_lo = round(confint(o)[2])
    ci_hi = round(confint(o)[3])
    ci_width = round(ci_hi - ci_lo)
    
    plot(n_mean ~ year, data=sub, type='b', pch=20, main=name)
    plot(o, add=TRUE,link=FALSE,lwd=2, lty=c(3,3))
    lines(o, col=2, pch=19, bottom=T, lwd=2)
    points(o, col=4, link=FALSE)
    text(2000, max(sub[!is.na(sub$n_mean),'n_mean'])*0.95, paste('CP:', bp), col='red')
    text(2000, max(sub[!is.na(sub$n_mean),'n_mean'])*0.90, paste0('95% CI (range, n. years): ',ci_lo, '-',ci_hi,' n=',ci_width), col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = bp
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = ci_lo
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = ci_hi
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = ci_width
    
  }else{
    plot(n_mean ~ year, data=sub, type='b', pch=20, main=name)
    text(2000, max(sub[!is.na(sub$n_mean),'n_mean'])*0.95, 'NO CHANGE POINT',col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = NA    
    
  }
  
  # final output
  if (exists("df_results") == TRUE){
    df_results = bind_rows(df_results, df_tmp)
  } else {df_results = df_tmp}
  
}
dev.off()

# create analysis specific df for combining with other df at end
df_gfed_vpd = df_results
df_gfed_vpd$experiment = 'gfed_vpd'
rm(df_results)



# =============================
# VPD (BIOMES)
# =============================

fp = paste0(dir_base, 'avhrr_and_vpd_fulljoin_1986-2016_TropTempBoreal.csv')
fp_out = paste0(dir_base, 'change_points/')

df = as_tibble(as.data.frame(fread(fp)))
df = df %>% dplyr::select(ttb, ttb_name, year, ncount, nvpd_mean)


# plot VPD change points (no-nans)
fp = paste0(fp_out, 'vpd_breakpoints_temptropboreal.png')
png(filename =  fp, res = 300, width = 8, height = 8, units = 'in')
par(mfrow=c(2, 2),mar=c(2,2,1,1))
assign("last.warning", NULL, envir = baseenv())

for (name in unique(df$ttb_name)) {
  sub = df %>% filter(ttb_name==name)
  out.lm = lm(nvpd_mean ~ year, data=sub)
  o = segmented(out.lm, seg.Z = ~year, psi=2000)
  
  #tmp outputs
  df_tmp = data.frame(matrix(nrow = 0, ncol= 5), stringsAsFactors = FALSE)
  colnames(df_tmp) = c("name","breakpnt", "ci_95_low", "ci_95_hi", "ci_width")
  
  if (!is.null(o$psi)) {
    
    bp = round(o$psi[2])
    ci_lo = round(confint(o)[2])
    ci_hi = round(confint(o)[3])
    ci_width = round(ci_hi - ci_lo)
    
    plot(nvpd_mean ~ year, data=sub, type='b', pch=20, main=name)
    plot(o, add=TRUE,link=FALSE,lwd=2, lty=c(3,3))
    lines(o, col=2, pch=19, bottom=T, lwd=2)
    points(o, col=4, link=FALSE)
    text(2000, max(sub[!is.na(sub$nvpd_mean),'nvpd_mean'])*0.95, paste('CP:', bp), col='red')
    text(2000, max(sub[!is.na(sub$nvpd_mean),'nvpd_mean'])*0.90, paste0('95% CI (range, n. years): ',ci_lo, '-',ci_hi,' n=',ci_width), col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = bp
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = ci_lo
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = ci_hi
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = ci_width
    
  }else{
    plot(nvpd_mean ~ year, data=sub, type='b', pch=20, main=name)
    text(2000, max(sub[!is.na(sub$nvpd_mean),'nvpd_mean'])*0.95, 'NO CHANGE POINT',col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = NA    
    
  }
  
  # final output
  if (exists("df_results") == TRUE){
    df_results = bind_rows(df_results, df_tmp)
  } else {df_results = df_tmp}
  
}
dev.off()

# create analysis specific df for combining with other df at end
df_ttb_vpd = df_results
df_ttb_vpd$experiment = 'ttb_vpd'
rm(df_results)


# =============================
# AVHRR SEASON LENGTH (GFED REGIONS)
# =============================

fp = paste0(dir_base, 'GFED_fire_season_length.csv')
fp_out = paste0(dir_base, 'change_points/')

df_season = as_tibble(as.data.frame(fread(file=fp)))

fp = paste0(fp_out, 'avhrr_seasonlength_breakpoints_gfed.png')
png(filename =  fp, res = 300, width = 8, height = 12, units = 'in')
par(mfrow=c(5, 3),mar=c(2,2,1,1))
assign("last.warning", NULL, envir = baseenv())

for (name in unique(df_season$gfed_name)) {
  sub = df_season %>% filter(gfed_name==name)
  out.lm <- lm(value ~ year, data=sub)
  o <- segmented(out.lm, seg.Z = ~year, psi=2000)
  
  #tmp outputs
  df_tmp = data.frame(matrix(nrow = 0, ncol= 5), stringsAsFactors = FALSE)
  colnames(df_tmp) = c("name","breakpnt", "ci_95_low", "ci_95_hi", "ci_width")
  
  if (!is.null(o$psi)) {
    
    bp = round(o$psi[2])
    ci_lo = round(confint(o)[2])
    ci_hi = round(confint(o)[3])
    ci_width = round(ci_hi - ci_lo)
    
    plot(value ~ year, data=sub, type='b', pch=20, main=name)
    plot(o, add=TRUE,link=FALSE,lwd=2, lty=c(3,3))
    lines(o, col=2, pch=19, bottom=T, lwd=2)
    points(o, col=4, link=FALSE)
    text(2000, max(sub[!is.na(sub$value),'value'])*0.95, paste('CP:', bp), col='red')
    text(2000, max(sub[!is.na(sub$value),'value'])*0.85, paste0('95% CI (range, n. years): ',ci_lo, '-',ci_hi,' n=',ci_width), col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = bp
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = ci_lo
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = ci_hi
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = ci_width
    
  }else{
    plot(value ~ year, data=sub, type='b', pch=20, main=name)
    text(2000, max(sub[!is.na(sub$value),'value'])*0.95, 'NO CHANGE POINT',col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = NA    
    
  }
  
  # final output
  if (exists("df_results") == TRUE){
    df_results = bind_rows(df_results, df_tmp)
  } else {df_results = df_tmp}
  
}
dev.off()

# create analysis specific df for combining with other df at end
df_gfed_sl = df_results
df_gfed_sl$experiment = 'gfed_sl'
rm(df_results)



# =============================
# AVHRR PEAK FIRE ACTIVITY MAGNITUDE (GFED REGIONS)
# =============================

fp = paste0(dir_base, 'GFED_peak_fire_activity_magnitude.csv')
fp_out = paste0(dir_base, 'change_points/')

df_peak = as_tibble(as.data.frame(fread(file=fp)))

fp = paste0(fp_out, 'avhrr_pfam_breakpoints_gfed.png')
png(filename =  fp, res = 300, width = 8, height = 12, units = 'in')
par(mfrow=c(5, 3),mar=c(2,2,1,1))
assign("last.warning", NULL, envir = baseenv())

for (name in unique(df_peak$gfed_name)) {
  sub = df_peak %>% filter(gfed_name==name)
  out.lm <- lm(value ~ year, data=sub)
  o <- segmented(out.lm, seg.Z = ~year, psi=2000)
  
  #tmp outputs
  df_tmp = data.frame(matrix(nrow = 0, ncol= 5), stringsAsFactors = FALSE)
  colnames(df_tmp) = c("name","breakpnt", "ci_95_low", "ci_95_hi", "ci_width")
  
  if (!is.null(o$psi)) {
    
    bp = round(o$psi[2])
    ci_lo = round(confint(o)[2])
    ci_hi = round(confint(o)[3])
    ci_width = round(ci_hi - ci_lo)
    
    plot(value ~ year, data=sub, type='b', pch=20, main=name)
    plot(o, add=TRUE,link=FALSE,lwd=2, lty=c(3,3))
    lines(o, col=2, pch=19, bottom=T, lwd=2)
    points(o, col=4, link=FALSE)
    text(2000, max(sub[!is.na(sub$value),'value'])*0.95, paste('CP:', bp), col='red')
    text(2000, max(sub[!is.na(sub$value),'value'])*0.85, paste0('95% CI (range, n. years): ',ci_lo, '-',ci_hi,' n=',ci_width), col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = bp
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = ci_lo
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = ci_hi
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = ci_width
    
  }else{
    plot(value ~ year, data=sub, type='b', pch=20, main=name)
    text(2000, max(sub[!is.na(sub$value),'value'])*0.95, 'NO CHANGE POINT',col='red')
    
    df_tmp[1,which(colnames(df_tmp)=="name")] = name
    df_tmp[1,which(colnames(df_tmp)=="breakpnt")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_low")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_95_hi")] = NA
    df_tmp[1,which(colnames(df_tmp)=="ci_width")] = NA    
    
  }
  
  # final output
  if (exists("df_results") == TRUE){
    df_results = bind_rows(df_results, df_tmp)
  } else {df_results = df_tmp}
  
}
dev.off()

# create analysis specific df for combining with other df at end
df_gfed_pfam = df_results
df_gfed_pfam$experiment = 'gfed_pfam'
rm(df_results)

# combine all change points and output for manual evaluation vs. cut off criteria
df_final = bind_rows(df_gfed_anf, df_gfed_fwsl, df_gfed_vpd, 
                     df_gfed_sl, df_gfed_pfam, 
                     df_ttb_anf, df_ttb_fwsl, df_ttb_vpd)
write.csv(df_final, paste0(fp_out,"potential_break_points.csv"))



