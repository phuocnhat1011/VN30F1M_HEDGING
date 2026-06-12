CALCULATE_SIGNAL_BY_DATASET = function(dt     = data.tabe(),
                                       sma_sh = 10,       sma_lt     = 50,
                                       spread_in  = -0.2, spread_out = c(0.3, 0.2),
                                       streak_in  = 20,   streak_out = c(10, 20),
                                       nb_except  = 5,
                                       per_profit = 0.6,
                                       spread_signal = 0.5,
                                       hedge_streak  = 10,
                                       hedge_red_pct = 0.7) {
  # ------------------------------------------------------------------------------------------------
  d = copy(dt)
  spread_signal_num = ifelse(is.na(spread_signal) | spread_signal == "", NA_real_, as.numeric(spread_signal))
  use_spread_signal = !is.na(spread_signal_num)
  
  d[, ma_sh := rolling_ma(close, sma_sh)]
  d[, ma_lt := rolling_ma(close, sma_lt)]
  d[, spread_ratio := ((ma_sh - ma_lt) / ((ma_lt + ma_sh) / 2)) * 100]
  
  d[, intraday_idx := rowid(date)]
  d[, skip_candle  := intraday_idx <= nb_except]
  
  # streak âm
  d[, valid_neg  := !skip_candle & !is.na(spread_ratio) & spread_ratio < 0]
  d[, grp        := rleid(date, valid_neg)]
  d[, neg_streak := rowid(grp) * valid_neg]
  d[, c("valid_neg", "grp") := NULL]
  
  # streak dương
  d[, valid_pos  := !skip_candle & !is.na(spread_ratio) & spread_ratio > 0]
  d[, grp        := rleid(date, valid_pos)]
  d[, pos_streak := rowid(grp) * valid_pos]
  d[, c("valid_pos", "grp") := NULL]
  
  d[, had_positive_today := {
    res          = logical(.N)
    had_pos      = F
    neg_started  = F
    last_pos_idx = 0L
    streak_start = 0L
    for (i in seq_len(.N)) {
      if (i > 1 && date[i] != date[i - 1]) {
        had_pos      = F
        neg_started  = F
        last_pos_idx = 0L
        streak_start = 0L
      }
      if (!skip_candle[i] && !is.na(spread_ratio[i]) && spread_ratio[i] > 0) {
        had_pos      = T
        neg_started  = F
        last_pos_idx = i
        streak_start = 0L
      }
      if (!is.na(spread_ratio[i]) && spread_ratio[i] < 0 && !neg_started) {
        neg_started  = T
        streak_start = i
      }
      res[i] = had_pos && neg_started && last_pos_idx > 0L && last_pos_idx < streak_start
    }
    res
  }]
  
  d[, hour      := as.integer(format(timestamp, "%H"))]
  d[, signal_in := neg_streak >= streak_in & spread_ratio <= spread_in & had_positive_today == TRUE & hour < 14]
  
  hedge_streak_num  = suppressWarnings(as.integer(hedge_streak))
  hedge_red_pct_num = suppressWarnings(as.numeric(hedge_red_pct))
  use_hedge         = !is.na(hedge_streak_num) & !is.na(hedge_red_pct_num)
  
  d[, pct_red    := frollapply(close < open, hedge_streak_num, mean, fill = NA, align = "right")]
  d[, cond_hedge := neg_streak == hedge_streak_num & !is.na(spread_ratio) & spread_ratio <= spread_in]
  d[, signal_hedge := if (!use_hedge) FALSE else cond_hedge & !is.na(pct_red) & pct_red >= hedge_red_pct_num]
  d[, c("pct_red", "cond_hedge") := NULL]
  
  # signal 5
  d[, spread_sign := fcase(spread_ratio > 0, 1L, spread_ratio < 0, -1L, default = NA_integer_)]
  
  d[, type := {
    cross       = spread_sign != shift(spread_sign)
    cross[1]    = FALSE
    first_cross = which(cross)[1]
    out         = rep(NA_character_, .N)
    if (!is.na(first_cross)) {
      out[first_cross:.N] = fifelse(spread_sign[first_cross:.N] > 0, "cross_up", "cross_down")
    }
    out
  }, by = date]
  
  d[, spread_sign := NULL]
  
  d_s5 = copy(d)
  d_s5[, grp := rleid(type)]
  d_s5[type == 'cross_down', min_close := shift(cummin(close), 1), by = .(date, grp)]
  d_s5[type == 'cross_down', ma_sh := close]
  d_s5[type == 'cross_down', ma_lt := min_close]
  d_s5[, spread_ratio_new := ((ma_sh - ma_lt) / ((ma_lt + ma_sh) / 2)) * 100]
  
  d_merged = merge(d, d_s5[, .(timestamp, spread_ratio_new, min_close)], by = 'timestamp')
  
  cond_entry = (signal_in == TRUE | signal_hedge == TRUE) & had_positive_today == TRUE & hour < 14
  cond_s5    = !use_spread_signal | (spread_ratio_new < spread_signal_num | close < min_close)
  d_merged[cond_entry & cond_s5, signal_in_final := TRUE]
  d = copy(d_merged)
  
  # take profit
  per_profit_num  = suppressWarnings(as.numeric(per_profit))
  use_take_profit = !is.na(per_profit_num)
  
  d[, signal_out := {
    res         = logical(.N)
    entry_price = NA_real_
    in_position = FALSE
    
    for (i in seq_len(.N)) {
      # reset khi sang ngày mới
      if (i > 1 && date[i] != date[i - 1]) {
        entry_price = NA_real_
        in_position = F
      }
      # đang trong vị thế → check điều kiện thoát
      if (in_position) {
        take_profit     = use_take_profit && !is.na(entry_price) &&
          close[i] <= entry_price * (1 - per_profit_num / 100)
        streak_out_cond = (pos_streak[i] >= streak_out[1] & spread_ratio[i] >= spread_out[1]) |
          (pos_streak[i] >= streak_out[2] & spread_ratio[i] >= spread_out[2])
        res[i] = streak_out_cond | take_profit
        if (res[i]) {
          entry_price = NA_real_
          in_position = FALSE
        }
      }
      # chưa có vị thế → check điều kiện vào
      if (!in_position && isTRUE(signal_in_final[i])) {
        entry_price = close[i]
        in_position = TRUE
      }
    }
    res
  }]
  
  d[, is_last_candle := c(date[-.N] != date[-1L], TRUE)]
  return(d)
}
# ==================================================================================================
GET_SUMMARY_BACKTESING = function(dt,
                                   sma_sh        = 10,
                                   sma_lt        = 50,
                                   spread_in     = -0.2,
                                   spread_out    = c(0.3, 0.2),
                                   streak_in     = 20,
                                   streak_out    = c(10, 20),
                                   nb_except     = 5,
                                   per_profit    = 3,
                                   spread_signal = 0.5,
                                   hedge_streak  = '',
                                   hedge_red_pct = '',
                                   tosumary      = FALSE) {
  # ------------------------------------------------------------------------------------------------
  per_profit_num  = suppressWarnings(as.numeric(per_profit))
  use_take_profit = !is.na(per_profit_num)
  
  d = CALCULATE_SIGNAL_BY_DATASET(
    dt            = dt,
    sma_sh        = sma_sh,
    sma_lt        = sma_lt,
    spread_in     = spread_in,
    spread_out    = spread_out,
    streak_in     = streak_in,
    streak_out    = streak_out,
    nb_except     = nb_except,
    per_profit    = per_profit,
    spread_signal = spread_signal,
    hedge_streak  = hedge_streak,
    hedge_red_pct = hedge_red_pct
  )
  
  make_summary = function(pnl = numeric(0)) data.table(
    spread_in     = spread_in,
    spread_out1   = spread_out[1],
    spread_out2   = spread_out[2],
    streak_in     = streak_in,
    streak_out1   = streak_out[1],
    streak_out2   = streak_out[2],
    nb_except     = nb_except,
    per_profit    = per_profit,
    spread_signal = spread_signal,
    hedge_streak  = hedge_streak,
    hedge_red_pct = hedge_red_pct,
    n_trades      = length(pnl),
    win_rate      = if (length(pnl) == 0) NA_real_ else round(mean(pnl > 0) * 100, 1),
    avg_pnl       = if (length(pnl) == 0) NA_real_ else round(mean(pnl), 3),
    total_pnl     = if (length(pnl) == 0) NA_real_ else round(sum(pnl), 3)
  )
  
  trades       = list()
  in_trade     = FALSE
  entry_price  = NA_real_
  entry_time   = NA
  entry_date   = NA
  traded_dates = character(0)
  
  for (i in seq_len(nrow(d))) {
    if (!in_trade) {
      if (isTRUE(d$signal_in_final[i])) {
        if (d$is_last_candle[i]) next
        if (as.integer(format(d$timestamp[i], "%H")) >= 14) next
        if (as.character(d$date[i]) %in% traded_dates) next
        
        in_trade    = TRUE
        entry_price = d$close[i]
        entry_time  = d$timestamp[i]
        entry_date  = d$date[i]
      }
      next
    }
    
    if (in_trade) {
      exit_reason = NA_character_
      exit_price  = NA_real_
      
      if (!is.na(d$signal_out[i]) && d$signal_out[i]) {
        if (use_take_profit && !is.na(entry_price) && d$close[i] <= entry_price * (1 - per_profit_num / 100)) {
          exit_reason = "take_profit"
        } else if (d$pos_streak[i] >= streak_out[1] & d$spread_ratio[i] >= spread_out[1]) {
          exit_reason = "signal_out_tier1"
        } else {
          exit_reason = "signal_out_tier2"
        }
        exit_price = d$close[i]
      }
      
      if (is.na(exit_reason) && d$is_last_candle[i]) {
        exit_reason = "end_of_day"
        exit_price  = d$close[i]
      }
      
      if (!is.na(exit_reason)) {
        pnl = entry_price - exit_price
        
        trades[[length(trades) + 1]] = list(
          trade_date  = entry_date,
          entry_time  = entry_time,
          entry_price = entry_price,
          exit_time   = d$timestamp[i],
          exit_price  = exit_price,
          exit_reason = exit_reason,
          pnl         = round(pnl, 3)
        )
        
        traded_dates = c(traded_dates, as.character(entry_date))
        in_trade     = FALSE
        entry_price  = NA_real_
      }
    }
  }
  
  if (length(trades) == 0) {
    if (tosumary) return(make_summary())
    return(data.table())
  }
  
  result = rbindlist(trades)
  if (tosumary) return(make_summary(result$pnl))
  return(result)
}
# ==================================================================================================
spread_pairs = list(
  c(-0.15,  0.15),
  c(-0.20,  0.10),
  c(-0.20,  0.15),
  c(-0.20,  0.20),
  c(-0.25,  0.10),
  c(-0.25,  0.15),
  c(-0.25,  0.20),
  c(-0.30,  0.10),
  c(-0.30,  0.15),
  c(-0.30,  0.20),
  c(-0.30,  0.25),
  c(-0.30,  0.30)
)
nb_except_list  = c(0, 5, 10)
streak_in_list  = c(15, 20, 25)
streak_out_list = c(10, 15, 20)
per_profit_list = c(0.3, 0.6, 1.0)

grid_results = rbindlist(lapply(spread_pairs, function(sp) {
  rbindlist(lapply(nb_except_list, function(nb) {
    rbindlist(lapply(streak_in_list, function(si) {
      rbindlist(lapply(streak_out_list, function(so) {
        rbindlist(lapply(per_profit_list, function(pp) {
          cat(sprintf("sp_in=%.2f sp_out=%.2f | nb=%d | str_in=%d str_out=%d | pp=%.1f\n",
                      sp[1], sp[2], nb, si, so, pp))
          GET_SUMMARY_BACKTESING(
            dt            = data_todo,
            sma_sh        = 10,
            sma_lt        = 50,
            spread_in     = sp[1],
            spread_out    = c(sp[2], sp[2]),
            streak_in     = si,
            streak_out    = c(so, so),
            nb_except     = nb,
            per_profit    = pp,
            spread_signal = 0.5,
            hedge_streak  = '',
            hedge_red_pct = '',
            tosumary      = TRUE
          )
        }))
      }))
    }))
  }))
}))