CANDLESTICK_CHART = function(Folder = '', 
                             File   = '',
                             DOWNLOAD_SOURCE = 'DSE', CODESOURCE = 'VN30F1M',
                             FREQ    = 'INTRADAY', SMA_SH = 5, SMA_LT = 10, 
                             ENDTIME = '04:00', PAST = 0,
                             nb_except = 5,
                             take_profit = 0.6,
                             ENDDATE = '',
                             ChartTitle  = 'S&P',
                             Save_HTML = '',
                             Save_JPG  = '') {
  # ------------------------------------------------------------------------------------------------
  start = Sys.time()
  if (file.exists(paste0(Folder, File))) {
    data = CCPR_READRDS(Folder, File)
    data = data[codesource == CODESOURCE]
  } else {
    data = switch(DOWNLOAD_SOURCE,
                  'YAH' = TRAINEE_DOWNLOAD_INTRADAY_YAH(pCode=CODESOURCE, pDur='1m', NbDays=250),
                  'DSE' = if (FREQ == 'INTRADAY') {
                    DOWNLOAD_DSE_INTRADAY_BY_CODE(Folder_Save = 'S:/CCPR/DATA/IFRCBEQ/INTRADAY/',
                                                  symbol      = CODESOURCE, frequency = 1,
                                                  history     = F, nb_day = 55, ToSave = F)
                  } else {
                    DOWNLOAD_DSE_DAY_BY_CODE(Folder_Save = 'S:/CCPR/DATA/IFRCBEQ/',
                                             symbol      = CODESOURCE, history = F,
                                             nb_day      = 1000, ToSave = F)
                  },
                  'EIK' = DOWNLOAD_EIK(pcode = CODESOURCE)
    )
  }
  if (nrow(data) == 0) {stop("NO DATA")}
  data_todo = data
  data_todo = data_todo[order(date)]
  My.Kable.Min(data_todo)
  if (FREQ == 'INTRADAY') {
    if (DOWNLOAD_SOURCE == 'DSE') {
      data_todo[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Ho_Chi_Minh")]
    } else {
      tzn = unique(data_todo$tzn)
      data_todo[, timestamp := as.POSIXct(as.numeric(timestamp), origin = "1970-01-01", tz = tzn[1])]
      data_todo[, min_tmp := floor_date(timestamp, unit = "minute")]
      data_todo = unique(data_todo, by = "min_tmp", fromLast = T)
      data_todo[, datetime  := min_tmp]
      data_todo[, timestamp := min_tmp]
      data_todo[, min_tmp   := NULL]
    }
  } else {
    if (DOWNLOAD_SOURCE == 'DSE') {
      data_todo[, timestamp := as.POSIXct(as.character(date), format = "%Y-%m-%d", tz = "Asia/Ho_Chi_Minh")]
    } else {
      tzn = unique(data_todo$tzn)
      data_todo[, timestamp := as.POSIXct(as.character(date), format = "%Y-%m-%d", tz = tzn[1])]
    }
  }
  if (DOWNLOAD_SOURCE == 'YAH') {
    data_todo = data_todo[!is.na(close)]
  }
  
  # MA + spread ─────────────────────────────────────────────────────────────────
  data_todo[, paste0('MA', SMA_SH) := rolling_ma(close, SMA_SH)]
  data_todo[, paste0('MA', SMA_LT) := rolling_ma(close, SMA_LT)]
  data_todo[, spread_ratio :=
              ((get(paste0("MA", SMA_SH)) - get(paste0("MA", SMA_LT))) /
                 ((get(paste0("MA", SMA_LT)) + get(paste0("MA", SMA_SH))) / 2)) * 100]
  
  # SIGNAL: spread âm liên tục >= 20 nến VÀ lần đó <= -0.2 ─────────────────────
  
  data_todo[, intraday_idx := rowid(date)]
  data_todo[, skip_candle  := intraday_idx <= nb_except]
  
  data_todo[, neg_streak := {
    streak = 0L
    res    = integer(.N)
    for (i in seq_len(.N)) {
      if (i > 1 && date[i] != date[i - 1]) streak = 0L
      
      if (!skip_candle[i] && !is.na(spread_ratio[i]) && spread_ratio[i] < 0) {
        streak = streak + 1L
      } else {
        streak = 0L
      }
      res[i] = streak
    }
    res
  }]
  
  data_todo[, pos_streak := {
    streak = 0L
    res    = integer(.N)
    for (i in seq_len(.N)) {
      if (i > 1 && date[i] != date[i - 1]) streak = 0L
      
      if (!skip_candle[i] && !is.na(spread_ratio[i]) && spread_ratio[i] > 0) {
        streak = streak + 1L
      } else {
        streak = 0L
      }
      res[i] = streak
    }
    res
  }]
  
  data_todo[, had_positive_today := {
    res          = logical(.N)
    had_pos      = FALSE
    neg_started  = FALSE
    last_pos_idx = 0L
    streak_start = 0L
    
    for (i in seq_len(.N)) {
      if (i > 1 && date[i] != date[i - 1]) {
        had_pos      = FALSE
        neg_started  = FALSE
        last_pos_idx = 0L
        streak_start = 0L
      }
      
      if (!skip_candle[i] && !is.na(spread_ratio[i]) && spread_ratio[i] > 0) {
        had_pos      = TRUE
        neg_started  = FALSE   # streak âm bị phá, reset
        last_pos_idx = i
        streak_start = 0L
      }
      
      if (!is.na(spread_ratio[i]) && spread_ratio[i] < 0) {
        if (!neg_started) {
          neg_started  = TRUE
          streak_start = i
        }
      }
      res[i] = had_pos && neg_started && (last_pos_idx > 0L) && (last_pos_idx < streak_start)
    }
    res
  }]
  # new_rules 0.25
  data_todo[, signal_out := (pos_streak >= 20 & spread_ratio >= 0.2) | (pos_streak >= 10L & spread_ratio >= 0.3)]
  # data_todo[, signal     := neg_streak >= 20 & spread_ratio <= -0.2 & had_positive_today == TRUE]   
  data_todo[, hour_min := as.numeric(format(timestamp_vn, "%H")) * 100 + 
              as.numeric(format(timestamp_vn, "%M"))]
  
  # Sửa signal: chỉ IN trước 14:00
  data_todo[, signal := neg_streak >= 20 & 
              spread_ratio <= -0.2 & 
              had_positive_today == TRUE &
              hour_min < 1400]  # ← thêm dòng này
  # Filter ngày ─────────────────────────────────────────────────────────────────
  ENDDATE = if (ENDDATE == '') Sys.Date() else as.Date(ENDDATE)
  if (FREQ == 'INTRADAY') {
    if (PAST > 0) {
      prev_day  = GET_PREVIOUS_TRADING_DAY(current_date = ENDDATE - PAST + 1)
      data_todo = data_todo[date %between% c(prev_day, ENDDATE)]
    } else {
      data_todo = data_todo[date %in% ENDDATE]
    }
    END_TIME  = as.POSIXct(paste0(ENDDATE, ' ', ENDTIME, ":00"),
                           tz = if (DOWNLOAD_SOURCE == 'YAH') unique(data_todo$tzn)[1] else "Asia/Ho_Chi_Minh")
    data_draw = data_todo[timestamp <= END_TIME]
  } else {
    if (PAST > 0) {
      data_draw = data_todo[GET_PREVIOUS_TRADING_DAY(current_date = ENDDATE - PAST) <= date & date <= ENDDATE]
    } else {
      data_draw = data_todo[date >= floor_date(ENDDATE, "year")]
    }
  }
  
  data_draw = data_draw %>%
    mutate(time_label = if (FREQ == 'INTRADAY') {
      format(timestamp, "%m/%d %H:%M")
    } else {
      format(timestamp, "%m/%d/%Y")
    })
  
  ma_sh_name = paste0("MA", SMA_SH)
  ma_lt_name = paste0("MA", SMA_LT)
  data_draw[, ma_sh := get(ma_sh_name)]
  data_draw[, ma_lt := get(ma_lt_name)]
  
  # Subtitle ────────────────────────────────────────────────────────────────────
  last = tail(data_draw, 1)
  if (FREQ == 'INTRADAY') {
    today_first = data_draw[format(data_draw$timestamp, "%Y-%m-%d") == as.character(ENDDATE), ]
    ref_close   = if (nrow(today_first) > 0) today_first$open[1] else data_draw$close[nrow(data_draw) - 1]
  } else {
    ref_close = data_draw$close[nrow(data_draw) - 1]
  }
  change_val  = last$close - ref_close
  varpc_val   = (change_val / ref_close) * 100
  change_str  = paste0(ifelse(change_val >= 0, "+", ""), formatC(change_val, format = "f", digits = 1))
  varpc_str   = paste0(ifelse(varpc_val  >= 0, "+", ""), formatC(varpc_val,  format = "f", digits = 2), "%")
  last_close  = formatC(last$close, format = "f", digits = 1, big.mark = ",")
  last_spread = formatC(tail(data_draw$spread_ratio[!is.na(data_draw$spread_ratio)], 1),
                        format = "f", digits = 3)
  subtitle_text = paste0(
    "Last: ", last_close,
    "  Change: ", change_str,
    "  VarPC: ", varpc_str,
    "  |  Spread(S-L): ", last_spread, "%"
  )
  updated_text   = paste0("Updated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  max_abs_spread = max(abs(data_draw$spread_ratio), na.rm = TRUE)
  spread_limit   = max(0.3, ceiling(max_abs_spread * 10) / 10)
  
  hedging_state         = FALSE
  hedging_price_val     = NA_real_
  hedging_vec           = logical(nrow(data_draw))
  hedging_price_vec     = numeric(nrow(data_draw))
  already_hedged_today  = FALSE   # ← thêm flag này
  
  for (i in seq_len(nrow(data_draw))) {
    # Reset khi sang ngày mới
    if (i > 1 && as.Date(data_draw$timestamp[i]) != as.Date(data_draw$timestamp[i - 1])) {
      hedging_state         = FALSE
      hedging_price_val     = NA_real_
      already_hedged_today  = FALSE   # ← reset theo ngày
    }
    
    # Chỉ vào hedge nếu chưa hedge lần nào hôm nay
    if (isTRUE(data_draw$signal[i]) && !hedging_state && !already_hedged_today) {
      hedging_state         = TRUE
      hedging_price_val     = data_draw$close[i]
      already_hedged_today  = TRUE    # ← đánh dấu đã hedge hôm nay
    }
    
    tp_price = if (!is.na(hedging_price_val)) ceiling(hedging_price_val * (1 - take_profit / 100)) else NA_real_
    
    if ((isTRUE(data_draw$signal_out[i]) || (!is.na(tp_price) && data_draw$close[i] <= tp_price)) && hedging_state) {
      hedging_state     = FALSE
      hedging_price_val = NA_real_
      # already_hedged_today vẫn = TRUE → không vào lại
    }
    
    hedging_vec[i]       = hedging_state
    hedging_price_vec[i] = hedging_price_val
  }
  
  data_draw[, hedging       := hedging_vec]
  data_draw[, hedging_price := hedging_price_vec]
  data_draw[, hedge_entry   := hedging == TRUE & shift(hedging, fill = FALSE) == FALSE]
  data_draw[, signal_price  := ifelse(hedge_entry == TRUE, close, NA_real_)]
  
  data_draw[, signal_spread := ifelse(hedge_entry == TRUE, spread_ratio, NA_real_)]
  
  data_draw[, signal_label := ifelse(
    hedge_entry == TRUE,
    paste0("🔴 SHORT\nTime:   ", time_label,
           "\nClose:  ", round(close, 1),
           "\nSpread: ", round(spread_ratio, 3), "%"),
    NA_character_
  )]
  
  data_draw[, signal_time_label := ifelse(
    hedge_entry == TRUE,
    paste0(time_label, "|||", round(spread_ratio, 3)),
    time_label
  )]
  
  data_draw[, out_entry := !hedging & shift(!hedging, fill = TRUE) == FALSE]
  data_draw[, out_price   := ifelse(out_entry == TRUE, close, NA_real_)]
  data_draw[, out_time_label := ifelse(
    out_entry == TRUE,
    paste0(time_label, "|||", round(spread_ratio, 3)),
    time_label
  )]
  # Chart ───────────────────────────────────────────────────────────────────────
  fig = data_draw %>%
    e_charts(signal_time_label) %>%
    
    e_candle(
      opening = close, closing = open, low = low, high = high,
      name    = "Giá", y_index = 0,
      itemStyle = list(
        color        = "#26a69a", color0       = "#ef5350",
        borderColor  = "#26a69a", borderColor0 = "#ef5350"
      )
    ) %>%
    e_line(serie = ma_sh, name = ma_sh_name, y_index = 0,
           smooth = FALSE, lineStyle = list(color = "#29b6f6", width = 1.2),
           color = "#29b6f6", symbol = "none") %>%
    e_line(serie = ma_lt, name = ma_lt_name, y_index = 0,
           smooth = FALSE, lineStyle = list(color = "#1565c0", width = 1.2),
           color = "#1565c0", symbol = "none") %>%
    e_scatter(
      serie      = signal_price,
      name       = "Signal",
      y_index    = 0,
      symbol     = "circle",
      symbolSize = 20,
      itemStyle  = list(color = "#ff1744"),
      label      = list(
        show       = TRUE,
        position   = "bottom",
        color      = "#ff1744",
        fontSize   = 11,
        fontWeight = "bold",
        lineHeight = 18,
        formatter  = htmlwidgets::JS("
      function(params) {
        if (!params.value[1]) return '';
        var parts  = params.name.split('|||');
        var time   = parts[0];
        var spread = parts.length > 1 ? parts[1] : '';
        return 'SHORT'
          + '\\nTime:   ' + time
          + '\\nClose:  ' + parseFloat(params.value[1]).toFixed(1)
          + (spread ? '\\nSpread: ' + spread + '%' : '');
      }
    ")
      )
    ) %>%
    # Sau e_scatter Signal, thêm:
    e_scatter(
      serie      = out_price,
      name       = "Out",
      y_index    = 0,
      symbol     = "circle",
      symbolSize = 20,
      itemStyle  = list(color = "#00e676"),
      label      = list(
        show       = TRUE,
        position   = "bottom",
        color      = "#00e676",
        fontSize   = 11,
        fontWeight = "bold",
        lineHeight = 18,
        formatter  = htmlwidgets::JS("
      function(params) {
        if (!params.value[1]) return '';
        return 'OUT'
          + '\\nTime:   ' + params.name
          + '\\nClose:  ' + parseFloat(params.value[1]).toFixed(1);
      }
    ")
      )
    ) %>%
    e_bar(
      serie     = spread_ratio, name = "Spread(S-L)",
      x_index   = 1, y_index = 1,
      itemStyle = list(
        color = htmlwidgets::JS("function(params) {
        return params.value[1] >= 0 ? 'rgba(38,166,154,0.7)' : 'rgba(239,83,80,0.7)';
      }")
      )
    ) %>%
    e_mark_line(data = list(yAxis = 0.2), symbol = "none", y_index = 1,
                lineStyle = list(type = "dashed", color = "#ffeb3b", width = 1)) %>%
    e_mark_line(data = list(yAxis = -0.2), symbol = "none", y_index = 1,
                lineStyle = list(type = "dashed", color = "#ffeb3b", width = 1)) %>%
    
    e_grid(index = 0, top = "15%", left = "6%", right = "2%", height = "55%") %>%
    e_grid(index = 1, top = "75%", left = "6%", right = "2%", height = "15%") %>%
    
    e_x_axis(index = 0, gridIndex = 0, type = "category", show = FALSE,
             axisLine  = list(lineStyle = list(color = "#2a2e39")),
             splitLine = list(lineStyle = list(color = "#2a2e39"))) %>%
    e_x_axis(index = 1, gridIndex = 1, type = "category",
             axisLabel = list(color = "#d1d4dc", rotate = 0),
             axisLine  = list(lineStyle = list(color = "#2a2e39")),
             splitLine = list(lineStyle = list(color = "#2a2e39"))) %>%
    
    e_y_axis(index = 0, gridIndex = 0, scale = TRUE,
             axisLabel = list(color = "#d1d4dc"),
             axisLine  = list(lineStyle = list(color = "#2a2e39")),
             splitLine = list(lineStyle = list(color = "#2a2e39"))) %>%
    e_y_axis(index = 1, gridIndex = 1,
             min = -spread_limit, max = spread_limit,
             axisLabel = list(color = "#d1d4dc", fontSize = 10),
             axisLine  = list(lineStyle = list(color = "#2a2e39")),
             splitLine = list(lineStyle = list(color = "#2a2e39"))) %>%
    
    # e_legend(
    #   orient    = "horizontal",
    #   left      = "0%", bottom = "0%",
    #   textStyle = list(color = "#d1d4dc", fontSize = 11)
    # ) %>%
    e_legend(
      orient    = "horizontal",
      left      = "0%", bottom = "0%",
      textStyle = list(color = "#d1d4dc", fontSize = 11),
      data      = list(
        list(name = "Giá"),
        list(name = ma_sh_name),
        list(name = ma_lt_name),
        list(name = "Spread(S-L)")
      )
    ) %>%
    e_tooltip(
      trigger     = "axis",
      axisPointer = list(type = "cross"),
      alwaysShowContent = TRUE,
      formatter   = htmlwidgets::JS("
        function(params) {
          var out = params[0].axisValueLabel + '<br/>';
          var seen = {};  // ← track series đã show
          var hasSignal = params.some(function(p) { 
            return p.seriesName === 'Signal' && p.value[1] !== null && p.value[1] !== undefined; 
          });
          
          params.forEach(function(p) {
            if (seen[p.seriesName]) return;  // ← skip nếu đã show
            seen[p.seriesName] = true;
            
            if (Array.isArray(p.value) && p.value.length >= 5 && p.seriesName === 'Giá') {
              if (!hasSignal) {
                out += p.marker + p.seriesName + '<br/>';
                out += '&nbsp;&nbsp;open: <b>'    + parseFloat(p.value[1]).toFixed(2) + '</b><br/>';
                out += '&nbsp;&nbsp;close: <b>'   + parseFloat(p.value[2]).toFixed(2) + '</b><br/>';
                out += '&nbsp;&nbsp;lowest: <b>'  + parseFloat(p.value[3]).toFixed(2) + '</b><br/>';
                out += '&nbsp;&nbsp;highest: <b>' + parseFloat(p.value[4]).toFixed(2) + '</b><br/>';
              }
            } else if (p.seriesName === 'Signal' && p.value[1] !== null && p.value[1] !== undefined) {
              var candle = params.find(function(x) { return x.seriesName === 'Giá'; });
              var spread = params.find(function(x) { return x.seriesName === 'Spread(S-L)'; });
              out += '🔴 <b>Signal</b><br/>';
              if (candle) {
                out += '&nbsp;&nbsp;open: <b>'    + parseFloat(candle.value[1]).toFixed(2) + '</b><br/>';
                out += '&nbsp;&nbsp;close: <b>'   + parseFloat(candle.value[2]).toFixed(2) + '</b><br/>';
                out += '&nbsp;&nbsp;lowest: <b>'  + parseFloat(candle.value[3]).toFixed(2) + '</b><br/>';
                out += '&nbsp;&nbsp;highest: <b>' + parseFloat(candle.value[4]).toFixed(2) + '</b><br/>';
              }
              if (spread) {
                var sv = Array.isArray(spread.value) ? spread.value[1] : spread.value;
                out += '&nbsp;&nbsp;spread: <b>' + parseFloat(sv).toFixed(3) + '%</b><br/>';
              }
            } else if (p.seriesName !== 'Signal' && p.seriesName !== 'Giá') {
              var val = Array.isArray(p.value) ? p.value[1] : p.value;
              val = (typeof val === 'number') ? val.toFixed(2) : val;
              out += p.marker + p.seriesName + ': <b>' + val + '</b><br/>';
            }
          });
          return out;
        }
    ")
    ) %>%
    
    e_datazoom(type = "inside", xAxisIndex = c(0, 1)) %>%
    e_title(
      text         = ifelse(is.na(ChartTitle) | ChartTitle == "", paste0(CODESOURCE, " ",format(max(data_draw$timestamp), "%Y-%m-%d %H:%M")), ChartTitle),
      subtext      = subtitle_text,
      left         = "center",
      textStyle    = list(color = "#d1d4dc", fontSize = 20, fontWeight = "bold"),
      subtextStyle = list(color = "#9598a1", fontSize = 13)
    ) %>%
    
    e_text_g(
      elements = list(list(
        type  = "text",
        right = 10, bottom = 2,
        style = list(text = updated_text, fill = "#9598a1", fontSize = 12)
      ))
    ) %>%
    
    htmlwidgets::onRender("
    function(el, x) {
      var chart = echarts.getInstanceByDom(el);
      chart.setOption({ backgroundColor: '#131722' });
    }
  ")
  
  if (nchar(Save_HTML) > 0) {
    saveWidget(fig, file = Save_HTML, selfcontained = FALSE)
    html <- readLines(Save_HTML)
    html <- sub(
      "<head>",
      '<head>\n<script>setTimeout(function(){ location.href = location.pathname + "?t=" + Date.now(); }, 30000);</script>\n<meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate">\n<meta http-equiv="Pragma" content="no-cache">\n<meta http-equiv="Expires" content="0">',
      html
    )
    writeLines(html, Save_HTML)
  }
  if (nchar(Save_JPG) > 0) {
    tmp_html = gsub('.jpg', '.html', Save_JPG)
    saveWidget(fig, file = tmp_html, selfcontained = TRUE)
    
    webshot2::webshot(tmp_html, file = Save_JPG, vwidth = 1400, vheight = 600, delay = 1.5)
    if (length(Save_HTML) == 0) file.remove(tmp_html)
  }
  print(fig)
  end = Sys.time()
  DURATION = end - start
  summary_table = data_draw[, .(
    name          = CODESOURCE,
    timestamp,
    close,
    ma_sh         = round(ma_sh, 3),
    ma_lt         = round(ma_lt, 3),
    spread        = round(spread_ratio, 3),
    short         = fifelse(spread_ratio <  0, 1L, 0L),
    out           = fifelse(spread_ratio >= 0, 1L, 0L),
    hedging,
    hedging_price = hedging_price_vec,
    nr            = fifelse(spread_ratio < 0, neg_streak, pos_streak)  
  )]
  setnames(summary_table, c("ma_sh", "ma_lt"), c(ma_sh_name, ma_lt_name))
  My.Kable(summary_table)
  return(summary_table)
  CATln_Border(paste0('DONE:', round(as.numeric(DURATION, units = "secs"), 1), 's'))
}
