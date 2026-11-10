# =============================================================================
# ByteBridge - Service Pricing (trade vendor quotes)
# -----------------------------------------------------------------------------
# R Shiny + SQLite. Companion to the Item Pricing & Lead Time app, for SERVICES
# (engineering, cabling, electrical, mechanical, commissioning ...).
#
#   Quotes        search & filter, KPI strip, quick view, bid leveling
#   Quote detail  pricing by section, cost mix, scope/exclusions, rate card
#                 vs market, project basis, source PDF viewer
#   Benchmarks    hourly rates by role, effective labor $/hr, fee per kW,
#                 quoted value by vendor & cost type
#   Vendors       vendor cards with contacts and quote history
#   Add quote     upload PDF -> auto-parse -> review next to the PDF -> save
#
# Files:  app.R, R/parse_quote.R, R/helpers.R, R/db.R, www/ (logo)
# Data:   service_quotes.sqlite + quote_files/ (created on first run)
# Pkgs:   shiny bslib DBI RSQLite DT ggplot2 pdftools httr2 (Entra sign-in + SharePoint inbox)
# Auth:   Microsoft Entra ID sign-in (R/auth.R), settings in entra.env
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DBI); library(RSQLite)
  library(DT); library(ggplot2); library(htmltools)
})
for (f in c("R/auth.R", "R/helpers.R", "R/parse_quote.R", "R/db.R", "R/sharepoint.R")) source(f, local = TRUE, encoding = "UTF-8")

options(shiny.maxRequestSize = 50 * 1024^2)       # quotes up to 50 MB
init_db()
addResourcePath("qfiles", normalizePath(FILES_DIR))

# =============================================================================
# UI components
# =============================================================================
avatar <- function(name, trade = NA, size = 38) {
  div(class = "avatar", style = sprintf("--c:%s;width:%dpx;height:%dpx;font-size:%.0fpx",
                                        trade_color(trade), size, size, size * 0.36), initials(name))
}
pill <- function(text, cls = "info") span(class = paste("pill", cls), text)
status_pill <- function(s) {
  pill(s, switch(s, "Awarded" = "ok", "Declined" = "bad", "Expired" = "bad",
                 "Under review" = "warn", "Superseded" = "muted", "info"))
}
expiry_pill <- function(days) {
  if (is.na(days)) return(pill("No expiry stated", "muted"))
  if (days < 0)    return(pill(paste0("Expired ", -days, "d ago"), "bad"))
  if (days <= 7)   return(pill(paste0(days, "d left"), "warn"))
  pill(paste0(days, "d left"), "ok")
}
trade_chip <- function(t) {
  if (length(t) != 1 || is.na(t) || !nzchar(t)) return(NULL)
  span(class = "trade-chip", style = sprintf("--c:%s", trade_color(t)), t)
}
fact <- function(label, value) div(class = "fact", div(class = "fact-l", label), div(class = "fact-v", value))
empty_state <- function(title, body, ...) {
  div(class = "empty", img(src = "bytebridge-mark.png", class = "empty-mark", alt = ""),
      h5(title), p(body), ...)
}
section_title <- function(...) div(class = "section-l", ...)

mix_bar <- function(mix, h = 10) {
  tot <- sum(mix)
  if (!length(mix) || !is.finite(tot) || tot <= 0) return(div(class = "mix mix-empty", style = sprintf("height:%dpx", h)))
  div(class = "mix", style = sprintf("height:%dpx", h),
      lapply(names(mix), function(k) span(
        style = sprintf("width:%.3f%%;background:%s", 100 * mix[[k]] / tot, mix_color(k)),
        title = sprintf("%s: %s (%.0f%%)", k, fmt_money(mix[[k]]), 100 * mix[[k]] / tot))))
}
mix_legend <- function(mix, amounts = TRUE) {
  tot <- sum(mix); if (!length(mix) || tot <= 0) return(NULL)
  div(class = "mix-legend", lapply(names(mix), function(k) div(class = "ml-row",
    span(class = "dot", style = sprintf("background:%s", mix_color(k))),
    span(class = "ml-l", k),
    if (amounts) span(class = "ml-v", fmt_money(mix[[k]])) else span(),
    span(class = "ml-p", sprintf("%.0f%%", 100 * mix[[k]] / tot)))))
}
key_metric <- function(r) {
  if (!is.na(r$fee_per_kw)) return(list(v = paste0(fmt_money(r$fee_per_kw, 0), "/kW"), l = "Fee per kW IT"))
  if (!is.na(r$blended_rate)) return(list(v = paste0(fmt_money(r$blended_rate, 0), "/hr"), l = "Effective labor rate"))
  list(v = "—", l = "Unit metric")
}

kpi_tile <- function(accent, label, value, sub = NULL, extra = NULL) {
  div(class = "kpi", style = sprintf("--accent:%s", accent),
      div(class = "kpi-l", label), div(class = "kpi-v", value),
      if (!is.null(extra)) extra, if (!is.null(sub)) div(class = "kpi-s", sub))
}

# ---------------------------------------------------------------------------- editable table module
editTableUI <- function(id, title, subtitle = NULL) {
  ns <- NS(id)
  card(class = "edit-card",
    card_header(class = "d-flex justify-content-between align-items-center gap-2 flex-wrap",
      div(style = "min-width:0;flex:1 1 220px", span(class = "card-t", title), if (!is.null(subtitle)) div(class = "card-sub", subtitle)),
      div(class = "d-flex gap-1 flex-shrink-0",
          actionButton(ns("add"), "+ Row", class = "btn btn-sm btn-light"),
          actionButton(ns("del"), "Remove selected", class = "btn btn-sm btn-outline-danger"))),
    card_body(class = "p-0", DTOutput(ns("tbl"))))
}
editTableServer <- function(id, data_rv, blank_row, reset_trigger) {
  moduleServer(id, function(input, output, session) {
    version <- reactiveVal(0)
    output$tbl <- renderDT({
      reset_trigger(); version()
      datatable(isolate(data_rv()), rownames = FALSE, editable = list(target = "cell"),
                selection = "multiple", class = "compact hover edit-dt",
                options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE, scrollY = "320px",
                               scrollCollapse = TRUE,
                               language = list(emptyTable = "No rows yet — click + Row")))
    })
    proxy <- dataTableProxy("tbl")
    observeEvent(input$tbl_cell_edit, {
      d <- editData(data_rv(), input$tbl_cell_edit, rownames = FALSE)
      data_rv(d); replaceData(proxy, d, resetPaging = FALSE, rownames = FALSE)
    })
    observeEvent(input$add, { data_rv(rbind(data_rv(), blank_row)); version(version() + 1) })
    observeEvent(input$del, {
      s <- input$tbl_rows_selected
      if (length(s)) { data_rv(data_rv()[-s, , drop = FALSE]); version(version() + 1) }
    })
  })
}
blank_lines <- data.frame(line_type = "base", section = "", category = "Labor", description = "", qty = 1,
                          unit = "ea", material = NA_real_, labor = NA_real_, amount = NA_real_, note = "",
                          stringsAsFactors = FALSE)
blank_rates <- data.frame(role = "", rate = NA_real_, unit = "hour", stringsAsFactors = FALSE)
blank_scope <- data.frame(kind = "inclusion", discipline = "", text = "", assumed = FALSE, stringsAsFactors = FALSE)
blank_equip <- data.frame(qty = NA_real_, description = "", kw_each = NA_real_, stringsAsFactors = FALSE)

# =============================================================================
# Theme & CSS
# =============================================================================
theme <- bs_theme(
  version = 5, bg = "#ffffff", fg = "#0f172a", primary = BRAND$blue, secondary = "#64748b",
  success = BRAND$green, warning = "#e0a800", danger = BRAND$red, info = BRAND$blue,
  # Google font with a full system fallback (works even where Google Fonts is blocked)
  base_font = font_collection(font_google("Plus Jakarta Sans", wght = c(400, 500, 600, 700, 800), local = FALSE),
                              "Segoe UI", "system-ui", "-apple-system", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection(font_google("Plus Jakarta Sans", wght = c(600, 700, 800), local = FALSE),
                                 "Segoe UI", "system-ui", "-apple-system", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
  "border-radius" = "0.65rem", "card-border-color" = "#e6e9f0", "font-size-base" = "0.9rem",
  "navbar-padding-y" = "0.55rem"
)

css <- "
:root { --navy:#0b1220; --ink:#0f172a; --muted:#64748b; --line:#e6e9f0; --soft:#f4f6fb;
        --blue:#2673fc; --green:#13ad47; --yellow:#fdbf01; --red:#fd4034; }
body { background:var(--soft); font-feature-settings:'tnum' 1, 'cv11' 1; }
.navbar { background:var(--navy) !important; border-bottom:1px solid #1c2638; }
.navbar .nav-link { color:#8b97ad !important; font-weight:600; font-size:.86rem; padding:.45rem .85rem !important; border-radius:.5rem; }
.navbar .nav-link:hover { color:#fff !important; background:rgba(255,255,255,.05); }
.navbar .nav-link.active { color:#fff !important; background:rgba(38,115,252,.2); }
.brand { display:flex; align-items:center; gap:.85rem; }
.brand img { height:26px; }
.brand .divider { width:1px; height:22px; background:#2a3549; }
.brand .app { color:#e2e8f0; font-weight:700; font-size:.95rem; letter-spacing:-.01em; line-height:1.15; }
.brand .app small { display:block; color:#64748b; font-weight:700; font-size:.62rem; letter-spacing:.09em; text-transform:uppercase; }
.nav-stamp { color:#64748b; font-size:.78rem; font-weight:600; }
.nav-stamp b { color:#cbd5e1; }

.wrap { max-width:1500px; margin:0 auto; padding:1.4rem 1.4rem 3rem; }
.page-h { display:flex; justify-content:space-between; align-items:flex-end; margin-bottom:1.1rem; gap:1rem; flex-wrap:wrap; }
.page-h h3 { margin:0; font-weight:800; letter-spacing:-.025em; font-size:1.55rem; }
.page-h p { margin:.2rem 0 0; color:var(--muted); }
.page-h .shiny-input-container { margin-bottom:0; }
.card { border-radius:.85rem; box-shadow:0 1px 2px rgba(15,23,42,.04); border-color:var(--line); }
.card-header { background:#fff; border-bottom:1px solid #eef1f6; padding:.85rem 1.1rem; }
.card-t { font-weight:700; letter-spacing:-.01em; }
.card-sub { color:var(--muted); font-size:.78rem; }
.btn { font-weight:600; }
.btn-primary { box-shadow:0 1px 2px rgba(38,115,252,.35); }
.btn-light { background:#fff; border-color:var(--line); }

/* KPI tiles - the top accent bars echo the logo's four colours */
.kpi { position:relative; background:#fff; border:1px solid var(--line); border-radius:.85rem; padding:1rem 1.1rem; height:100%; overflow:hidden; }
.kpi::before { content:''; position:absolute; left:1.1rem; top:0; width:34px; height:4px; border-radius:0 0 4px 4px; background:var(--accent); }
.kpi-l { color:var(--muted); font-size:.7rem; text-transform:uppercase; letter-spacing:.08em; font-weight:700; margin-top:.25rem; }
.kpi-v { font-size:1.7rem; font-weight:800; letter-spacing:-.03em; margin-top:.1rem; line-height:1.2; }
.kpi-s { color:#94a3b8; font-size:.78rem; font-weight:500; margin-top:.2rem; }
.kpi .mix { margin:.55rem 0 .15rem; }

.filters { background:#fff; border:1px solid var(--line); border-radius:.85rem; padding:.8rem 1rem .05rem; margin:1rem 0; }
.filters label, .form-label, .control-label { font-size:.7rem; text-transform:uppercase; letter-spacing:.06em; color:var(--muted); font-weight:700; }
.form-control, .selectize-input { border-color:#dfe3eb; }
.form-control:focus, .selectize-input.focus { border-color:var(--blue); box-shadow:0 0 0 3px rgba(38,115,252,.14); }

.pill { display:inline-flex; align-items:center; padding:.16rem .6rem; border-radius:999px; font-size:.7rem; font-weight:700; white-space:nowrap; }
.pill.ok { background:#e8f8ee; color:#0b7a33; } .pill.bad { background:#ffedeb; color:#c42417; }
.pill.warn { background:#fff6d9; color:#8a6400; } .pill.info { background:#e9f1ff; color:#1650c4; }
.pill.muted { background:#eef1f6; color:#475569; }
.trade-chip { display:inline-block; font-size:.68rem; font-weight:700; padding:.12rem .5rem; border-radius:6px;
  color:color-mix(in srgb, var(--c) 78%, #000); background:color-mix(in srgb, var(--c) 13%, #fff); }
.avatar { flex:0 0 auto; border-radius:11px; display:grid; place-items:center; font-weight:800; letter-spacing:-.02em;
  color:color-mix(in srgb, var(--c) 80%, #000); background:color-mix(in srgb, var(--c) 15%, #fff);
  box-shadow:inset 0 0 0 1px color-mix(in srgb, var(--c) 22%, #fff); }
.vcell { display:flex; align-items:center; gap:.7rem; }
.vcell b { font-weight:700; } .vcell .sub { color:var(--muted); font-size:.76rem; }

/* cost mix */
.mix { display:flex; width:100%; border-radius:99px; overflow:hidden; background:#eef1f6; gap:2px; }
.mix span { display:block; height:100%; min-width:3px; }
.mix-empty { background:repeating-linear-gradient(45deg,#eef1f6,#eef1f6 6px,#f7f8fb 6px,#f7f8fb 12px); }
.mix-legend { display:grid; gap:.35rem; margin-top:.75rem; }
.ml-row { display:grid; grid-template-columns:12px 1fr auto 44px; align-items:center; gap:.55rem; font-size:.84rem; }
.ml-row .dot { width:10px; height:10px; border-radius:3px; }
.ml-l { font-weight:600; } .ml-v { font-weight:700; } .ml-p { color:#94a3b8; text-align:right; font-weight:600; }

/* quick view */
.qv-top { display:flex; gap:.8rem; align-items:center; }
.qv-vendor { font-weight:700; }
.qv-title { font-size:1.08rem; font-weight:800; line-height:1.3; letter-spacing:-.015em; margin:.9rem 0 .2rem; }
.qv-price { font-size:2.2rem; font-weight:800; letter-spacing:-.04em; line-height:1.1; }
.qv-sub { color:var(--muted); font-size:.8rem; margin-bottom:1rem; }
.facts { display:grid; grid-template-columns:1fr 1fr; gap:.8rem 1rem; }
.fact-l { font-size:.66rem; text-transform:uppercase; letter-spacing:.07em; color:#94a3b8; font-weight:700; }
.fact-v { font-weight:600; font-size:.88rem; }
.section-l { font-size:.68rem; text-transform:uppercase; letter-spacing:.08em; color:var(--muted); font-weight:800; margin:1.1rem 0 .6rem; }
.line-mini { display:flex; justify-content:space-between; gap:.6rem; font-size:.84rem; padding:.42rem 0; border-bottom:1px dashed #eef1f6; }
.line-mini .d { color:#334155; } .line-mini .d small { color:#94a3b8; margin-left:.3rem; } .line-mini .a { font-weight:700; }

/* detail hero */
.hero { position:relative; overflow:hidden; background:radial-gradient(1200px 300px at 85% -40%, #1d3b78 0%, transparent 60%), linear-gradient(135deg,#0b1220 0%,#111c33 100%);
  color:#fff; border-radius:1rem; padding:1.5rem 1.6rem; margin-bottom:1rem; }
.hero::after { content:''; position:absolute; right:-6px; bottom:-20px; width:170px; height:140px; opacity:.07;
  background:url('bytebridge-mark.png') no-repeat center / contain; pointer-events:none; }
.hero .eyebrow { color:#8b97ad; font-size:.8rem; font-weight:700; display:flex; gap:.5rem; align-items:center; flex-wrap:wrap; }
.hero h4 { font-weight:800; margin:.45rem 0 .6rem; letter-spacing:-.025em; font-size:1.55rem; }
.hero .meta { display:flex; gap:1.2rem; flex-wrap:wrap; color:#b8c3d6; font-size:.84rem; }
.hero .meta b { color:#fff; font-weight:600; }
.hero .total { font-size:2.4rem; font-weight:800; letter-spacing:-.04em; line-height:1; }
.hero .total-l { color:#8b97ad; font-size:.72rem; text-transform:uppercase; letter-spacing:.08em; font-weight:700; margin-bottom:.25rem; }
.hero .form-select { background-color:rgba(255,255,255,.08); color:#fff; border-color:rgba(255,255,255,.18); }
.hero .form-select option { color:#000; }
.hero .shiny-input-container, .hero .form-group { margin-bottom:0; }
.tile { background:#fff; border:1px solid var(--line); border-radius:.85rem; padding:.9rem 1.05rem; height:100%; }
.tile .l { font-size:.68rem; text-transform:uppercase; letter-spacing:.07em; color:var(--muted); font-weight:700; }
.tile .v { font-size:1.4rem; font-weight:800; letter-spacing:-.03em; margin-top:.1rem; }
.tile .v-sm { font-size:1.02rem; line-height:1.3; margin-top:.3rem; }
.tile .s { font-size:.76rem; color:#94a3b8; font-weight:500; }

/* pricing table */
.ptable { width:100%; font-size:.86rem; }
.ptable th { font-size:.66rem; text-transform:uppercase; letter-spacing:.07em; color:#94a3b8; font-weight:800; padding:.5rem .6rem; border-bottom:1px solid var(--line); }
.ptable td { padding:.6rem .6rem; border-bottom:1px solid #f1f3f8; vertical-align:middle; }
.ptable .num { text-align:right; font-variant-numeric:tabular-nums; white-space:nowrap; }
.ptable tr.sec td { background:#f8f9fc; font-weight:800; font-size:.72rem; text-transform:uppercase; letter-spacing:.06em; color:#475569; }
.ptable tr.sub td { font-weight:700; color:#475569; }
.ptable tr.grand td { font-weight:800; font-size:1rem; border-top:2px solid var(--ink); border-bottom:0; }
.cat-dot { display:inline-block; width:8px; height:8px; border-radius:2px; margin-right:.5rem; vertical-align:middle; }

/* scope */
.callout { background:#f3f7ff; border:1px solid #dbe7ff; border-radius:.7rem; padding:.8rem 1rem; font-size:.9rem; color:#1e3a6e; }
.scope-group h6 { font-size:.72rem; text-transform:uppercase; letter-spacing:.07em; color:var(--muted); margin:1.1rem 0 .45rem; font-weight:800; }
.scope-item { display:flex; gap:.65rem; padding:.55rem .1rem; border-bottom:1px dashed #edf0f5; font-size:.87rem; line-height:1.45; }
.scope-ic { flex:0 0 22px; height:22px; border-radius:7px; display:grid; place-items:center; font-size:.72rem; font-weight:800; margin-top:1px; }
.ic-in { background:#e8f8ee; color:#0b7a33; } .ic-ex { background:#ffedeb; color:#c42417; }
.ic-cl { background:#e9f1ff; color:#1650c4; } .ic-dv { background:#f1ecff; color:#6d28d9; } .ic-as { background:#fff6d9; color:#8a6400; }
.disc { font-size:.74rem; font-weight:800; color:var(--blue); margin-top:.7rem; }
.badge-n { background:#eef1f6; color:#475569; border-radius:6px; padding:.05rem .4rem; font-size:.7rem; margin-left:.35rem; }

/* bars */
.bar-row { margin-bottom:.85rem; }
.bar-head { display:flex; justify-content:space-between; gap:.5rem; font-size:.86rem; margin-bottom:.3rem; align-items:center; }
.bar-l { font-weight:600; } .bar-v { font-weight:800; white-space:nowrap; }
.bar-track { height:8px; background:#eef1f6; border-radius:99px; position:relative; }
.bar-fill { height:100%; background:linear-gradient(90deg,var(--blue),#5b9bff); border-radius:99px; }
.bar-mark { position:absolute; top:-3px; width:3px; height:14px; background:var(--ink); border-radius:2px; }
.legend-mark { display:inline-block; width:3px; height:11px; background:var(--ink); border-radius:2px; vertical-align:middle; margin-right:.3rem; }
.delta { font-size:.68rem; font-weight:800; padding:.05rem .4rem; border-radius:5px; margin-left:.45rem; }
.delta.up { background:#ffedeb; color:#c42417; } .delta.dn { background:#e8f8ee; color:#0b7a33; } .delta.eq { background:#eef1f6; color:#475569; }

/* vendors */
.vcard { background:#fff; border:1px solid var(--line); border-radius:.95rem; padding:1.1rem; height:100%; display:flex; flex-direction:column; gap:.85rem; transition:box-shadow .15s, transform .15s; }
.vcard:hover { box-shadow:0 8px 24px rgba(15,23,42,.08); transform:translateY(-1px); }
.vcard h6 { margin:0; font-weight:800; letter-spacing:-.01em; font-size:1rem; }
.vcard .hq { color:var(--muted); font-size:.78rem; }
.vstats { display:grid; grid-template-columns:repeat(3,1fr); gap:.5rem; background:#f8f9fc; border-radius:.65rem; padding:.6rem .75rem; }
.vstats .l { font-size:.62rem; text-transform:uppercase; letter-spacing:.07em; color:#94a3b8; font-weight:800; }
.vstats .v { font-weight:800; font-size:.95rem; }
.vlinks { display:flex; flex-direction:column; gap:.25rem; font-size:.82rem; color:#334155; }
.vlinks a { color:#334155; text-decoration:none; } .vlinks a:hover { color:var(--blue); }
.vlinks .fa, .vlinks svg { width:14px; color:#94a3b8; margin-right:.25rem; }

/* add / import */
.steps { display:flex; gap:.5rem; margin-bottom:1.1rem; flex-wrap:wrap; }
.step { display:flex; align-items:center; gap:.55rem; padding:.4rem .9rem .4rem .45rem; border-radius:999px; background:#fff; border:1px solid var(--line); color:var(--muted); font-weight:700; font-size:.8rem; }
.step .n { width:24px; height:24px; border-radius:50%; background:#eef1f6; display:grid; place-items:center; font-size:.72rem; }
.step.on { border-color:var(--blue); color:#1650c4; box-shadow:0 0 0 3px rgba(38,115,252,.1); } .step.on .n { background:var(--blue); color:#fff; }
.step.done { color:#0b7a33; } .step.done .n { background:var(--green); color:#fff; }
.drop { border:2px dashed #cdd5e3; border-radius:1rem; background:linear-gradient(180deg,#fbfcff,#f4f7fd); padding:2.4rem 1.5rem 1rem; text-align:center; transition:border-color .15s, background .15s; }
.drop:hover { border-color:var(--blue); background:#f3f7ff; }
.drop .drop-mark { width:64px; margin-bottom:.9rem; }
.drop h5 { font-weight:800; letter-spacing:-.02em; }
.drop .shiny-input-container { width:100% !important; max-width:460px; margin:1.1rem auto .2rem; }
.fmt-card { border:1px solid var(--line); border-radius:.75rem; padding:.85rem .95rem; background:#fff; margin-bottom:.6rem; }
.fmt-card b { font-size:.88rem; } .fmt-card div { color:var(--muted); font-size:.8rem; margin-top:.15rem; }
.report { background:#fff; border:1px solid var(--line); border-radius:.85rem; padding:1rem 1.1rem; margin-bottom:1rem; }
.report .r-top { display:flex; justify-content:space-between; gap:1rem; flex-wrap:wrap; align-items:center; }
.chips { display:flex; flex-wrap:wrap; gap:.35rem; margin-top:.75rem; }
.chip { font-size:.72rem; font-weight:700; border-radius:7px; padding:.2rem .55rem; }
.chip.y { background:#e8f8ee; color:#0b7a33; } .chip.n { background:#f1f3f8; color:#a3adbd; }
.chip.y::before { content:'\\2713  '; } .chip.n::before { content:'\\2013  '; }
.msg { border-radius:.6rem; padding:.55rem .8rem; font-size:.84rem; margin-top:.55rem; display:flex; gap:.55rem; align-items:flex-start; }
.msg.warn { background:#fff8e1; color:#7a5800; } .msg.err { background:#ffedeb; color:#a51d12; } .msg.ok { background:#e8f8ee; color:#0b6b2e; }
.pdf-frame { width:100%; height:calc(100vh - 200px); min-height:520px; border:0; border-radius:.6rem; background:#e9edf3; }
.bslib-sidebar-layout { --_main-bg:transparent; background:transparent; }
.bslib-sidebar-layout > .main { background:transparent; }
.bslib-sidebar-layout > .sidebar { border-radius:.85rem; border:1px solid var(--line) !important; }
.bslib-sidebar-layout > .sidebar > .sidebar-content { position:sticky; top:.5rem; }
/* SharePoint inbox */
.nav-badge { display:inline-grid; place-items:center; min-width:18px; height:18px; padding:0 5px; margin-left:.35rem; border-radius:99px;
  background:var(--red); color:#fff; font-size:.66rem; font-weight:800; vertical-align:1px; }
.ib-tabs { display:flex; gap:.35rem; flex-wrap:wrap; margin-bottom:.6rem; }
.ib-tab { border:1px solid var(--line); background:#fff; color:var(--muted); border-radius:99px; padding:.25rem .75rem; font-size:.78rem; font-weight:700; cursor:pointer; }
.ib-tab.on { background:var(--navy); color:#fff; border-color:var(--navy); }
.ib-tab .c { margin-left:.35rem; opacity:.7; }
.ib-row { display:grid; grid-template-columns:36px minmax(0,1.6fr) minmax(0,1fr) auto; gap:.9rem; align-items:center; padding:.8rem .2rem; border-top:1px solid #f1f3f8; }
.ib-row:first-of-type { border-top:0; }
.ib-ic { width:36px; height:36px; border-radius:10px; display:grid; place-items:center; background:#fff1ef; color:#d93a2b; }
.ib-name a { font-weight:700; color:var(--ink); text-decoration:none; word-break:break-word; } .ib-name a:hover { color:var(--blue); }
.ib-crumbs { font-size:.75rem; color:var(--muted); margin-top:.1rem; }
.ib-crumbs span + span::before { content:'\\203A'; margin:0 .35rem; color:#c2c9d6; }
.ib-meta { font-size:.72rem; color:#94a3b8; margin-top:.15rem; }
.ib-note { font-size:.72rem; color:#8a6400; margin-top:.2rem; }
.ib-guess .tot { font-weight:800; font-size:1rem; letter-spacing:-.02em; }
.ib-act { display:flex; gap:.35rem; align-items:center; justify-content:flex-end; flex-wrap:wrap; }
.sp-setup ol { margin:.4rem 0 0; padding-left:1.1rem; color:#334155; font-size:.85rem; }
.sp-setup code { background:#f1f3f8; padding:.05rem .35rem; border-radius:4px; font-size:.8rem; color:#0f172a; }
.from-sp { display:flex; gap:.5rem; align-items:center; flex-wrap:wrap; font-size:.8rem; color:var(--muted); margin-top:.35rem; }
@media (max-width: 900px) { .ib-row { grid-template-columns:36px 1fr; } .ib-guess, .ib-act { grid-column:2; justify-content:flex-start; } }
.save-bar { position:sticky; bottom:0; z-index:5; background:rgba(244,246,251,.92); backdrop-filter:blur(6px); border-top:1px solid var(--line); padding:.8rem 0; margin-top:1rem; display:flex; justify-content:flex-end; gap:.5rem; }

.empty { text-align:center; padding:3rem 1rem; color:var(--muted); }
.empty-mark { width:54px; margin-bottom:1rem; }
.empty h5 { color:var(--ink); font-weight:800; letter-spacing:-.02em; }

/* signed-in user (Entra ID) */
.user-chip { display:flex; align-items:center; gap:.55rem; margin-left:.9rem; padding-left:.9rem; border-left:1px solid #2a3549; }
.user-chip .avatar { border-radius:50%; }
.user-who { display:flex; flex-direction:column; line-height:1.15; min-width:0; }
.user-n { color:#e2e8f0; font-weight:700; font-size:.8rem; white-space:nowrap; }
.user-e { color:#64748b; font-size:.68rem; font-weight:600; white-space:nowrap; }
.user-out { color:#8b97ad; padding:.3rem .45rem; border-radius:.45rem; }
.user-out:hover { color:#fff; background:rgba(255,255,255,.07); }
@media (max-width: 1200px) { .user-who { display:none; } }

/* DataTables */
table.dataTable thead th { font-size:.66rem; text-transform:uppercase; letter-spacing:.07em; color:#94a3b8 !important; font-weight:800; border-bottom:1px solid var(--line) !important; }
table.dataTable tbody td { border-top:1px solid #f1f3f8 !important; vertical-align:middle; padding:.65rem .6rem !important; }
table.dataTable.hover tbody tr:hover > * { box-shadow:inset 0 0 0 9999px rgba(38,115,252,.035) !important; }
table.dataTable tbody tr.selected > * { box-shadow:inset 0 0 0 9999px rgba(38,115,252,.08) !important; color:inherit !important; }
table.dataTable tbody tr.selected > td:first-child { box-shadow:inset 3px 0 0 var(--blue), inset 0 0 0 9999px rgba(38,115,252,.08) !important; }
table.dataTable tbody tr { cursor:pointer; }
.dataTables_info, .dataTables_paginate { font-size:.8rem; color:var(--muted) !important; }
.edit-dt td { font-size:.82rem; } .edit-dt tbody tr { cursor:text; }
.cmp td.best { background:#e8f8ee !important; font-weight:800; color:#0b7a33; }
.nav-underline .nav-link { font-weight:700; font-size:.86rem; }
.modal-content { border-radius:1rem; border:0; }
.shiny-notification { border-radius:.7rem; border:0; box-shadow:0 10px 30px rgba(15,23,42,.18); font-weight:600; }
"

# =============================================================================
# UI
# =============================================================================
# dark navbar: navbar_options() in bslib >= 0.9, inverse/bg before that
nav_args <- if (utils::packageVersion("bslib") >= "0.9.0") {
  list(navbar_options = bslib::navbar_options(theme = "dark", bg = BRAND$navy))
} else list(inverse = TRUE, bg = BRAND$navy)

# The page is built per signed-in user (R/auth.R decides who gets it)
ui_for <- function(user, ticket) do.call(page_navbar, c(list(
  id = "nav", theme = theme, fillable = FALSE, window_title = "ByteBridge · Service Pricing",
  title = div(class = "brand",
              img(src = "bytebridge-logo.png", alt = "ByteBridge"), span(class = "divider"),
              div(class = "app", "Service Pricing", tags$small("Trade vendor quotes"))),
  header = tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                     tags$style(HTML(css)), auth_ticket_input(ticket)),

  nav_panel("Quotes", value = "quotes", icon = icon("layer-group"),
    div(class = "wrap",
      div(class = "page-h",
        div(h3("Service quotes"), p("Every trade-vendor proposal on one scale — compare fees, labor rates and scope gaps.")),
        div(class = "d-flex gap-2 flex-wrap",
            actionButton("cmp_btn", "Compare selected", icon = icon("scale-balanced"), class = "btn-light"),
            downloadButton("dl_quotes", "Export", class = "btn-light"),
            actionButton("go_add", "Add quote", icon = icon("plus"), class = "btn-primary"))),
      uiOutput("kpis"),
      div(class = "filters",
        layout_columns(col_widths = c(4, 2, 2, 2, 2),
          textInput("f_text", "Search", placeholder = "Project, vendor, site, quote #…"),
          selectizeInput("f_trade", "Trade", choices = NULL, multiple = TRUE, options = list(placeholder = "All trades")),
          selectizeInput("f_vendor", "Vendor", choices = NULL, multiple = TRUE, options = list(placeholder = "All vendors")),
          selectizeInput("f_status", "Status", choices = c(STATUSES, "Expired"), multiple = TRUE, options = list(placeholder = "Any status")),
          selectizeInput("f_site", "Site / location", choices = NULL, multiple = TRUE, options = list(placeholder = "Anywhere")))),
      layout_columns(col_widths = c(8, 4), class = "align-items-start",
        card(card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-1",
                         span(class = "card-t", "All quotes"),
                         span(class = "card-sub", "Click a row for a quick view · Ctrl/Shift-click to compare")),
             card_body(DTOutput("quotes_tbl"))),
        uiOutput("quick_view")))),

  nav_panel("Quote detail", value = "detail", icon = icon("file-invoice-dollar"),
    div(class = "wrap",
      div(class = "page-h",
        div(h3("Quote detail"), p("Pricing, scope, rates and the original document for one proposal.")),
        div(style = "min-width:min(440px,100%)", selectizeInput("d_quote", NULL, choices = NULL, width = "100%",
                                                              options = list(placeholder = "Pick a quote…")))),
      uiOutput("detail_body"))),

  nav_panel("Benchmarks", value = "bench", icon = icon("chart-simple"),
    div(class = "wrap",
      div(class = "page-h",
        div(h3("Benchmarks"), p("How vendors price the same kind of work — hourly rates, effective labor cost and fee per kW.")),
        div(style = "min-width:min(320px,100%)", selectizeInput("b_trade", NULL, choices = NULL, multiple = TRUE, width = "100%",
                                                              options = list(placeholder = "All trades")))),
      uiOutput("bench_body"))),

  nav_panel("Vendors", value = "vendors", icon = icon("building"),
    div(class = "wrap",
      div(class = "page-h",
        div(h3("Trade vendors"), p("Who we get service quotes from and how they've priced.")),
        div(style = "min-width:min(300px,100%)", textInput("v_search", NULL, placeholder = "Search vendors…", width = "100%"))),
      uiOutput("vendor_cards"))),

  nav_panel(tagList("Add quote", uiOutput("inbox_badge", inline = TRUE)), value = "add", icon = icon("cloud-arrow-up"),
    div(class = "wrap",
      div(class = "page-h",
        div(h3("Add a service quote"), p("Drop in a vendor PDF — we read it, you check it against the original, then save."))),
      uiOutput("steps"),
      uiOutput("add_body"))),

  nav_spacer(),
  nav_item(uiOutput("db_stamp", inline = TRUE)),
  nav_item(auth_user_chip(user, avatar))
), nav_args))
ui <- auth_ui(ui_for)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # Entra ID: the page is only served to signed-in staff; this stops anyone talking to the server directly
  user <- auth_session_user(session, isolate(input$auth_tok))
  if (is.null(user)) { session$close(); return(invisible(NULL)) }
  session$userData$user <- user

  refresh <- reactiveVal(0)
  bump <- function() refresh(refresh() + 1)

  lines_all <- reactive({ refresh(); q_all("SELECT * FROM quote_lines") })
  quotes <- reactive({
    refresh()
    d <- load_quotes(); if (!nrow(d)) return(d)
    la <- lines_all()
    mixes <- lapply(d$quote_id, function(id) cost_mix(la[la$quote_id == id, , drop = FALSE]))
    names(mixes) <- d$quote_id
    d$labor_total  <- vapply(mixes, function(m) if ("Labor" %in% names(m)) m[["Labor"]] else NA_real_, 0)
    d$blended_rate <- ifelse(!is.na(d$labor_hours) & d$labor_hours > 0 & !is.na(d$labor_total),
                             d$labor_total / d$labor_hours, NA_real_)
    attr(d, "mixes") <- mixes
    d
  })
  mix_of <- function(d, qid) { m <- attr(d, "mixes")[[as.character(qid)]]; if (is.null(m)) numeric(0) else m }
  keep_mix <- function(sub, d) { attr(sub, "mixes") <- attr(d, "mixes"); sub }

  output$db_stamp <- renderUI({
    d <- quotes()
    span(class = "nav-stamp", tags$b(nrow(d)), " quotes · ", tags$b(length(unique(d$vendor))), " vendors")
  })

  # keep choice lists in sync --------------------------------------------------------------
  observe({
    d <- quotes()
    keep <- function(id) isolate(input[[id]])
    trades <- sort(unique(na.omit(d$trade)))
    updateSelectizeInput(session, "f_trade", choices = trades, selected = keep("f_trade"))
    updateSelectizeInput(session, "b_trade", choices = trades, selected = keep("b_trade"))
    updateSelectizeInput(session, "f_vendor", choices = sort(unique(d$vendor)), selected = keep("f_vendor"))
    locs <- if (nrow(d)) unlist(strsplit(na.omit(d$locations), ";\\s*")) else character()
    sites <- sort(unique(na.omit(c(d$site_operator, d$site_code, locs, d$city))))
    updateSelectizeInput(session, "f_site", choices = sites, selected = keep("f_site"))
    labs <- if (nrow(d)) setNames(d$quote_id, paste0(d$vendor, " — ", nna(d$project_name, "Untitled"),
                                                     " (#", nna(d$proposal_no, "n/a"), ")")) else character()
    sel <- keep("d_quote"); if (is.null(sel) || !sel %in% d$quote_id) sel <- if (nrow(d)) d$quote_id[1] else NULL
    updateSelectizeInput(session, "d_quote", choices = labs, selected = sel)
  })

  filtered <- reactive({
    d <- quotes(); if (!nrow(d)) return(d)
    keep <- rep(TRUE, nrow(d))
    t <- tolower(trimws(input$f_text %||% ""))
    if (nzchar(t)) {
      hay <- tolower(paste(d$vendor, d$project_name, d$proposal_no, d$site_operator, d$site_code, d$locations,
                           d$city, d$state, d$data_hall, d$trade, d$client_contact, d$scope_summary))
      keep <- keep & grepl(t, hay, fixed = TRUE)
    }
    if (length(input$f_trade))  keep <- keep & d$trade %in% input$f_trade
    if (length(input$f_vendor)) keep <- keep & d$vendor %in% input$f_vendor
    if (length(input$f_status)) keep <- keep & d$status_eff %in% input$f_status
    if (length(input$f_site)) {
      hit <- vapply(seq_len(nrow(d)), function(i) any(input$f_site %in% c(d$site_operator[i], d$site_code[i], d$city[i],
                                                                     strsplit(nna(d$locations[i]), ";\\s*")[[1]])), TRUE)
      keep <- keep & hit
    }
    keep_mix(d[keep, , drop = FALSE], d)
  })

  # KPIs ---------------------------------------------------------------------------------------
  output$kpis <- renderUI({
    d <- filtered()
    mix <- sum_mixes(lapply(d$quote_id, function(id) mix_of(d, id)))
    open_ <- d[d$status_eff %in% c("Received", "Under review"), , drop = FALSE]
    top <- if (length(mix)) names(mix)[which.max(mix)] else NULL
    layout_columns(col_widths = c(3, 3, 3, 3),
      kpi_tile(BRAND$red, "Quotes on file", nrow(d),
               sprintf("%d vendors · %d trades", length(unique(d$vendor)), length(unique(na.omit(d$trade))))),
      kpi_tile(BRAND$yellow, "Total quoted", fmt_compact(sum(d$base_total, na.rm = TRUE)),
               paste("+", fmt_compact(sum(d$reimbursables, na.rm = TRUE)), "est. reimbursables")),
      kpi_tile(BRAND$green, "Where the money goes",
               if (is.null(top)) "—" else sprintf("%.0f%% %s", 100 * max(mix) / sum(mix), tolower(top)),
               if (length(mix) > 1) paste(names(mix)[-which.max(mix)], collapse = " · ") else NULL,
               extra = mix_bar(mix, 8)),
      kpi_tile(BRAND$blue, "Open quotes", nrow(open_),
               sprintf("%d expire within 7 days", sum(!is.na(open_$days_left) & open_$days_left >= 0 & open_$days_left <= 7))))
  })

  # Quotes table ---------------------------------------------------------------------------------
  output$quotes_tbl <- renderDT({
    d <- filtered()
    if (!nrow(d)) return(datatable(data.frame(x = "No quotes match — clear the filters or add a quote."),
                                   rownames = FALSE, colnames = "", options = list(dom = "t", ordering = FALSE), selection = "none"))
    km <- vapply(seq_len(nrow(d)), function(i) {
      k <- key_metric(d[i, ])
      if (k$v == "—") "<span class='text-muted'>—</span>" else
        sprintf("<b>%s</b><div class='text-muted small'>%s</div>", k$v, if (grepl("kW", k$v)) "fee / kW" else "labor / hr")
    }, "")
    tbl <- data.frame(
      Vendor  = vapply(seq_len(nrow(d)), function(i) as.character(div(class = "vcell", avatar(d$vendor[i], d$trade[i], 34),
                  div(tags$b(d$vendor[i]), div(class = "sub", nna(d$trade[i], "Trade not set"))))), ""),
      Project = sprintf("<div style='max-width:300px'><span style='font-weight:600'>%s</span><div class='text-muted small'>#%s · %s</div></div>",
                        htmlEscape(nna(d$project_name, "Untitled")), htmlEscape(nna(d$proposal_no, "n/a")), htmlEscape(nna(d$where, "—"))),
      Mix     = vapply(d$quote_id, function(id) as.character(div(style = "min-width:90px", mix_bar(mix_of(d, id), 8))), ""),
      Total   = d$base_total,
      Unit    = km,
      Date    = d$quote_date,
      Status  = vapply(d$status_eff, function(s) as.character(status_pill(s)), ""),
      stringsAsFactors = FALSE)
    datatable(tbl, escape = FALSE, rownames = FALSE, selection = "multiple", class = "hover",
              colnames = c("Vendor", "Project", "Cost mix", "Total", "Unit metric", "Date", "Status"),
              options = list(pageLength = 12, dom = "tip", order = list(list(5, "desc")),
                             columnDefs = list(list(className = "dt-right", targets = c(3, 4)),
                                               list(orderable = FALSE, targets = c(2, 4))))) |>
      formatCurrency("Total", digits = 0) |>
      formatDate("Date", method = "toLocaleDateString", params = list("en-US", list(month = "short", day = "numeric", year = "numeric")))
  })

  sel_quotes <- reactive({
    d <- filtered(); s <- input$quotes_tbl_rows_selected
    if (!length(s) || !nrow(d)) return(d[0, , drop = FALSE])
    keep_mix(d[s[s <= nrow(d)], , drop = FALSE], d)
  })

  output$quick_view <- renderUI({
    s <- sel_quotes()
    if (!nrow(s)) {
      if (!nrow(quotes())) return(card(card_body(empty_state("No quotes yet", "Import your first vendor proposal to start building pricing history.",
                                                             actionButton("go_add3", "Add a quote", class = "btn-primary")))))
      return(card(card_body(empty_state("Select a quote", "Click any row to see its total, cost mix and fee lines here."))))
    }
    r <- s[nrow(s), ]; ch <- quote_children(r$quote_id); mix <- mix_of(s, r$quote_id)
    base <- ch$lines[ch$lines$line_type == "base", , drop = FALSE]
    km <- key_metric(r)
    card(card_body(fillable = FALSE,
      div(class = "qv-top", avatar(r$vendor, r$trade, 42),
          div(class = "flex-grow-1", style = "min-width:0", div(class = "qv-vendor", r$vendor), trade_chip(r$trade)),
          status_pill(r$status_eff)),
      div(class = "qv-title", nna(r$project_name, "Untitled project")),
      div(class = "qv-price", fmt_money(r$base_total)),
      div(class = "qv-sub", paste(c(nna(r$fee_basis, "Total quoted"),
                                    if (!is.na(r$reimbursables)) paste("+", fmt_money(r$reimbursables), "reimbursables")), collapse = " · ")),
      mix_bar(mix, 12), mix_legend(mix),
      section_title("At a glance"),
      div(class = "facts",
          fact(km$l, km$v), fact("Where", nna(r$where, "—")),
          fact("Quote date", fmt_date(r$quote_date)), fact("Validity", expiry_pill(r$days_left)),
          fact("Scope items", r$n_scope), fact("Exclusions", r$n_excl)),
      section_title(sprintf("Fee lines (%d)", nrow(base))),
      lapply(head(seq_len(nrow(base)), 6), function(i) div(class = "line-mini",
        span(class = "d", base$description[i], if (!is.na(base$section[i])) tags$small(base$section[i])),
        span(class = "a", fmt_money(base$amount[i])))),
      if (nrow(base) > 6) div(class = "text-muted small mt-1", sprintf("+ %d more", nrow(base) - 6)),
      actionButton("qv_open", "Open full detail", icon = icon("arrow-right"), class = "btn-primary w-100 mt-3"),
      if (nrow(s) > 1) div(class = "text-muted small text-center mt-2", sprintf("%d selected — use Compare selected", nrow(s)))
    ))
  })
  go_detail <- function(qid) { updateSelectizeInput(session, "d_quote", selected = qid); nav_select("nav", "detail") }
  observeEvent(input$qv_open, { s <- sel_quotes(); if (nrow(s)) go_detail(s$quote_id[nrow(s)]) })
  observeEvent(input$go_add,  nav_select("nav", "add"))
  observeEvent(input$go_add2, nav_select("nav", "add"))
  observeEvent(input$go_add3, nav_select("nav", "add"))

  output$dl_quotes <- downloadHandler(
    filename = function() paste0("service_quotes_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- filtered()
      cols <- intersect(c("vendor", "trade", "proposal_no", "quote_date", "status_eff", "project_name", "site_operator",
                          "site_code", "locations", "city", "state", "it_load_kw", "labor_hours", "base_total",
                          "reimbursables", "fee_per_kw", "blended_rate", "fee_basis", "expires_on"), names(d))
      write.csv(d[, cols, drop = FALSE], file, row.names = FALSE, na = "")
    })

  # Compare (bid leveling) ---------------------------------------------------------------------
  observeEvent(input$cmp_btn, {
    s <- sel_quotes()
    if (nrow(s) < 2) { showNotification("Select two or more quotes in the table (Ctrl/Shift-click) to compare.", type = "warning"); return() }
    ids <- s$quote_id; idl <- paste(as.integer(ids), collapse = ",")
    lines <- q_all(sprintf("SELECT quote_id, line_type, section, description, amount FROM quote_lines WHERE quote_id IN (%s)", idl))
    excl  <- q_all(sprintf("SELECT quote_id, COUNT(*) AS n FROM scope_items WHERE kind = 'exclusion' AND quote_id IN (%s) GROUP BY quote_id", idl))
    mixes <- lapply(ids, function(id) mix_of(s, id))
    cats  <- names(MIX_COLORS)[names(MIX_COLORS) %in% unique(unlist(lapply(mixes, names)))]
    row <- function(label, vals, fmt, lower_better = TRUE, dot = NULL) {
      ok <- vals[!is.na(vals)]
      best <- if (length(ok) < 2) NA else if (lower_better) min(ok) else max(ok)
      tags$tr(tags$td(if (!is.null(dot)) span(class = "cat-dot", style = sprintf("background:%s", dot)), tags$b(label)),
              lapply(vals, function(v) tags$td(class = if (!is.na(v) && !is.na(best) && v == best) "best num" else "num", fmt(v))))
    }
    b <- lines[lines$line_type == "base", , drop = FALSE]
    keys <- unique(paste(nna(b$section), b$description, sep = "|"))
    showModal(modalDialog(
      title = "Bid leveling", size = "xl", easyClose = TRUE,
      tags$table(class = "ptable cmp",
        tags$thead(tags$tr(tags$th(""), lapply(seq_along(ids), function(i)
          tags$th(class = "num", div(class = "vcell justify-content-end", avatar(s$vendor[i], s$trade[i], 28),
                                     div(style = "text-align:left;text-transform:none;letter-spacing:0;color:#0f172a;font-size:.82rem",
                                         s$vendor[i], div(class = "text-muted small", paste0("#", nna(s$proposal_no[i], "n/a"))))))))),
        tags$tbody(
          row("Total", s$base_total, fmt_money),
          row("Reimbursables", s$reimbursables, fmt_money),
          row("Fee per kW", s$fee_per_kw, fmt_money),
          row("Effective labor $/hr", s$blended_rate, fmt_money),
          row("Labor hours quoted", s$labor_hours, function(v) fmt_num(v), lower_better = FALSE),
          row("Exclusions", vapply(ids, function(i) { n <- excl$n[excl$quote_id == i]; if (length(n)) n else 0 }, numeric(1)), function(v) fmt_num(v)),
          tags$tr(class = "sec", tags$td(colspan = length(ids) + 1, "Cost mix")),
          lapply(cats, function(k) row(k, vapply(mixes, function(m) if (k %in% names(m)) m[[k]] else NA_real_, 0), fmt_money, dot = mix_color(k))),
          tags$tr(class = "sec", tags$td(colspan = length(ids) + 1, "Line items")),
          lapply(keys, function(kk) {
            parts <- strsplit(kk, "|", fixed = TRUE)[[1]]; sec <- parts[1]; dsc <- paste(parts[-1], collapse = "|")
            row(paste(c(if (nzchar(sec)) sec, dsc), collapse = " · "),
                vapply(ids, function(i) { a <- b$amount[b$quote_id == i & nna(b$section) == sec & b$description == dsc]
                                          if (length(a)) sum(a) else NA_real_ }, numeric(1)), fmt_money)
          }))),
      div(class = "text-muted small mt-2", "Green = lowest cost (or most hours). Line items are matched by section and description, so differently named phases won't line up."),
      footer = modalButton("Close")))
  })

  # Detail -------------------------------------------------------------------------------------------
  market_rates <- reactive({ refresh(); q_all("SELECT vendor_id, role_std, rate FROM rate_cards WHERE rate IS NOT NULL") })

  output$detail_body <- renderUI({
    d <- quotes()
    if (!nrow(d)) return(card(card_body(empty_state("No quotes yet", "Import a vendor proposal to get started.",
                                                    actionButton("go_add2", "Add a quote", class = "btn-primary")))))
    qid <- suppressWarnings(as.integer(input$d_quote)); r <- d[d$quote_id %in% qid, , drop = FALSE]
    if (!nrow(r)) return(NULL)
    ch <- quote_children(qid); mix <- mix_of(d, qid); km <- key_metric(r)
    L <- ch$lines
    base  <- L[L$line_type == "base", , drop = FALSE]
    other <- L[L$line_type != "base", , drop = FALSE]
    has_split <- any(!is.na(base$material) | !is.na(base$labor))
    n_racks <- if (nrow(ch$equip)) sum(ch$equip$qty[grepl("rack", ch$equip$description, ignore.case = TRUE)], na.rm = TRUE) else 0
    ncol_p <- if (has_split) 6 else 4

    # pricing table grouped by section
    sec <- nna(base$section)
    groups <- split(seq_len(nrow(base)), factor(sec, levels = unique(sec)))
    num_td <- function(x) tags$td(class = "num", fmt_money(x))
    ptable <- tags$table(class = "ptable",
      tags$thead(tags$tr(tags$th("Item"), tags$th(class = "num", "Qty"),
                         if (has_split) tagList(tags$th(class = "num", "Material"), tags$th(class = "num", "Labor")),
                         tags$th(class = "num", "Amount"), tags$th(class = "num", "Share"))),
      tags$tbody(
        lapply(seq_along(groups), function(k) {
          g <- names(groups)[k]; idx <- groups[[k]]
          tagList(
            if (nzchar(g)) tags$tr(class = "sec", tags$td(colspan = ncol_p, icon("location-dot"), " ", g)),
            lapply(idx, function(i) tags$tr(
              tags$td(span(class = "cat-dot", style = sprintf("background:%s", mix_color(base$category[i]))), base$description[i],
                      if (!is.na(base$note[i]) && nzchar(base$note[i])) span(class = "text-muted small ms-1", paste0("(", base$note[i], ")"))),
              tags$td(class = "num", paste(fmt_num(base$qty[i], if (isTRUE(base$qty[i] %% 1 != 0)) 1 else 0), nna(base$unit[i]))),
              if (has_split) tagList(num_td(base$material[i]), num_td(base$labor[i])),
              num_td(base$amount[i]),
              tags$td(class = "num text-muted", if (isTRUE(r$base_total > 0)) sprintf("%.0f%%", 100 * base$amount[i] / r$base_total) else ""))),
            if (nzchar(g) && length(groups) > 1) tags$tr(class = "sub",
              tags$td(paste(g, "subtotal")), tags$td(),
              if (has_split) tagList(num_td(sum(base$material[idx], na.rm = TRUE)), num_td(sum(base$labor[idx], na.rm = TRUE))),
              num_td(sum(base$amount[idx], na.rm = TRUE)), tags$td()))
        }),
        tags$tr(class = "grand", tags$td("Total"), tags$td(),
                if (has_split) tagList(num_td(sum(base$material, na.rm = TRUE)), num_td(sum(base$labor, na.rm = TRUE))),
                num_td(r$base_total), tags$td())))

    scope_block <- function(kind) {
      it <- ch$scope[ch$scope$kind == kind, , drop = FALSE]
      if (!nrow(it)) return(NULL)
      ic <- switch(kind, inclusion = c("ic-in", "✓"), exclusion = c("ic-ex", "✕"), client = c("ic-cl", "◆"),
                   deliverable = c("ic-dv", "▸"), assumption = c("ic-as", "!"), c("ic-dv", "•"))
      disc <- ifelse(is.na(it$discipline) | it$discipline == "", " ", it$discipline)
      groups <- split(seq_len(nrow(it)), factor(disc, levels = unique(disc)))
      div(class = "scope-group",
          h6(SCOPE_LABELS[[kind]], span(class = "badge-n", nrow(it))),
          lapply(seq_along(groups), function(k) tagList(
            if (names(groups)[k] != " " && length(groups) > 1) div(class = "disc", names(groups)[k]),
            lapply(groups[[k]], function(i) div(class = "scope-item", div(class = paste("scope-ic", ic[1]), ic[2]), div(it$text[i]))))))
    }

    # rate card vs market
    mk <- market_rates(); vid <- q_all("SELECT vendor_id FROM quotes WHERE quote_id = ?", list(qid))$vendor_id
    rr <- ch$rates
    rate_ui <- if (!nrow(rr)) empty_state("No hourly rates on this quote", "Rates are captured when a proposal includes an hourly rate schedule.") else {
      mx <- max(c(rr$rate, mk$rate, 1), na.rm = TRUE)
      tagList(
        div(class = "d-flex justify-content-between align-items-center flex-wrap",
            section_title(paste("Additional-service rates", if (!is.na(rr$rate_year[1])) paste0("· ", rr$rate_year[1]) else "")),
            span(class = "card-sub", span(class = "legend-mark"), "market median from other vendors")),
        lapply(seq_len(nrow(rr)), function(i) {
          others <- mk$rate[mk$role_std == rr$role_std[i] & mk$vendor_id != vid]
          med <- if (length(others)) stats::median(others) else NA
          dl <- if (!is.na(med) && !is.na(rr$rate[i])) {
            p <- 100 * (rr$rate[i] - med) / med
            span(class = paste("delta", if (p > 2) "up" else if (p < -2) "dn" else "eq"), sprintf("%+.0f%% vs market", p))
          }
          div(class = "bar-row",
              div(class = "bar-head", span(class = "bar-l", rr$role[i], dl),
                  span(class = "bar-v", if (is.na(rr$rate[i])) rr$unit[i] else paste0(fmt_money(rr$rate[i], 0), "/hr"))),
              if (!is.na(rr$rate[i])) div(class = "bar-track",
                div(class = "bar-fill", style = sprintf("width:%.1f%%", 100 * rr$rate[i] / mx)),
                if (!is.na(med)) div(class = "bar-mark", style = sprintf("left:%.1f%%", 100 * med / mx))))
        }))
    }

    pdf_ok <- !is.na(r$source_file) && file.exists(file.path(FILES_DIR, r$source_file))
    pdf_url <- if (pdf_ok) paste0("qfiles/", utils::URLencode(r$source_file)) else NULL
    tile <- function(l, v, s = NULL) div(class = "tile", div(class = "l", l),
                                         div(class = if (nchar(v) > 14) "v v-sm" else "v", v), if (!is.null(s)) div(class = "s", s))
    n_opt <- sum(other$line_type == "optional")

    tagList(
      div(class = "hero",
        div(class = "d-flex justify-content-between flex-wrap gap-3 position-relative", style = "z-index:1",
          div(style = "min-width:0;flex:1 1 420px",
            div(class = "eyebrow", avatar(r$vendor, r$trade, 30), span(r$vendor), span("·"), span(nna(r$trade, "Trade not set"))),
            h4(nna(r$project_name, "Untitled project")),
            div(class = "meta",
                span("Quote ", tags$b(paste0("#", nna(r$proposal_no, "n/a")))),
                span("Dated ", tags$b(fmt_date(r$quote_date))),
                if (nzchar(nna(r$where))) span(icon("location-dot"), " ", tags$b(r$where)),
                if (!is.na(r$client_contact)) span("Attn ", tags$b(r$client_contact)),
                if (!is.na(r$sp_web_url)) tags$a(href = r$sp_web_url, target = "_blank", style = "color:#9cc0ff;text-decoration:none",
                                                 icon("folder-open"), " Open in SharePoint"))),
          div(class = "text-end",
            div(class = "total-l", nna(r$fee_basis, "Total quoted")), div(class = "total", fmt_money(r$base_total)),
            div(class = "mt-2 d-flex gap-1 justify-content-end align-items-center", status_pill(r$status_eff), expiry_pill(r$days_left)),
            div(class = "mt-2 d-flex gap-1 justify-content-end align-items-center",
                selectInput("d_status", NULL, STATUSES, selected = r$status, width = "150px", selectize = FALSE),
                actionButton("d_status_save", "Set", class = "btn-light btn-sm"))))),
      layout_columns(col_widths = c(3, 3, 3, 3), class = "mb-3",
        tile(km$l, km$v, if (!is.na(r$fee_per_kw)) paste(fmt_num(r$it_load_kw, 0, " kW"), "IT load") else
                           if (!is.na(r$blended_rate)) paste(fmt_money(r$labor_total), "labor ÷", fmt_num(r$labor_hours), "hrs") else "Add IT load or labor hours to compare"),
        tile("Reimbursables", fmt_money(r$reimbursables),
             if (n_opt) sprintf("%d optional service%s priced separately", n_opt, if (n_opt > 1) "s" else "") else "Not included in the total"),
        tile("Scope", sprintf("%d items", r$n_scope), sprintf("%d exclusions · %d hourly rates", r$n_excl, r$n_rates)),
        tile("Billing", nna(r$billing_terms, "—"),
             if (!is.na(r$valid_days)) paste("Valid", r$valid_days, "days · to", fmt_date(r$expires_on)) else "No validity period stated")),
      navset_card_underline(
        nav_panel("Pricing", icon = icon("dollar-sign"),
          layout_columns(col_widths = c(8, 4),
            div(ptable,
                if (nrow(other)) tagList(section_title("Not in the total"),
                  tags$table(class = "ptable", tags$tbody(lapply(seq_len(nrow(other)), function(i) tags$tr(
                    tags$td(pill(other$line_type[i], "muted"), " ", other$description[i],
                            if (!is.na(other$note[i]) && nzchar(other$note[i])) span(class = "text-muted small ms-1", other$note[i])),
                    tags$td(class = "num", tags$b(fmt_money(other$amount[i])),
                            span(class = "text-muted small", paste0(" / ", nna(other$unit[i], "ls")))))))))),
            div(section_title("Cost mix"), mix_bar(mix, 14), mix_legend(mix),
                if (!is.na(r$notes) && nzchar(r$notes)) tagList(section_title("Fee notes"),
                  div(class = "small text-secondary", style = "white-space:pre-wrap;line-height:1.5", r$notes))))),
        nav_panel(sprintf("Scope (%d)", nrow(ch$scope)), icon = icon("list-check"),
          if (!is.na(r$scope_summary) && nzchar(r$scope_summary)) div(class = "callout mb-2", r$scope_summary),
          if (!nrow(ch$scope)) empty_state("No scope captured", "Scope items are read from Scope / Exclusions / Assumptions sections.") else
          layout_columns(col_widths = c(6, 6),
            div(scope_block("deliverable"), scope_block("inclusion")),
            div(scope_block("exclusion"), scope_block("assumption"), scope_block("client")))),
        nav_panel(sprintf("Rates (%d)", nrow(rr)), icon = icon("clock"), rate_ui),
        nav_panel("Project basis", icon = icon("server"),
          layout_columns(col_widths = c(6, 6),
            div(section_title("Site & design basis"),
                div(class = "facts",
                    fact("Operator", or_dash(r$site_operator)), fact("Site code", or_dash(r$site_code)),
                    fact("Address", or_dash(paste(na.omit(c(r$address, r$city, r$state)), collapse = ", "))),
                    fact("Locations", or_dash(r$locations)),
                    fact("Data hall", or_dash(r$data_hall)), fact("IT load", fmt_num(r$it_load_kw, 0, " kW")),
                    fact("Redundancy", or_dash(r$redundancy)), fact("Cooling", or_dash(r$cooling)),
                    fact("Delivery method", or_dash(r$delivery_method)), fact("Labor hours", fmt_num(r$labor_hours)),
                    fact("Design duration", fmt_num(r$design_months, 1, " mo")), fact("Construction duration", fmt_num(r$construction_months, 1, " mo")))),
            div(section_title("Equipment basis"),
                if (!nrow(ch$equip)) p(class = "text-muted", "No equipment listed on this quote.") else tagList(
                  tags$table(class = "ptable",
                    tags$thead(tags$tr(tags$th("Qty"), tags$th("Equipment"), tags$th(class = "num", "kW each"), tags$th(class = "num", "Total kW"))),
                    tags$tbody(lapply(seq_len(nrow(ch$equip)), function(i) tags$tr(
                      tags$td(ch$equip$qty[i]), tags$td(ch$equip$description[i]),
                      tags$td(class = "num", fmt_num(ch$equip$kw_each[i])), tags$td(class = "num", fmt_num(ch$equip$qty[i] * ch$equip$kw_each[i])))))),
                  if (n_racks > 0 && !is.na(r$base_total))
                    div(class = "callout mt-3", sprintf("≈ %s per rack across %d racks", fmt_money(r$base_total / n_racks, 0), as.integer(n_racks))))))),
        nav_panel("Source PDF", icon = icon("file-pdf"),
          if (pdf_ok) tagList(
            div(class = "d-flex gap-2 mb-2", downloadButton("d_pdf", "Download original", class = "btn-light btn-sm"),
                if (!is.na(r$sp_web_url)) tags$a(class = "btn btn-light btn-sm", href = r$sp_web_url, target = "_blank", icon("folder-open"), " Open in SharePoint"),
                tags$a(class = "btn btn-light btn-sm", href = pdf_url, target = "_blank", icon("up-right-from-square"), " Open in new tab")),
            tags$iframe(class = "pdf-frame", src = paste0(pdf_url, "#view=FitH")))
          else empty_state("No original file", "This quote was entered by hand or its file was removed.")),
        nav_panel("Manage", icon = icon("gear"),
          div(class = "d-flex gap-2 align-items-center flex-wrap",
              actionButton("d_delete", "Delete this quote", icon = icon("trash"), class = "btn-outline-danger"),
              span(class = "text-muted small", "Removes the quote with its lines, scope, rates and stored PDF.")))
      ))
  })

  output$d_pdf <- downloadHandler(
    filename = function() { d <- quotes(); sub("^\\d+_", "", d$source_file[d$quote_id == as.integer(input$d_quote)]) },
    content  = function(file) { d <- quotes(); file.copy(file.path(FILES_DIR, d$source_file[d$quote_id == as.integer(input$d_quote)]), file) })

  observeEvent(input$d_status_save, {
    q_exec("UPDATE quotes SET status = ?, updated_at = CURRENT_TIMESTAMP WHERE quote_id = ?", list(input$d_status, as.integer(input$d_quote)))
    showNotification(paste("Status set to", input$d_status), type = "message"); bump()
  })
  observeEvent(input$d_delete, {
    showModal(modalDialog(title = "Delete this quote?", "This removes the quote, its fee lines, scope, rate card and stored PDF. It can't be undone.",
                          footer = tagList(modalButton("Cancel"), actionButton("d_delete_ok", "Delete", class = "btn-danger"))))
  })
  observeEvent(input$d_delete_ok, {
    delete_quote(as.integer(input$d_quote)); removeModal()
    showNotification("Quote deleted.", type = "message"); bump()
  })

  # Benchmarks -------------------------------------------------------------------------------------------
  bq <- reactive({
    d <- quotes()
    if (length(input$b_trade)) d <- keep_mix(d[d$trade %in% input$b_trade, , drop = FALSE], d)
    d
  })
  rates_data <- reactive({
    refresh()
    r <- q_all("SELECT r.role_std, r.rate, v.name AS vendor, v.trade FROM rate_cards r JOIN vendors v ON v.vendor_id = r.vendor_id WHERE r.rate IS NOT NULL")
    if (length(input$b_trade)) r <- r[r$trade %in% input$b_trade, , drop = FALSE]
    r
  })
  gg_theme <- function() theme_minimal(base_size = 12.5) + theme(
    panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "#eef1f6"), legend.position = "top", legend.justification = "left",
    legend.title = element_blank(), axis.text.y = element_text(colour = "#0f172a", face = "bold"),
    axis.text.x = element_text(colour = "#94a3b8"), axis.title = element_text(colour = "#64748b", size = 10.5),
    plot.margin = margin(4, 12, 4, 4))
  dollar_lab <- function(x) paste0("$", formatC(x, format = "f", big.mark = ",", digits = 0))

  output$bench_body <- renderUI({
    d <- bq(); r <- rates_data()
    if (!nrow(d)) return(card(card_body(empty_state("Nothing to benchmark yet", "Benchmarks fill in as you add quotes."))))
    n_roles <- length(unique(r$role_std))
    tagList(
      layout_columns(col_widths = c(6, 6),
        card(card_header(span(class = "card-t", "Quoted value by vendor"), div(class = "card-sub", "Stacked by cost type")),
             card_body(plotOutput("p_vendor", height = paste0(max(200, 46 * length(unique(d$vendor)) + 80), "px")))),
        card(card_header(span(class = "card-t", "Unit economics by quote"),
                         div(class = "card-sub", "Effective labor $/hr (labor ÷ quoted hours) or fee per kW of IT load")),
             card_body(uiOutput("unit_cards")))),
      card(card_header(span(class = "card-t", "Hourly rate cards by role"),
                       div(class = "card-sub", "Each dot is one vendor; the band spans low–high and the label is the median")),
           card_body(if (!nrow(r)) empty_state("No rate cards yet", "Rates are captured from proposals that include an hourly rate schedule.")
                     else plotOutput("p_rates", height = paste0(max(240, 34 * n_roles + 80), "px")))))
  })
  output$p_vendor <- renderPlot({
    d <- bq(); req(nrow(d))
    rows <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
      m <- mix_of(d, d$quote_id[i]); if (!length(m)) return(NULL)
      data.frame(vendor = d$vendor[i], cat = names(m), amt = as.numeric(m))
    }))
    req(!is.null(rows), nrow(rows) > 0)
    agg <- aggregate(amt ~ vendor + cat, rows, sum)
    ord <- names(sort(tapply(agg$amt, agg$vendor, sum)))
    agg$vendor <- factor(agg$vendor, ord); agg$cat <- factor(agg$cat, rev(names(MIX_COLORS)))
    ggplot(agg, aes(x = amt, y = vendor, fill = cat)) + geom_col(width = 0.62) +
      scale_fill_manual(values = MIX_COLORS, breaks = names(MIX_COLORS)) +
      scale_x_continuous(labels = dollar_lab, expand = expansion(mult = c(0, 0.06))) +
      labs(x = NULL, y = NULL) + gg_theme()
  }, res = 96)
  output$unit_cards <- renderUI({
    d <- bq()
    u <- d[!is.na(d$blended_rate) | !is.na(d$fee_per_kw), , drop = FALSE]
    if (!nrow(u)) return(empty_state("No unit metrics yet", "Add labor hours or IT load (kW) to a quote to compare on a per-unit basis."))
    mxr <- max(c(u$blended_rate, 1), na.rm = TRUE); mxk <- max(c(u$fee_per_kw, 1), na.rm = TRUE)
    tagList(lapply(seq_len(nrow(u)), function(i) {
      isr <- !is.na(u$blended_rate[i]); v <- if (isr) u$blended_rate[i] else u$fee_per_kw[i]
      div(class = "bar-row",
          div(class = "bar-head",
              span(class = "bar-l d-flex align-items-center gap-2", avatar(u$vendor[i], u$trade[i], 26),
                   span(u$vendor[i], div(class = "text-muted small fw-normal", nna(u$project_name[i])))),
              span(class = "bar-v", paste0(fmt_money(v, 0), if (isr) "/hr" else "/kW"))),
          div(class = "bar-track", div(class = "bar-fill", style = sprintf("width:%.1f%%;background:%s",
                                                                          100 * v / (if (isr) mxr else mxk), if (isr) BRAND$blue else BRAND$green))))
    }))
  })
  output$p_rates <- renderPlot({
    r <- rates_data(); req(nrow(r))
    lat <- aggregate(rate ~ role_std + vendor, r, max)
    sp  <- split(lat$rate, lat$role_std)
    rng <- data.frame(role_std = names(sp), lo = vapply(sp, min, 0), hi = vapply(sp, max, 0), med = vapply(sp, stats::median, 0))
    lvl <- rng$role_std[order(rng$med)]
    lat$role_std <- factor(lat$role_std, lvl); rng$role_std <- factor(rng$role_std, lvl)
    ggplot() +
      geom_segment(data = rng, aes(x = lo, xend = hi, y = role_std, yend = role_std), linewidth = 7, colour = "#e3ecff", lineend = "round") +
      geom_point(data = lat, aes(x = rate, y = role_std, colour = vendor), size = 3.6) +
      geom_text(data = rng, aes(x = hi, y = role_std, label = dollar_lab(med)), hjust = -0.55, size = 3.4, colour = "#64748b", fontface = "bold") +
      scale_colour_manual(values = rep(c(BRAND$blue, BRAND$green, BRAND$red, "#e0a800", "#8b5cf6", "#0ea5e9", "#f97316"), 5)) +
      scale_x_continuous(labels = dollar_lab, expand = expansion(mult = c(0.04, 0.14))) +
      labs(x = "Hourly rate", y = NULL) + gg_theme()
  }, res = 96)

  # Vendors -----------------------------------------------------------------------------------------------
  vendors_df <- reactive({
    refresh()
    q_all("SELECT v.*, COUNT(q.quote_id) AS quotes, SUM(q.base_total) AS quoted, MAX(q.quote_date) AS latest,
           SUM(CASE WHEN q.status = 'Awarded' THEN 1 ELSE 0 END) AS awarded
           FROM vendors v LEFT JOIN quotes q ON q.vendor_id = v.vendor_id GROUP BY v.vendor_id ORDER BY v.name")
  })
  output$vendor_cards <- renderUI({
    v <- vendors_df()
    t <- tolower(trimws(input$v_search %||% ""))
    if (nzchar(t) && nrow(v)) v <- v[grepl(t, tolower(paste(v$name, v$trade, v$contact, v$hq, v$email)), fixed = TRUE), , drop = FALSE]
    if (!nrow(v)) return(card(card_body(empty_state(if (nzchar(t)) "No vendors match" else "No vendors yet",
                                                    "Vendors are created automatically when you import a quote."))))
    cards <- lapply(seq_len(nrow(v)), function(i) div(class = "vcard",
      div(class = "d-flex gap-3 align-items-start",
          avatar(v$name[i], v$trade[i], 46),
          div(class = "flex-grow-1", style = "min-width:0", h6(v$name[i]),
              div(class = "d-flex gap-2 align-items-center flex-wrap mt-1",
                  trade_chip(v$trade[i]), if (!is.na(v$hq[i])) span(class = "hq", icon("location-dot"), " ", v$hq[i]))),
          tags$button(class = "btn btn-sm btn-light", title = "Edit vendor", type = "button",
                      onclick = sprintf("Shiny.setInputValue('vendor_edit', %d, {priority: 'event'})", as.integer(v$vendor_id[i])), icon("pen"))),
      div(class = "vstats",
          div(div(class = "l", "Quotes"), div(class = "v", v$quotes[i])),
          div(div(class = "l", "Quoted"), div(class = "v", fmt_compact(v$quoted[i]))),
          div(div(class = "l", "Awarded"), div(class = "v", na0(v$awarded[i])))),
      div(class = "vlinks",
          if (!is.na(v$contact[i])) span(icon("user"), v$contact[i]),
          if (!is.na(v$email[i])) tags$a(href = paste0("mailto:", v$email[i]), icon("envelope"), v$email[i]),
          if (!is.na(v$phone[i])) tags$a(href = paste0("tel:", gsub("[^0-9+]", "", v$phone[i])), icon("phone"), v$phone[i]),
          if (!is.na(v$website[i])) tags$a(href = paste0("https://", sub("^https?://", "", v$website[i])), target = "_blank", icon("globe"), v$website[i])),
      div(class = "text-muted small mt-auto", if (!is.na(v$latest[i])) paste("Latest quote", fmt_date(v$latest[i])) else "No quotes yet")))
    do.call(layout_column_wrap, c(list(width = "320px", fill = FALSE, heights_equal = "row"), cards))
  })
  observeEvent(input$vendor_edit, {
    v <- q_all("SELECT * FROM vendors WHERE vendor_id = ?", list(as.integer(input$vendor_edit)))
    req(nrow(v)); session$userData$vendor_edit <- v$vendor_id
    showModal(modalDialog(title = div(class = "vcell", avatar(v$name, v$trade, 34), paste("Edit", v$name)), easyClose = TRUE,
      textInput("ve_name", "Vendor name", v$name, width = "100%"),
      selectizeInput("ve_trade", "Trade", unique(c(TRADES, na.omit(v$trade))), selected = v$trade, options = list(create = TRUE), width = "100%"),
      layout_columns(textInput("ve_contact", "Contact", nna(v$contact)), textInput("ve_email", "Email", nna(v$email))),
      layout_columns(textInput("ve_phone", "Phone", nna(v$phone)), textInput("ve_web", "Website", nna(v$website))),
      textInput("ve_hq", "Headquarters", nna(v$hq), width = "100%"),
      textAreaInput("ve_notes", "Notes", nna(v$notes), rows = 3, width = "100%"),
      footer = tagList(modalButton("Cancel"), actionButton("ve_save", "Save vendor", class = "btn-primary"))))
  })
  observeEvent(input$ve_save, {
    blank_na <- function(x) if (is.null(x) || !nzchar(trimws(x))) NA else trimws(x)
    ok <- tryCatch({
      q_exec("UPDATE vendors SET name=?, trade=?, contact=?, email=?, phone=?, website=?, hq=?, notes=? WHERE vendor_id=?",
             list(trimws(input$ve_name), blank_na(input$ve_trade), blank_na(input$ve_contact), blank_na(input$ve_email),
                  blank_na(input$ve_phone), blank_na(input$ve_web), blank_na(input$ve_hq), blank_na(input$ve_notes),
                  session$userData$vendor_edit)); TRUE
    }, error = function(e) { showNotification(paste("Couldn't save:", conditionMessage(e)), type = "error"); FALSE })
    if (ok) { removeModal(); showNotification("Vendor updated.", type = "message"); bump() }
  })

  # Add / Import ------------------------------------------------------------------------------------------
  stage <- reactiveVal("upload"); parsed <- reactiveVal(NULL); src <- reactiveVal(NULL)
  saved_id <- reactiveVal(NULL); reset_n <- reactiveVal(0); parse_err <- reactiveVal(NULL)
  lines_rv <- reactiveVal(blank_lines[0, ]); rates_rv <- reactiveVal(blank_rates[0, ])
  scope_rv <- reactiveVal(blank_scope[0, ]); equip_rv <- reactiveVal(blank_equip[0, ])
  editTableServer("e_lines", lines_rv, blank_lines, reset_n)
  editTableServer("e_rates", rates_rv, blank_rates, reset_n)
  editTableServer("e_scope", scope_rv, blank_scope, reset_n)
  editTableServer("e_equip", equip_rv, blank_equip, reset_n)

  # per-session folder so the browser can show the uploaded PDF next to the form
  prev_dir <- file.path(tempdir(), paste0("preview_", gsub("[^A-Za-z0-9]", "", session$token)))
  dir.create(prev_dir, showWarnings = FALSE, recursive = TRUE)
  addResourcePath(basename(prev_dir), prev_dir)
  session$onSessionEnded(function() unlink(prev_dir, recursive = TRUE))

  output$steps <- renderUI({
    st <- stage(); ord <- c(upload = 1, review = 2, done = 3)
    mk <- function(n, label) {
      cls <- if (ord[[st]] == n) "step on" else if (ord[[st]] > n) "step done" else "step"
      div(class = cls, span(class = "n", if (ord[[st]] > n) "✓" else n), label)
    }
    div(class = "steps", mk(1, "Upload"), mk(2, "Review against the PDF"), mk(3, "Saved"))
  })

  empty_parse <- function() list(
    vendor = list(), quote = list(quote_date = Sys.Date()),
    lines = blank_lines, rates = blank_rates[0, ], rate_year = NA_integer_, equipment = blank_equip[0, ],
    scope = blank_scope[0, ], layout = "manual", warnings = character(), pages = NA)

  load_into_form <- function(p) {
    parsed(p)
    pick <- function(df, blank) {
      if (is.null(df) || !nrow(df)) return(blank[0, , drop = FALSE])
      for (nm in setdiff(names(blank), names(df))) df[[nm]] <- blank[[nm]][1]
      df <- df[, names(blank), drop = FALSE]
      for (nm in names(df)) if (is.character(df[[nm]])) df[[nm]][is.na(df[[nm]])] <- ""
      rownames(df) <- NULL
      df
    }
    lines_rv(pick(p$lines, blank_lines)); rates_rv(pick(p$rates, blank_rates))
    scope_rv(pick(p$scope, blank_scope)); equip_rv(pick(p$equipment, blank_equip))
    reset_n(reset_n() + 1); stage("review")
  }

  # one entry point for "read this PDF and open the review screen" (upload or SharePoint)
  start_review <- function(path, name, sp = NULL, hints = list()) {
    dest <- file.path(prev_dir, paste0(format(Sys.time(), "%H%M%S"), "_", gsub("[^A-Za-z0-9._-]+", "_", name)))
    file.copy(path, dest, overwrite = TRUE)
    src(list(path = dest, name = name, url = paste0(basename(prev_dir), "/", basename(dest)), sp = sp))
    parse_err(NULL)
    p <- withProgress(message = "Reading the quote…", value = 0.5,
                      tryCatch(parse_service_quote(dest, file_name = name, hints = hints),
                               error = function(e) { parse_err(conditionMessage(e)); NULL }))
    if (is.null(p)) p <- empty_parse()
    load_into_form(p)
  }
  observeEvent(input$up_pdf, { f <- input$up_pdf; req(f); start_review(f$datapath, f$name) })

  # ---- SharePoint inbox ----------------------------------------------------------------------------
  inbox_v <- reactiveVal(0)
  inbox <- reactive({
    inbox_v(); refresh()
    q_all("SELECT * FROM sp_files ORDER BY CASE status WHEN 'updated' THEN 0 WHEN 'new' THEN 1 WHEN 'imported' THEN 2 ELSE 3 END,
           modified DESC")
  })
  output$inbox_badge <- renderUI({
    n <- sum(inbox()$status %in% c("new", "updated"))
    if (n > 0) span(class = "nav-badge", n)
  })
  do_scan <- function() {
    withProgress(message = "Checking SharePoint…", value = 0.05, {
      res <- tryCatch(sp_sync(function(m) incProgress(0.03, detail = m)), error = function(e) e)
    })
    if (inherits(res, "error")) {
      showNotification(paste("SharePoint scan failed:", conditionMessage(res)), type = "error", duration = 15)
    } else {
      showNotification(sprintf("SharePoint: %d quote PDFs · %d new · %d changed", res$found, res$new, res$changed), type = "message")
    }
    inbox_v(inbox_v() + 1); bump()
  }
  observeEvent(input$sp_scan, do_scan())
  observeEvent(input$nav, {
    if (identical(input$nav, "add") && sp_configured() && stage() == "upload") {
      last <- sp_last_scan()
      if (is.na(last) || as.numeric(difftime(Sys.time(), last, units = "mins")) > SP_CONF()$auto_min) do_scan()
    }
  }, ignoreInit = TRUE)
  observeEvent(input$ib_filter, inbox_v(inbox_v() + 1))
  observeEvent(input$ib_ignore,  { q_exec("UPDATE sp_files SET status = 'ignored' WHERE item_id = ?", list(input$ib_ignore)); inbox_v(inbox_v() + 1) })
  observeEvent(input$ib_restore, { q_exec("UPDATE sp_files SET status = CASE WHEN quote_id IS NULL THEN 'new' ELSE 'imported' END WHERE item_id = ?",
                                          list(input$ib_restore)); inbox_v(inbox_v() + 1) })
  observeEvent(input$ib_open, { qid <- q_all("SELECT quote_id FROM sp_files WHERE item_id = ?", list(input$ib_open))$quote_id
                                if (length(qid) && !is.na(qid)) go_detail(qid) })
  observeEvent(input$ib_review, {
    r <- q_all("SELECT * FROM sp_files WHERE item_id = ?", list(input$ib_review)); req(nrow(r))
    path <- r$local_path
    if (is.na(path) || !file.exists(path)) {
      path <- tryCatch(sp_download(r$item_id, file.path(FILES_DIR, "inbox", paste0(gsub("[^A-Za-z0-9]", "", r$item_id), ".pdf"))),
                       error = function(e) { showNotification(paste("Couldn't download from SharePoint:", conditionMessage(e)), type = "error", duration = 12); NULL })
      if (is.null(path)) return()
    }
    hints <- folder_hints(r$folder)
    start_review(path, r$name, hints = hints,
                 sp = list(item_id = r$item_id, web_url = r$web_url, folder = r$folder, project_folder = hints$project_folder,
                           uploaded_by = r$uploaded_by, status = r$status))
  })

  output$inbox_card <- renderUI({
    if (!sp_configured()) return(card(class = "mb-3", card_body(class = "sp-setup",
      div(class = "d-flex gap-3 align-items-start",
        div(class = "ib-ic", style = "background:#e9f1ff;color:#1650c4", icon("folder-open")),
        div(div(class = "card-t", "Connect the SharePoint inbox"),
            div(class = "card-sub", "Let coworkers just save quotes in the project's “… Partners” folders — they'll show up here to review."),
            tags$ol(
              tags$li("IT grants the existing Entra app ", tags$b("Sites.Selected (read)"), " on the ", tags$b(SP_CONF()$site), " site."),
              tags$li("Add ", tags$code("SP_TENANT_ID"), ", ", tags$code("SP_CLIENT_ID"), ", ", tags$code("SP_CLIENT_SECRET"), " to ", tags$code(".Renviron"), " next to app.R."),
              tags$li("Install ", tags$code("httr2"), " and restart the app.")))))))
    f <- inbox(); flt <- input$ib_filter %||% "todo"
    groups <- list(todo = c("new", "updated"), imported = "imported", ignored = "ignored", all = c("new", "updated", "imported", "ignored"))
    labels <- c(todo = "To review", imported = "Imported", ignored = "Ignored", all = "All")
    n <- vapply(groups, function(g) sum(f$status %in% g), 0)
    rows <- f[f$status %in% groups[[flt]], , drop = FALSE]
    last <- sp_last_scan()
    cf <- SP_CONF()
    tab <- function(k) tags$button(type = "button", class = paste("ib-tab", if (k == flt) "on"),
                                   onclick = sprintf("Shiny.setInputValue('ib_filter', '%s', {priority: 'event'})", k),
                                   labels[[k]], span(class = "c", n[[k]]))
    js <- function(id, v) sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", id, jsonlite::toJSON(v, auto_unbox = TRUE))
    row_ui <- function(r) {
      pillk <- switch(r$status, new = pill("New", "info"), updated = pill("Changed in SharePoint", "warn"),
                      imported = pill("Imported", "ok"), ignored = pill("Ignored", "muted"))
      div(class = "ib-row",
        div(class = "ib-ic", icon("file-pdf")),
        div(style = "min-width:0",
          div(class = "ib-name", tags$a(href = r$web_url, target = "_blank", title = "Open in SharePoint", r$name)),
          div(class = "ib-crumbs", lapply(folder_crumbs(r$folder), span)),
          div(class = "ib-meta", paste(c(if (!is.na(r$uploaded_by)) paste("Added by", r$uploaded_by),
                                         if (!is.na(r$modified)) paste("updated", fmt_date(substr(r$modified, 1, 10)))), collapse = " · ")),
          if (!is.na(r$parse_note) && r$status %in% c("new", "updated")) div(class = "ib-note", icon("triangle-exclamation"), " ", substr(r$parse_note, 1, 160))),
        div(class = "ib-guess",
          if (!is.na(r$vendor_guess)) div(class = "vcell", avatar(r$vendor_guess, r$trade_guess, 30),
            div(style = "min-width:0", tags$b(r$vendor_guess),
                div(class = "sub", paste(c(if (!is.na(r$trade_guess)) r$trade_guess, if (!is.na(r$quote_no_guess)) paste0("#", r$quote_no_guess)), collapse = " · "))))
          else span(class = "text-muted small", if (is.na(r$etag_parsed)) "Not read yet" else "Vendor not recognized"),
          if (!is.na(r$total_guess)) div(class = "tot mt-1", fmt_money(r$total_guess))),
        div(class = "ib-act", pillk,
          if (r$status %in% c("new", "updated")) tagList(
            tags$button(type = "button", class = "btn btn-primary btn-sm", onclick = js("ib_review", r$item_id), icon("magnifying-glass"), " Review"),
            tags$button(type = "button", class = "btn btn-light btn-sm", title = "Not a quote / don't import", onclick = js("ib_ignore", r$item_id), icon("eye-slash")))
          else if (r$status == "imported") tags$button(type = "button", class = "btn btn-light btn-sm", onclick = js("ib_open", r$item_id), "View quote")
          else tags$button(type = "button", class = "btn btn-light btn-sm", onclick = js("ib_restore", r$item_id), "Restore")))
    }
    card(class = "mb-3",
      card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        div(span(class = "card-t", icon("folder-open"), " SharePoint inbox"),
            div(class = "card-sub", sprintf("%s › %s · PDFs in “… Partners” folders", cf$site, cf$root))),
        div(class = "d-flex align-items-center gap-2",
            span(class = "card-sub", if (is.na(last)) "Not scanned yet" else paste("Last checked", format(last, "%b %d, %H:%M", tz = Sys.timezone()))),
            actionButton("sp_scan", "Scan now", icon = icon("rotate"), class = "btn-light btn-sm"))),
      card_body(fillable = FALSE,
        div(class = "ib-tabs", lapply(names(labels), tab)),
        if (!nrow(rows)) div(class = "text-muted small py-3 text-center",
                             if (flt == "todo") "Nothing waiting — new quotes saved to SharePoint will appear here." else "Nothing here.")
        else tagList(lapply(seq_len(min(nrow(rows), 50)), function(i) row_ui(rows[i, ])),
                     if (nrow(rows) > 50) div(class = "text-muted small text-center pt-2", sprintf("+ %d more", nrow(rows) - 50)))))
  })

  observeEvent(input$start_blank, { src(NULL); parse_err(NULL); load_into_form(empty_parse()) })
  observeEvent(input$add_another, { parsed(NULL); src(NULL); parse_err(NULL); stage("upload") })
  observeEvent(input$add_cancel,  { parsed(NULL); src(NULL); parse_err(NULL); stage("upload") })

  output$add_body <- renderUI({
    st <- stage()
    if (st == "upload") return(tagList(uiOutput("inbox_card"), layout_columns(col_widths = c(8, 4),
      card(card_body(class = "p-4",
        div(class = "drop",
            img(src = "bytebridge-mark.png", class = "drop-mark", alt = ""),
            h5("Drop a vendor quote here"),
            div(class = "text-muted", "PDF proposals, fee letters and line-item quotes · up to 50 MB"),
            fileInput("up_pdf", NULL, accept = c(".pdf", "application/pdf"), width = "100%",
                      buttonLabel = "Choose PDF…", placeholder = "or drag it onto this box")),
        div(class = "d-flex align-items-center justify-content-center gap-2 mt-3",
            span(class = "text-muted small", "No PDF?"),
            actionButton("start_blank", "Enter a quote by hand", class = "btn-light btn-sm")))),
      div(
        section_title("Layouts it reads"),
        div(class = "fmt-card", tags$b("Engineering fee letters"), div("Proposal #, phase fees, reimbursables, scope by discipline, exclusions, hourly rate schedule.")),
        div(class = "fmt-card", tags$b("Line-item labor quotes"), div("Quote #, Prepared By, per-location tables with qty / material / labor / total, caveats and exclusions.")),
        div(class = "fmt-card", tags$b("Anything else"), div("You still get the PDF side-by-side with an editable form — nothing is saved until you click Save."))))))

    if (st == "done") return(card(card_body(empty_state("Quote saved",
      "It's searchable on Quotes, comparable with other bids and feeding the benchmarks.",
      div(class = "d-flex gap-2 justify-content-center",
          actionButton("view_saved", "View quote", class = "btn-primary"),
          actionButton("add_another", "Add another", class = "btn-light"))))))

    # ---- review ----
    p <- parsed(); v <- p$vendor; q <- p$quote; s <- src()
    g <- function(x, default = "") if (is.null(x) || length(x) == 0 || all(is.na(x))) default else x
    form <- tagList(
      uiOutput("parse_report"),
      card(card_header(span(class = "card-t", "Quote")), card_body(fillable = FALSE,
        textInput("a_project", "Project name *", g(q$project_name), width = "100%"),
        layout_columns(col_widths = c(4, 4, 4),
          textInput("a_prop", "Quote / proposal #", g(q$proposal_no)),
          dateInput("a_date", "Quote date", value = g(q$quote_date, Sys.Date())),
          numericInput("a_valid", "Valid for (days)", g(q$valid_days, NA))),
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("a_total", "Total ($) *", g(q$base_total, NA)),
          numericInput("a_reimb", "Est. reimbursables ($)", g(q$reimbursables, NA)),
          selectInput("a_status", "Status", STATUSES, selected = "Received")),
        layout_columns(col_widths = c(4, 4, 4),
          selectizeInput("a_basis", "Fee basis", c("", "Lump Sum", "Not-To-Exceed", "Time And Materials", "Hourly", "Unit Price"),
                         selected = g(q$fee_basis), options = list(create = TRUE)),
          textInput("a_billing", "Billing terms", g(q$billing_terms)),
          textInput("a_attn", "Our contact (Attn)", g(q$client_contact))))),
      card(card_header(span(class = "card-t", "Vendor")), card_body(fillable = FALSE,
        layout_columns(col_widths = c(6, 6),
          textInput("a_vendor", "Vendor name *", g(v$name), width = "100%"),
          selectizeInput("a_trade", "Trade", c("", TRADES), selected = g(v$trade), options = list(create = TRUE), width = "100%")),
        layout_columns(col_widths = c(4, 4, 4),
          textInput("a_contact", "Contact", g(v$contact)), textInput("a_email", "Email", g(v$email)), textInput("a_phone", "Phone", g(v$phone))),
        layout_columns(col_widths = c(6, 6),
          textInput("a_web", "Website", g(v$website)), textInput("a_hq", "Headquarters", g(v$hq))))),
      card(card_header(span(class = "card-t", "Where & what")), card_body(fillable = FALSE,
        layout_columns(col_widths = c(4, 4, 4),
          textInput("a_operator", "Site operator", g(q$site_operator)), textInput("a_site", "Site code", g(q$site_code)),
          textInput("a_hall", "Data hall", g(q$data_hall))),
        layout_columns(col_widths = c(6, 3, 3),
          textInput("a_address", "Address", g(q$address)), textInput("a_city", "City", g(q$city)), textInput("a_state", "State", g(q$state))),
        layout_columns(col_widths = c(6, 3, 3),
          textInput("a_locations", "Locations (multi-site, separate with ;)", g(q$locations)),
          numericInput("a_kw", "IT load (kW)", g(q$it_load_kw, NA)), numericInput("a_hours", "Labor hours", g(q$labor_hours, NA))),
        layout_columns(col_widths = c(4, 4, 4),
          textInput("a_red", "Redundancy", g(q$redundancy)), textInput("a_cool", "Cooling", g(q$cooling)),
          textInput("a_deliv", "Delivery method", g(q$delivery_method))),
        layout_columns(col_widths = c(6, 6),
          numericInput("a_dmo", "Design (months)", g(q$design_months, NA)), numericInput("a_cmo", "Construction (months)", g(q$construction_months, NA))),
        textAreaInput("a_summary", "Scope summary", g(q$scope_summary), rows = 2, width = "100%"),
        textAreaInput("a_notes", "Notes", g(q$notes), rows = 2, width = "100%"))),
      editTableUI("e_lines", "Pricing lines", "line_type: base \u00b7 reimbursable \u00b7 optional \u00b7 allowance \u2014 category drives the cost mix. Double-click a cell to edit."),
      editTableUI("e_scope", "Scope, exclusions & assumptions", "kind: deliverable \u00b7 inclusion \u00b7 exclusion \u00b7 assumption \u00b7 client"),
      layout_columns(col_widths = c(6, 6),
        editTableUI("e_rates", "Hourly rate card", if (!is.na(g(p$rate_year, NA))) paste("Rate year", p$rate_year) else "Role, $/hr"),
        editTableUI("e_equip", "Equipment basis", "Racks, CDUs \u2014 used for $/rack")),
      div(class = "save-bar",
          actionButton("add_cancel", "Discard", class = "btn-light"),
          actionButton("add_save", "Save quote", icon = icon("check"), class = "btn-primary px-4")))

    if (is.null(s)) return(form)
    layout_sidebar(fillable = FALSE, border = FALSE, fg = "#0f172a",
      sidebar = sidebar(position = "right", width = "38%", open = TRUE, bg = "#ffffff",
                        title = div(class = "d-flex justify-content-between align-items-center gap-2",
                                    span(class = "card-t text-truncate", icon("file-pdf"), " ", s$name),
                                    tags$a(href = s$url, target = "_blank", class = "small text-nowrap", "Open ↗")),
                        tags$iframe(class = "pdf-frame", src = paste0(s$url, "#view=FitH"))),
      form)
  })

  output$parse_report <- renderUI({
    req(stage() == "review"); p <- parsed(); s <- src(); err <- parse_err()
    ln <- lines_rv(); errs <- character(); warns <- character()
    if (!is.null(err)) errs <- c(errs, paste("Couldn't read this PDF automatically:", err, "Fill the form from the PDF alongside."))
    if (!nzchar(trimws(input$a_vendor %||% ""))) errs <- c(errs, "Vendor name is required.")
    if (!nzchar(trimws(input$a_project %||% ""))) errs <- c(errs, "Project name is required.")
    if (is.na(input$a_total %||% NA)) errs <- c(errs, "Total is required.")
    base_sum <- sum(suppressWarnings(as.numeric(ln$amount[ln$line_type == "base"])), na.rm = TRUE)
    mismatch <- !is.na(input$a_total %||% NA) && base_sum > 0 && abs(base_sum - input$a_total) > 0.5
    if (mismatch) warns <- c(warns, sprintf("Base lines add up to %s but the total is %s.", fmt_money(base_sum), fmt_money(input$a_total)))
    if (length(p$warnings)) warns <- c(warns, p$warnings)
    if (!is.null(input$a_date) && length(input$a_date) && !is.na(input$a_valid %||% NA) && input$a_date + input$a_valid < Sys.Date())
      warns <- c(warns, sprintf("This quote expired on %s.", fmt_date(input$a_date + input$a_valid)))
    if (nzchar(input$a_prop %||% "") && nzchar(input$a_vendor %||% "")) {
      dup <- q_all("SELECT q.quote_id FROM quotes q JOIN vendors v ON v.vendor_id = q.vendor_id WHERE v.name = ? COLLATE NOCASE AND q.proposal_no = ?",
                   list(input$a_vendor, input$a_prop))
      if (nrow(dup)) warns <- c(warns, sprintf("%s #%s is already saved — saving adds another copy (mark the old one Superseded if this is a revision).", input$a_vendor, input$a_prop))
    }
    found <- function(x) !is.null(x) && length(x) > 0 && !all(is.na(x)) && !identical(x, "")
    q <- p$quote; sc <- scope_rv()
    chips <- c("Vendor" = found(p$vendor$name), "Quote #" = found(q$proposal_no), "Date" = found(q$quote_date),
               "Project" = found(q$project_name), "Contact" = found(p$vendor$contact), "Pricing lines" = nrow(ln) > 0,
               "Scope" = any(sc$kind %in% c("inclusion", "deliverable")), "Exclusions" = any(sc$kind == "exclusion"),
               "Assumptions" = any(sc$kind == "assumption"), "Rate card" = nrow(rates_rv()) > 0,
               "Location" = found(q$locations) || found(q$site_operator), "Labor hours" = found(q$labor_hours), "IT load" = found(q$it_load_kw))
    auto <- !is.null(s) && is.null(err)
    div(class = "report",
      div(class = "r-top",
          div(div(class = "card-t", if (is.null(s)) "Manual entry" else if (!is.null(err)) "Couldn't auto-read this file" else
                    sprintf("Read %d of %d fields automatically", sum(chips), length(chips))),
              if (!is.null(s$sp)) div(class = "from-sp", icon("folder-open"), lapply(folder_crumbs(s$sp$folder), span),
                                      if (!is.na(s$sp$uploaded_by %||% NA)) span(paste("· added by", s$sp$uploaded_by)),
                                      if (identical(s$sp$status, "updated")) pill("Changed since last import", "warn")),
              div(class = "card-sub", if (auto) sprintf("Layout: %s · %s pages · check each value against the PDF before saving", p$layout, p$pages)
                  else "Fill in the fields below")),
          if (mismatch) actionLink("use_line_sum", sprintf("Set total to %s", fmt_money(base_sum)))),
      if (auto) div(class = "chips", lapply(names(chips), function(k) span(class = paste("chip", if (chips[[k]]) "y" else "n"), k))),
      lapply(errs, function(e) div(class = "msg err", icon("circle-exclamation"), span(e))),
      lapply(warns, function(w) div(class = "msg warn", icon("triangle-exclamation"), span(w))),
      if (!length(errs) && !length(warns)) div(class = "msg ok", icon("circle-check"), span("Everything checks out. Review the tables, then save.")))
  })
  observeEvent(input$use_line_sum, {
    ln <- lines_rv()
    updateNumericInput(session, "a_total", value = round(sum(suppressWarnings(as.numeric(ln$amount[ln$line_type == "base"])), na.rm = TRUE), 2))
  })

  observeEvent(input$add_save, {
    if (!nzchar(trimws(input$a_vendor)) || !nzchar(trimws(input$a_project)) || is.na(input$a_total)) {
      showNotification("Vendor, project name and total are required.", type = "error"); return()
    }
    p <- parsed(); s <- src()
    v <- list(name = trimws(input$a_vendor), trade = input$a_trade, contact = input$a_contact, email = input$a_email,
              phone = input$a_phone, website = input$a_web, hq = input$a_hq)
    q <- list(proposal_no = input$a_prop, quote_date = input$a_date, valid_days = input$a_valid, status = input$a_status,
              project_name = input$a_project, client_contact = input$a_attn, site_operator = input$a_operator,
              site_code = input$a_site, address = input$a_address, city = input$a_city, state = input$a_state,
              locations = input$a_locations, data_hall = input$a_hall, it_load_kw = input$a_kw, labor_hours = input$a_hours,
              redundancy = input$a_red, cooling = input$a_cool, delivery_method = input$a_deliv,
              design_months = input$a_dmo, construction_months = input$a_cmo, fee_basis = input$a_basis,
              billing_terms = input$a_billing, base_total = input$a_total, reimbursables = input$a_reimb,
              doc_total = p$quote$doc_total, scope_summary = input$a_summary, layout = p$layout, notes = input$a_notes,
              sp_item_id = s$sp$item_id, sp_web_url = s$sp$web_url, project_folder = s$sp$project_folder %||% p$project_folder)
    qid <- tryCatch(save_quote(v, q, lines_rv(), rates_rv(), p$rate_year, scope_rv(), equip_rv(),
                               src_path = s$path, src_name = s$name),
                    error = function(e) { showNotification(paste("Save failed:", conditionMessage(e)), type = "error", duration = 12); NULL })
    if (is.null(qid)) return()
    saved_id(qid); stage("done"); bump(); inbox_v(inbox_v() + 1)
    showNotification(sprintf("Saved %s quote #%s", v$name, nna(input$a_prop, "")), type = "message")
  })
  observeEvent(input$view_saved, go_detail(saved_id()))
}

shinyApp(ui, server)
