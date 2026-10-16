# Decision tree: questions, sampling designs and the rules that choose between them.
# No Shiny code; used by modules/decision_tree_module.R and tested in
# tests/testthat/test-decision-tree.R.

# ---------------------------------------------------------------------------
# Questions (answer label = code)
# ---------------------------------------------------------------------------

DECISION_CHOICES <- list(
  purpose = c(
    "Measure a level (PDM, coverage, satisfaction)" = "level",
    "Measure change or impact (baseline/endline, comparison group)" = "change",
    "Verify sites, sub-projects or beneficiaries" = "verify",
    "Understand why or how (KIIs, FGDs)" = "qualitative",
    "Pilot or test a tool" = "pilot"
  ),
  unit = c(
    "Households or people" = "people",
    "Sites, facilities or sub-projects" = "sites"
  ),
  frame = c(
    "A full list of the units, with their village or location (e.g. a beneficiary list)" = "unit_list",
    "A list of villages or sites with population or beneficiary counts" = "cluster_sizes",
    "A list of villages or sites without counts" = "cluster_list",
    "No list" = "none",
    "A group that cannot be listed (hidden or hard to reach)" = "unlistable"
  ),
  reporting = c(
    "The whole programme (overall)" = "overall",
    "Province or district" = "area",
    "Gender" = "gender",
    "Programme component" = "component",
    "Other group" = "other"
  ),
  spread = c(
    "A few sites, all reachable" = "few",
    "Many villages or sites across districts" = "many"
  ),
  mode = c(
    "In person" = "in_person",
    "Phone" = "phone",
    "Phone, with in-person visits for people without a phone" = "mixed"
  ),
  constraints = c(
    "Insecure or inaccessible areas" = "insecure",
    "Limits on reaching women" = "women",
    "Tight budget or time" = "budget",
    "The list is incomplete or out of date" = "list_outdated"
  ),
  comparison = c(
    "Before and after, same programme area (no comparison group)" = "before_after",
    "Treatment and comparison groups, endline only" = "groups_endline",
    "Treatment and comparison groups, at baseline and endline" = "groups_both"
  ),
  panel = c(
    "Yes, the same households at each round" = "yes",
    "No, new households each round" = "no"
  ),
  random = c(
    "Yes, assigned at random (e.g. by lottery of villages)" = "yes",
    "No, the programme chose where to work" = "no"
  )
)

DECISION_QUESTIONS <- c(
  purpose = "What is the study for?",
  unit = "What are you sampling?",
  frame = "What list do you have?",
  reporting = "What must results be reported for?",
  spread = "How spread out are the units?",
  mode = "How will you interview?",
  constraints = "What constraints apply?",
  comparison = "How is the comparison set up?",
  panel = "Are the same households followed over time?",
  random = "Were areas or households assigned at random?"
)

#' Questions that apply to a set of answers, in display order
decision_applicable <- function(a) {
  p <- a$purpose
  if (is.null(p) || !nzchar(p)) return("purpose")
  q <- switch(p,
    pilot = c("purpose", "mode"),
    qualitative = c("purpose", "frame", "reporting", "mode", "constraints"),
    verify = c("purpose", "unit", "frame", "reporting", "spread", "mode", "constraints"),
    c("purpose", "unit", "frame", "reporting", "spread", "mode", "constraints")
  )
  if (p == "change") {
    q <- c(q, "comparison")
    if (identical(a$comparison, "before_after") || identical(a$comparison, "groups_both")) q <- c(q, "panel")
    if (!is.null(a$comparison) && a$comparison %in% c("groups_endline", "groups_both")) q <- c(q, "random")
  }
  q
}

#' Readable label for an answer code (several codes are joined)
decision_label <- function(question, value) {
  if (length(value) == 0 || all(!nzchar(value))) return("None")
  ch <- DECISION_CHOICES[[question]]
  labs <- vapply(value, function(v) { l <- names(ch)[ch == v]; if (length(l)) l else v }, character(1))
  paste(labs, collapse = "; ")
}

# ---------------------------------------------------------------------------
# Designs. Each has the text shown to the user, what it needs, common field
# problems, the guideline article and the settings handed to other tabs.
#   calculator: "sample" (Sample Calculator), "evaluation" or NA
#   method:     Sample Calculator / Sample Generator method, or NA when the
#               generator cannot draw this design
# ---------------------------------------------------------------------------

DESIGNS <- list(
  srs_list = list(
    name = "Simple random or systematic sample from the list",
    category = "Probability",
    when = "A full list of units, in a small area or where travel cost does not depend much on who is selected.",
    needs = "A complete, current list of units with an ID.",
    field = c("Units that moved or cannot be found: replace from the reserve list only, and log every replacement.",
              "Sort the list by location before a systematic draw, so the sample is spread across areas."),
    article = "simple_random", method = "systematic", calculator = "sample"
  ),
  stratified_list = list(
    name = "Stratified random sample from the list",
    category = "Probability",
    when = "A full list that records the reporting groups (e.g. province or programme component).",
    needs = "A complete list with the stratum of every unit.",
    field = c("Each stratum needs its own sample size if it is reported separately.",
              "Weight the results when strata are not sampled in proportion to their size."),
    article = "stratified", method = "stratified", calculator = "sample"
  ),
  cluster = list(
    name = "Two-stage cluster sample",
    category = "Probability",
    when = "Units spread over many villages, where visiting a random household in every village is too costly or unsafe.",
    needs = "A list of villages (clusters), ideally with population or beneficiary counts; a household list or a listing in each selected village.",
    field = c("Clustering lowers precision: use the design effect in the Sample Calculator.",
              "Draw reserve villages in advance for those that turn out to be inaccessible.",
              "Without a household list, list households in each selected village (or use segmentation) before selecting them."),
    article = "cluster", method = "cluster", calculator = "sample"
  ),
  stratified_cluster = list(
    name = "Stratified two-stage cluster sample",
    category = "Probability",
    when = "Many villages across several reporting areas or components, each needing its own result.",
    needs = "A list of villages with their stratum, ideally with population or beneficiary counts.",
    field = c("Allocate enough clusters to every stratum; a stratum with very few clusters gives unstable results.",
              "Weight the results when the sample is not proportional to stratum size.",
              "Draw reserve villages per stratum."),
    article = "cluster", method = "stratified_cluster", calculator = "sample"
  ),
  site_census = list(
    name = "All sites, with a random sample within each",
    category = "Probability",
    when = "A few sites that can all be reached.",
    needs = "A list of units, or a household listing, in every site.",
    field = c("Small samples per site give wide margins of error for single sites.",
              "Use the sites as strata, so each site gets a set number of interviews."),
    article = "stratified", method = "stratified", calculator = "sample"
  ),
  site_all = list(
    name = "Visit every site (census of sites)",
    category = "Census",
    when = "Verification with a small number of sites or sub-projects, where every one can be visited.",
    needs = "The full site list from the implementing partner.",
    field = c("Record sites that could not be visited and why; report them as a gap, not as verified."),
    article = "site_verification", method = NA, calculator = NA
  ),
  site_sample = list(
    name = "Random sample of sites",
    category = "Probability",
    when = "Verification of many sites or sub-projects, where visiting all of them is not possible.",
    needs = "The full site list, with province, component or size if the sample should be stratified or PPS.",
    field = c("Select the sites at random from the list; do not let the partner choose them.",
              "Draw reserve sites in the same strata for sites that cannot be reached."),
    article = "site_verification", method = "simple_random", calculator = "sample"
  ),
  phone_list = list(
    name = "Random sample from a phone list",
    category = "Probability",
    when = "Phone interviews with people on a beneficiary or partner list that has phone numbers.",
    needs = "The list with phone numbers (kept offline) and anonymised IDs.",
    field = c("Set a call-back rule (e.g. 3 attempts on different days and times) before replacing a number.",
              "Draw extra numbers as replacements, and log every replacement.",
              "People without a phone, or with a wrong number, are missed: report this coverage gap."),
    article = "phone_monitoring", method = "simple_random", calculator = "sample"
  ),
  mixed_mode = list(
    name = "Phone sample from the list, with in-person follow-up",
    category = "Probability",
    when = "A list where some people have no working phone, and leaving them out would bias the results.",
    needs = "A list with phone numbers where known, and the village of everyone on it.",
    field = c("Visit a random subsample of the people who could not be reached by phone, rather than all of them.",
              "Ask the same questions in the same order in both modes, and record the mode for each interview."),
    article = "phone_monitoring", method = "simple_random", calculator = "sample"
  ),
  rdd = list(
    name = "Random digit dialling (RDD)",
    category = "Probability",
    when = "A general-population phone survey with no list.",
    needs = "The number ranges of each operator, and a random number generator.",
    field = c("Afghanistan: there is no public list of allocated number ranges, many women have no phone of their own, and coverage is lower in rural areas.",
              "Expect many invalid numbers and refusals; plan for a low response rate."),
    article = "phone_monitoring", method = NA, calculator = "sample"
  ),
  hidden_group = list(
    name = "Build a list first, or respondent-driven sampling",
    category = "Indicative",
    when = "A group that cannot be listed, but results should describe the whole group.",
    needs = "Contacts through community leaders, service providers or members of the group.",
    field = c("Building a list (mapping) first allows a probability sample afterwards.",
              "Respondent-driven sampling (RDS) tracks who recruited whom, which makes estimates possible, but it needs specialist analysis.",
              "Otherwise, treat the results as indicative."),
    article = "probability_vs_nonprobability", method = NA, calculator = NA
  ),
  purposive = list(
    name = "Purposive sample",
    category = "Non-Probability",
    when = "KIIs or FGDs, chosen for what they know or represent.",
    needs = "Clear selection criteria written before fieldwork.",
    field = c("Stop when new interviews add no new themes (saturation); plan for that rather than a fixed number.",
              "Record why each respondent was chosen."),
    article = "probability_vs_nonprobability", method = NA, calculator = NA
  ),
  purposive_groups = list(
    name = "Purposive sample covering every group",
    category = "Non-Probability",
    when = "KIIs or FGDs that must cover several provinces, components or genders.",
    needs = "Selection criteria, and a set number of interviews or groups per reporting group.",
    field = c("Hold separate FGDs for women and men, led by a facilitator of the same gender.",
              "Aim for saturation in each group, not just overall."),
    article = "probability_vs_nonprobability", method = NA, calculator = NA
  ),
  snowball = list(
    name = "Snowball sample",
    category = "Non-Probability",
    when = "Qualitative work with a hidden group that has no list.",
    needs = "A few initial contacts who can introduce others.",
    field = c("Start from several different contacts so one network does not dominate.",
              "Protect respondents: ask for introductions, not names and phone numbers."),
    article = "probability_vs_nonprobability", method = NA, calculator = NA
  ),
  pilot = list(
    name = "Convenience sample for the pilot",
    category = "Non-Probability",
    when = "Testing a questionnaire or procedure, not producing findings.",
    needs = "Respondents similar to the study population, outside the selected sample.",
    field = c("Do not report pilot results as findings.",
              "Pilot outside the villages selected for the main survey."),
    article = "quality_control", method = NA, calculator = NA
  )
)

# Evaluation designs (codes as in EVALUATION_DESIGNS in sample_size_functions.R)
EVALUATION_DESIGN_LABELS <- c(
  independent = "Two independent groups",
  panel = "Panel: same households at baseline and endline",
  did_cross = "Difference-in-differences, repeated cross-sections",
  did_panel = "Difference-in-differences, panel"
)

# ---------------------------------------------------------------------------
# Rules. Read top to bottom: the first rule whose condition holds sets the
# design. `a` is the list of answers, plus derived flags from decision_facts().
# ---------------------------------------------------------------------------

DESIGN_RULES <- list(
  list(design = "pilot",              when = function(a) a$purpose == "pilot"),
  list(design = "snowball",           when = function(a) a$purpose == "qualitative" && a$frame == "unlistable"),
  list(design = "purposive_groups",   when = function(a) a$purpose == "qualitative" && a$stratify),
  list(design = "purposive",          when = function(a) a$purpose == "qualitative"),
  list(design = "hidden_group",       when = function(a) a$frame == "unlistable"),
  list(design = "site_all",           when = function(a) a$purpose == "verify" && a$unit == "sites" && a$spread == "few"),
  list(design = "site_sample",        when = function(a) a$purpose == "verify" && a$unit == "sites"),
  list(design = "phone_list",         when = function(a) a$mode == "phone" && a$frame == "unit_list"),
  list(design = "rdd",                when = function(a) a$mode == "phone"),
  list(design = "mixed_mode",         when = function(a) a$mode == "mixed" && a$frame == "unit_list"),
  list(design = "site_census",        when = function(a) a$spread == "few"),
  list(design = "stratified_list",    when = function(a) a$frame == "unit_list" && a$stratify && a$compact),
  list(design = "srs_list",           when = function(a) a$frame == "unit_list" && a$compact),
  list(design = "stratified_cluster", when = function(a) a$stratify),
  list(design = "cluster",            when = function(a) TRUE)
)

#' Fill in defaults and derived flags
#'
#' stratify: results are reported for groups that can be strata (area,
#' component, other; gender only when it is on a unit list).
#' compact: a unit list where a non-clustered draw is practical (no insecure
#' areas and no tight budget).
decision_facts <- function(a) {
  defaults <- list(purpose = "", unit = "people", frame = "unit_list", reporting = "overall", spread = "many",
                   mode = "in_person", constraints = character(0), comparison = "before_after",
                   panel = "no", random = "no", other_group = "")
  for (k in names(defaults)) if (is.null(a[[k]]) || (length(a[[k]]) == 1 && is.na(a[[k]]))) a[[k]] <- defaults[[k]]
  if (length(a$reporting) == 0) a$reporting <- "overall"
  # Questions that do not apply take their default, so hidden answers never change the result
  applicable <- decision_applicable(a)
  for (k in setdiff(names(DECISION_QUESTIONS), applicable)) a[[k]] <- defaults[[k]]
  groups <- setdiff(a$reporting, "overall")
  strata_groups <- setdiff(groups, if (a$frame != "unit_list") "gender")
  a$groups <- groups
  a$stratify <- length(strata_groups) > 0
  a$strata_groups <- strata_groups
  a$compact <- a$frame == "unit_list" && !any(c("insecure", "budget") %in% a$constraints)
  a$probability <- !a$purpose %in% c("qualitative", "pilot")
  a
}

#' Evaluation design code for the comparison answers (NULL unless purpose is change)
decision_evaluation_design <- function(a) {
  if (a$purpose != "change") return(NULL)
  panel <- identical(a$panel, "yes")
  switch(a$comparison,
    before_after = if (panel) "panel" else "independent",
    groups_endline = "independent",
    groups_both = if (panel) "did_panel" else "did_cross"
  )
}

#' Names of the reporting groups, for text
decision_group_names <- function(a, groups = a$groups) {
  labs <- c(area = "province or district", gender = "gender", component = "programme component",
            other = if (nzchar(trimws(a$other_group))) trimws(a$other_group) else "the other group")
  unname(labs[groups])
}

#' Stages of the recommended design, spelled out for the field team
decision_stages <- function(d, a) {
  strata <- paste(decision_group_names(a, a$strata_groups), collapse = " and ")
  stage2 <- switch(a$frame,
    unit_list = "households (or people) selected systematically from the list for that village",
    "households selected after listing every household in the village (or by segmentation for large villages); use a random walk only if listing is impossible"
  )
  pps <- a$frame %in% c("unit_list", "cluster_sizes")
  stage1 <- if (pps) "villages selected with probability proportional to size (PPS), using population or beneficiary counts"
            else "villages selected with equal probability (no counts available; record each village's size at listing for the weights)"
  vlist <- if (a$frame == "none") "Get a village list first (e.g. from the implementing partner or official settlement lists). " else ""
  reserves <- "Draw reserve units (about 10%) in advance."
  switch(d,
    srs_list = c("Sort the list by province, district and village.",
                 "Select units systematically with a random start (or by simple random sampling).", reserves),
    stratified_list = c(paste0("Strata: ", strata, "."),
                        "Within each stratum, select units at random (or systematically from a list sorted by location).", reserves),
    cluster = c(paste0(vlist, "Stage 1: ", stage1, "."), paste0("Stage 2: in each selected village, a fixed number of ", stage2, "."),
                "Draw reserve villages, and reserve households within each village."),
    stratified_cluster = c(paste0("Strata: ", strata, "."), paste0(vlist, "Stage 1: within each stratum, ", stage1, "."),
                           paste0("Stage 2: in each selected village, a fixed number of ", stage2, "."),
                           "Draw reserve villages in every stratum, and reserve households within each village."),
    site_census = c("Include every site; treat each site as a stratum.",
                    paste0("In each site, select ", if (a$frame == "unit_list") "units at random from the list" else "households at random after listing them", "."),
                    reserves),
    site_all = c("Visit every site on the list.", "Use the same checklist at every site, and record sites that could not be visited."),
    site_sample = c(if (a$stratify) paste0("Strata: ", strata, ".") else "Use the full site list as the frame.",
                    paste0("Select sites at random", if (a$stratify) " within each stratum" else "", ", or with PPS by budget or size if larger sites matter more."),
                    "Draw reserve sites in the same strata."),
    phone_list = c(if (a$stratify) paste0("Strata: ", strata, ".") else "Use the full list as the frame.",
                   paste0("Select numbers at random", if (a$stratify) " within each stratum" else "", "."),
                   "Call each number up to 3 times on different days and times before replacing it from the reserve list."),
    mixed_mode = c("Select people at random from the full list (with or without a phone number).",
                   "Call those with a number, using the call-back rule.",
                   "Visit in person those without a working number, or a random subsample of them in each village."),
    rdd = c("Generate random numbers within the operators' number ranges.", "Screen each answered call for eligibility.",
            "Use quotas or weights for province, gender and urban/rural to reduce coverage bias."),
    hidden_group = c("Map the group through community leaders and service providers to build a list, then sample from it.",
                     "If that is not possible, use respondent-driven sampling with trained staff, or a snowball sample with results reported as indicative."),
    purposive = c("Write the selection criteria (role, experience, location) before fieldwork.",
                  "Select respondents who meet them, and add interviews until no new themes appear."),
    purposive_groups = c(paste0("Cover every group: ", paste(decision_group_names(a), collapse = ", "), "."),
                         "Set a number of interviews or FGDs per group, then add more until no new themes appear."),
    snowball = c("Start with several initial contacts from different networks.",
                 "Ask each respondent to introduce others who meet the criteria, until no new themes appear."),
    pilot = c("Interview a small number of respondents similar to the study population, outside the selected villages.",
              "Use the results only to fix the tool and procedures.")
  )
}

#' Reasons for the recommendation, one per answer that drove it
decision_reasons <- function(d, a) {
  r <- character(0)
  add <- function(cond, text) if (isTRUE(cond)) r <<- c(r, text)
  add(a$purpose == "pilot", "The study is a pilot, so findings are not reported and random selection is not needed.")
  add(a$purpose == "qualitative", "Qualitative work selects people for what they can tell you, not to estimate percentages.")
  add(a$purpose == "verify" && a$unit == "sites", "The study verifies sites, so the sites are the units sampled.")
  add(a$purpose == "change", "The study measures change, so the sample size comes from the Evaluation Calculator.")
  add(a$probability && a$frame == "unlistable", "The group cannot be listed, so a standard random sample is not possible.")
  add(a$frame == "unlistable" && a$purpose == "qualitative", "The group has no list, so respondents introduce others.")
  add(d %in% c("site_all", "site_census"), "There are only a few sites and all can be reached, so every site is included.")
  add(d == "site_sample", "There are too many sites to visit all of them.")
  add(a$mode == "phone" && a$frame == "unit_list", "Interviews are by phone and you have a list, so numbers are drawn from the list.")
  add(d == "rdd", "Interviews are by phone with no list, so numbers must be generated.")
  add(d == "mixed_mode", "Some people have no phone, so in-person visits reach the people a phone sample would miss.")
  add(d %in% c("cluster", "stratified_cluster") && a$frame != "unit_list", "There is no list of households across all villages, so villages are selected first.")
  add(d %in% c("cluster", "stratified_cluster") && a$frame == "unit_list",
      paste0("Units are spread over many villages", if (length(intersect(c("insecure", "budget"), a$constraints))) " and travel is limited by security or budget" else "",
             ", so grouping interviews in selected villages keeps fieldwork feasible."))
  add(d %in% c("srs_list", "stratified_list"), "You have a full list and no security or budget limits, so units can be drawn directly from the list.")
  add(a$stratify && a$probability && !d %in% c("site_all", "pilot"),
      paste0("Results are reported by ", paste(decision_group_names(a, a$strata_groups), collapse = " and "), ", so these are used as strata."))
  add(a$frame %in% c("unit_list", "cluster_sizes") && d %in% c("cluster", "stratified_cluster"), "Village counts are available, so villages can be selected by PPS.")
  r
}

#' Alternatives, and when to switch to them
decision_alternatives <- function(d, a) {
  alt <- character(0)
  add <- function(cond, text) if (isTRUE(cond)) alt <<- c(alt, text)
  add(d %in% c("cluster", "stratified_cluster") && a$frame == "unit_list",
      "If every village is accessible and travel is cheap, a systematic sample from the list sorted by location is more precise.")
  add(d %in% c("cluster", "stratified_cluster") && a$frame == "unit_list",
      "If the household list in a selected village turns out to be wrong, list the households on site, or use segmentation.")
  add(d %in% c("cluster", "stratified_cluster") && a$frame != "unit_list",
      "If listing every household is too slow in large villages, divide the village into segments of similar size, select one at random and list only that segment.")
  add(d %in% c("srs_list", "stratified_list"), "If units turn out to be spread over many villages, or some areas become inaccessible, switch to a two-stage cluster sample.")
  add(d == "site_census", "If the number of sites grows, or some cannot be reached, select sites at random instead (two-stage cluster sample).")
  add(d == "site_all", "If there are more sites than the team can visit, select a random sample of sites, stratified by province or component.")
  add(d == "site_sample", "If only a few sites remain, visit all of them.")
  add(d == "phone_list", "If many people have no working number, add in-person visits for a random subsample of them (mixed mode).")
  add(d == "rdd", "If a beneficiary or partner contact list exists, sample from it instead: coverage is better and costs are lower.")
  add(d == "mixed_mode", "If almost everyone has a working phone, a phone-only sample is simpler.")
  add(d %in% c("purposive", "purposive_groups"), "If you need percentages for the population, add a survey with a random sample, and select KII and FGD sites from its villages.")
  add(d == "hidden_group", "If results only need to be indicative, a snowball sample is quicker.")
  add(a$probability && !d %in% c("site_all", "hidden_group"),
      "Add KIIs or FGDs in some of the sampled villages to explain the survey results.")
  alt
}

#' Warnings for the answers given
decision_warnings <- function(d, a) {
  w <- character(0)
  add <- function(cond, text) if (isTRUE(cond)) w <<- c(w, text)
  add(!a$probability || d == "hidden_group",
      "Without random selection, results describe the people interviewed, not the population, and have no margin of error.")
  add("list_outdated" %in% a$constraints && a$probability,
      "Coverage: the list is incomplete or out of date, so results only describe the units on it. Update the list first, or add an area sample for the missing parts, and report the gap.")
  add(a$probability && a$mode %in% c("phone", "mixed") && d != "mixed_mode",
      "Coverage: phone samples miss households without a phone, and many women have no phone of their own. State this gap in the report.")
  add("insecure" %in% a$constraints && a$probability,
      "Insecure areas: draw reserve villages or sites in advance, record every inaccessible one with the reason, and report it as a coverage gap. Never replace them without a record.")
  add("women" %in% a$constraints || "gender" %in% a$groups,
      "Women: plan female enumerators (or female phone operators), and decide in advance who is interviewed in each household (e.g. the household head and an adult woman, or a random adult with a Kish grid).")
  add("gender" %in% a$groups && a$probability && a$frame != "unit_list",
      "Gender is not on a village list, so it cannot be a stratum. To report by gender, interview both a man and a woman per household, or screen for female-headed households, and size the sample for the smaller group.")
  add(a$stratify && a$probability,
      "Each reporting group needs enough interviews of its own: use \"Each stratum\" in the Sample Calculator.")
  add(d == "rdd",
      "Afghanistan: there is no public list of allocated number ranges, many women have no phone, and coverage is lower in rural areas. Weight the results, or use a contact list where one exists.")
  add(a$purpose == "verify" && a$frame %in% c("none", "cluster_list") && a$unit == "sites",
      "Verification needs the full site or sub-project list from the implementing partner. Ask for it before sampling, and do not let the partner choose the sites.")
  add(a$frame == "none" && a$probability && d %in% c("cluster", "stratified_cluster"),
      "No list: the village list and the household listing are part of the fieldwork plan and budget.")
  if (a$purpose == "change") {
    add(a$comparison == "before_after",
        "Before-and-after without a comparison group shows change, but cannot show that the programme caused it.")
    add(a$comparison %in% c("groups_endline", "groups_both") && a$random == "yes",
        "Random assignment: assign whole villages (not households) to each group, record the seed, and keep enough villages per group (20 or more is safer).")
    add(a$comparison %in% c("groups_endline", "groups_both") && a$random == "no",
        "No random assignment: pick comparison villages that are similar to programme villages (same districts, size and access), check that the groups are similar at baseline, and report this as a limitation.")
    add(a$comparison == "groups_endline" && a$random == "no",
        "Endline-only comparison without random assignment is weak: differences may have existed before the programme. Collect a baseline if possible.")
    add(identical(a$panel, "yes"),
        "Panel: record how to find each household again (village, landmarks, a contact number kept offline), and plan for attrition in the Evaluation Calculator.")
  }
  w
}

#' Guideline articles for the field protocols that apply
decision_protocols <- function(d, a) {
  ids <- DESIGNS[[d]]$article
  if (a$probability && !d %in% c("hidden_group")) ids <- c(ids, "replacement_protocol")
  if (a$mode %in% c("phone", "mixed")) ids <- c(ids, "phone_monitoring")
  if (a$purpose == "verify") ids <- c(ids, "site_verification")
  ids <- c(ids, "quality_control")
  unique(ids)
}

#' Settings handed to the calculators and the Sample Generator (NULL when the
#' design has no sample to calculate or draw)
decision_handover <- function(d, a) {
  des <- DESIGNS[[d]]
  calc <- NULL
  eval_design <- decision_evaluation_design(a)
  clustered <- d %in% c("cluster", "stratified_cluster")
  if (!is.null(eval_design)) {
    calc <- list(target = "evaluation", design = eval_design, clustered = clustered, m = 10, icc = 0.06)
  } else if (!is.na(des$calculator)) {
    method <- switch(d, srs_list = "simple_random", site_census = "stratified", mixed_mode = "simple_random",
                     phone_list = if (a$stratify) "stratified" else "simple_random",
                     site_sample = if (a$stratify) "stratified" else "simple_random", rdd = "simple_random", des$method)
    calc <- list(target = "sample", method = method, cluster_size = 10, icc = 0.06,
                 non_response = switch(d, phone_list = 20, mixed_mode = 15, rdd = 30, 5),
                 precision_target = if (d == "site_census" && !a$stratify) "total" else "per_stratum")
  }
  gen <- NULL
  if (!is.na(des$method) && !(a$frame %in% c("none", "unlistable") && !clustered)) {
    stratified <- a$stratify || decision_has_groups(a)
    method <- switch(d, site_sample = , phone_list = , srs_list = if (stratified) "stratified" else des$method,
                     cluster = if (stratified) "stratified_cluster" else "cluster", des$method)
    gen <- list(method = method,
                cluster_list = clustered && a$frame != "unit_list",
                pps = clustered && a$frame %in% c("unit_list", "cluster_sizes"),
                strata = if (d == "site_census") "site (village)"
                         else if (a$stratify) decision_group_names(a, a$strata_groups) else character(0))
  }
  list(calculator = calc, generator = gen)
}

#' Recommend a sampling design
#'
#' @param answers List of answer codes (see DECISION_CHOICES); reporting and
#'   constraints may hold several codes. Questions that do not apply are ignored.
#'
#' @return NULL until the purpose is answered; otherwise a list with design
#'   (id in DESIGNS), stages, reasons, alternatives, warnings, protocols
#'   (guideline article ids), evaluation design (or NULL), handover and the
#'   answers used
recommend_design <- function(answers) {
  if (is.null(answers$purpose) || !nzchar(answers$purpose)) return(NULL)
  a <- decision_facts(answers)
  d <- NULL
  for (rule in DESIGN_RULES) if (isTRUE(rule$when(a))) { d <- rule$design; break }
  stages <- decision_stages(d, a)
  handover <- decision_handover(d, a)
  if (decision_has_groups(a)) {
    stages <- c("Sample the programme (treatment) and comparison areas separately, as two strata, with the sample per group from the Evaluation Calculator.",
                stages)
    if (!is.null(handover$generator)) handover$generator$strata <- c("treatment or comparison group", handover$generator$strata)
  }
  list(design = d, stages = stages, reasons = decision_reasons(d, a),
       alternatives = decision_alternatives(d, a), warnings = decision_warnings(d, a),
       protocols = decision_protocols(d, a), evaluation = decision_evaluation_design(a),
       handover = handover, answers = a, applicable = decision_applicable(a))
}

#' TRUE when an evaluation compares treatment and comparison groups
decision_has_groups <- function(a) {
  a$purpose == "change" && a$comparison %in% c("groups_endline", "groups_both")
}
