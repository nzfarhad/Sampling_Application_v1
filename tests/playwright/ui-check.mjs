// UI check for the ATR Sampling Tool, using Playwright with the installed Microsoft Edge.
//
// 1. Start the app:   Rscript -e "shiny::runApp(port = 8787)"
// 2. Run the check:   npm --prefix tests/playwright run check
//
// Visits every tab at desktop and phone width, uses each tab's main control, and checks
// that the footer sits below the content, nothing overlaps, there is no horizontal scroll,
// no output shows an error and the browser console has no errors. Screenshots go to
// tests/playwright/screenshots/.

import { chromium } from "playwright";
import { mkdirSync, writeFileSync, rmSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";

const here = path.dirname(fileURLToPath(import.meta.url));
const shots = path.join(here, "screenshots");
const url = process.env.APP_URL || "http://127.0.0.1:8787";
mkdirSync(shots, { recursive: true });

const problems = [];
const report = (where, msg) => problems.push(`${where}: ${msg}`);

const browser = await chromium.launch({ channel: process.env.PW_CHANNEL || "msedge" });

// Wait until Shiny is idle (no busy flag, no recalculating outputs)
async function settle(page) {
  // Outputs on hidden tabs can stay "recalculating", so only visible ones count
  await page.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()
    && !document.documentElement.classList.contains("shiny-busy")
    && [...document.querySelectorAll(".recalculating")].every((el) => el.offsetParent === null),
    null, { timeout: 30000 });
  await page.waitForTimeout(400);
}

async function openTab(page, name) {
  const link = page.locator(`#main_navbar a.nav-link[data-value="${name}"]`);
  const toggle = page.locator("nav button[data-bs-toggle='collapse']");
  // At phone width the menu is collapsed behind the toggle button
  if (!(await link.isVisible())) {
    await toggle.click();
    await link.waitFor({ state: "visible" });
  }
  await link.click();
  await settle(page);
  // Close the phone menu again so it does not cover the page
  if (await page.locator(".navbar-collapse.show").count()) {
    await toggle.click();
    await page.waitForTimeout(500);
  }
}

// Decision Tree answers: radio buttons and check boxes by input name and value
async function answer(page, question, value) {
  await page.locator(`input[name="decision_tree-${question}"][value="${value}"]`).check();
  await settle(page);
}

async function expectTab(page, label, name) {
  const active = (await page.locator("#main_navbar a.nav-link.active").getAttribute("data-value")) || "";
  if (active !== name) report(label, `expected the ${name} tab, but "${active}" is open`);
}

// Layout checks on the active tab
async function checkLayout(page, where) {
  const res = await page.evaluate(() => {
    const out = { issues: [] };
    const visible = (el) => { const r = el.getBoundingClientRect(); const s = getComputedStyle(el);
      return r.width > 0 && r.height > 0 && s.visibility !== "hidden" && s.display !== "none"; };
    const pane = document.querySelector(".tab-pane.active");
    const footer = document.querySelector("footer, .bslib-page-navbar > .navbar ~ div:last-child .border-top, .border-top.text-muted");
    const docRect = (el) => { const r = el.getBoundingClientRect(); return { top: r.top + scrollY, bottom: r.bottom + scrollY, left: r.left, right: r.right }; };

    // Horizontal scroll
    if (document.documentElement.scrollWidth > window.innerWidth + 2) {
      out.issues.push(`page is ${document.documentElement.scrollWidth}px wide for a ${window.innerWidth}px window`);
    }

    // Footer: present, version only, below all content of the active tab
    const f = [...document.querySelectorAll("div")].find((d) => /ATR Sampling Tool - version/.test(d.textContent) && d.children.length === 0);
    if (!f) {
      out.issues.push("footer not found");
    } else {
      out.footer = f.textContent.trim();
      if (/commit/i.test(f.textContent)) out.issues.push("footer shows the commit");
      const fr = docRect(f);
      const content = pane ? [...pane.querySelectorAll("*")].filter(visible) : [];
      const lowest = Math.max(0, ...content.map((el) => docRect(el).bottom));
      if (fr.top < lowest - 1) out.issues.push(`footer (top ${Math.round(fr.top)}) overlaps content ending at ${Math.round(lowest)}`);
      const docH = document.documentElement.scrollHeight;
      if (docH > window.innerHeight && fr.bottom < docH - 40) out.issues.push(`footer is not at the end of the page (bottom ${Math.round(fr.bottom)} of ${docH})`);
    }

    // Overlapping siblings among cards, form groups and layout columns
    if (pane) {
      const boxes = [...pane.querySelectorAll(".card, .form-group, .wiki-sidebar, .wiki-content, .btn, .dataTables_wrapper, .plotly")].filter(visible);
      for (let i = 0; i < boxes.length; i++) {
        for (let j = i + 1; j < boxes.length; j++) {
          const a = boxes[i], b = boxes[j];
          if (a.contains(b) || b.contains(a)) continue;
          const r1 = a.getBoundingClientRect(), r2 = b.getBoundingClientRect();
          const w = Math.min(r1.right, r2.right) - Math.max(r1.left, r2.left);
          const h = Math.min(r1.bottom, r2.bottom) - Math.max(r1.top, r2.top);
          if (w > 4 && h > 4) {
            const label = (el) => `${el.tagName.toLowerCase()}.${[...el.classList].slice(0, 2).join(".")}${el.id ? "#" + el.id : ""} "${(el.innerText || "").trim().slice(0, 30)}"`;
            out.issues.push(`overlap ${Math.round(w)}x${Math.round(h)}px: ${label(a)} / ${label(b)}`);
            if (out.issues.length > 25) return out;
          }
        }
      }
      // Visible error outputs (validation messages excluded)
      for (const e of pane.querySelectorAll(".shiny-output-error")) {
        if (visible(e) && !e.classList.contains("shiny-output-error-validation")) out.issues.push(`output error: ${e.textContent.trim().slice(0, 80)}`);
      }
    }
    return out;
  });
  res.issues.forEach((i) => report(where, i));
  return res;
}

async function runAt(viewport, label) {
  const context = await browser.newContext({ viewport, acceptDownloads: true });
  const page = await context.newPage();
  page.on("response", (r) => { if (r.status() >= 400) report(`${label} network`, `${r.status()} ${r.url()}`); });
  page.on("console", (m) => {
    // Failed requests are reported with their URL by the response handler
    if (m.type() === "error" && !/Failed to load resource/.test(m.text())) report(`${label} console`, m.text().slice(0, 160));
  });
  page.on("pageerror", (e) => report(`${label} page error`, e.message.slice(0, 160)));
  await page.goto(url);
  await settle(page);

  const shot = async (name) => page.screenshot({ path: path.join(shots, `${label}-${name}.png`), fullPage: true });

  // How to Use
  await openTab(page, "How to Use");
  if (!(await page.locator("#user_guide-content_area").innerText()).includes("A Typical Workflow")) report(label, "guide: Getting Started not shown");
  await page.locator("#user_guide-toc_container .toc-item", { hasText: "Sample Generator" }).click();
  await settle(page);
  if (!(await page.locator("#user_guide-content_area").innerText()).includes("Frame Checks")) report(label, "guide: TOC click did not open Sample Generator");
  const emoji = await page.locator("#user_guide-content_area").evaluate((el) => /[\u{1F300}-\u{1FAFF}\u{2600}-\u{27BF}]/u.test(el.innerText));
  if (emoji) report(label, "guide: emoji in content");
  const res = await checkLayout(page, `${label} How to Use`);
  await shot("1-how-to-use");

  // Decision Tree: a PDM survey by province in insecure areas
  await openTab(page, "Decision Tree");
  if (await page.locator("#decision_tree-design_name").count()) report(label, "decision tree: recommendation shown before the purpose is answered");
  await answer(page, "purpose", "level");
  await answer(page, "reporting", "area");
  await answer(page, "constraints", "insecure");
  const design = (await page.locator("#decision_tree-design_name").innerText()).trim();
  if (design !== "Stratified two-stage cluster sample") report(label, `decision tree: PDM design is "${design}"`);
  await checkLayout(page, `${label} Decision Tree`);
  await shot("2-decision-tree");

  // Sample Calculator: SRS, then stratified
  await openTab(page, "Sample Calculator");
  await page.locator("#sample_calculator-calculate").click();
  await settle(page);
  const n = (await page.locator("#sample_calculator-sample_size_result").innerText()).trim();
  if (n !== "390") report(label, `calculator: default SRS result is "${n}", expected 390`);
  await checkLayout(page, `${label} Sample Calculator`);
  await shot("3-sample-calculator");

  // Evaluation Calculator
  await openTab(page, "Evaluation Calculator");
  const head = await page.locator("#evaluation_calculator-headline").innerText();
  if (!/660/.test(head)) report(label, `evaluation: headline is "${head.replace(/\s+/g, " ").slice(0, 80)}"`);
  await checkLayout(page, `${label} Evaluation Calculator`);
  await shot("4-evaluation-calculator");

  // Evaluation: a mean (FCS 40, SD 12, change 3 -> 430 per group with the default clustering)
  const ev = (id) => page.locator(`#evaluation_calculator-${id}`);
  await page.locator('input[name="evaluation_calculator-outcome"][value="mean"]').check();
  await settle(page);
  const meanHead = await ev("headline").innerText();
  if (!/430/.test(meanHead)) report(label, `evaluation mean: headline is "${meanHead.replace(/\s+/g, " ").slice(0, 80)}"`);
  if (!/is a mean/.test(await ev("explanation").innerText())) report(label, "evaluation mean: methods text does not describe a mean");
  await shot("4b-evaluation-mean");

  // Several indicators: the default table's proportion (0.30, -8 points) needs 810 and sets the sample; then subgroups
  await page.locator('input[name="evaluation_calculator-indicator_count"][value="several"]').check();
  await settle(page);
  const sevHead = await ev("headline").innerText();
  if (!/810/.test(sevHead) || !/poor food consumption/.test(sevHead)) {
    report(label, `evaluation indicators: headline is "${sevHead.replace(/\s+/g, " ").slice(0, 120)}"`);
  }
  // Each indicator says what its entries mean; the same change stated as % of baseline gives the same sample
  const preview2 = await ev("ind_2_preview").innerText();
  if (!preview2.includes("0.3 → 0.22: a decrease of 8 percentage points")) report(label, `evaluation: indicator 2 preview is "${preview2}"`);
  await ev("ind_2_scale").selectOption("relative");
  await ev("ind_2_size").fill("26.6667");
  await ev("ind_2_size").press("Tab");
  await settle(page);
  const relHead = await ev("headline").innerText();
  if (!/810/.test(relHead)) report(label, `evaluation: % of baseline changed the sample: "${relHead.replace(/s+/g, " ").slice(0, 80)}"`);
  if (!(await ev("ind_2_preview").innerText()).includes("0.3 → 0.22")) report(label, "evaluation: preview does not follow the % of baseline entry");
  await ev("ind_2_scale").selectOption("absolute");
  await ev("ind_2_size").fill("0.08");
  await ev("ind_2_size").press("Tab");
  await settle(page);
  await ev("use_subgroups").check();
  await settle(page);
  const details = await ev("detail_tables").innerText();
  if (!details.includes("Key Indicators") || !details.includes("Female-headed households")) report(label, "evaluation: indicator or subgroup table missing");
  await checkLayout(page, `${label} Evaluation Calculator (indicators, subgroups)`);
  await shot("4c-evaluation-indicators");
  if (label === "desktop") {
    // Fetch the PDF from the download link (a browser download leaves a tab that breaks context.close)
    const pdf = await ev("download_pdf").evaluate(async (el) => {
      const r = await fetch(el.href);
      const b = new Uint8Array(await r.arrayBuffer());
      return { ok: r.ok, size: b.length, head: String.fromCharCode(...b.slice(0, 4)) };
    });
    if (!pdf.ok || pdf.head !== "%PDF" || pdf.size < 10000) report(label, `evaluation: PDF download failed (${JSON.stringify(pdf)})`);
  }
  await ev("use_subgroups").uncheck();
  await page.locator('input[name="evaluation_calculator-indicator_count"][value="one"]').check();
  await page.locator('input[name="evaluation_calculator-outcome"][value="proportion"]').check();
  await settle(page);

  // Sample Generator: upload a small frame, generate, confirm
  await openTab(page, "Sample Generator");
  const csv = path.join(here, "frame.tmp.csv");
  const rows = ["id,province,village"];
  for (let i = 1; i <= 600; i++) rows.push(`${i},P${(i % 3) + 1},P${(i % 3) + 1}_V${i % 20}`);
  writeFileSync(csv, rows.join("\n"));
  await page.locator("#sampling_generator-dataset_file").setInputFiles(csv);
  await settle(page);
  await page.locator("#sampling_generator-sample_size").fill("60");
  await page.locator("#sampling_generator-generate_sample").click();
  await settle(page);
  const modal = page.locator(".modal-dialog");
  if (await modal.isVisible()) {
    await shot("5a-generator-frame-check");
    await page.locator("#sampling_generator-confirm_draw").click();
    await settle(page);
  } else {
    report(label, "generator: frame-check dialog did not open");
  }
  rmSync(csv, { force: true });
  await checkLayout(page, `${label} Sample Generator`);
  await shot("5-sample-generator");

  // Sampling Guideline: search
  await openTab(page, "Sampling Guideline");
  await page.locator("#wiki-search_input").fill("reserve_order");
  await settle(page);
  const toc = await page.locator("#wiki-toc_container").innerText();
  if (!toc.includes("Replacement Protocol")) report(label, "guideline: search did not find Replacement Protocol");
  await checkLayout(page, `${label} Sampling Guideline`);
  await shot("6-sampling-guideline");
  await page.locator("#wiki-search_input").fill("");
  await settle(page);

  // Decision Tree hand-over (last, because it changes the other tabs' settings)
  await openTab(page, "Decision Tree");
  await page.locator("#decision_tree-to_calculator").click();
  await settle(page);
  await expectTab(page, label, "Sample Calculator");
  if (await page.locator("#sample_calculator-sampling_method").inputValue() !== "stratified_cluster") report(label, "hand-over: calculator method not set");
  if (!(await page.locator("#sample_calculator-handover_note").innerText()).includes("Settings from the Decision Tree")) report(label, "hand-over: no note on the Sample Calculator");
  if (await page.locator("#sample_calculator-results_container").isVisible()) report(label, "hand-over: old calculator results still shown");
  await page.locator("#sample_calculator-calculate").click();
  await settle(page);
  if (!(await page.locator("#sample_calculator-sample_size_explanation").innerText()).includes("Stratified Cluster")) report(label, "hand-over: calculation does not use the new method");
  await checkLayout(page, `${label} Sample Calculator (hand-over)`);
  await shot("7-handover-calculator");

  await openTab(page, "Decision Tree");
  await page.locator("#decision_tree-to_generator").click();
  await settle(page);
  await expectTab(page, label, "Sample Generator");
  if (await page.locator("#sampling_generator-sampling_method").inputValue() !== "stratified_cluster") report(label, "hand-over: generator method not set");

  await openTab(page, "Decision Tree");
  await answer(page, "purpose", "change");
  await answer(page, "comparison", "groups_both");
  await answer(page, "panel", "yes");
  await page.locator("#decision_tree-to_calculator").click();
  await settle(page);
  await expectTab(page, label, "Evaluation Calculator");
  if (await page.locator("#evaluation_calculator-design").inputValue() !== "did_panel") report(label, "hand-over: evaluation design not set");
  await shot("8-handover-evaluation");

  await openTab(page, "Decision Tree");
  await page.locator("#decision_tree-recommendation_area .dt-article", { hasText: "Replacement Protocol" }).click();
  await settle(page);
  await expectTab(page, label, "Sampling Guideline");
  if (!(await page.locator("#wiki-content_area").innerText()).includes("Replacement Protocol")) report(label, "decision tree: protocol link did not open the article");

  await context.close();
  return res.footer;
}

const footer = await runAt({ width: 1366, height: 860 }, "desktop");
await runAt({ width: 390, height: 844 }, "phone");
await browser.close();

console.log(`Footer text: ${footer}`);
console.log(`Screenshots: ${shots}`);
if (problems.length) {
  console.log(`\n${problems.length} problem(s):`);
  problems.forEach((p) => console.log(" - " + p));
  process.exitCode = 1;
} else {
  console.log("No problems found.");
}
