# SG vs SAA animations — handoff notes

**To resume with Claude:** upload this zip (or its contents) and say something like
*"Here are my SG vs SAA animations for the DADVI talk; read README.md, then I want to tweak …"*.
Optionally also upload the talk PDF (`main.pdf`) for context.

## Context

Animations for the Beamer talk *Black Box Variational Inference with a Deterministic Objective*
(Giordano, Ingram, Broderick; CSFAI 2026), illustrating the "Two approaches" slide (slide 6):

- **SG (stochastic gradient, ADVI):** draw a fresh z every step (N = 1), take a gradient step.
- **SAA (sample average approximation, DADVI):** draw z_1..z_N once, then optimize the fixed,
  deterministic average F̂.

Beamer frame is 4:3 (362.8 × 272.1 pt). Slide colour conventions: red = draws, blue = steps,
purple = convergence.

## Files

| File | What it is |
|---|---|
| `two_approaches.py` | The only source. All parameters at the top. |
| `animations_example.tex` | Minimal Beamer deck using all four animations (tested with pdflatex). |
| `key_frames.png` | One representative frame from each animation (current look). |
| `1_draws.pdf` … `4_ensemble.pdf` | Current exports: one page per frame, for `\animategraphics`. |

Run: `python3 two_approaches.py frames` (all) or `python3 two_approaches.py frames 2_sg 4_ensemble` (some).
Writes `frames/<name>/frame_###.png`, `<name>.gif` (preview) and `<name>.pdf` (for LaTeX).
Needs numpy, matplotlib, Pillow.

## The math (1-D toy problem)

- True objective (slightly non-quadratic, single minimum):
  `F(η) = 0.5(η² + 1) + C·exp(Bη + B²/2)`, with C = 0.4, B = 1.2; η* ≈ −0.525.
- Random draw, z = (z0, z1, z2) ~ N(0, I₃), clipped to ±2:
  `F(η; z) = F(η) + A0·z0 + A1·z1·(η − PIV) + A2·z2·(η − PIV)²/2`, with A0, A1, A2, PIV = 0.5, 1.0, 0.25, −0.5.
  Smooth, convex, and E[F(η; z)] = F(η) exactly, so draws are "centered" on F.
- Both methods start at η0 = −2.6 and use constant step sizes (it's an illustration).

## The four animations (as specified)

1. **`1_draws`** — *single panel* (not side by side). Black F, then gray draws F(η; z) appear one at a time (8 draws).
2. **`2_sg`** — SG animated on the left; right is a *static* SAA panel showing only F.
   Initial guess, then per iteration: one new gray draw → blue gradient (tangent of that draw) → step.
   Only the current draw is shown. Constant α = 0.25, **10 steps**, settles near η*.
3. **`3_saa`** — SAA animated on the right; left is a *static* SG panel showing the last iterate (faint trail of earlier iterates).
   Ten gray draws appear one at a time → red dashed average F̂ → initial guess →
   plain gradient descent (constant α = 0.25) on the fixed F̂ → converged; η̂ marked in purple.
4. **`4_ensemble`** — emphasizes that *both* are random, but SAA's randomness is easier to characterize.
   Left: final iterates of 15 independent SG runs (each newest run's path flashes in faint blue).
   Right: 15 independent SAA replicates — each a red dashed F̂ with its minimum marked.
   Purple dots on the curves plus purple ticks on the η axis show where the solutions land, around η*.

## Style decisions already made

- Y-axis label on every panel: `F(η) and F(η; z)` (no y ticks). X axis: only an η* tick, η label at right.
- True F: solid black. Draws: smooth gray. Averages F̂: red dashed. Iterates/gradients/steps: blue. Minima: purple.
- **No** "draw / gradient / step / initial guess" text captions on frames (removed on request).
  Kept: iteration counter `t = …`, the η̂ label, panel titles/subtitles.
- Python/matplotlib pipeline chosen over a Node/browser version.

## Exporting to LaTeX

```latex
\usepackage{animate}
...
\animategraphics[width=\textwidth,controls=all,autoplay]{3}{2_sg}{}{}
```
- `3` = FPS (matches `FPS` in the script). Empty `{}{}` = use all pages of the multipage PDF.
- Pauses are baked in by repeating PDF pages (the `holds` dict in `ANIMATIONS`; last frame holds 3 s).
- Use `step` instead of `autoplay` to advance frames by clicking.
- Plays in Adobe Acrobat/Reader and Okular; most other viewers (Preview, browsers, Skim) show a still frame.

## Open items / possible next tweaks

- **Text size:** two-panel figures are 6.4 in wide, so at `\textwidth` they shrink to ~2/3 (labels ≈ 6 pt).
  Fix: set `FIGSIZE_2` / `FIGSIZE_1` to the actual width used on the slide.
- **Fonts:** math labels use matplotlib's DejaVu, not the Beamer font. Could switch to PGF export so LaTeX typesets labels.
- **Ensemble spreads:** currently SG final iterates have sd ≈ 0.41 vs SAA minima sd ≈ 0.09, so frame 4 visually says
  "SAA is *less* random" rather than "*easier to characterize*". Options: smaller SG step / more SG steps,
  smaller N for SAA, or overlay SAA's asymptotic normal (M-estimator) distribution of η̂.
- Seeds were chosen so the first SG draw is on-screen at η0 (SEED_SG = 25) and the SAA η̂ is visibly offset from η* (SEED_SAA = 31).
