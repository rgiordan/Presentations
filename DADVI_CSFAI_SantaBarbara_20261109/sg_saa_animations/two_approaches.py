"""SG vs SAA on a 1-D, slightly non-quadratic objective with a single minimum.

Four animations:
  1. draws     single panel: true F and random draws F(eta; z) appearing one at a time
  2. sg        SG animated (left), static SAA panel (right)
  3. saa       SAA animated (right), static SG panel showing the last iterate (left)
  4. ensemble  randomness of SG final iterates (left) vs SAA minima (right)

Usage:  python3 two_approaches.py [outdir]
Writes  outdir/<name>/frame_###.png  and  <name>.gif  for each animation.

Exact objective:  F(eta) = 0.5 (eta^2 + 1) + C exp(B eta + B^2/2)
Random draw:      F(eta; z) = F(eta) + A0 z0 + A1 z1 (eta - PIV) + A2 z2 (eta - PIV)^2 / 2,
                  z = (z0, z1, z2) ~ N(0, I_3)   (so E F(eta; z) = F(eta))
"""
import os
import sys
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from PIL import Image
from matplotlib.backends.backend_pdf import PdfPages

# ----------------------------- parameters -----------------------------------
C, B = 0.4, 1.2                  # shape of F
A0, A1, A2, PIV = 0.5, 1.0, 0.25, -0.5   # size of the random perturbation
ZCLIP = 2.0                      # clip draws so curves stay on screen
ETA0 = -2.6                      # common starting point
N_INTRO_DRAWS = 8                # animation 1
SG_ALPHA, SG_ITERS = 0.25, 10    # animation 2: constant step size
N_SAA, SAA_ALPHA = 10, 0.25      # animation 3: draws, constant GD step on F-hat
N_ENS = 15                       # animation 4: replicates per method
SEED_INTRO, SEED_SG, SEED_SAA, SEED_ENS_SG, SEED_ENS_SAA = 5, 25, 31, 200, 100
XLIM, YLIM = (-3.2, 1.8), (-1.0, 8.0)
FIGSIZE_2 = (6.4, 3.0)           # two-panel animations (inches)
FIGSIZE_1 = (3.6, 3.0)           # single-panel animation
DPI, FPS = 150, 3
YLABEL = r"$F(\eta)$ and $F(\eta; z)$"

RED, BLUE, PURPLE, DRAW = "#d62728", "#1f3fbf", "#8e2a8e", "0.62"
SG_TITLE = ("Stochastic gradient (SG)", r"fresh $z_t$ every step, $N=1$")
SAA_TITLE = ("Sample average approximation (SAA)",
             rf"draw $z_1,\ldots,z_{{{N_SAA}}}$ once, then optimize $\hat F$")

plt.rcParams.update({
    "font.family": "sans-serif", "mathtext.fontset": "dejavusans",
    "font.size": 9, "axes.spines.top": False, "axes.spines.right": False,
})

# ----------------------------- objective ------------------------------------
def F(eta):   return 0.5 * (eta ** 2 + 1) + C * np.exp(B * eta + 0.5 * B * B)
def dF(eta):  return eta + C * B * np.exp(B * eta + 0.5 * B * B)
def d2F(eta): return 1.0 + C * B * B * np.exp(B * eta + 0.5 * B * B)
def f(eta, z):   return F(eta) + A0 * z[0] + A1 * z[1] * (eta - PIV) + 0.5 * A2 * z[2] * (eta - PIV) ** 2
def df(eta, z):  return dF(eta) + A1 * z[1] + A2 * z[2] * (eta - PIV)
def d2f(eta, z): return d2F(eta) + A2 * z[2]

def draws(seed, n):
    return np.clip(np.random.default_rng(seed).standard_normal((n, 3)), -ZCLIP, ZCLIP)

def newton(g, h, x=0.0):
    for _ in range(100):
        x -= g(x) / h(x)
    return x

eta_star = newton(dF, d2F)
grid = np.linspace(*XLIM, 400)

def run_sg(zs):
    eta = [ETA0]
    for z in zs:
        eta.append(eta[-1] - SG_ALPHA * df(eta[-1], z))
    return eta

def saa_parts(zs):
    Fh = lambda e: np.mean([f(e, z) for z in zs], axis=0)
    dFh = lambda e: np.mean([df(e, z) for z in zs])
    d2Fh = lambda e: np.mean([d2f(e, z) for z in zs])
    return Fh, dFh, d2Fh

# ----------------------------- precomputed runs -----------------------------
intro_z = draws(SEED_INTRO, N_INTRO_DRAWS)

sg_z = draws(SEED_SG, SG_ITERS)
sg_eta = run_sg(sg_z)

saa_z = draws(SEED_SAA, N_SAA)
Fhat, dFhat, d2Fhat = saa_parts(saa_z)
saa_eta = [ETA0]
while True:
    step = SAA_ALPHA * dFhat(saa_eta[-1])
    saa_eta.append(saa_eta[-1] - step)
    if abs(step) < 0.01 or len(saa_eta) > 40:
        break
eta_hat = saa_eta[-1]
N_SAA_GD = len(saa_eta) - 1

ens_sg = [run_sg(draws(SEED_ENS_SG + r, SG_ITERS)) for r in range(N_ENS)]
ens_saa = []
ens_saa_h = []
for r in range(N_ENS):
    Fh, dFh, d2Fh = saa_parts(draws(SEED_ENS_SAA + r, N_SAA))
    ens_saa.append((Fh, newton(dFh, d2Fh, eta_star)))
    ens_saa_h.append(d2Fh(ens_saa[-1][1]))     # curvature of F-hat at its minimum

# ----------------------------- drawing helpers ------------------------------
def base_axes(ax, title=None, subtitle=None):
    ax.set_xlim(*XLIM); ax.set_ylim(*YLIM)
    ax.set_xlabel(r"$\eta$", labelpad=-8, loc="right")
    ax.set_ylabel(YLABEL, labelpad=2)
    ax.set_yticks([])
    ax.set_xticks([eta_star]); ax.set_xticklabels([r"$\eta^*$"])
    if title:
        ax.set_title(title, fontsize=10, loc="left", pad=16)
        ax.text(0, 1.03, subtitle, transform=ax.transAxes, fontsize=8, color="0.3")
    ax.plot(grid, F(grid), color="k", lw=2.0, zorder=3)
    ax.axvline(eta_star, color="0.5", lw=0.6, ls=":", zorder=0)

def tangent(ax, x0, y0, slope, half=0.8, **kw):
    xs = np.array([x0 - half, x0 + half])
    ax.plot(xs, y0 + slope * (xs - x0), **kw)

def arrow(ax, x0, y0, x1, y1):
    ax.annotate("", xy=(x1, y1), xytext=(x0, y0),
                arrowprops=dict(arrowstyle="->", color=BLUE, lw=1.3), zorder=5)

def dot(ax, x, y, **kw):
    ax.plot([x], [y], "o", color=kw.pop("color", BLUE), ms=kw.pop("ms", 5), zorder=6, **kw)

def counter(ax, t):
    ax.text(0.96, 0.97, f"$t = {t}$", transform=ax.transAxes, ha="right", va="top", color="0.4")

def mark_min(ax, x, y, strong):
    a = 1.0 if strong else 0.8
    ax.plot([x], [y], "o", color=PURPLE, ms=4 if strong else 3, alpha=a, zorder=7)
    ax.plot([x, x], [YLIM[0], YLIM[0] + 0.35], color=PURPLE, lw=1.4, alpha=a, zorder=7)

# ----------------------------- animation 1: draws ---------------------------
N1 = N_INTRO_DRAWS + 1

def frame_draws(ax, k):
    base_axes(ax)
    for i in range(k):
        newest = (i == k - 1)
        ax.plot(grid, f(grid, intro_z[i]), color=DRAW, lw=1.0 if newest else 0.8,
                alpha=1.0 if newest else 0.55, zorder=2)

# ----------------------------- animation 2/3: SG ----------------------------
SG_SUB = 3                       # per iteration: new draw, gradient, step
N_SG = 1 + SG_SUB * SG_ITERS

def frame_sg(ax, k):
    base_axes(ax, *SG_TITLE)
    if k == 0:
        dot(ax, ETA0, F(ETA0)); return
    t, sub = divmod(k - 1, SG_SUB)
    past = sg_eta[:t]
    ax.plot(past, F(np.array(past)), "o", color=BLUE, ms=2.5, alpha=0.25, zorder=3)
    e, z = sg_eta[t], sg_z[t]
    ax.plot(grid, f(grid, z), color=DRAW, lw=1.3, zorder=2)
    ax.plot([e, e], [F(e), f(e, z)], color=BLUE, lw=0.6, ls=":", zorder=4)
    ax.plot([e], [f(e, z)], "o", mfc="white", mec=BLUE, ms=4, zorder=5)
    if sub >= 1:
        tangent(ax, e, f(e, z), df(e, z), color=BLUE, lw=1.6, alpha=1 if sub == 1 else 0.35, zorder=4)
    if sub == 2:
        e_new = sg_eta[t + 1]
        arrow(ax, e, F(e), e_new, F(e_new))
        dot(ax, e, F(e), ms=3, alpha=0.4)
        e = e_new
    dot(ax, e, F(e))
    counter(ax, t + 1)

def static_sg_last(ax):
    base_axes(ax, *SG_TITLE)
    past = sg_eta[:-1]
    ax.plot(past, F(np.array(past)), "o", color=BLUE, ms=2.5, alpha=0.25, zorder=3)
    dot(ax, sg_eta[-1], F(sg_eta[-1]))
    counter(ax, SG_ITERS)

# ----------------------------- animation 3: SAA -----------------------------
N_SAA_FRAMES = N_SAA + 2 + 2 * N_SAA_GD + 1   # draws, average, guess, GD, converged

def frame_saa(ax, k):
    base_axes(ax, *SAA_TITLE)
    n_shown = min(k + 1, N_SAA)
    for i, z in enumerate(saa_z[:n_shown]):
        newest = (k < N_SAA and i == n_shown - 1)
        ax.plot(grid, f(grid, z), color=DRAW, lw=1.2 if newest else 0.8,
                alpha=1.0 if newest else 0.45, zorder=2)
    if k < N_SAA:
        return
    ax.plot(grid, Fhat(grid), color=RED, lw=1.8, ls="--", zorder=4)
    j = k - N_SAA
    if j == 0:
        return
    if j == 1:
        dot(ax, ETA0, Fhat(ETA0)); return
    t, sub = divmod(j - 2, 2)
    past = saa_eta[:t]
    ax.plot(past, Fhat(np.array(past)), "o", color=BLUE, ms=2.5, alpha=0.3, zorder=5)
    if t >= N_SAA_GD:
        ax.axvline(eta_hat, color=PURPLE, lw=0.9, zorder=0)
        ax.text(eta_hat - 0.06, YLIM[1] * 0.75, r"$\hat\eta$", color=PURPLE, fontsize=10, ha="right")
        dot(ax, eta_hat, Fhat(eta_hat))
        return
    e = saa_eta[t]
    tangent(ax, e, Fhat(e), dFhat(e), color=BLUE, lw=1.6, alpha=1 if sub == 0 else 0.35, zorder=5)
    if sub == 1:
        e_new = saa_eta[t + 1]
        arrow(ax, e, Fhat(e), e_new, Fhat(e_new))
        e = e_new
    dot(ax, e, Fhat(e))
    counter(ax, t + 1)

# ----------------------------- animation 4: ensembles -----------------------
N4 = N_ENS + 1                   # one replicate per frame, then a hold frame

def frame_ens_sg(ax, j):
    base_axes(ax, "SG: final iterate, independent runs", rf"constant $\alpha$, $T={SG_ITERS}$ steps each")
    n = min(j + 1, N_ENS)
    for r in range(n):
        newest = (r == n - 1 and j < N_ENS)
        if newest:
            path = ens_sg[r]
            ax.plot(path, F(np.array(path)), "-o", color=BLUE, lw=0.6, ms=2, alpha=0.35, zorder=4)
        e = ens_sg[r][-1]
        mark_min(ax, e, F(e), newest)

def frame_ens_saa(ax, j):
    base_axes(ax, r"SAA: $\hat\eta$, independent draw sets", rf"$N={N_SAA}$ draws each")
    n = min(j + 1, N_ENS)
    for r in range(n):
        newest = (r == n - 1 and j < N_ENS)
        Fh, m = ens_saa[r]
        ax.plot(grid, Fh(grid), color=RED, ls="--", lw=1.6 if newest else 1.0,
                alpha=1.0 if newest else 0.5, zorder=4)
        mark_min(ax, m, Fh(m), newest)

# ----------------------------- animation 5: SAA curvature -------------------
QUAD_HALF = 1.6                  # half-width of the plotted quadratic approximation

def frame_saa_curv(ax, j):
    base_axes(ax, r"SAA: $\hat\eta$ and local curvature", rf"$N={N_SAA}$ draws each; purple = quadratic at first $\hat\eta$")
    n = min(j + 1, N_ENS)
    for r in range(n):
        newest = (r == n - 1 and j < N_ENS)
        Fh, m = ens_saa[r]
        ax.plot(grid, Fh(grid), color=RED, ls="--", lw=1.6 if newest else 1.0,
                alpha=1.0 if newest else 0.5, zorder=4)
        mark_min(ax, m, Fh(m), newest)
    Fh, m = ens_saa[0]           # quadratic approximation for the first replicate only
    xs = np.linspace(m - QUAD_HALF, m + QUAD_HALF, 200)
    ax.plot(xs, Fh(m) + 0.5 * ens_saa_h[0] * (xs - m) ** 2, color=PURPLE, lw=1.8, zorder=5)

# ----------------------------- rendering ------------------------------------
_PDF = {"pages": None, "reps": 1}   # multipage-PDF sink for the LaTeX `animate` package

def _finish(fig, path):
    fig.savefig(path, dpi=DPI)
    if _PDF["pages"] is not None:
        for _ in range(_PDF["reps"]):        # holds are baked in by repeating pages
            _PDF["pages"].savefig(fig)
    plt.close(fig)

def save_two(path, left, right):
    fig, (a1, a2) = plt.subplots(1, 2, figsize=FIGSIZE_2)
    left(a1); right(a2)
    fig.subplots_adjust(left=0.05, right=0.99, bottom=0.09, top=0.83, wspace=0.12)
    _finish(fig, path)

def save_one(path, draw):
    fig, ax = plt.subplots(figsize=FIGSIZE_1)
    draw(ax)
    fig.subplots_adjust(left=0.09, right=0.98, bottom=0.09, top=0.97)
    _finish(fig, path)

def save_one_titled(path, draw):
    fig, ax = plt.subplots(figsize=FIGSIZE_1)
    draw(ax)
    fig.subplots_adjust(left=0.09, right=0.98, bottom=0.09, top=0.83)
    _finish(fig, path)

ANIMATIONS = {
    # name: (n_frames, render(path, k), {frame index: hold ms})
    "1_draws": (N1, lambda p, k: save_one(p, lambda ax: frame_draws(ax, k)), {0: 1500}),
    "2_sg": (N_SG, lambda p, k: save_two(p, lambda ax: frame_sg(ax, k),
                                         lambda ax: base_axes(ax, *SAA_TITLE)), {0: 1500}),
    "3_saa": (N_SAA_FRAMES, lambda p, k: save_two(p, static_sg_last, lambda ax: frame_saa(ax, k)),
              {N_SAA - 1: 1200, N_SAA: 1500}),
    "4_ensemble": (N4, lambda p, k: save_two(p, lambda ax: frame_ens_sg(ax, k),
                                             lambda ax: frame_ens_saa(ax, k)), {}),
    "5_saa_curvature": (N4, lambda p, k: save_one_titled(p, lambda ax: frame_saa_curv(ax, k)), {}),
}

if __name__ == "__main__":
    out = sys.argv[1] if len(sys.argv) > 1 else "frames"
    only = sys.argv[2:] or list(ANIMATIONS)
    for name in only:
        n, render, holds = ANIMATIONS[name]
        d = os.path.join(out, name); os.makedirs(d, exist_ok=True)
        for old in os.listdir(d):
            os.remove(os.path.join(d, old))
        durs = [1000 // FPS] * n
        for i, ms in holds.items():
            durs[i] = ms
        durs[-1] = 3000
        paths = []
        with PdfPages(f"{name}.pdf") as pdf:
            _PDF["pages"] = pdf
            for k in range(n):
                _PDF["reps"] = max(1, round(durs[k] * FPS / 1000))
                p = os.path.join(d, f"frame_{k:03d}.png"); render(p, k); paths.append(p)
            _PDF["pages"] = None
        imgs = [Image.open(p).convert("RGB") for p in paths]
        imgs[0].save(f"{name}.gif", save_all=True, append_images=imgs[1:], duration=durs, loop=0)
        print(f"{name}: {n} frames")
    sg_fin = np.array([r[-1] for r in ens_sg]) - eta_star
    saa_fin = np.array([m for _, m in ens_saa]) - eta_star
    print(f"eta*={eta_star:.3f}  SG final offset mean {sg_fin.mean():+.3f} sd {sg_fin.std():.3f}"
          f"  SAA offset mean {saa_fin.mean():+.3f} sd {saa_fin.std():.3f}")
