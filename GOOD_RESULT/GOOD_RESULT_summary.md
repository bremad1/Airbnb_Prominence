# GOOD RESULT

Filter:
`ex_super2=t`, `ex_super=f`, `host_identity_verified=t`, `last_review within 90 days`, and `availability_365 not in {0,365}`.

Panel B FULL raw N: 484.

Conventional FULL estimates:

| Spec | Coef | SE | p |
|---|---:|---:|---:|
| (1) msetwo + time FE | -0.056 | 0.071 | 0.434 |
| (2) double MSE + time FE | -0.033 | 0.048 | 0.496 |
| (3) h=(0.2,0.1) + time FE | -0.050 | 0.064 | 0.437 |
| (4) h=(0.3,0.15) + time FE | -0.011 | 0.045 | 0.805 |
| (5) h=(0.4,0.2) + time FE | -0.022 | 0.040 | 0.583 |
| (6) msetwo, no time FE | -0.078 | 0.060 | 0.190 |

This is the FULL Panel B candidate with all six conventional coefficients negative.

## Unsold Discount Proxy Candidates

Saved two additional GOOD RESULT candidates:

- `drop availability_365=365`: excludes fully open annual calendars, proxying weak-demand or unsold listings.
- `drop number_of_reviews_ltm=0`: excludes listings with no last-twelve-month reviews.

Both use the base filter `ex_super2=t`, `ex_super=f`, `host_identity_verified=t`, and Panel B FULL.

## LTM Review-Count Matched Panel B

Saved:
`good_result_ltm_scr_current_ltm_within2_single_listing_no_exsuper2`

Filter:
`ex_super=f`; no `ex_super2`; keep single-listing host-quarter observations with `abs(ltm_scr - number_of_reviews_ltm) <= 2`.

Panel B FULL raw N: 18483.
Kept N: 6525.
Usable N: 6475.
Unique hosts: 3105.

Conventional FULL estimates:

| Spec | Price coef | Price SE | Price p | First stage | First-stage SE | First-stage p |
|---|---:|---:|---:|---:|---:|---:|
| (1) | -0.045 | .026 | .084 | .287 | .039 | .000 |
| (2) | -0.023 | .019 | .226 | .340 | .035 | .000 |
| (3) | -0.108 | .047 | .023 | .263 | .047 | .000 |
| (4) | -0.044 | .027 | .099 | .321 | .036 | .000 |
| (5) | -0.034 | .024 | .154 | .331 | .033 | .000 |
| (6) | -0.055 | .029 | .059 | .281 | .040 | .000 |

Rationale:
This keeps hosts for which the scraped host-level LTM review count is close to the Inside Airbnb listing-level LTM review count. The single-listing restriction makes the host-level scraped count equivalent to listing-level count and avoids multi-listing host coverage mismatch.
