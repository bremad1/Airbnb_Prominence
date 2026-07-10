# GOOD RESULT: LTM review-count matched Panel B

Filter:
`ex_super=f`; no `ex_super2` filter; keep single-listing host-quarter observations with `abs(ltm_scr - number_of_reviews_ltm) <= 2`.

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
This keeps hosts for which the scraped host-level LTM review count is close to the Inside Airbnb LTM review count. The single-listing restriction makes the host-level scraped count equivalent to the listing-level count, avoiding multi-listing host coverage mismatch.
