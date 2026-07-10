# GOOD RESULT: review-count matched Panel B

Filter:
`ex_super=f`; no `ex_super2` filter; keep single-listing host-quarter observations where the scraped host LTM review count (`ltm_scr`) exactly equals Inside Airbnb's previous-quarter LTM review count (`ex_quarter_ltm`).

Panel B FULL raw N: 18483.
Kept N: 3996.
Unique hosts: 2223.

Conventional FULL estimates:

| Spec | Price coef | Price SE | Price p | First stage | First-stage SE | First-stage p |
|---|---:|---:|---:|---:|---:|---:|
| (1) | -0.064 | .036 | .073 | -- | -- | -- |
| (2) | -0.018 | .031 | .564 | -- | -- | -- |
| (3) | -0.149 | .052 | .005 | .304 | .047 | .000 |
| (4) | -0.060 | .035 | .089 | .314 | .044 | .000 |
| (5) | -0.039 | .034 | .251 | .304 | .042 | .000 |
| (6) | -0.083 | .040 | .038 | -- | -- | -- |

Raw listing-level scraped review files were not present in the checkout, so this is the strongest exact-match filter available from the saved RData. It is closest to a listing-level match because the retained observations are single-listing hosts.
