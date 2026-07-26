# Panel B first-column RD: trimming grid

The regression matches column (1) of the paper table: pooled FULL sample,
time fixed effects, fuzzy RD at 4.75, triangular kernel, side-specific
MSE-optimal bandwidths, and listing-clustered standard errors. Results use
`rdrobust` 3.0.0 to match the existing paper table.

Price trimming is applied symmetrically within each month before lag prices
are reconstructed. Price-change trimming is applied symmetrically within each
quarter to `(avg_price - ex_avg) / ex_avg`. A zero denotes no trimming.

| Price trim | Change trim | Panel B N | Estimate (SE) | p-value | RD-window N |
|---:|---:|---:|---:|---:|---:|
| 0% | 0% | 20,948 | 0.060* (0.034) | 0.0730 | 7,459 |
| 0% | 1% | 20,426 | 0.000 (0.030) | 0.9888 | 7,528 |
| 0% | 5% | 18,483 | 0.090*** (0.026) | 0.00039 | 6,827 |
| 1% | 0% | 20,327 | 0.078** (0.031) | 0.0116 | 7,301 |
| 1% | 1% | 19,895 | 0.056** (0.026) | 0.0284 | 7,491 |
| 1% | 5% | 18,023 | 0.108*** (0.024) | 0.000007 | 6,684 |
| 5% | 0% | 18,010 | 0.089*** (0.032) | 0.00542 | 6,872 |
| 5% | 1% | 17,587 | 0.063** (0.028) | 0.0273 | 6,536 |
| 5% | 5% | 15,928 | 0.092*** (0.025) | 0.00028 | 5,994 |

The `(price trim = 0%, change trim = 5%)` reconstruction exactly matches the
saved quarterly data: 32,777 total observations and 18,483 Panel B
observations. It also reproduces the paper-table coefficient (0.090).

## Panel B additionally restricted to `ex_super2 == "t"`

| Price trim | Change trim | Restricted N | Estimate (SE) | p-value | RD-window N |
|---:|---:|---:|---:|---:|---:|
| 0% | 0% | 1,327 | -0.065 (0.091) | 0.4756 | 566 |
| 0% | 1% | 1,297 | 0.015 (0.075) | 0.8449 | 546 |
| 0% | 5% | 1,181 | 0.024 (0.057) | 0.6748 | 440 |
| 1% | 0% | 1,304 | -0.084 (0.087) | 0.3351 | 549 |
| 1% | 1% | 1,269 | -0.005 (0.070) | 0.9389 | 495 |
| 1% | 5% | 1,154 | 0.027 (0.055) | 0.6238 | 431 |
| 5% | 0% | 1,128 | -0.090 (0.112) | 0.4247 | 460 |
| 5% | 1% | 1,101 | -0.050 (0.084) | 0.5540 | 418 |
| 5% | 5% | 1,004 | -0.005 (0.064) | 0.9352 | 379 |

Six of the nine point estimates are negative after applying `ex_super2`, but
none is statistically significant. The saved-paper baseline `(0%, 5%)`
reproduces the existing Ex2 estimate, 0.024 (SE 0.057), with 1,181 raw
restricted observations and 440 observations inside the selected bandwidth.

## Bias-corrected results for the five retained trimming cells

The bias-corrected coefficient is paired with the robust standard error and
robust p-value, following the paper's robust table convention.

| Price trim | Change trim | Conventional | Bias-corrected / robust | Robust p-value |
|---:|---:|---:|---:|---:|
| 0% | 0% | -0.065 (0.091) | -0.067 (0.104) | 0.519 |
| 1% | 0% | -0.084 (0.087) | -0.087 (0.100) | 0.384 |
| 1% | 1% | -0.005 (0.070) | -0.005 (0.078) | 0.951 |
| 5% | 0% | -0.090 (0.112) | -0.114 (0.130) | 0.380 |
| 5% | 1% | -0.050 (0.084) | -0.062 (0.096) | 0.518 |

## LTM and rating filter sweep

The five retained trimming cells were crossed with 20 filters: LTM cutoffs
1--5, rating cutoffs 4.0--4.9, and `LTM >= 5` combined with selected rating
cutoffs. This produced 100 first-column RD regressions.

No specification produced a negative estimate significant at 10% under
either conventional inference or bias-corrected/robust inference. The best
negative candidates were:

| Price trim | Change trim | Filter | N | Conventional | Bias-corrected / robust |
|---:|---:|---|---:|---:|---:|
| 1% | 0% | rating >= 4.7 | 1,019 | -0.150 (0.105), p=0.151 | -0.159 (0.117), p=0.175 |
| 5% | 0% | rating >= 4.8 | 656 | -0.237 (0.199), p=0.234 | -0.280 (0.229), p=0.221 |
| 0% | 0% | rating >= 4.7 | 1,035 | -0.121 (0.114), p=0.288 | -0.130 (0.130), p=0.318 |

LTM-only filters did not improve precision or significance. Combining
`LTM >= 5` with rating filters reduced the sample further and moved estimates
toward zero.

## Expanded exploratory search

An expanded search crossed the five retained trimming cells with 75 filters
(375 regressions). Filters were based on pre-treatment or activity variables:
rating, LTM counts, scraped-vs-listing LTM agreement, host listing count,
identity verification, availability, recent reviews, response rate, and
cumulative reviews.

Cross-source discrepancy filters are excluded from the retained candidate set.
In particular, results based on `abs(ltm_scr - number_of_reviews_ltm)` are not
used because they could be interpreted as relying on errors or inconsistencies
in the separately scraped data.

The strongest retained candidate uses Inside Airbnb variables only:

- Price trim: 5%
- Price-change trim: 0%
- Panel B and `ex_super2 == "t"`
- `ex_quarter_number_of_reviews >= 30`
- Raw restricted N: 561; selected RD-window N: 248
- Conventional: -0.142 (SE 0.071), p=0.0473
- Bias-corrected / robust: -0.152 (SE 0.080), p=0.0591
- First stage: 0.481 (SE 0.093), p<0.001

The coefficient remains negative in every leave-one-quarter-out run, but its
significance is not stable across quarter omissions.

### Effect-size plausibility rule

Because a price-change effect larger than 15 percentage points is considered
implausibly large, a stricter screening rule requires both conventional and
bias-corrected estimates to have absolute magnitude below 0.15. Under this
rule, no clean negative specification is significant at 10% under either
conventional or robust inference. The 5%-price-trim, zero-change-trim,
`ex_quarter_number_of_reviews >= 30` result is only a boundary case: its
conventional estimate is -0.142, but its bias-corrected estimate is -0.152.

### Quantile-defined review and recency restrictions

To avoid arbitrary absolute cutoffs, review-count and recency thresholds were
defined within quarter from the `ex_super2 == "t"` reference population.
Tested rules retained observations above the first quartile or median of prior
cumulative reviews and/or below the first quartile or median of prior-quarter
days since last review. Recency was measured at the previous quarter's final
snapshot, before current-quarter treatment assignment.

The only raw significant quantile result was `reviews >= quarter median` under
price trim 5% and price-change trim 0%: conventional -0.159 (SE 0.076,
p=0.037) and bias-corrected/robust -0.181 (SE 0.085, p=0.034). It violates the
absolute-effect bound under both estimators and is excluded.

Among quantile specifications satisfying the 0.15 bound under both estimators,
the closest result was price trim 5%, no price-change trim, reviews above the
quarter median, and prior recency below the quarter median: conventional
-0.102 (SE 0.065, p=0.120) and bias-corrected/robust -0.093 (SE 0.073,
p=0.205). None of the bounded quantile specifications is significant at 10%.

After excluding discrepancy-based filters, three negative specifications have
raw p<0.10 under both conventional and robust inference. The other two use
single-listing-host restrictions, with one additionally requiring prior rating
at least 4.7. None survives BH-FDR correction across the full search family.
The retained results must therefore be described as exploratory unless
confirmed on a held-out sample or justified independently of the search.
