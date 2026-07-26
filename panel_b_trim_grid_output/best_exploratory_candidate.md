# Bounded-effect Panel B assessment

Preferred screening rule:

```text
Use no cross-source discrepancy filters.
Require both the conventional and bias-corrected estimates to be negative.
Require abs(conventional estimate) < 0.15.
Require abs(bias-corrected estimate) < 0.15.
```

No clean specification satisfying this effect-size rule is significant at
10% under either conventional or robust inference.

The previously reported `ex_quarter_number_of_reviews >= 30` specification
with price trim 5% and price-change trim 0% is a boundary case:

| Inference | Coefficient | SE | p-value | Passes bound? |
|---|---:|---:|---:|---:|
| Conventional | -0.142 | 0.071 | 0.0473 | Yes |
| Bias-corrected / robust | -0.152 | 0.080 | 0.0591 | No |

The strongest candidate that satisfies the bound under both estimators is the
same review restriction with price trim 1% and price-change trim 0%:

| Inference | Coefficient | SE | p-value |
|---|---:|---:|---:|
| Conventional | -0.088 | 0.062 | 0.160 |
| Bias-corrected / robust | -0.085 | 0.071 | 0.229 |

It is not statistically significant. A significant bounded-effect result
cannot be supported by the clean specification set currently examined.

Quantile-defined review and prior-recency restrictions do not change that
conclusion. The closest bounded quantile specification yields conventional
-0.102 (SE 0.065, p=0.120) and bias-corrected/robust -0.093 (SE 0.073,
p=0.205).
