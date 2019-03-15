
## Data `nba2018-salary-points.RData`

Here's the description of the R objects in `nba2018-salary-points.RData`:

- `player`: name of the player.
- `team`: team name abbreviation.
- `position`: player position.
- `age`: age (in years).
- `experience`: years of experience.
- `salary`: salary (in dollars).
- `scored`: total scored points.
- `points1`: number of free throws, worth 1 point each.
- `points2`: number of 2-point field goals, worth 2 points each.
- `points3`: number of 3-point field goals, worth 3 points each.

There are five types of player positions (see [wikipedia](https://en.wikipedia.org/wiki/Basketball_positions) for more details):

+ `PG`: point guard
+ `SG`: shooting guard
+ `SF`: small forward
+ `PF`: power forward
+ `C`: center

The values in `scored` result from adding all scored points:

```r
points1 + (2 * points2) + (3 * points3)
```

Although each object has its own data type, you can think of each of them as a variable from a statistics standpoint like so:

| Object       | Variable     |
|:-------------|:-------------|
| `player`     | categorical  |
| `team`       | categorical  |
| `position`   | categorical  |
| `age`        | quantitative |
| `experience` | quantitative |
| `salary`     | quantitative |
| `scored`     | quantitative |
| `points1`    | quantitative |
| `points2`    | quantitative |
| `points3`    | quantitative |
