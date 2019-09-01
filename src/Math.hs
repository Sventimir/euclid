module Math (
    quadratic
) where


-- tuple holds coefficients of equation: ax^2 + bx + c = 0
quadratic :: (Floating a, Ord a) => (a, a, a) -> [a]
quadratic (a, b, c) =
        let delta = b ** 2 - 4 * a * c in
        case compare delta 0 of
            LT -> []
            EQ -> [ (-b) / (2 * a) ]
            GT -> [ (-b + d) / (2 * a) | d <- [sqrt delta, -(sqrt delta)] ]
