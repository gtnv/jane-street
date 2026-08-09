-- Andy remembers every turn but home has no labeled edges. Map the 4 white
-- sphere faces onto the floor: 1 home face + 3 others. While his turns still
-- say "not home", the floor walk is stuck on the 6 whites around one black
-- hexagon. Each next step goes left, right, or exits, all 1/3. Only exiting
-- where he entered finds the real pheromones; any other exit proves hes not on
-- the sphere. Find that exact bad-exit chance.

type Probability = Rational

-- Removing home from the tetrahedron's K4 leaves a triangle. On the floor
-- that triangle lifts twice around a black hexagon, which is the 6-cycle here.

-- Let qd be the chance of eventually exiting at the entry cell when currently
-- distance d around the ring. Reflection leaves only d = 0, 1, 2, 3:
--   3q0 = 1 + 2q1,  3q1 = q0 + q2,
--   3q2 = q1 + q3,  3q3 = 2q2.
-- The missing 1 in the last three equations is exiting somewhere wrong.
q3OverQ2, q2OverQ1, q1OverQ0 :: Probability
q3OverQ2 = 2 / 3
q2OverQ1 = 1 / (3 - q3OverQ2)
q1OverQ0 = 1 / (3 - q2OverQ1)

indistinguishableProbability :: Probability
indistinguishableProbability =
  1 / (3 - 2 * q1OverQ0)

discoveryProbability :: Probability
discoveryProbability = 1 - indistinguishableProbability

showProbability :: Probability -> String
showProbability = map replacePercent . filter (/= ' ') . show
  where
    replacePercent '%' = '/'
    replacePercent character = character

main :: IO ()
main = putStrLn $ "answer: " ++ showProbability discoveryProbability
